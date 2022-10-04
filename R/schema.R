#' @export
#' @title Identify which Data Pack columns are inputs for other columns.
#'
#' @description Takes in a reference column on a given sheet of the Data Pack
#' and determines whether this data is linked to other columns
#' in the Data Pack. e.g. Is the data in this column used in formulas to calculate
#' the values of other columns throughout the Data Pack?
#'
#' @param sheet_name The Data Pack sheet that the reference column is on.
#' @param col The column in the Data Pack being referenced.
#'
#' @return Boolean as to whether the reference column is used in formulas of other columns.
#'
link_identification <- function(sheet_name, col) {
  # Hard coding for Spectrum tab since formulas and metadata for this tab are
  # not brought into the Data Pack schema by \code{datapackr}.
  if (sheet_name == "Spectrum") {
      if (col %in% c(4, 5, 7, 9, 11, 13, 14, 17)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

  # Pulls out relevant columns from schema, e.g. sheet name, column, and formula
  # This will be used to determine if the reference column is utilized in formulas
  # of other sheets.
  schema <-
    datapackr::pick_schema(cop_year = 2022, # TODO: Remove hard-coding COP year
                           tool = "Data Pack")[c("sheet_name",
                                                 "col",
                                                 "formula")] |>
    unique()

  # NOTE: Unsure what this code was doing so commenting out for now.
  # if (!is.na(schema[schema$sheet_name == sheet_name &&
  #                   schema$col == col,
  #                   names(schema) == "formula"])) {
  #   return(TRUE)
  # }

  # Converts integer value for Data Pack column into an Excel column reference
  col_ref <- openxlsx::int2col(col)

  # Searches for formulas on the same sheet that reference this column.
  same_sheet_refs <- schema[schema$sheet_name == sheet_name, ]["formula"] %>%
    {grepl(pattern = glue::glue("(?<!`{sheet_name}`\\!){col_ref}"),
           x = .,
           perl = TRUE) &
        grepl(pattern = glue::glue("(?<!`{sheet_name}`\\!\\$){col_ref}"),
              x = .,
              perl = TRUE)}

  # Search for formulas on other sheets that reference this column
  diff_sheet_refs <- schema[schema$sheet_name != sheet_name, ]["formula"] %>%
    {grepl(pattern = glue::glue("(?<=`{sheet_name}`\\!){col_ref}"),
           x = .,
           perl = TRUE) &
        grepl(pattern = glue::glue("(?<=`{sheet_name}`\\!\\$){col_ref}"),
              x = .,
              perl = TRUE)}

  # Determine whether this column is referenced either by columns on the same
  # sheet OR by columns on other sheets
  linked <- same_sheet_refs | diff_sheet_refs

  # Return boolean value
  linked
}

#' @export
#' @title Generate User Guide Schema
#'
#' @description Pulls the appropriate Data Pack schema
#' from the \code{datapackr} package for the given COP year and then
#' formats it to add relevant information for use in the User Guide.
#' 
#' The User Guide schema contains the following columns:
#'  * \code{Sheet Name}: The name of the Data Pack sheet
#'  * \code{Column}: The Excel reference for the column in the Data Pack.
#'  (e.g. "A", "C", "BQ")
#'  * \code{Column Number}: The numeric reference for the column in the
#'  Data Pack.
#'  * \code{Column Name}: The plain-English name for the column as written in
#'  Row C of the Data Pack Excel tool.
#'  * \code{UID}: The internal Data Pack UID as shown in column 14 of the Data
#'  Pack. This is also the internal UID referenced on the PSNUxIM tab.
#'  * \code{Column Type?}: Whether the column is an \code{assumption},
#'  \code{calculation} (e.g. determined by a formula),
#'  \code{past} (e.g. results data from a prior year),
#'  \code{reference} (e.g. a column not used in calculation but presented for
#'  reference), or \code{target} (e.g. a target from the current COP year or
#'  the COP year being planned for). 
#'  * \code{What type of data?}: Whether a column is an integer or percentage.
#'  * \code{Prepopulated data?}: Whether this data is pre-populated in the
#'  Data Pack for the user prior to them receiving it.
#'  * \code{Enter or modify data?}: Whether a user has permission to modify the
#'  data in this column. Can either be "Y", "N", or "?". If it is a question
#'  mark, then the user should consult DUIT prior to modifying the data
#'  in the column.
#'  * \code{Calculated column?}: Whether the data in the column is calculated
#'  by formula using data in other columns.
#'  * \code{Linked column?}: Whether the data in the column is used in the
#'  formulas of other columns to calculate the values of those columns.
#'
#' @return Schema table
#'
userguide_schema <- function() {

  # Hard codes certain data for the Spectrum tab since this is not in
  # the vanilla `datapackr` schema
  spectrum_schema <- tibble::tribble(
    ~sheet_name, ~col,          ~col_name,    ~col_type,  ~value_type,
    "Spectrum",     4,             "psnu", "assumption",     "string",
    "Spectrum",     5,         "psnu_uid", "assumption",     "string",
    "Spectrum",     6,          "area_id", "assumption",     "string",
    "Spectrum",     7,   "indicator_code", "assumption",     "string",
    "Spectrum",     8,  "dataelement_uid", "assumption",     "string",
    "Spectrum",     9,              "age", "assumption",     "string",
    "Spectrum",    10,          "age_uid", "assumption",     "string",
    "Spectrum",    11,              "sex", "assumption",     "string",
    "Spectrum",    12,          "sex_uid", "assumption",     "string",
    "Spectrum",    13, "calendar_quarter", "assumption",     "string",
    "Spectrum",    14,            "value", "assumption",    "integer",
    "Spectrum",    15,      "age_sex_rse", "assumption", "percentage",
    "Spectrum",    16,     "district_rse", "assumption", "percentage",
    "Spectrum",    17,               "ID", "assumption",     "string"
  )
  
  schema <-
    # Pulls vanilla schema from `datapackr`
    # TODO: Remove hard-coded COP year
    datapackr::pick_schema(cop_year = 2022, tool = "Data Pack") |>

    # Drops Home tab, Spectrum tab, and PSNUxIM tab
    # Also drops any header rows
    dplyr::filter(!sheet_name %in% c("Home", "Spectrum", "PSNUxIM"),
                  col_type != "row_header") %>%

    # Binds spectrum data created above back to the schema
    dplyr::bind_rows(spectrum_schema, .) |>

    # Creates certain information for usage in the User Guide
    dplyr::rowwise() |>
    dplyr::mutate(
      col_ref = openxlsx::int2col(col),
      col_num = col,
      prepopulated = dplyr::if_else(col_type == "past", "Y", "N"),
      # TODO: This calculation for `enter_or_modify` is broken, I'm pretty sure.
      # Not sure what happened here. However, if the "Green vs. Grey" column
      # information is brought into the schema this could be automated using 
      # that information (e.g. grey columns can't be edited, green columns can).
      enter_or_modify = dplyr::case_when(col_type == "past" ~ "?",
                                         col %in% c(8:9) ~ "Y",
                                         TRUE ~ "N"),
      calculated = dplyr::if_else(!is.na(formula) |
                                    (sheet_name == "Spectrum" &
                                       col_name == "ID"), "Y", "N"),
      linked = ifelse(link_identification(`sheet_name`, `col`), "Y", "N")) |>

    # Selects and renames columns that will be used throughout the User Guide
    dplyr::select(
      "Sheet Name" = sheet_name,
      "Column" = col_ref,
      "Column Number" = col_num,
      "Column Name" = col_name,
      "UID" = indicator_code,
      "Column Type?" = col_type,
      "What type of data?" = value_type,
      "Prepopulated data?" = prepopulated,
      "Enter or modify data?" = enter_or_modify,
      "Calculated column?" = calculated,
      "Linked column?" = linked)
  
  return(schema)
}
