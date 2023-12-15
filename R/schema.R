link_identification <- function(sheet_name, col) {
  if (sheet_name == "Spectrum") {
      if (col %in% c(4, 5, 7, 9, 11, 13, 14, 17)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }
  
  schema <- datapackr::pick_schema(cop_year = 2024, tool = "Data Pack") %>%
    .[c("sheet_name", "col", "formula")] %>%
    unique()

  if (!is.na(schema[schema$sheet_name == sheet_name &
                    schema$col == col,
                    names(schema) == "formula"])) {
    return(TRUE)
  }

  col_ref <- openxlsx::int2col(col)

  same_sheet_refs <- schema[schema$sheet_name == sheet_name, ]["formula"] %>%
    {grepl(pattern = glue::glue("(?<!`{sheet_name}`\\!){col_ref}"),
           x = .,
           perl = TRUE) &
        grepl(pattern = glue::glue("(?<!`{sheet_name}`\\!\\$){col_ref}"),
              x = .,
              perl = TRUE)}

  diff_sheet_refs <- schema[schema$sheet_name != sheet_name, ]["formula"] %>%
    {grepl(pattern = glue::glue("(?<=`{sheet_name}`\\!){col_ref}"),
           x = .,
           perl = TRUE) &
        grepl(pattern = glue::glue("(?<=`{sheet_name}`\\!\\$){col_ref}"),
              x = .,
              perl = TRUE)}

  linked <- same_sheet_refs | diff_sheet_refs
  
  linked
}

userguide_schema <- function() {
  
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
  
  schema <- datapackr::pick_schema(cop_year = 2024, tool = "Data Pack") %>%
    dplyr::filter(!sheet_name %in% c("Home", "Spectrum", "PSNUxIM"),
                  col_type != "row_header") %>%
    dplyr::bind_rows(spectrum_schema, .) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      col_ref = openxlsx::int2col(col),
      col_num = col,
      prepopulated = dplyr::if_else(col_type == "past", "Y", "N"),
      enter_or_modify = dplyr::case_when(col_type == "past" ~ "?",
                                         col %in% c(8:9) ~ "Y",
                                         TRUE ~ "N"),
      calculated = dplyr::if_else(!is.na(formula) |
                                    (sheet_name == "Spectrum" &
                                       col_name == "ID"), "Y", "N"),
      linked = ifelse(link_identification(`sheet_name`, `col`), "Y", "N")) %>%
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
