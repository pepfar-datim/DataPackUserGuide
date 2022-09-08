#' @export
#' @title Create flextable from schema data
#'
#' @description Creates and properly formats a flextable table
#' using input data from a \code{datapackr} Data Pack schema.
#'
#' @param t Table of schema data to be used.
#' @param section Name to be placed atop the section.
#'
#' @return Formatted flextable table.
#'
make_table <- function(t, section = NULL) {
  t |>
    as.data.frame() |>
    flextable::flextable() |>
    flextable::align(part = "all", align = "center") |>
    flextable::align(j = 1, part = "body", align = "right") |>
    flextable::void(j = 1, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::bg(part = "header", bg = "#E6DFA7") |>
    flextable::bg(part = "body",
                  bg = rep(c("#FFFFFF", "#F2F3F4"), length.out = NROW(t))) |>
    flextable::hline_top(
      part = "header",
      border = officer::fp_border(color = "#D7DBDD", width = 2)) |>
    flextable::hline_bottom(
      part = "header",
      border = officer::fp_border(color = "#D7DBDD", width = 2)) |>
    flextable::border(
      border = officer::fp_border(color = "#E5E7E9", width = 1)) |>
    flextable::fontsize(size = 10) |>
    flextable::set_table_properties(layout = "fixed", width = 1) |>
    flextable::autofit() |>
    flextable::flextable_to_rmd(ft.split = FALSE,
                                bookdown = TRUE)
}

#' @export
#' @title Prepare schema data for table creation
#'
#' @description Takes in a sheet name and a list of columns and plucks
#' this information from the Data Pack schema. Then pivots and prepares this
#' data for use in a table in the User Guide.
#'
#' @param sheet_name The Data Pack sheet that is being referenced in the table.
#' @param columns A list of columns to be included in the table.
#'
#' @return A table references the sheet and columns specified.
#'
prepare_table_data <- function(sheet_name = NULL, columns = NULL) {

  schema <- userguide_schema()

  data <- schema %>%
    {if(sheet_name == "Spectrum") dplyr::select(schema, -UID) else .} %>%
    dplyr::filter(`Sheet Name` == sheet_name,
                  `Column` %in% columns) %>%
    dplyr::select(-`Sheet Name`, -`Column Number`) %>%
    dplyr::rowwise() %>%
    {if(sheet_name != "Spectrum") dplyr::mutate(., UID = uid_wrap(UID)) else .} %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = !Column) %>%
    tidyr::pivot_wider(id_cols = name, names_from = Column, values_from = value)
  
  data
}

# TODO: this function below used to be dependent on whether an HTML site or
# PDF document was being knit. In the HTML site, tables used to be one single
# table with a scrollbar at the bottom. Maintaining this code for two different
# types of tables became too much during COP22 but this would improve the user
# experience to reimplement at some point.

#' Break up long table into shorter sections
#'
#' @description Takes a long table with too many rows to fit across
#' on one page and breaks it up into several shorter tables of
#' appropriate length.
#'
#' @param t Table of schema data to be broken up.
#' @param max_cols Maximum number of rows across allowed.
#' @param is_html_knit Defines whether this is for a PDF
#' output or an HTML output.
#'
#' @return A list of tables broken up into approriate sized chunks
#'
table_seq <- function(t = NULL,
                      max_cols = 6,
                      is_html_knit = knitr::is_html_output()) {
  table_split <-
    split(2:NCOL(t), ceiling(seq_along(2:NCOL(t)) / max_cols)) |>
    lapply(function(x) {
      cbind(t[1], t[x])
    })
}
