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

table_seq <- function(t = NULL,
                      max_cols = 6,
                      is_html_knit = knitr::is_html_output()) {
  table_split <-
    split(2:NCOL(t), ceiling(seq_along(2:NCOL(t)) / max_cols)) |>
    lapply(function(x) {
      cbind(t[1], t[x])
    })
}
