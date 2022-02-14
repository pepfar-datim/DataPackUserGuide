html_table <- function(t, section = NULL) {
  t %<>%
    tibble::as_tibble() %>%
    tibble::column_to_rownames(var = "name") %>%
    knitr::kable(
      format = "html",
      escape = FALSE,
      booktabs = TRUE
    ) %>%
    kableExtra::kable_styling(font_size = 12) %>%
    kableExtra::column_spec(seq_len(NCOL(t)),
                            width = paste0(toString(7.5/NCOL(t)),
                                           "in")) %>%
    kableExtra::row_spec(0, background = "#E6DFA7", align = "c") %>%
    kableExtra::row_spec(1:{ifelse(sheet_name == "Spectrum", 7, 8)},
                         extra_css = "border:1px solid lightgrey;") %>%
    kableExtra::scroll_box(width = "100%")
  
  t
}

ft_table <- function(t, section = NULL) {
  t %<>%
    flextable::flextable() %>%
    flextable::align(part = "all", align = "center") %>%
    flextable::align(j = 1, part = "body", align = "right") %>%
    flextable::void(j = 1, part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::bg(part = "header", bg = "#E6DFA7") %>%
    flextable::bg(part = "body",
                  bg = rep(c("#FFFFFF", "#F2F3F4"), length.out = NROW(t))) %>%
    flextable::hline_top(
      part = "header",
      border = officer::fp_border(color = "#D7DBDD", width = 2)) %>%
    flextable::hline_bottom(
      part = "header",
      border = officer::fp_border(color = "#D7DBDD", width = 2)) %>%
    flextable::border(
      border = officer::fp_border(color = "#E5E7E9", width = 1)) %>%
    flextable::set_table_properties(width = 1, layout = "autofit") %>%
    flextable::fit_to_width(max_width = 1) %>%
    flextable::flextable_to_rmd()
  
  t
}

prepare_table_data <- function(sheet_name = NULL, columns = NULL) {
  
  schema <- userguide_schema()

  data <- schema %>%
    {if(sheet_name == "Spectrum") dplyr::select(schema, -UID) else .} %>%
    dplyr::filter(`Sheet Name` == sheet_name,
                  `Column` %in% columns) %>%
    dplyr::select(-`Sheet Name`, -`Column Number`) %>%
    dplyr::rowwise() %>%
    # dplyr::mutate(`Column Name` = name_wrap(`Column Name`)) %>%
    # {if(sheet_name != "Spectrum") dplyr::mutate(., UID = uid_wrap(UID)) else .} %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = !Column) %>%
    tidyr::pivot_wider(id_cols = name, names_from = Column, values_from = value)
  
  data
}

make_table <- function(t = NULL,
                       section = NULL,
                       is_html_knit = knitr::is_html_output()) {
  if (is_html_knit) {
    html_table(t, section)
  } else {
    ft_table(t, section)
  }
}

table_seq <- function(t = NULL,
                      is_html_knit = knitr::is_html_output()) {
    if (is_html_knit) {
      tables <- c(t)
    } else {
      tables <- split(2:NCOL(t), ceiling(seq_along(2:NCOL(t)) / 6)) %>%
        lapply(function(x) {
          cbind(t[1], t[x])
        })
    }
}

schema_table <- function(sheet_name = NULL,
                         section = NULL,
                         columns = NULL,
                         is_html_knit = knitr::is_html_output()) {
  print("Blank for now.")
}
