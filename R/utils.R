col2number <- function(x) {
  col_ref <- stringr::str_to_upper(x)
  digits <- stringr::str_length(col_ref)
  ls <- lapply(seq_len(digits), function(y) {
    stringr::str_sub(col_ref, y, y) %>%
      {(utf8ToInt(.) - utf8ToInt("A") + 1L) * (26**(digits - y))}
  })
  n <- Reduce(`+`, ls)
  return(n)
}

col_seq <- function(col_start, col_end) {
  n_start <- col2number(col_start)
  n_end <- col2number(col_end)
  
  # Applies `int2col` against list from start number to end number
  col_list <- sapply(n_start:n_end, int2col)
  
  # Return list of columns
  col_list
}

uid_wrap <- function(x, length = 15) {
  if (stringr::str_length(x) < length) {return(x)}
  x |>
    stringr::str_extract_all(pattern = ".{1,20}(\\.|$)", simplify = TRUE) |>
    paste(collapse = " ")
}

# ```{r, schema_tables, eval=FALSE}
# data <- prepare_table_data(sheet_name, columns)
# for (t in table_seq(data)) {
#   make_table(t, section)
# }
# ```
#
# ```{r echo=FALSE, results='asis'}
# sheet_name <- "Cascade"
# section <- "HTS_INDEX"
# columns <- col_seq("CQ", "CX")
# 
# <<schema_tables>>
# ```
#
# ```{r, cascade_diagnosedsubnat, echo=FALSE, results='asis'}
# sheet_name <- "Cascade"
# section <- "DIAGNOSED_SUBNAT"
# columns <- "CY"
#
# <<schema_tables>>
# ```
