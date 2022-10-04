#' @export
#' @title Returns a list of columns to be included in a table.
#'
#' @description Takes in a starting and ending column as letters,
#' converts those letters into integers, creates a list, then converts
#' that list back into Excel column references.
#'
#' @param col_start Starting column for a table.
#' @param col_end Ending column for a table.
#'
#' @return A list of columns expressed as Excel column references.
#'
#' @examples
#' col_seq("A", "D")
#' col_seq("Z", "AC")
#' col_seq("BC", "BQ")
#'
col_seq <- function(col_start, col_end) {
  n_start <- col2int(col_start)
  n_end <- col2int(col_end)
  
  # Applies `int2col` against list from start number to end number
  col_list <- sapply(n_start:n_end, int2col)
  
  # Return list of columns
  col_list
}

#' @export
#' @title Wrap UIDs to multiple lines
#'
#' @description When UIDs are longer than 15 characters, adds spaces such that 
#' flextable will wrap content within cells to multiple lines. Splits
#' UIDs where there are periods between characters.
#' @param x String containing a UID to be split
#' @param length The maximum length of characters that a cell can be across,
#' e.g. max characters that each string chunk should be.
#'
#' @return A string with spaces inserted to allow for line wrapping.
#' @export
#'
uid_wrap <- function(x, length = 15) {
  # If string is less than the max length allowed, return string as is.
  if (stringr::str_length(x) < length) {return(x)}
  
  # If string is over the limit, add spaces after periods
  # TODO: In reviewing this, it doesn't actually seem to be dynamic with
  # the `length` parameter.
  x |>
    stringr::str_extract_all(pattern = ".{1,20}(\\.|$)", simplify = TRUE) |>
    paste(collapse = " ")
}

# TODO embedding one code chunk within others
# The below code was an attempt to create a reusable code chunk called `schema_tables`
# that could be embedded inside other code chunks to reduce repeatedly writing the
# same code over and over again to create schema tables.
# I never quite got it working, but this could be a way to clean up the code quite a bit.
# See: https://bookdown.org/yihui/rmarkdown-cookbook/reuse-chunks.html#embed-chunk

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
