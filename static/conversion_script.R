library(rmarkdown)

rmarkdown::pandoc_convert(
  input = "COP21 Data Pack User Guide.docx",
  to = "markdown",
  from = "docx",
  output = "user_guide.md",
  options = "--extract-media=.",
  verbose = TRUE
)
