# arrow: dplyr verbs run by Acero on Arrow tables.
list(
  packages = c("dplyr", "arrow"),
  fallback_pattern = "pulling data into R",
  # Where Package Manager has no binary, the R package is compiled and Arrow's
  # C++ library is downloaded prebuilt from Apache instead of built too.
  install = list(env = c(NOT_CRAN = "true", LIBARROW_BINARY = "true")),
  setup = c(
    "library(dplyr)",
    "df <- arrow::arrow_table(df)",
    "lookup <- arrow::arrow_table(lookup)"
  )
)
