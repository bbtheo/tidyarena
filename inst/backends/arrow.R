# arrow: dplyr verbs run by Acero on Arrow tables.
list(
  packages = c("dplyr", "arrow"),
  fallback_pattern = "pulling data into R",
  # Where Package Manager has no binary, the R package is compiled and Arrow's
  # C++ library is downloaded prebuilt from Apache instead of built too.
  install = list(env = c(NOT_CRAN = "true", LIBARROW_BINARY = "true")),
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "arrow::arrow_table(x)",
  # Reads the Parquet file at `path` itself: a dataset, so every query scans
  # the file.
  read = "arrow::open_dataset(path)",
  setup = c(
    "library(dplyr)"
  )
)
