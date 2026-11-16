# tidypolars: dplyr verbs run by Polars.
list(
  packages = c("dplyr", "polars", "tidypolars"),
  # Neither is on CRAN. They are installed from their latest GitHub
  # releases: R-universe builds polars from its development branch, which
  # would make results change between releases. polars' Rust library is
  # downloaded prebuilt (and checked against its sha256) rather than built,
  # which would take long and need a Rust toolchain.
  install = list(
    github = c(polars = "pola-rs/r-polars", tidypolars = "etiennebacher/tidypolars"),
    env = c(NOT_CRAN = "true", LIBR_POLARS_BUILD = "false")
  ),
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "polars::as_polars_df(x)",
  # Reads the Parquet file at `path` itself: a lazy frame, so every query
  # scans the file.
  read = "polars::pl$scan_parquet(path)",
  setup = c(
    "library(dplyr)",
    "library(tidypolars)",
    ".collect <- function(x) tibble::as_tibble(as.data.frame(x))",
    # Eager Polars data frames are computed already.
    ".compute <- function(x) if (inherits(x, \"polars_lazy_frame\")) dplyr::compute(x) else x"
  )
)
