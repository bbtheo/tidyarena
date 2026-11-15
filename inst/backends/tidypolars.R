# tidypolars: dplyr verbs run by Polars.
list(
  packages = c("dplyr", "polars", "tidypolars"),
  # Neither is on CRAN. polars' Rust library is downloaded prebuilt (and
  # checked against its sha256) rather than built, which would take long and
  # need a Rust toolchain.
  install = list(
    repos = c(
      rpolars = "https://rpolars.r-universe.dev",
      etiennebacher = "https://etiennebacher.r-universe.dev"
    ),
    env = c(NOT_CRAN = "true", LIBR_POLARS_BUILD = "false")
  ),
  setup = c(
    "library(dplyr)",
    "library(tidypolars)",
    "df <- polars::as_polars_df(df)",
    "lookup <- polars::as_polars_df(lookup)",
    ".collect <- function(x) tibble::as_tibble(as.data.frame(x))"
  )
)
