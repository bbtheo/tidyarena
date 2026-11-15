# tidypolars: dplyr verbs run by Polars.
list(
  packages = c("dplyr", "polars", "tidypolars"),
  install_hint = "Install polars and tidypolars from R-universe: {.url https://rpolars.r-universe.dev}.",
  setup = c(
    "library(dplyr)",
    "library(tidypolars)",
    "df <- polars::as_polars_df(df)",
    "lookup <- polars::as_polars_df(lookup)",
    ".collect <- function(x) tibble::as_tibble(as.data.frame(x))"
  )
)
