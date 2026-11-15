# arrow: dplyr verbs run by Acero on Arrow tables.
list(
  packages = c("dplyr", "arrow"),
  fallback_pattern = "pulling data into R",
  setup = c(
    "library(dplyr)",
    "df <- arrow::arrow_table(df)",
    "lookup <- arrow::arrow_table(lookup)"
  )
)
