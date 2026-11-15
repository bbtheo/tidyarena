# dplyr on tibbles: the anchor every other backend is compared with.
list(
  packages = "dplyr",
  setup = c(
    "library(dplyr)",
    "df <- tibble::as_tibble(df)",
    "lookup <- tibble::as_tibble(lookup)"
  )
)
