# dtplyr: dplyr code translated to data.table.
list(
  packages = c("dplyr", "dtplyr", "data.table"),
  setup = c(
    "library(dplyr)",
    "df <- dtplyr::lazy_dt(data.table::as.data.table(df))",
    "lookup <- dtplyr::lazy_dt(data.table::as.data.table(lookup))",
    # compute() on a lazy_dt only builds another step; as.data.table() runs
    # the translated data.table code.
    ".compute <- function(x) data.table::as.data.table(x)"
  )
)
