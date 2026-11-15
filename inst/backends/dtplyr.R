# dtplyr: dplyr code translated to data.table.
list(
  packages = c("dplyr", "dtplyr", "data.table"),
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "dtplyr::lazy_dt(data.table::as.data.table(x))",
  setup = c(
    "library(dplyr)",
    # compute() on a lazy_dt only builds another step; as.data.table() runs
    # the translated data.table code.
    ".compute <- function(x) data.table::as.data.table(x)"
  )
)
