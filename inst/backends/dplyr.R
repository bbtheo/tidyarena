# dplyr on tibbles: the anchor every other backend is compared with.
list(
  packages = "dplyr",
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "tibble::as_tibble(x)",
  setup = "library(dplyr)",
  # compute() on a tibble is the collected result: measured once.
  time_compute = FALSE
)
