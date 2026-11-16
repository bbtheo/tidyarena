# Count by day with as.Date().
list(
  family = "datetime",
  part = 1L,
  data = "datetimes",
  packages = "lubridate",
  code = "log |> mutate(day = as.Date(ts)) |> count(day)",
  mem_factor = 1
)
