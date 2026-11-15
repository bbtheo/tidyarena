# Count by day with as.Date().
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = "log |> mutate(day = as.Date(ts)) |> count(day)",
  mem_factor = 1
)
