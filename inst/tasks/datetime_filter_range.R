# Filter on a timestamp range.
list(
  family = "datetime",
  part = 1L,
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  filter(ts >= as.POSIXct("2024-01-01", tz = "UTC"), ts < as.POSIXct("2024-07-01", tz = "UTC")) |>
  summarise(n = n(), total = sum(amount))
)",
  mem_factor = 1
)
