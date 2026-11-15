# Parse timestamps from strings with ymd_hms().
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(t = ymd_hms(ts_str)) |>
  summarise(first = min(t), last = max(t), n = n())
)",
  mem_factor = 1
)
