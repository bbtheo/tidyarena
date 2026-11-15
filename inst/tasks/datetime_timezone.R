# Convert to another time zone with with_tz() and count by local hour.
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(local = with_tz(ts, "Europe/Helsinki")) |>
  count(local_hour = hour(local))
)",
  mem_factor = 1
)
