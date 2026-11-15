# Count by weekday and hour.
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = "log |> count(weekday = wday(ts, week_start = 1), hour = hour(ts))",
  mem_factor = 1
)
