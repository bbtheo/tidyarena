# Count by week (starting on Monday) with floor_date() on dates.
list(
  family = "datetime",
  part = 4L,
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(week = floor_date(date, "week", week_start = 1)) |>
  count(week)
)",
  mem_factor = 1
)
