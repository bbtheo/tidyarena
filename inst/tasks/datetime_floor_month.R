# Group by month with floor_date().
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(month = floor_date(ts, "month")) |>
  group_by(month) |>
  summarise(n = n(), total = sum(amount), .groups = "drop")
)",
  mem_factor = 1
)
