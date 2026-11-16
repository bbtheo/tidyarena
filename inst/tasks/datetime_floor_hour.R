# Group by hour with floor_date(), with distinct users.
list(
  family = "datetime",
  part = 2L,
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(hour = floor_date(ts, "hour")) |>
  group_by(hour) |>
  summarise(n = n(), users = n_distinct(user), .groups = "drop")
)",
  mem_factor = 1
)
