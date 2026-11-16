# Add a month to a date with %m+%, which rolls back to the month's end.
list(
  family = "datetime",
  part = 4L,
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(due = date %m+% months(1)) |>
  summarise(first = min(due), last = max(due), rolled_back = sum(mday(due) < mday(date)))
)",
  mem_factor = 1
)
