# Add days to a date and filter on the result.
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(due = date + 30L) |>
  filter(due > as.Date("2025-06-01")) |>
  summarise(n = n(), total = sum(amount))
)",
  mem_factor = 1
)
