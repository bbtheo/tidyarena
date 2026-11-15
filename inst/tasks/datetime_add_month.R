# Add a month to a date with +, which gives NA where the day doesn't exist.
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(due = date + months(1)) |>
  summarise(invalid = sum(is.na(due)), last = max(due, na.rm = TRUE))
)",
  mem_factor = 1
)
