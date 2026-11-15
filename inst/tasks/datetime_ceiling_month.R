# The last day of each date's month, with ceiling_date().
list(
  family = "datetime",
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(month_end = ceiling_date(date, "month") - 1L) |>
  count(month_end)
)",
  mem_factor = 1
)
