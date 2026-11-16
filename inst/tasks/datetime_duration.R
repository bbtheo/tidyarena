# Duration between two timestamps with difftime().
list(
  family = "datetime",
  part = 3L,
  data = "datetimes",
  packages = "lubridate",
  code = r"(
log |>
  mutate(minutes = as.numeric(difftime(ts_end, ts, units = "mins"))) |>
  summarise(mean_minutes = mean(minutes), long = sum(minutes > 60))
)",
  mem_factor = 1
)
