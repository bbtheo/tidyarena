# tidyarena's date and time data: one table, `log`, n rows. Runs in a fresh R
# process with base R and nanoparquet.
#
# * `id`: row number.
# * `user`: 10,000 users.
# * `ts`: a time in UTC, uniform over 2023 to 2025, to the second.
# * `ts_end`: `ts` plus up to two hours.
# * `date`: the date of `ts`, about 2% of them the 31st of a month.
# * `ts_str`: `ts` as `"YYYY-MM-DD HH:MM:SS"`.
# * `amount`: a value from 0 to 200.
generate_datetimes <- function(n, seed, paths, options = list()) {
  # No min/max statistics: nanoparquet 0.5.2 writes wrong ones for columns
  # of more than one page, and readers that trust them, DuckDB among them,
  # then skip rows that match.
  no_minmax <- nanoparquet::parquet_options(write_minmax_values = FALSE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  n <- as.integer(n)
  start <- as.POSIXct("2023-01-01", tz = "UTC")
  span <- as.integer(difftime(as.POSIXct("2026-01-01", tz = "UTC"), start, units = "secs"))
  ts <- start + (sample.int(span, n, replace = TRUE) - 1L)
  log <- data.frame(
    id = seq_len(n),
    user = sample.int(10000L, n, replace = TRUE),
    ts = ts,
    ts_end = ts + sample.int(7200L, n, replace = TRUE),
    date = as.Date(ts, tz = "UTC"),
    ts_str = format(ts, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    amount = round(stats::runif(n, 0, 200), 2)
  )
  nanoparquet::write_parquet(log, paths$log, compression = "uncompressed", options = no_minmax)
  list(log = list(
    nrow = n,
    ts = sum(as.double(log$ts)),
    amount = sum(log$amount)
  ))
}
