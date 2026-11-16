# Row filter on a numeric and a low-cardinality key.
list(
  family = "filter",
  part = 1L,
  code = 'df |> filter(x > 0, g_low %in% c("k01", "k02", "k03"))',
  mem_factor = 0.3
)
