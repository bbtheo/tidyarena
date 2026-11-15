# Rolling join: the last price at or before each event.
list(
  family = "join",
  data = "joins",
  code = r"(
events |>
  left_join(prices, join_by(key_fct, closest(t >= t_change))) |>
  group_by(key_fct) |>
  summarise(revenue = sum(x * price, na.rm = TRUE), unpriced = sum(is.na(price)), .groups = "drop")
)",
  mem_factor = 2
)
