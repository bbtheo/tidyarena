# Grouped summary over Zipf-distributed groups.
list(
  family = "summarise",
  part = 3L,
  code = paste(
    'df |> group_by(g_skew) |>',
    'summarise(n = n(), sum_x = sum(x), .groups = "drop")'
  ),
  mem_factor = 0.3
)
