# Grouped summary with n / 100 groups.
list(
  family = "summarise",
  part = 4L,
  code = paste(
    'df |> group_by(g_high) |>',
    'summarise(n = n(), mean_x = mean(x), max_y = max(y), .groups = "drop")'
  ),
  mem_factor = 0.5
)
