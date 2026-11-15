# Grouped summary with 10 groups.
list(
  family = "summarise",
  code = paste(
    'df |> group_by(g_low) |>',
    'summarise(n = n(), mean_x = mean(x), sum_y = sum(y), .groups = "drop")'
  ),
  mem_factor = 0.3
)
