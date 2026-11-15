# Grouped summary of several columns with across().
list(
  family = "summarise",
  code = paste(
    'df |> group_by(g_low) |>',
    'summarise(across(c(x, y), list(mean = mean, sd = sd)), .groups = "drop")'
  ),
  mem_factor = 0.3
)
