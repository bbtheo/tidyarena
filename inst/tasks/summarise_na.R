# Grouped summary of a column with missing values.
list(
  family = "summarise",
  part = 1L,
  code = paste(
    'df |> group_by(g_low) |>',
    'summarise(mean_z = mean(z, na.rm = TRUE), n_na = sum(is.na(z)), .groups = "drop")'
  ),
  mem_factor = 0.3
)
