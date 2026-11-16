# Inner join with the lookup table, then a grouped summary.
list(
  family = "join",
  part = 3L,
  code = paste(
    'df |> inner_join(lookup, by = "g_high") |> group_by(region) |>',
    'summarise(total = sum(x * weight), .groups = "drop")'
  ),
  mem_factor = 1.5
)
