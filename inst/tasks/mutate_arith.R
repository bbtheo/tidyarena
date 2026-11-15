# Arithmetic and if_else() on every row.
list(
  family = "mutate",
  code = paste(
    'df |> mutate(r = x * y + z, sign = if_else(x > 0, "pos", "neg")) |>',
    'select(id, r, sign)'
  ),
  mem_factor = 1
)
