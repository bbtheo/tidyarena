# Sort by two keys; the row order is part of the answer.
list(
  family = "arrange",
  code = "df |> arrange(g_low, desc(x)) |> select(id, g_low, x)",
  order_key = "id",
  mem_factor = 1.2
)
