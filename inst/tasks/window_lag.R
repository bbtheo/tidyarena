# Grouped lag() ordered by id.
list(
  family = "window",
  code = paste(
    "df |> group_by(g_low) |> mutate(dx = x - lag(x, order_by = id)) |>",
    "ungroup() |> summarise(s = sum(dx, na.rm = TRUE))"
  ),
  mem_factor = 1.5
)
