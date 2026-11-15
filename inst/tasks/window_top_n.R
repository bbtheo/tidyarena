# Top 10 rows per group by rank.
list(
  family = "window",
  code = paste(
    "df |> group_by(g_low) |> filter(min_rank(desc(x)) <= 10) |>",
    "ungroup() |> select(g_low, id, x)"
  ),
  mem_factor = 1.5
)
