# Left join with the lookup table, every row kept.
list(
  family = "join",
  code = 'df |> left_join(lookup, by = "g_high") |> select(id, x, region)',
  mem_factor = 1.5
)
