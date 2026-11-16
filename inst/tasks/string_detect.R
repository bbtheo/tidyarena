# Fixed-string match on a character column, then a count.
list(
  family = "string",
  part = 3L,
  code = 'df |> filter(grepl("7", s, fixed = TRUE)) |> count(g_low)',
  mem_factor = 0.5
)
