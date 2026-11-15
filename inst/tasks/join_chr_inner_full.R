# Inner join on a string key, every matched row kept.
list(
  family = "join",
  data = "joins",
  code = 'events |> inner_join(dim, by = "key_chr")',
  mem_factor = 1.5
)
