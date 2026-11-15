# Inner join on an integer key, every matched row kept.
list(
  family = "join",
  data = "joins",
  code = 'events |> inner_join(dim, by = "key_int")',
  mem_factor = 1.5
)
