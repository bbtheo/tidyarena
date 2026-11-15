# Semi join on a factor key.
list(
  family = "join",
  data = "joins",
  code = 'events |> semi_join(cats, by = "key_fct") |> count(key_fct)',
  mem_factor = 1
)
