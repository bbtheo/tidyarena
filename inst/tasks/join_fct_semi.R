# Semi join on a factor key.
list(
  family = "join",
  part = 1L,
  data = "joins",
  code = 'events |> semi_join(cats, by = "key_fct") |> count(key_fct)',
  mem_factor = 1
)
