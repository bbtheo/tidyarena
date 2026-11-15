# Inner join on an integer and a string key together.
list(
  family = "join",
  data = "joins",
  code = 'events |> inner_join(dim, by = c("key_int", "key_chr"))',
  mem_factor = 1.5
)
