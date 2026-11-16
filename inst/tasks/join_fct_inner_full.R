# Inner join on factor keys whose levels differ between the tables.
list(
  family = "join",
  part = 3L,
  data = "joins",
  code = 'events |> inner_join(cats, by = "key_fct")',
  mem_factor = 1.5
)
