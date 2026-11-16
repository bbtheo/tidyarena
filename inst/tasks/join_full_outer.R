# Full join, then a count of the rows matched on both sides.
list(
  family = "join",
  part = 1L,
  data = "joins",
  code = r"(
events |>
  full_join(dim, by = "key_int") |>
  summarise(n = n(), both = sum(!is.na(id) & !is.na(score)))
)",
  mem_factor = 1.5
)
