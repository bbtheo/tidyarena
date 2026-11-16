# Anti join on a string key.
list(
  family = "join",
  part = 1L,
  data = "joins",
  code = r"(
events |>
  anti_join(dim, by = "key_chr") |>
  summarise(n = n(), total = sum(x))
)",
  mem_factor = 1
)
