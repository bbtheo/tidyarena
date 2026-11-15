# Filter on a regular expression anchored at the end.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  filter(str_detect(email, "\\.(org|net)$")) |>
  summarise(n = n(), total = sum(value))
)",
  mem_factor = 1
)
