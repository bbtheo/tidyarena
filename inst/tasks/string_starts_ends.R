# Filter on how strings start and end.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  filter(str_starts(code, "A"), str_ends(email, "\\.com")) |>
  summarise(n = n(), total = sum(value))
)",
  mem_factor = 1
)
