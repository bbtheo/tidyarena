# Group by the first word of a cleaned name.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(first = word(str_squish(name), 1)) |>
  count(first)
)",
  mem_factor = 1
)
