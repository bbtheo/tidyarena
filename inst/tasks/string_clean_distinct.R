# Normalise names (case, spaces) and count the distinct ones.
list(
  family = "string",
  part = 4L,
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(name = str_to_lower(str_squish(name))) |>
  summarise(people = n_distinct(name))
)",
  mem_factor = 1
)
