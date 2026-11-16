# Group by the domain of an email, removed with a regular expression.
list(
  family = "string",
  part = 3L,
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(domain = str_remove(email, "^.*@")) |>
  group_by(domain) |>
  summarise(n = n(), mean_value = mean(value), .groups = "drop")
)",
  mem_factor = 1
)
