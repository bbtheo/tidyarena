# Group by a substring computed with str_sub().
list(
  family = "string",
  part = 3L,
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  group_by(prefix = str_sub(code, 1, 2)) |>
  summarise(n = n(), total = sum(value), .groups = "drop")
)",
  twin = FALSE,
  mem_factor = 1
)
