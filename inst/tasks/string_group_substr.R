# Group by a substring computed with base substr().
list(
  family = "string",
  part = 1L,
  data = "strings",
  code = r"(
customers |>
  group_by(prefix = substr(code, 1, 2)) |>
  summarise(n = n(), total = sum(value), .groups = "drop")
)",
  twin = FALSE,
  mem_factor = 1
)
