# Extract the number from a code and use it.
list(
  family = "string",
  part = 2L,
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(num = as.integer(str_extract(code, "[0-9]+"))) |>
  summarise(total = sum(num), big = sum(num > 5000L))
)",
  mem_factor = 1
)
