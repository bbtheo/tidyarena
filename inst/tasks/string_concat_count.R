# Build a key from two substrings and count it.
list(
  family = "string",
  part = 4L,
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(key = str_c(str_sub(code, 1, 2), str_sub(code, -4, -1), sep = "_")) |>
  count(key)
)",
  mem_factor = 1
)
