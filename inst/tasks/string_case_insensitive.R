# Filter on a case-insensitive regular expression.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  filter(str_detect(name, regex("^\\s*ka", ignore_case = TRUE))) |>
  summarise(n = n())
)",
  mem_factor = 1
)
