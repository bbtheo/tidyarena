# Count the words of free text.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = 'customers |> summarise(words = sum(str_count(text, " ") + 1L))',
  mem_factor = 1
)
