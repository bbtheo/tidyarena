# Replace every vowel in free text, then measure it.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(text = str_replace_all(text, "[aeiou]", "")) |>
  summarise(chars = sum(str_length(text)))
)",
  mem_factor = 1
)
