# Mean text length by email domain ending.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  group_by(tld = str_extract(email, "[a-z]+$")) |>
  summarise(mean_length = mean(str_length(text)), .groups = "drop")
)",
  twin = FALSE,
  mem_factor = 1
)
