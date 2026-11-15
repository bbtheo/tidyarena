# Format labels with sprintf() and count the distinct ones.
list(
  family = "string",
  data = "strings",
  packages = "stringr",
  code = r"(
customers |>
  mutate(label = sprintf("%s:%04d", str_sub(code, 1, 2), id %% 1000L)) |>
  summarise(labels = n_distinct(label))
)",
  mem_factor = 1
)
