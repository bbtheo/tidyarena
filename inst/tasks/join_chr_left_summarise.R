# Left join on a string key, then a grouped summary.
list(
  family = "join",
  part = 2L,
  data = "joins",
  code = r"(
events |>
  left_join(dim, by = "key_chr") |>
  group_by(category) |>
  summarise(n = n(), mean_score = mean(score), total = sum(x), .groups = "drop")
)",
  mem_factor = 1.5
)
