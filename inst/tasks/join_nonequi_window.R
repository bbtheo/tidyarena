# Non-equi join on a key and a time range, overlapping windows.
list(
  family = "join",
  part = 3L,
  data = "joins",
  code = r"(
events |>
  inner_join(windows, join_by(key_fct, t >= start, t < end)) |>
  group_by(window_id) |>
  summarise(n = n(), total = sum(x), .groups = "drop")
)",
  mem_factor = 2
)
