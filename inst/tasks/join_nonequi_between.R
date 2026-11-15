# Non-equi join on a key and a time range, written with between().
list(
  family = "join",
  data = "joins",
  code = r"-(
events |>
  inner_join(windows, join_by(key_fct, between(t, start, end, bounds = "[)"))) |>
  group_by(window_id) |>
  summarise(n = n(), total = sum(x), .groups = "drop")
)-",
  mem_factor = 2
)
