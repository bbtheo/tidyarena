# db-benchmark: max v1 - min v2 by id3.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id3) |>
  summarise(range_v1_v2 = max(v1, na.rm = TRUE) - min(v2, na.rm = TRUE), .groups = "drop")
)",
  mem_factor = 1
)
