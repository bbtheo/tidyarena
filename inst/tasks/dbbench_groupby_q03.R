# db-benchmark: sum v1 mean v3 by id3.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id3) |>
  summarise(v1 = sum(v1, na.rm = TRUE), v3 = mean(v3, na.rm = TRUE), .groups = "drop")
)",
  mem_factor = 1
)
