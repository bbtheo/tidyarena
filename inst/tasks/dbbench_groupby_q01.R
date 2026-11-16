# db-benchmark: sum v1 by id1.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  part = 4L,
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id1) |>
  summarise(v1 = sum(v1, na.rm = TRUE), .groups = "drop")
)",
  mem_factor = 0.3
)
