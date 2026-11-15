# db-benchmark: sum v3 count by id1:id6.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id1, id2, id3, id4, id5, id6) |>
  summarise(v3 = sum(v3, na.rm = TRUE), count = n(), .groups = "drop")
)",
  mem_factor = 2
)
