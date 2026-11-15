# db-benchmark: regression v1 v2 by id2 id4.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id2, id4) |>
  summarise(r2 = cor(v1, v2, use = "na.or.complete")^2, .groups = "drop")
)",
  mem_factor = 0.5
)
