# db-benchmark: median v3 sd v3 by id4 id5.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id4, id5) |>
  summarise(median_v3 = median(v3, na.rm = TRUE), sd_v3 = sd(v3, na.rm = TRUE), .groups = "drop")
)",
  mem_factor = 1
)
