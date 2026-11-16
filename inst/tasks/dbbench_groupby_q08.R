# db-benchmark: largest two v3 by id6.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Same question, with .groups = "drop"
# so that it has a .by form.
list(
  family = "dbbench-groupby",
  part = 2L,
  data = "dbbench_groupby",
  code = r"(
x |>
  select(id6, largest2_v3 = v3) |>
  filter(!is.na(largest2_v3)) |>
  arrange(desc(largest2_v3)) |>
  group_by(id6) |>
  filter(row_number() <= 2L) |>
  ungroup()
)",
  mem_factor = 1.5
)
