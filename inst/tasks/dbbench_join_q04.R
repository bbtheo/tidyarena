# db-benchmark: medium inner on factor.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS.
list(
  family = "dbbench-join",
  data = "dbbench_join",
  code = 'x |> inner_join(medium, by = "id5")',
  mem_factor = 1.5
)
