# db-benchmark: small inner on int.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS.
list(
  family = "dbbench-join",
  data = "dbbench_join",
  code = 'x |> inner_join(small, by = "id1")',
  mem_factor = 1.5
)
