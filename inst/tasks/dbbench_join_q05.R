# db-benchmark: big inner on int.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS.
list(
  family = "dbbench-join",
  part = 4L,
  data = "dbbench_join",
  code = 'x |> inner_join(big, by = "id3")',
  mem_factor = 1.5
)
