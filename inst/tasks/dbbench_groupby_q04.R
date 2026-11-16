# db-benchmark: mean v1:v3 by id4.
# From https://github.com/duckdblabs/db-benchmark (dplyr solution, commit
# 2f05a6f), MPL-2.0; see inst/COPYRIGHTS. Its code unchanged, but for the |>
# pipe. summarise_at() has no .by form.
list(
  family = "dbbench-groupby",
  part = 4L,
  data = "dbbench_groupby",
  code = r"(
x |>
  group_by(id4) |>
  summarise_at(.funs = "mean", .vars = c("v1", "v2", "v3"), na.rm = TRUE)
)",
  twin = FALSE,
  mem_factor = 0.3
)
