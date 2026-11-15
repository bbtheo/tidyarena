# TPC-H-derived query 13: Customer distribution.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  data = "tpch",
  code = r"(
c_orders <- customer |>
  left_join(
    orders |>
      filter(!grepl("special.*?requests", o_comment)),
    by = c("c_custkey" = "o_custkey")
  ) |>
  group_by(c_custkey) |>
  summarise(
    c_count = sum(!is.na(o_orderkey))
  )

c_orders |>
  group_by(c_count) |>
  summarise(custdist = n()) |>
  ungroup() |>
  arrange(desc(custdist), desc(c_count))
)",
  mem_factor = 2
)
