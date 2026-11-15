# TPC-H-derived query 18: Large volume customer.
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
big_orders <- lineitem |>
  group_by(l_orderkey) |>
  summarise(`sum(l_quantity)` = sum(l_quantity)) |>
  filter(`sum(l_quantity)` > 300)

orders |>
  inner_join(big_orders, by = c("o_orderkey" = "l_orderkey")) |>
  inner_join(customer, by = c("o_custkey" = "c_custkey")) |>
  select(
    c_name, c_custkey = o_custkey, o_orderkey,
    o_orderdate, o_totalprice, `sum(l_quantity)`
  ) |>
  arrange(desc(o_totalprice), o_orderdate) |>
  head(100)
)",
  mem_factor = 2
)
