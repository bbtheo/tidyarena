# TPC-H-derived query 21: Suppliers who kept orders waiting.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  part = 3L,
  data = "tpch",
  code = r"(
orders_with_more_than_one_supplier <- lineitem |>
  group_by(l_orderkey) |>
  count(l_suppkey) |>
  group_by(l_orderkey) |>
  summarise(n_supplier = n()) |>
  filter(n_supplier > 1)

line_items_needed <- lineitem |>
  semi_join(orders_with_more_than_one_supplier) |>
  inner_join(orders, by = c("l_orderkey" = "o_orderkey")) |>
  filter(o_orderstatus == "F") |>
  group_by(l_orderkey, l_suppkey) |>
  summarise(failed_delivery_commit = sum(ifelse(l_receiptdate > l_commitdate, 1L, 0L))) |>
  group_by(l_orderkey) |>
  summarise(n_supplier = n(), num_failed = sum(failed_delivery_commit)) |>
  filter(n_supplier > 1 & num_failed == 1)

line_items <- lineitem |>
  semi_join(line_items_needed)

supplier |>
  inner_join(line_items, by = c("s_suppkey" = "l_suppkey")) |>
  filter(l_receiptdate > l_commitdate) |>
  inner_join(nation, by = c("s_nationkey" = "n_nationkey")) |>
  filter(n_name == "SAUDI ARABIA") |>
  group_by(s_name) |>
  summarise(numwait = n()) |>
  ungroup() |>
  arrange(desc(numwait), s_name) |>
  head(100)
)",
  mem_factor = 2
)
