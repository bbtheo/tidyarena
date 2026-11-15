# TPC-H-derived query 4: Order priority checking.
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
l <- lineitem |>
  select(l_orderkey, l_commitdate, l_receiptdate) |>
  filter(l_commitdate < l_receiptdate) |>
  select(l_orderkey)

o <- orders |>
  select(o_orderkey, o_orderdate, o_orderpriority) |>
  # kludge: filter(o_orderdate >= "1993-07-01", o_orderdate < "1993-07-01" + interval '3' month) |>
  filter(o_orderdate >= as.Date("1993-07-01"), o_orderdate < as.Date("1993-10-01")) |>
  select(o_orderkey, o_orderpriority)

# distinct after join, tested and indeed faster
lo <- inner_join(l, o, by = c("l_orderkey" = "o_orderkey")) |>
  distinct() |>
  select(o_orderpriority)

lo |>
  group_by(o_orderpriority) |>
  summarise(order_count = n()) |>
  ungroup() |>
  arrange(o_orderpriority)
)",
  mem_factor = 2
)
