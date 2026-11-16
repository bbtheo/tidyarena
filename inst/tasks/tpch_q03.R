# TPC-H-derived query 3: Shipping priority.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  part = 2L,
  data = "tpch",
  code = r"(
oc <- inner_join(
  orders |>
    select(o_orderkey, o_custkey, o_orderdate, o_shippriority) |>
    # kludge, should be: filter(o_orderdate < "1995-03-15"),
    filter(o_orderdate < as.Date("1995-03-15")),
  customer |>
    select(c_custkey, c_mktsegment) |>
    filter(c_mktsegment == "BUILDING"),
  by = c("o_custkey" = "c_custkey")
) |>
  select(o_orderkey, o_orderdate, o_shippriority)

loc <- inner_join(
  lineitem |>
    select(l_orderkey, l_shipdate, l_extendedprice, l_discount) |>
    filter(l_shipdate > as.Date("1995-03-15")) |>
    select(l_orderkey, l_extendedprice, l_discount),
  oc, by = c("l_orderkey" = "o_orderkey")
)

loc |> mutate(volume=l_extendedprice * (1 - l_discount)) |>
  group_by(l_orderkey, o_orderdate, o_shippriority) |>
  summarise(revenue = sum(volume)) |>
  select(l_orderkey, revenue, o_orderdate, o_shippriority) |>
  ungroup() |>
  arrange(desc(revenue), o_orderdate) |>
  head(10)
)",
  mem_factor = 2
)
