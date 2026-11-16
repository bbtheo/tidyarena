# TPC-H-derived query 10: Returned item reporting.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  part = 1L,
  data = "tpch",
  code = r"(
l <- lineitem |>
  select(l_orderkey, l_returnflag, l_extendedprice, l_discount) |>
  filter(l_returnflag == "R") |>
  select(l_orderkey, l_extendedprice, l_discount)

o <- orders |>
  select(o_orderkey, o_custkey, o_orderdate) |>
  # kludge, filter(o_orderdate >= "1993-10-01", o_orderdate < "1994-01-01") |>
  filter(o_orderdate >= as.Date("1993-10-01"), o_orderdate < as.Date("1994-01-01")) |>
  select(o_orderkey, o_custkey)

lo <- inner_join(l, o,
                 by = c("l_orderkey" = "o_orderkey")) |>
  select(l_extendedprice, l_discount, o_custkey)
# first aggregate, then join with customer/nation,
# otherwise we need to aggr over lots of cols

lo_aggr <- lo |> mutate(volume=l_extendedprice * (1 - l_discount)) |>
  group_by(o_custkey) |>
  summarise(revenue = sum(volume))

c <- customer |>
  select(c_custkey, c_nationkey, c_name, c_acctbal, c_phone, c_address, c_comment)

loc <- inner_join(c, lo_aggr, by = c("c_custkey" = "o_custkey"))

locn <- inner_join(loc, nation |> select(n_nationkey, n_name),
                   by = c("c_nationkey" = "n_nationkey"))

locn |>
  select(c_custkey, c_name, revenue, c_acctbal, n_name,
         c_address, c_phone, c_comment) |>
  arrange(desc(revenue)) |>
  head(20)
)",
  mem_factor = 2
)
