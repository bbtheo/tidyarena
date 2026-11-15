# TPC-H-derived query 5: Local supplier volume.
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
nr <- inner_join(
  nation |>
    select(n_nationkey, n_regionkey, n_name),
  region |>
    select(r_regionkey, r_name) |>
    filter(r_name == "ASIA"),
  by = c("n_regionkey" = "r_regionkey")
) |>
  select(n_nationkey, n_name)

snr <- inner_join(
  supplier |>
    select(s_suppkey, s_nationkey),
  nr,
  by = c("s_nationkey" = "n_nationkey")
) |>
  select(s_suppkey, s_nationkey, n_name)

lsnr <- inner_join(
  lineitem |> select(l_suppkey, l_orderkey, l_extendedprice, l_discount),
  snr, by = c("l_suppkey" = "s_suppkey"))

o <- orders |>
  select(o_orderdate, o_orderkey, o_custkey) |>
  # kludge: filter(o_orderdate >= "1994-01-01", o_orderdate < "1994-01-01" + interval '1' year) |>
  filter(o_orderdate >= as.Date("1994-01-01"), o_orderdate < as.Date("1995-01-01")) |>
  select(o_orderkey, o_custkey)

oc <- inner_join(o, customer |> select(c_custkey, c_nationkey),
                 by = c("o_custkey" = "c_custkey")) |>
  select(o_orderkey, c_nationkey)

lsnroc <- inner_join(lsnr, oc,
                     by = c("l_orderkey" = "o_orderkey", "s_nationkey" = "c_nationkey")) |>
  select(l_extendedprice, l_discount, n_name)

lsnroc |>
  mutate(volume=l_extendedprice * (1 - l_discount)) |>
  group_by(n_name) |>
  summarise(revenue = sum(volume)) |>
  ungroup() |>
  arrange(desc(revenue))
)",
  mem_factor = 2
)
