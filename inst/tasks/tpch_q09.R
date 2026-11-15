# TPC-H-derived query 9: Product type profit measure.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  data = "tpch",
  packages = "lubridate",
  code = r"(
p <- part |>
  select(p_name, p_partkey) |>
  filter(grepl(".*green.*", p_name)) |>
  select(p_partkey)

psp <- inner_join(
  partsupp |>
    select(ps_suppkey, ps_partkey, ps_supplycost),
  p, by = c("ps_partkey" = "p_partkey"))

sn <- inner_join(
  supplier |>
    select(s_suppkey, s_nationkey),
  nation |>
    select(n_nationkey, n_name),
  by = c("s_nationkey" = "n_nationkey")) |>
  select(s_suppkey, n_name)

pspsn <- inner_join(psp, sn, by = c("ps_suppkey" = "s_suppkey"))

lpspsn <- inner_join(
  lineitem |>
    select(l_suppkey, l_partkey, l_orderkey, l_extendedprice, l_discount, l_quantity),
  pspsn,
  by = c("l_suppkey" = "ps_suppkey", "l_partkey" = "ps_partkey")) |>
  select(l_orderkey, l_extendedprice, l_discount, l_quantity, ps_supplycost, n_name)

all <- inner_join(
  orders |>
    select(o_orderkey, o_orderdate),
  lpspsn,
  by = c("o_orderkey"= "l_orderkey" )) |>
  select(l_extendedprice, l_discount, l_quantity, ps_supplycost, n_name, o_orderdate)

all |>
  mutate(
    nation = n_name,
    # kludge, o_year = as.integer(format(o_orderdate, "%Y")),
    # also ARROW-14200
    o_year = lubridate::year(o_orderdate),
    amount = l_extendedprice * (1 - l_discount) - ps_supplycost * l_quantity) |>
  select(nation, o_year, amount) |>
  group_by(nation, o_year) |>
  summarise(sum_profit = sum(amount)) |>
  ungroup() |>
  arrange(nation, desc(o_year))
)",
  mem_factor = 2
)
