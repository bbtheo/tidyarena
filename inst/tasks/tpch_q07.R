# TPC-H-derived query 7: Volume shipping.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  part = 4L,
  data = "tpch",
  packages = "lubridate",
  code = r"(
sn <- inner_join(
  supplier |>
    select(s_nationkey, s_suppkey),
  nation |>
    select(n1_nationkey = n_nationkey, n1_name = n_name) |>
    filter(n1_name %in% c("FRANCE", "GERMANY")),
  by = c("s_nationkey" = "n1_nationkey")) |>
  select(s_suppkey, n1_name)

cn <- inner_join(
  customer |>
    select(c_custkey, c_nationkey),
  nation |>
    select(n2_nationkey = n_nationkey, n2_name = n_name) |>
    filter(n2_name %in% c("FRANCE", "GERMANY")),
  by = c("c_nationkey" = "n2_nationkey")) |>
  select(c_custkey, n2_name)

cno <- inner_join(
  orders |>
    select(o_custkey, o_orderkey),
  cn, by = c("o_custkey" = "c_custkey")) |>
  select(o_orderkey, n2_name)

cnol <- inner_join(
  lineitem |>
    select(l_orderkey, l_suppkey, l_shipdate, l_extendedprice, l_discount) |>
    # kludge, should be: filter(l_shipdate >= "1995-01-01", l_shipdate <= "1996-12-31"),
    filter(l_shipdate >= as.Date("1995-01-01"), l_shipdate <= as.Date("1996-12-31")),
  cno,
  by = c("l_orderkey" = "o_orderkey")) |>
  select(l_suppkey, l_shipdate, l_extendedprice, l_discount, n2_name)

all <- inner_join(cnol, sn, by = c("l_suppkey" = "s_suppkey"))

all |>
  filter((n1_name == "FRANCE" & n2_name == "GERMANY") |
           (n1_name == "GERMANY" & n2_name == "FRANCE")) |>
  mutate(
    supp_nation = n1_name,
    cust_nation = n2_name,
    # kludge (?) l_year = as.integer(strftime(l_shipdate, "%Y")),
    l_year = lubridate::year(l_shipdate),
    volume = l_extendedprice * (1 - l_discount)) |>
  select(supp_nation, cust_nation, l_year, volume) |>
  group_by(supp_nation, cust_nation, l_year) |>
  summarise(revenue = sum(volume)) |>
  ungroup() |>
  arrange(supp_nation, cust_nation, l_year)
)",
  mem_factor = 2
)
