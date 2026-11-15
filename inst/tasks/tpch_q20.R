# TPC-H-derived query 20: Potential part promotion.
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
supplier_ca <- supplier |>
  inner_join(
    nation |> filter(n_name == "CANADA"),
    by = c("s_nationkey" = "n_nationkey")
  ) |>
  select(s_suppkey, s_name, s_address)

part_forest <- part |>
  filter(grepl("^forest", p_name))

partsupp_forest_ca <- partsupp |>
  semi_join(supplier_ca, c("ps_suppkey" = "s_suppkey")) |>
  semi_join(part_forest, by = c("ps_partkey" = "p_partkey"))

qty_threshold <- lineitem |>
  filter(
    l_shipdate >= as.Date("1994-01-01"),
    l_shipdate < as.Date("1995-01-01")
  ) |>
  semi_join(partsupp_forest_ca, by = c("l_partkey" = "ps_partkey", "l_suppkey" = "ps_suppkey")) |>
  group_by(l_suppkey) |>
  summarise(qty_threshold = 0.5 * sum(l_quantity))

partsupp_forest_ca_filtered <- partsupp_forest_ca |>
  inner_join(
    qty_threshold,
    by = c("ps_suppkey" = "l_suppkey")
  ) |>
  filter(ps_availqty > qty_threshold)

supplier_ca |>
  semi_join(partsupp_forest_ca_filtered, by = c("s_suppkey" = "ps_suppkey")) |>
  select(s_name, s_address) |>
  arrange(s_name)
)",
  mem_factor = 2
)
