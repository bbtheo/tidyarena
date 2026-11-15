# TPC-H-derived query 15: Top supplier.
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
revenue_by_supplier <- lineitem |>
  filter(
    l_shipdate >= as.Date("1996-01-01"),
    l_shipdate < as.Date("1996-04-01")
  ) |>
  group_by(l_suppkey) |>
  summarise(
    total_revenue = sum(l_extendedprice * (1 - l_discount))
  )

global_revenue <- revenue_by_supplier |>
  mutate(global_agr_key = 1L) |>
  group_by(global_agr_key) |>
  summarise(
    max_total_revenue = max(total_revenue)
  )

revenue_by_supplier |>
  mutate(global_agr_key = 1L) |>
  inner_join(global_revenue, by = "global_agr_key") |>
  filter(abs(total_revenue - max_total_revenue) < 1e-9) |>
  inner_join(supplier, by = c("l_suppkey" = "s_suppkey")) |>
  select(s_suppkey = l_suppkey, s_name, s_address, s_phone, total_revenue)
)",
  mem_factor = 2
)
