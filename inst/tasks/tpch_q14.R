# TPC-H-derived query 14: Promotion effect.
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
lineitem |>
  filter(
    l_shipdate >= as.Date("1995-01-01"),
    l_shipdate < as.Date("1995-10-01")
  ) |>
  inner_join(part, by = c("l_partkey" = "p_partkey")) |>
  summarise(
    promo_revenue = 100 * sum(
      if_else(grepl("^PROMO", p_type), l_extendedprice * (1 - l_discount), 0)
    ) / sum(l_extendedprice * (1 - l_discount))
  )
)",
  mem_factor = 2
)
