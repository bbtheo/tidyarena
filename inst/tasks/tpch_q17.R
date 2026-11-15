# TPC-H-derived query 17: Small-quantity-order revenue.
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
parts_filtered <- part |>
  filter(
    p_brand == "Brand#23",
    p_container == "MED BOX"
  )

joined <- lineitem |>
  inner_join(parts_filtered, by = c("l_partkey" = "p_partkey"))

quantity_by_part <- joined |>
  group_by(l_partkey) |>
  summarise(quantity_threshold = 0.2 * mean(l_quantity))

joined |>
  inner_join(quantity_by_part, by = "l_partkey") |>
  filter(l_quantity < quantity_threshold) |>
  summarise(avg_yearly = sum(l_extendedprice) / 7.0)
)",
  mem_factor = 2
)
