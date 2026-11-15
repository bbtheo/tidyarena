# TPC-H-derived query 11: Important stock identification.
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
nation <- nation |>
  filter(n_name == "GERMANY")

joined_filtered <- partsupp |>
  inner_join(supplier, by = c("ps_suppkey" = "s_suppkey")) |>
  inner_join(nation, by = c("s_nationkey" = "n_nationkey"))

global_agr <- joined_filtered |>
  summarise(
    global_value = sum(ps_supplycost * ps_availqty) * 0.0001000000
  ) |>
  mutate(global_agr_key = 1L)

partkey_agr <- joined_filtered |>
  group_by(ps_partkey) |>
  summarise(value = sum(ps_supplycost * ps_availqty))

partkey_agr |>
  mutate(global_agr_key = 1L) |>
  inner_join(global_agr, by = "global_agr_key") |>
  filter(value > global_value) |>
  arrange(desc(value)) |>
  select(ps_partkey, value)
)",
  mem_factor = 2
)
