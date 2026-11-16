# TPC-H-derived query 16: Parts/supplier relationship.
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
  code = r"(
part_filtered <- part |>
  filter(
    p_brand != "Brand#45",
    !grepl("^MEDIUM POLISHED", p_type),
    p_size %in% c(49, 14, 23, 45, 19, 3, 36, 9)
  )

supplier_filtered <- supplier |>
  filter(!grepl("Customer.*?Complaints", s_comment))

partsupp_filtered <- partsupp |>
  inner_join(supplier_filtered, by = c("ps_suppkey" = "s_suppkey")) |>
  select(ps_partkey, ps_suppkey)

part_filtered |>
  inner_join(partsupp_filtered, by = c("p_partkey" = "ps_partkey")) |>
  group_by(p_brand, p_type, p_size) |>
  summarise(supplier_cnt = n_distinct(ps_suppkey)) |>
  ungroup() |>
  select(p_brand, p_type, p_size, supplier_cnt) |>
  arrange(desc(supplier_cnt), p_brand, p_type, p_size)
)",
  mem_factor = 2
)
