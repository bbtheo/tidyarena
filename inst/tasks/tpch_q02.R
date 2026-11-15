# TPC-H-derived query 2: Minimum cost supplier.
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
ps <- partsupp |> select(ps_partkey, ps_suppkey, ps_supplycost)

p <- part |>
  select(p_partkey, p_type, p_size, p_mfgr) |>
  filter(p_size == 15, grepl(".*BRASS$", p_type)) |>
  select(p_partkey, p_mfgr)
psp <- inner_join(p, ps, by = c("p_partkey" = "ps_partkey"))

sp <- supplier |>
  select(s_suppkey, s_nationkey, s_acctbal, s_name,
         s_address, s_phone, s_comment)

psps <- inner_join(psp, sp,
                   by = c("ps_suppkey" = "s_suppkey")) |>
  select(p_partkey, ps_supplycost, p_mfgr, s_nationkey,
         s_acctbal, s_name, s_address, s_phone, s_comment)

nr <- inner_join(nation,
                 region |> filter(r_name == "EUROPE"),
                 by = c("n_regionkey" = "r_regionkey")) |>
  select(n_nationkey, n_name)

pspsnr <- inner_join(psps, nr,
                     by = c("s_nationkey" = "n_nationkey")) |>
  select(p_partkey, ps_supplycost, p_mfgr, n_name, s_acctbal,
         s_name, s_address, s_phone, s_comment)

aggr <- pspsnr |>
  group_by(p_partkey) |>
  summarise(min_ps_supplycost = min(ps_supplycost))

sj <- inner_join(pspsnr, aggr,
                 by=c("p_partkey" = "p_partkey", "ps_supplycost" = "min_ps_supplycost"))


sj |>
  select(s_acctbal, s_name, n_name, p_partkey, p_mfgr,
         s_address, s_phone, s_comment) |>
  arrange(desc(s_acctbal), n_name, s_name, p_partkey) |>
  head(100)
)",
  mem_factor = 2
)
