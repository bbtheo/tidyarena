# TPC-H-derived query 22: Global sales opportunity.
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
acctbal_mins <- customer |>
  filter(
    substr(c_phone, 1, 2) %in% c("13", "31", "23", "29", "30", "18", "17") &
      c_acctbal > 0
  ) |>
  summarise(acctbal_min = mean(c_acctbal, na.rm = TRUE), join_id = 1L)

customer |>
  mutate(cntrycode = substr(c_phone, 1, 2), join_id = 1L) |>
  left_join(acctbal_mins, by = "join_id") |>
  filter(
    cntrycode %in% c("13", "31", "23", "29", "30", "18", "17") &
      c_acctbal > acctbal_min
  ) |>
  anti_join(orders, by = c("c_custkey" = "o_custkey")) |>
  select(cntrycode, c_acctbal) |>
  group_by(cntrycode) |>
  summarise(
    numcust = n(),
    totacctbal = sum(c_acctbal)
  ) |>
  ungroup() |>
  arrange(cntrycode)
)",
  mem_factor = 2
)
