# TPC-H-derived query 12: Shipping modes and order priority.
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
    l_shipmode %in% c("MAIL", "SHIP"),
    l_commitdate < l_receiptdate,
    l_shipdate < l_commitdate,
    l_receiptdate >= as.Date("1994-01-01"),
    l_receiptdate < as.Date("1995-01-01")
  ) |>
  inner_join(
    orders,
    by = c("l_orderkey" = "o_orderkey")
  ) |>
  group_by(l_shipmode) |>
  summarise(
    high_line_count = sum(
      if_else(
        (o_orderpriority == "1-URGENT") | (o_orderpriority == "2-HIGH"),
        1L,
        0L
      )
    ),
    low_line_count = sum(
      if_else(
        (o_orderpriority != "1-URGENT") & (o_orderpriority != "2-HIGH"),
        1L,
        0L
      )
    )
  ) |>
  ungroup() |>
  arrange(l_shipmode)
)",
  mem_factor = 2
)
