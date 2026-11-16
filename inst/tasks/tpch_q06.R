# TPC-H-derived query 6: Forecasting revenue change.
# Not the TPC-H benchmark: results are not comparable with published TPC-H
# results. From arrowbench
# (https://github.com/voltrondata-labs/arrowbench, R/tpch-queries.R,
# commit deacdeb), MIT, Copyright (c) 2021 Ursa Computing; see
# inst/COPYRIGHTS. Tables are referred to by name, the pipe is |>, and
# year() is lubridate::year().
list(
  family = "tpch",
  part = 1L,
  data = "tpch",
  code = r"(
lineitem |>
  select(l_shipdate, l_extendedprice, l_discount, l_quantity) |>
  # kludge, should be: filter(l_shipdate >= "1994-01-01",
  filter(l_shipdate >= as.Date("1994-01-01"),
         # kludge: should be: l_shipdate < "1994-01-01" + interval '1' year,
         l_shipdate < as.Date("1995-01-01"),
         # Should be the following, but https://issues.apache.org/jira/browse/ARROW-14125
         # Need to round because 0.06 - 0.01 != 0.05
         l_discount >= round(0.06 - 0.01, digits = 2),
         l_discount <= round(0.06 + 0.01, digits = 2),
         # l_discount >= 0.05,
         # l_discount <= 0.07,
         l_quantity < 24) |>
  select(l_extendedprice, l_discount) |>
  summarise(revenue = sum(l_extendedprice * l_discount))
)",
  mem_factor = 2
)
