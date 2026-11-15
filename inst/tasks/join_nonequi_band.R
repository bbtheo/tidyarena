# Non-equi join: the value band each event falls in.
list(
  family = "join",
  data = "joins",
  code = r"(
events |>
  inner_join(bands, join_by(x >= lo, x < hi)) |>
  count(band)
)",
  mem_factor = 1.5
)
