# case_when() on every row, then a count.
list(
  family = "mutate",
  part = 1L,
  code = paste(
    'df |>',
    'mutate(band = case_when(x < -1 ~ "low", x < 1 ~ "mid", TRUE ~ "high")) |>',
    'count(band)'
  ),
  mem_factor = 0.5
)
