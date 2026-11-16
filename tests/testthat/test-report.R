test_that("source_summary() compares each Parquet variant with its base", {
  cells <- tibble::tibble(
    size = 1e6,
    task = c("a", "b", "a", "b"),
    backend = c("arrow", "arrow", "arrow_parquet", "arrow_parquet"),
    source = c("memory", "memory", "parquet", "parquet"),
    status = c("ok", "ok", "ok", "error"),
    correct = c(TRUE, TRUE, TRUE, NA),
    setup_seconds = c(2, 2, 0.1, 0.1),
    seconds = c(1, 1, 0.5, NA),
    compute_seconds = c(1, 1, 2, NA)
  )
  s <- source_summary(cells)
  expect_equal(s$backend, "arrow")
  expect_equal(s$cells, 1)
  expect_equal(s$time_ratio, 0.5)
  expect_equal(s$compute_ratio, 2)
  expect_equal(s$parquet_coverage, 0.5)
  expect_equal(s$memory_coverage, 1)
  expect_equal(s$setup_parquet, 0.1)
  expect_equal(source_summary(cells[1:2, ]), tibble::tibble())
})
