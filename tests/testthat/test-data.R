test_that("data generation is deterministic", {
  dir <- withr::local_tempdir()
  a <- arena_data(1e3, file.path(dir, "a"))
  b <- arena_data(1e3, file.path(dir, "b"))
  expect_identical(a$fingerprint, b$fingerprint)
  expect_identical(tools::md5sum(a$df)[[1]], tools::md5sum(b$df)[[1]])
  df <- readRDS(a$df)
  expect_named(df, c("id", "g_low", "g_high", "g_skew", "x", "y", "z", "s"))
  expect_equal(nrow(df), 1e3)
})

test_that("format_n() writes sizes compactly", {
  expect_equal(format_n(1e6), "1e6")
  expect_equal(format_n(1e10), "1e10")
  expect_equal(format_n(2.5e7), "2.5e7")
})
