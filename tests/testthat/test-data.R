test_that("data generation is deterministic", {
  dir <- withr::local_tempdir()
  a <- arena_data(1e3, file.path(dir, "a"))
  b <- arena_data(1e3, file.path(dir, "b"))
  expect_identical(a$fingerprint, b$fingerprint)
  expect_identical(tools::md5sum(a$df)[[1]], tools::md5sum(b$df)[[1]])
  expect_match(a$df, "\\.parquet$")
  df <- nanoparquet::read_parquet(a$df)
  expect_named(df, c("id", "g_low", "g_high", "g_skew", "x", "y", "z", "s"))
  expect_equal(nrow(df), 1e3)
})

test_that("format_n() writes sizes compactly", {
  expect_equal(format_n(1e6), "1e6")
  expect_equal(format_n(1e10), "1e10")
  expect_equal(format_n(2.5e7), "2.5e7")
})

test_that("the worker reads the tables as plain data frames, without nanoparquet loaded", {
  dir <- withr::local_tempdir()
  data <- arena_data(100, file.path(dir, "data"))
  spec <- list(
    code = 'stopifnot(identical(class(df), "data.frame"), !"nanoparquet" %in% loadedNamespaces()); df',
    order_key = NA_character_,
    setup = ".collect <- function(x) x",
    data = data[c("df", "lookup")],
    reader_lib = reader_lib(),
    reps = 0L,
    budget_mb = 1024,
    libpath = .libPaths(),
    tmpdir = dir,
    out = file.path(dir, "out.rds")
  )
  result <- run_worker(spec, budget = 2^31, timeout = 60, guard = "watchdog", log = file.path(dir, "log"))
  expect_equal(result$status, "ok", info = result$error)
})
