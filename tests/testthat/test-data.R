test_that("data generation is deterministic", {
  dir <- withr::local_tempdir()
  a <- arena_data(1e3, file.path(dir, "a"))
  b <- arena_data(1e3, file.path(dir, "b"))
  expect_identical(a$fingerprint, b$fingerprint)
  expect_identical(tools::md5sum(a$tables$df)[[1]], tools::md5sum(b$tables$df)[[1]])
  expect_match(a$tables$df, "\\.parquet$")
  df <- nanoparquet::read_parquet(a$tables$df)
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
    setup = c(".collect <- function(x) x", ".convert <- function(x, name) x"),
    data = data$tables,
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

test_that("the db-benchmark groupby data is deterministic and shaped like db-benchmark's", {
  dir <- withr::local_tempdir()
  a <- arena_data(1e4, file.path(dir, "a"), "dbbench_groupby")
  b <- arena_data(1e4, file.path(dir, "b"), "dbbench_groupby")
  expect_identical(a$fingerprint, b$fingerprint)
  x <- nanoparquet::read_parquet(a$tables$x)
  expect_named(x, c("id1", "id2", "id3", "id4", "id5", "id6", "v1", "v2", "v3"))
  expect_true(is.factor(x$id1))
  expect_equal(nlevels(x$id1), 100)
  expect_equal(max(x$id6), 1e4 / 100)
})

test_that("data sets refuse sizes they don't have", {
  expect_snapshot(error = TRUE, arena_data(1e6, withr::local_tempdir(), "dbbench_join"))
  expect_snapshot(error = TRUE, arena_data(1e6, withr::local_tempdir(), "nope"))
})

test_that("the TPC-H data has the eight tables", {
  skip_on_cran()
  skip_if_not(packages_installed("duckdb", c(.libPaths(), arena_lib())), "duckdb is not installed")
  data <- arena_data(1e5, withr::local_tempdir(), "tpch")
  expect_named(data$tables, c("customer", "lineitem", "nation", "orders", "part", "partsupp", "region", "supplier"))
  expect_equal(data$fingerprint$region$nrow, 5)
  expect_equal(data$fingerprint$nation$nrow, 25)
  lineitem <- nanoparquet::read_parquet(data$tables$lineitem)
  expect_type(lineitem$l_orderkey, "integer")
  expect_type(lineitem$l_extendedprice, "double")
  expect_s3_class(lineitem$l_shipdate, "Date")
})

test_that("the join, string and date data sets have their tables", {
  dir <- withr::local_tempdir()
  joins <- arena_data(1e4, dir, "joins")
  expect_named(joins$tables, c("events", "dim", "cats", "prices", "bands", "windows"))
  events <- nanoparquet::read_parquet(joins$tables$events)
  expect_true(is.factor(events$key_fct))
  expect_type(events$key_chr, "character")
  cats <- nanoparquet::read_parquet(joins$tables$cats)
  expect_false(identical(levels(cats$key_fct), levels(events$key_fct)))

  strings <- arena_data(1e3, dir, "strings")
  customers <- nanoparquet::read_parquet(strings$tables$customers)
  expect_false(any(grepl("^NA-", customers$code)))
  expect_equal(length(unique(substr(customers$code, 1, 2))), 50)

  dates <- arena_data(1e4, dir, "datetimes")
  log <- nanoparquet::read_parquet(dates$tables$log)
  expect_s3_class(log$ts, "POSIXct")
  expect_equal(attr(log$ts, "tzone"), "UTC")
  expect_gt(as.numeric(difftime(max(log$ts), min(log$ts), units = "days")), 1000)
})
