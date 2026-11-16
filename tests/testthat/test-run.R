test_that("arena_run() measures every repetition", {
  skip_on_cran()
  res <- local_tiny_run()
  expect_s3_class(res, "arena_results")
  expect_equal(nrow(res$cells), 2)
  expect_true(all(res$cells$status == "ok"))
  expect_true(all(res$cells$correct))
  # One warm-up plus the timed repetitions per cell.
  expect_equal(nrow(res$reps), 2 * (tier_settings("s")$reps + 1))
  expect_true(all(res$reps$seconds >= 0))
  if (Sys.info()[["sysname"]] == "Linux") {
    expect_true(all(res$reps$peak_rss >= res$reps$base_rss))
  }
})

test_that("an interrupted run resumes without repeating cells", {
  skip_on_cran()
  res <- local_tiny_run()
  path <- file.path(res$dir, "results.jsonl")
  lines <- readLines(path)
  writeLines(lines[[1]], path)

  resumed <- suppressMessages(arena_run(dir = res$dir))
  expect_equal(nrow(resumed$cells), 2)
  expect_identical(readLines(path)[[1]], lines[[1]])
})

test_that("resuming refuses changed settings", {
  dir <- withr::local_tempdir()
  writeLines("{}", file.path(dir, "run.json"))
  expect_snapshot(arena_run(dir = dir, tasks = "filter_rows"), error = TRUE)
})

test_that("arena_report() summarises a run", {
  skip_on_cran()
  res <- local_tiny_run()
  out <- suppressMessages(arena_report(res))
  expect_named(out, c("cells", "backends"))
  expect_equal(out$backends$coverage, 1)
})

test_that("scrub() removes the home directory and user name", {
  msg <- paste("extensions are under", path.expand("~/.local/share/duckdb"))
  expect_false(grepl(path.expand("~"), scrub(msg), fixed = TRUE))
  expect_match(scrub(msg), "~/.local/share/duckdb", fixed = TRUE)
  expect_identical(scrub(character()), character())
  expect_identical(scrub(NA_character_), NA_character_)
})

test_that("resuming refuses tasks whose code changed", {
  res <- local_tiny_run()
  run <- read_run_json(res$dir)
  run$task_hashes$filter_rows <- "000000000000"
  write_run_json(run, res$dir)
  expect_snapshot(error = TRUE, arena_run(dir = res$dir), transform = function(x) gsub("'.*'", "<dir>", x))
})

test_that("results record the hash of the task code", {
  res <- local_tiny_run()
  expect_equal(res$cells$task_hash[res$cells$task == "filter_rows"], with(arena_tasks(), hash[task == "filter_rows"]))
})

test_that("each repetition is timed in R and computed in the backend", {
  res <- local_tiny_run(tasks = "filter_rows")
  reps <- res$reps[!res$reps$warmup, ]
  expect_true(all(reps$compute_seconds > 0))
  expect_true(all(reps$seconds > 0))
  expect_true(all(!is.na(res$cells$compute_seconds)))
})

test_that("a backend whose compute fails still gets its in-R timing", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  defs <- backend_defs()
  defs$nocompute <- defs$dplyr
  defs$nocompute$setup <- c(defs$dplyr$setup[!grepl("^\\.compute", defs$dplyr$setup)], ".compute <- function(x) stop('no compute here')")
  local_mocked_bindings(backend_defs = function() defs)
  cells <- suppressMessages(conformance("filter_rows", c("dplyr", "nocompute"), 100, .libPaths()))
  expect_equal(cells$status, c("ok", "ok"))
  expect_true(cells$correct[[2]])
})

test_that("a setup that takes too long is stopped on its own limit", {
  dir <- withr::local_tempdir()
  data <- arena_data(100, file.path(dir, "data"))
  spec <- list(
    code = "df",
    order_key = NA_character_,
    setup = c(".collect <- function(x) x", ".convert <- function(x, name) x", "Sys.sleep(30)"),
    data = data$tables,
    reader_lib = reader_lib(),
    reps = 0L,
    budget_mb = 1024,
    libpath = .libPaths(),
    tmpdir = dir,
    out = file.path(dir, "out.rds")
  )
  t0 <- Sys.time()
  result <- run_worker(
    spec,
    budget = 2^31,
    timeout = 60,
    guard = "watchdog",
    log = file.path(dir, "log"),
    setup_timeout = 2
  )
  expect_equal(result$status, "setup_timeout")
  expect_lt(as.double(Sys.time() - t0, units = "secs"), 20)
})

test_that("a Parquet variant reads the files itself", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  defs <- backend_defs()
  defs$dplyr_parquet <- defs$dplyr
  defs$dplyr_parquet$source <- "parquet"
  defs$dplyr_parquet$setup <- c(
    defs$dplyr$setup[!grepl("^\\.convert", defs$dplyr$setup)],
    # A data frame in R here would mean the worker read the file.
    ".read <- function(path, name) { stopifnot(is.character(path)); tibble::as_tibble(nanoparquet::read_parquet(path)) }"
  )
  local_mocked_bindings(backend_defs = function() defs)
  cells <- suppressMessages(conformance("filter_rows", c("dplyr", "dplyr_parquet"), 100, .libPaths()))
  expect_equal(cells$status, c("ok", "ok"))
  expect_true(cells$correct[[2]])
})
