test_that("arena_run() measures every repetition", {
  skip_on_cran()
  res <- local_tiny_run()
  expect_s3_class(res, "arena_results")
  expect_equal(nrow(res$cells), 2)
  expect_true(all(res$cells$status == "ok"))
  expect_true(all(res$cells$correct))
  # One warm-up plus the timed repetitions per cell.
  expect_equal(nrow(res$reps), 2 * (arena_reps + 1))
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

test_that("each repetition is timed in R and computed in the backend, except dplyr's", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  defs <- backend_defs()
  defs$computed <- defs$dplyr
  defs$computed$time_compute <- NULL
  local_mocked_bindings(backend_defs = function() defs)
  dir <- withr::local_tempdir()
  cells <- suppressMessages(arena_run(
    backends = "computed", tasks = "filter_rows", sizes = 100,
    install = "never", dir = file.path(dir, "run")
  ))$cells
  expect_true(is.na(cells$compute_seconds[cells$backend == "dplyr"]))
  expect_false(is.na(cells$compute_seconds[cells$backend == "computed"]))
  expect_true(all(cells$seconds > 0))
})

test_that("a backend that falls back in the warm-up fails without being timed", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  defs <- backend_defs()
  defs$falls <- defs$dplyr
  defs$falls$fallback_pattern <- "fell back"
  defs$falls$setup <- c(
    defs$dplyr$setup[!grepl("^\\.collect", defs$dplyr$setup)],
    ".collect <- function(x) { message('fell back to dplyr'); x }"
  )
  local_mocked_bindings(backend_defs = function() defs)
  dir <- withr::local_tempdir()
  res <- suppressMessages(arena_run(
    backends = "falls", tasks = "filter_rows", sizes = c(100, 1000),
    install = "never", dir = file.path(dir, "run")
  ))
  falls <- res$cells[res$cells$backend == "falls", ]
  expect_equal(falls$status, c("fallback", "skipped"))
  expect_equal(falls$reason[[2]], "failed at a smaller size")
  expect_equal(nrow(res$reps[res$reps$backend == "falls", ]), 1)
})

test_that("a backend gets 10 times dplyr's time, at least 30 seconds a run", {
  computed <- list()
  dplyr <- list(time_compute = FALSE)
  expect_equal(run_timeout(NULL, 1e6, 3, computed), Inf)
  expect_equal(run_timeout(0.1, 1e6, 3, computed), 30 * 4 * 2)
  expect_equal(run_timeout(5, 1e7, 3, dplyr), 50 * 4)
  # Not more than the size's own limit.
  expect_equal(run_timeout(20, 1e6, 3, computed), Inf)
})

test_that("a pipeline past its limit is too slow", {
  dir <- withr::local_tempdir()
  data <- arena_data(100, file.path(dir, "data"))
  spec <- list(
    code = "Sys.sleep(30); df",
    order_key = NA_character_,
    setup = c(".collect <- function(x) x", ".compute <- function(x) x", ".convert <- function(x, name) x"),
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
    run_timeout = 2
  )
  expect_equal(result$status, "too_slow")
  expect_lt(as.double(Sys.time() - t0, units = "secs"), 20)
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

# A run under runs_dir() that stopped after its first cell.
local_unfinished_run <- function(env = parent.frame()) {
  withr::local_options(tidyarena.runs_dir = withr::local_tempdir(.local_envir = env), .local_envir = env)
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env), .local_envir = env)
  dir <- file.path(runs_dir(), "old")
  suppressMessages(arena_run(
    backends = "dplyr",
    tasks = c("filter_rows", "summarise_lowcard"),
    sizes = 1e3,
    install = "never",
    dir = dir
  ))
  run <- read_run_json(dir)
  run$finished <- NULL
  write_run_json(run, dir)
  path <- file.path(dir, "results.jsonl")
  writeLines(readLines(path)[[1]], path)
  dir
}

rerun <- function() {
  suppressMessages(arena_run(
    backends = "dplyr",
    tasks = c("filter_rows", "summarise_lowcard"),
    sizes = 1e3,
    install = "never"
  ))
}

test_that("an unfinished run of the same cells is continued when the user agrees", {
  skip_on_cran()
  dir <- local_unfinished_run()
  first <- readLines(file.path(dir, "results.jsonl"))[[1]]
  withr::local_options(rlang_interactive = TRUE)
  local_mocked_bindings(ask_yes_no = function(...) TRUE)
  res <- rerun()
  expect_equal(res$dir, dir)
  expect_equal(nrow(res$cells), 2)
  expect_identical(readLines(file.path(dir, "results.jsonl"))[[1]], first)
  expect_false(is.null(read_run_json(dir)$finished))
  expect_length(list.files(runs_dir()), 1)
})

test_that("an unfinished run turned down starts a new run and is not offered again", {
  skip_on_cran()
  dir <- local_unfinished_run()
  withr::local_options(rlang_interactive = TRUE)
  asked <- 0
  local_mocked_bindings(ask_yes_no = function(...) {
    asked <<- asked + 1
    FALSE
  })
  res <- rerun()
  expect_false(res$dir == dir)
  expect_false(is.null(read_run_json(dir)$abandoned))
  rerun()
  expect_equal(asked, 1)
  expect_length(list.files(runs_dir()), 3)
})

test_that("an unfinished run is not offered for other cells, or outside interactive sessions", {
  skip_on_cran()
  dir <- local_unfinished_run()
  plan <- arena_plan(backends = "dplyr", tasks = "filter_rows", sizes = 1e3)
  expect_null(find_unfinished(plan))
  plan <- arena_plan(backends = "dplyr", tasks = c("filter_rows", "summarise_lowcard"), sizes = 1e3)
  expect_equal(find_unfinished(plan), dir)
  withr::local_options(rlang_interactive = FALSE)
  expect_message(expect_null(offer_resume(plan)), "unfinished run")
})

test_that("a run still going in another process is not resumed", {
  skip_on_cran()
  dir <- local_unfinished_run()
  lock_run(dir)
  expect_true(run_active(dir))
  plan <- arena_plan(backends = "dplyr", tasks = c("filter_rows", "summarise_lowcard"), sizes = 1e3)
  expect_null(find_unfinished(plan))
  expect_snapshot(error = TRUE, arena_run(dir = dir), transform = function(x) gsub("'[^']*old'", "<dir>", x))
  writeLines(c("999999999", "0"), file.path(dir, "pid"))
  expect_false(run_active(dir))
})

test_that("a run that stops says how to continue it", {
  withr::local_options(tidyarena.runs_dir = withr::local_tempdir())
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  local_mocked_bindings(run_cell = function(...) stop("power cut"))
  msgs <- character()
  withCallingHandlers(
    expect_error(
      arena_run(backends = "dplyr", tasks = "filter_rows", sizes = 1e3, install = "never"),
      "power cut"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(msgs, "stopped before it finished", all = FALSE)
  dir <- list.files(runs_dir(), full.names = TRUE)
  expect_false(file.exists(file.path(dir, "pid")))
  expect_null(read_run_json(dir)$finished)
})
