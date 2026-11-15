test_that("arena_run() measures every repetition", {
  skip_on_cran()
  res <- local_tiny_run()
  expect_s3_class(res, "arena_results")
  expect_equal(nrow(res$cells), 2)
  expect_true(all(res$cells$status == "ok"))
  expect_true(all(res$cells$correct))
  # One warm-up plus three timed repetitions per cell.
  expect_equal(nrow(res$reps), 2 * 4)
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
