test_that("arena_check_backend() runs every task and compares with dplyr", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  cells <- suppressMessages(arena_check_backend("dplyr", tasks = c("filter_rows", "arrange_two"), n = 100))
  expect_equal(cells$task, c("filter_rows", "arrange_two"))
  expect_equal(cells$status, c("ok", "ok"))
  expect_true(all(cells$correct))
})

test_that("arena_check_backend() reports a backend that fails or differs", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  defs <- backend_defs()
  defs$broken <- defs$dplyr
  defs$broken$setup <- c("df$x <- df$x + 1", defs$dplyr$setup)
  local_mocked_bindings(backend_defs = function() defs)
  cells <- suppressMessages(arena_check_backend("broken", tasks = c("filter_rows", "distinct_pairs"), n = 100))
  expect_equal(cells$correct, c(FALSE, TRUE))
  expect_match(cells$note[[1]], "differs from dplyr")

  defs$broken$setup <- c("stop('no engine')", defs$dplyr$setup)
  cells <- suppressMessages(arena_check_backend("broken", tasks = "filter_rows", n = 100))
  expect_equal(cells$status, "setup_error")
  expect_match(cells$note, "no engine")
})

test_that("arena_check_task() checks a task and its .by form", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  local_mocked_bindings(measure_mem_factor = function(tasks, libpath) rep(0.2, length(tasks)))
  result <- suppressMessages(arena_check_task("summarise_skew", backends = "dplyr", n = 100))
  expect_true(result$deterministic)
  expect_named(result$mem_factor, c("summarise_skew", "summarise_skew_by"))
  expect_equal(nrow(result$cells), 0)
})

test_that("arena_check_task() warns when the memory factor is too low", {
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  local_mocked_bindings(measure_mem_factor = function(tasks, libpath) rep(5, length(tasks)))
  messages <- capture_messages(arena_check_task("filter_rows", backends = "dplyr", n = 100))
  expect_match(paste(messages, collapse = ""), "too low")
})
