test_that("arena_plan() always includes the anchor", {
  plan <- arena_plan("l", backends = character(), budget = 1e12)
  expect_setequal(unique(plan$backend), anchor_backend)
  expect_setequal(unique(plan$size), c(1e6, 1e7, 1e8))
  expect_setequal(unique(plan$task), arena_tasks()$task)
})

test_that("arena_plan() skips cells over budget", {
  plan <- arena_plan("s", backends = "dplyr", tasks = c("filter_rows", "summarise_lowcard"), budget = 1e9)
  expect_true(all(plan$action[plan$size == 1e7] == "skip"))
  expect_true(all(plan$reason[plan$size == 1e7] %in% c(
    "over memory budget",
    "data too large to generate in the memory budget"
  )))
})

test_that("arena_plan() rejects unknown ids", {
  expect_snapshot(arena_plan("s", tasks = "nope"), error = TRUE)
})

test_that("arena_plan() skips sizes a data set doesn't have, or can't generate", {
  plan <- arena_plan("s", backends = character(), tasks = "dbbench_join_q01", budget = 1e12)
  expect_match(plan$reason[plan$size == 1e6], "no dbbench_join data at this size")
  expect_equal(plan$action[plan$size == 1e7], "run")
  plan <- arena_plan("s", backends = character(), tasks = "dbbench_join_q01", budget = 3e9)
  expect_equal(plan$reason[plan$size == 1e7], "data too large to generate in the memory budget")
})

test_that("arena_plan() skips tasks whose packages are missing", {
  plan <- arena_plan("s", backends = character(), tasks = "tpch_q07", budget = 1e12, libpath = withr::local_tempdir())
  expect_true(all(plan$action == "skip"))
})

test_that("xs, s and m run a sample of the tasks, with the settings of l", {
  n <- nrow(arena_tasks())
  for (tier in c("xs", "s", "m")) {
    plan <- arena_plan(tier, backends = character(), budget = 1e12)
    share <- c(xs = 1 / 8, s = 1 / 4, m = 1 / 2)[[tier]]
    expect_length(unique(plan$task), ceiling(n * share))
    expect_equal(unique(plan$size), c(1e6, 1e7, 1e8))
    expect_equal(attr(plan, "settings")$reps, tier_settings("l")$reps)
  }
  xs <- arena_plan("xs", backends = character())
  # The same tasks each time, until a run finishes.
  expect_equal(unique(arena_plan("xs", backends = character())$task), unique(xs$task))
  expect_equal(unique(arena_plan("xs", backends = character(), tasks = "filter_rows")$task), "filter_rows")
})

test_that("successive samples cover every task before any repeats", {
  tasks <- arena_tasks()$task
  counts <- stats::setNames(integer(length(tasks)), tasks)
  seen <- character()
  for (i in 1:8) {
    picked <- sample_tasks(tasks, 1 / 8, counts)
    expect_length(intersect(picked, seen), 0)
    seen <- c(seen, picked)
    counts[picked] <- counts[picked] + 1L
  }
  expect_setequal(seen, tasks)
  # A new round starts once every task has run.
  expect_length(sample_tasks(tasks, 1 / 8, counts), ceiling(length(tasks) / 8))
})

test_that("task_counts() counts finished runs with unchanged tasks", {
  dir <- withr::local_tempdir()
  hashes <- task_hashes(c("filter_rows", "summarise_lowcard"))
  write_run <- function(id, tier, finished = "2026-01-01", hashes_ = hashes) {
    run <- list(
      suite_version = suite_version, tier = tier, finished = finished,
      task_hashes = as.list(hashes_)
    )
    dir.create(file.path(dir, id))
    write_run_json(run, file.path(dir, id))
  }
  write_run("a", "xs")
  write_run("b", "m")
  write_run("c", "xl")
  write_run("d", "s", finished = NULL)
  write_run("e", "s", hashes_ = c(filter_rows = "old"))
  counts <- task_counts(dir)
  expect_equal(unname(counts[c("filter_rows", "summarise_lowcard", "arrange_two")]), c(3L, 3L, 0L))
})
