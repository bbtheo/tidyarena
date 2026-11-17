test_that("arena_plan() always includes the anchor", {
  plan <- arena_plan("l", backends = character(), budget = 1e12)
  expect_setequal(unique(plan$backend), anchor_backend)
  expect_setequal(unique(plan$size), c(1e6, 1e7))
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

test_that("the parts split the tasks evenly, twins together", {
  tasks <- arena_tasks()
  n <- table(factor(tasks$part, 1:4))
  expect_lte(max(n) - min(n), 3)
  base <- sub("_by$", "", tasks$task[tasks$idiom %in% ".by"])
  twins <- tasks$task[tasks$idiom %in% ".by"]
  expect_equal(tasks$part[match(twins, tasks$task)], tasks$part[match(base, tasks$task)])
  # Every data set with four tasks or more is in every part.
  by_data <- table(tasks$data, tasks$part)
  expect_true(all(by_data[rowSums(by_data) >= 4, ] > 0))
})

test_that("tiers run their parts at their sizes", {
  tasks <- arena_tasks()
  for (tier in c("xs", "s", "m", "l")) {
    plan <- arena_plan(tier, backends = character(), budget = 1e12)
    parts <- unique(tasks$part[match(plan$task, tasks$task)])
    expect_length(parts, c(xs = 1, s = 1, m = 2, l = 4)[[tier]])
    expect_setequal(unique(plan$task), tasks$task[tasks$part %in% parts])
    expect_equal(unique(plan$size), tier_settings(tier)$sizes)
  }
  plan <- arena_plan("s", backends = character(), part = 3)
  expect_setequal(unique(plan$task), tasks$task[tasks$part == 3])
  # The same parts each time, until a run finishes.
  expect_equal(arena_plan("m", backends = character())$task, arena_plan("m", backends = character())$task)
  expect_snapshot(arena_plan("s", part = 5), error = TRUE)
  expect_snapshot(arena_plan("s", tasks = "filter_rows", part = 1), error = TRUE)
})

test_that("successive runs cover every part before any repeats", {
  counts <- integer(4)
  seen <- integer()
  for (i in 1:4) {
    picked <- pick_parts(1, 1e6, counts)
    expect_false(picked %in% seen)
    seen <- c(seen, picked)
    counts[picked] <- counts[picked] + 1L
  }
  expect_setequal(seen, 1:4)
  # Parts taken across the end of a round are still different parts, on
  # every machine.
  for (seed in as.character(1:50)) {
    local_mocked_bindings(sample_seed = function() seed)
    expect_length(unique(pick_parts(2, 1e6, c(1L, 1L, 1L, 0L))), 2)
    expect_length(unique(pick_parts(3, 1e6, c(2L, 1L, 2L, 2L))), 3)
  }
})

test_that("part_counts() counts finished runs of whole parts at the sizes, with unchanged tasks", {
  dir <- withr::local_tempdir()
  tasks <- arena_tasks()
  write_run <- function(id, parts, sizes = c(1e6, 1e7), finished = "2026-01-01", stale = character()) {
    hashes <- stats::setNames(tasks$hash, tasks$task)[tasks$part %in% parts]
    hashes[names(hashes) %in% stale] <- "old"
    run <- list(suite_version = suite_version, sizes = sizes, finished = finished, task_hashes = as.list(hashes))
    dir.create(file.path(dir, id))
    write_run_json(run, file.path(dir, id))
  }
  write_run("a", 1)
  write_run("b", 1:2)
  write_run("c", 1:4, sizes = c(1e6, 1e7, 1e8))
  write_run("d", 3, finished = NULL)
  write_run("e", 4, stale = tasks$task[tasks$part == 4][1])
  write_run("f", 3, sizes = 1e6)
  expect_equal(part_counts(c(1e6, 1e7), dir), c(3L, 2L, 1L, 1L))
  expect_equal(part_counts(1e6, dir), c(3L, 2L, 2L, 1L))
})
