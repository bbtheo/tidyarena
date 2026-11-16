test_that("arena_plan() always includes the anchor", {
  plan <- arena_plan("s", backends = character(), budget = 1e12)
  expect_setequal(unique(plan$backend), anchor_backend)
  expect_setequal(unique(plan$size), c(1e6, 1e7))
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
