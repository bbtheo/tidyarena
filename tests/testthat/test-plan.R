test_that("arena_plan() always includes the anchor", {
  plan <- arena_plan("quick", backends = character(), budget = 1e12)
  expect_setequal(unique(plan$backend), anchor_backend)
  expect_setequal(unique(plan$size), c(1e6, 1e7))
})

test_that("arena_plan() skips cells over budget", {
  plan <- arena_plan("quick", backends = "dplyr", budget = 1e9)
  expect_true(all(plan$action[plan$size == 1e7] == "skip"))
  expect_true(all(plan$reason[plan$size == 1e7] == "over memory budget"))
})

test_that("arena_plan() rejects unknown ids", {
  expect_snapshot(arena_plan("quick", tasks = "nope"), error = TRUE)
})
