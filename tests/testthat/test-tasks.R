test_that("every task parses and has a known family", {
  tasks <- arena_tasks()
  expect_true(all(nzchar(tasks$code)))
  for (code in tasks$code) {
    expect_no_error(parse(text = code))
  }
  expect_false(anyDuplicated(tasks$task) > 0)
})

test_that("every task runs on plain dplyr", {
  skip_if_not_installed("dplyr")
  env <- small_tables()
  for (code in arena_tasks()$code) {
    expect_no_error(eval(parse(text = code), envir = env), message = code)
  }
})

test_that("every grouped task has a .by twin", {
  tasks <- arena_tasks()
  grouped <- tasks$task[tasks$idiom %in% "group_by"]
  twins <- tasks$task[tasks$idiom %in% ".by"]
  expect_setequal(paste0(grouped, "_by"), twins)
  expect_true(all(grepl("group_by(", tasks$code[tasks$idiom %in% "group_by"], fixed = TRUE)))
  expect_true(all(grepl(".by =", tasks$code[tasks$idiom %in% ".by"], fixed = TRUE)))
  expect_false(any(grepl("group_by(", tasks$code[tasks$idiom %in% ".by"], fixed = TRUE)))
})

test_that("both idioms give the same result on dplyr", {
  skip_if_not_installed("dplyr")
  tasks <- arena_tasks()
  env <- small_tables()
  for (task in tasks$task[tasks$idiom %in% "group_by"]) {
    a <- as.data.frame(eval(parse(text = tasks$code[tasks$task == task]), envir = env))
    b <- as.data.frame(eval(parse(text = tasks$code[tasks$task == paste0(task, "_by")]), envir = env))
    sort_rows <- function(d) d[do.call(order, unname(d)), , drop = FALSE]
    expect_equal(sort_rows(b), sort_rows(a), ignore_attr = TRUE, info = task)
  }
})
