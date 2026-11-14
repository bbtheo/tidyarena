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
