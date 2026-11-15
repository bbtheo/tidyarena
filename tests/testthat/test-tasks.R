test_that("every task parses and has a known family", {
  tasks <- arena_tasks()
  expect_true(all(nzchar(tasks$code)))
  for (code in tasks$code) {
    expect_no_error(parse(text = code))
  }
  expect_false(anyDuplicated(tasks$task) > 0)
})

test_that("every task of tidyarena's own data runs on plain dplyr", {
  skip_if_not_installed("dplyr")
  env <- small_tables()
  tasks <- arena_tasks()
  for (code in tasks$code[tasks$data == "arena"]) {
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
  for (task in tasks$task[tasks$idiom %in% "group_by" & tasks$data == "arena"]) {
    a <- as.data.frame(eval(parse(text = tasks$code[tasks$task == task]), envir = env))
    b <- as.data.frame(eval(parse(text = tasks$code[tasks$task == paste0(task, "_by")]), envir = env))
    sort_rows <- function(d) d[do.call(order, unname(d)), , drop = FALSE]
    expect_equal(sort_rows(b), sort_rows(a), ignore_attr = TRUE, info = task)
  }
})

test_that("tasks of every data set run on plain dplyr", {
  skip_if_not_installed("dplyr")
  skip_on_cran()
  tasks <- arena_tasks()
  dir <- withr::local_tempdir()
  for (package in unique(unlist(tasks$packages))) {
    skip_if_not_installed(package)
    withr::local_package(package)
  }
  for (dataset in c("dbbench_groupby", "joins", "strings", "datetimes", "tpch")) {
    if (dataset == "tpch") {
      skip_if_not(packages_installed("duckdb", c(.libPaths(), arena_lib())), "duckdb is not installed")
      skip_if_not_installed("lubridate")
    }
    data <- arena_data(dataset_def(dataset)$min_size * 10, dir, dataset)
    env <- new.env(parent = asNamespace("dplyr"))
    for (name in names(data$tables)) {
      env[[name]] <- tibble::as_tibble(as.data.frame(nanoparquet::read_parquet(data$tables[[name]])))
    }
    for (code in tasks$code[tasks$data == dataset]) {
      expect_no_error(
        suppressWarnings(suppressMessages(eval(parse(text = code), new.env(parent = env)))),
        message = code
      )
    }
  }
})
