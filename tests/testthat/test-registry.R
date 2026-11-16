test_that("every task and backend file loads", {
  tasks <- task_defs()
  expect_length(tasks, 119)
  expect_equal(sum(vapply(tasks, `[[`, character(1), "data") == "arena"), 23)
  expect_equal(names(backend_defs())[[1]], anchor_backend)
})

test_that("backends get default fields", {
  dplyr <- backend_defs()$dplyr
  expect_false(dplyr$gpu)
  expect_equal(dplyr$mem_factor, 3)
  expect_true(any(grepl("^\\.collect <-", dplyr$setup)))
  # A backend with its own collect keeps it.
  polars <- backend_defs()$tidypolars$setup
  expect_equal(sum(grepl("^\\.collect <-", polars)), 1)
})

test_that("definition files are validated", {
  expect_snapshot(error = TRUE, {
    check_def(list(code = "df"), "bad", "tasks", task_fields)
    check_def(list(family = "f", code = "df", memfactor = 1), "bad", "tasks", task_fields)
    check_def("df", "bad", "tasks", task_fields)
  })
})

test_that("a task file can use tidyarena's internals", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines("list(code = 'df', family = 'f', anchor = anchor_backend)", path)
  expect_equal(read_def_file(path)$anchor, "dplyr")
})

test_that("by_twin() rewrites group_by() pipelines", {
  expect_equal(
    by_twin('df |> group_by(g_low) |> summarise(n = n(), .groups = "drop")'),
    "df |> summarise(n = n(), .by = g_low)"
  )
  expect_equal(
    by_twin('df |> group_by(a, b) |> summarise(n = n(), .groups = "drop") |> arrange(n)'),
    "df |> summarise(n = n(), .by = c(a, b)) |> arrange(n)"
  )
  expect_equal(
    by_twin("df |> group_by(g) |> mutate(y = x - mean(x)) |> ungroup() |> select(y)"),
    "df |> mutate(y = x - mean(x), .by = g) |> select(y)"
  )
  expect_equal(
    by_twin("df |> group_by(g) |> filter(x == max(x))"),
    "df |> filter(x == max(x), .by = g)"
  )
})

test_that("by_twin() refuses pipelines it can't rewrite", {
  expect_snapshot(error = TRUE, {
    by_twin("df |> group_by(g) |> mutate(y = x) |> filter(y > 0)", "keeps_grouping")
    by_twin("df |> group_by(g) |> arrange(x)", "no_by_verb")
    by_twin("df |> group_by(g, .add = TRUE) |> summarise(n = n())", "named")
    by_twin("df |> group_by(a, b) |> summarise(n = n()) |> mutate(m = n)", "partial_drop")
  })
})

test_that("a task can opt out of its twin", {
  dir <- withr::local_tempdir()
  writeLines(
    "list(family = 'f', code = 'df |> group_by(g) |> arrange(x)', twin = FALSE)",
    file.path(dir, "sorted.R")
  )
  local_mocked_bindings(read_defs = function(kind) list(sorted = read_def_file(file.path(dir, "sorted.R"))))
  defs <- task_defs()
  expect_named(defs, "sorted")
  expect_null(defs$sorted$twin)
  expect_equal(defs$sorted$mem_factor, 1)
})

test_that("tasks have a stable hash of their code", {
  tasks <- arena_tasks()
  expect_equal(unname(tasks$hash), vapply(tasks$code, task_hash, character(1), USE.NAMES = FALSE))
  expect_false(anyDuplicated(tasks$hash) > 0)
  expect_equal(task_hash("df |> count(g_low)"), task_hash("df |> count(g_low)"))
  expect_false(task_hash("df |> count(g_low)") == task_hash("df |> count(g_high)"))
})

test_that("a backend that reads Parquet gets a Parquet variant", {
  defs <- backend_defs()
  arrow <- defs$arrow_parquet
  expect_equal(arrow$source, "parquet")
  expect_equal(arrow$base, "arrow")
  expect_equal(defs$arrow$source, "memory")
  expect_true(any(grepl("^\\.read <- function\\(path, name\\)", arrow$setup)))
  expect_false(any(grepl("^\\.convert <-", arrow$setup)))
  expect_null(arrow$read)
  # Right after its base backend.
  expect_equal(match("arrow_parquet", names(defs)), match("arrow", names(defs)) + 1)
  expect_false("dplyr_parquet" %in% names(defs))
  expect_equal(defs$cuplyr_parquet$base, "cuplyr")
})
