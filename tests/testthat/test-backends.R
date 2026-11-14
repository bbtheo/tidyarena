test_that("backend setup code parses", {
  for (def in backend_defs()) {
    expect_no_error(parse(text = def$setup))
    expect_true(any(grepl(".collect <-", def$setup, fixed = TRUE)))
  }
})

test_that("arena_backends() reports availability per library", {
  backends <- arena_backends(libpath = withr::local_tempdir())
  expect_false(any(backends$available))
  expect_true(anchor_backend %in% backends$backend)
})
