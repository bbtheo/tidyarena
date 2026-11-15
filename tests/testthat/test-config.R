test_that("arena_config() starts from the defaults and saves changes", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_equal(suppressMessages(arena_config()), config_defaults())
  expect_snapshot(arena_config(exclude = c("cuplyr", "dtplyr"), build = "capped"))
  config <- read_config()
  expect_equal(config$exclude, c("cuplyr", "dtplyr"))
  expect_equal(config$build, "capped")
  expect_false(config$only_cran)

  suppressMessages(arena_config(only_cran = TRUE))
  expect_equal(read_config()$exclude, c("cuplyr", "dtplyr"))
  expect_true(read_config()$only_cran)

  suppressMessages(arena_config(exclude = character()))
  expect_equal(read_config()$exclude, character())
  suppressMessages(arena_config(reset = TRUE))
  expect_equal(read_config(), config_defaults())
})

test_that("arena_config() checks its arguments", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_snapshot(error = TRUE, {
    arena_config(exclude = "dplyr")
    arena_config(exclude = "nope")
    arena_config(build = "always")
    arena_config(only_cran = "yes")
  })
})

test_that("left-out backends are not planned unless named", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(packages_installed = function(packages, lib) rep(TRUE, length(packages)))
  suppressMessages(arena_config(exclude = "dtplyr"))
  plan <- arena_plan("quick", tasks = "filter_rows")
  expect_false("dtplyr" %in% plan$backend)
  plan <- arena_plan("quick", backends = "dtplyr", tasks = "filter_rows")
  expect_true("dtplyr" %in% plan$backend)
})

test_that("install routes follow the backend definitions and only_cran", {
  expect_equal(install_route("dplyr"), "cran")
  expect_equal(install_route("arrow"), "cran")
  expect_equal(install_route("tidypolars"), "repos")
  expect_equal(install_route("cuplyr"), "manual")
  expect_true(installable("tidypolars", only_cran = FALSE))
  expect_false(installable("tidypolars", only_cran = TRUE))
  expect_false(installable("cuplyr", only_cran = FALSE))
})

test_that("install_sources() adds R-universe and the build environment", {
  local_mocked_bindings(binary_target = function() NULL)
  sources <- install_sources(c("arrow", "tidypolars"))
  expect_equal(
    names(sources$repos),
    c("rpolars", "etiennebacher", "CRAN")
  )
  expect_equal(sources$env[["NOT_CRAN"]], "true")
  expect_equal(sources$env[["LIBARROW_BINARY"]], "true")
  expect_equal(sources$env[["LIBR_POLARS_BUILD"]], "false")

  cran <- install_sources(c("arrow", "tidypolars"), only_cran = TRUE)
  expect_named(cran$repos, "CRAN")
  expect_equal(cran$env, character())

  expect_equal(install_sources("dplyr")$env, character())
})

test_that("arena_install() with only_cran explains why tidypolars is skipped", {
  skip_if_not_installed("pak")
  local_mocked_bindings(packages_installed = function(packages, lib) rep(FALSE, length(packages)))
  messages <- capture_messages(arena_install("tidypolars", lib = withr::local_tempdir(), only_cran = TRUE))
  expect_match(paste(messages, collapse = ""), "only CRAN is allowed")
})
