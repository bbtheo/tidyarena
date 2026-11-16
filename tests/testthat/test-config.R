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
  expect_equal(install_route("tidypolars"), "github")
  expect_equal(install_route("cuplyr"), "manual")
  expect_true(installable("tidypolars", only_cran = FALSE))
  expect_false(installable("tidypolars", only_cran = TRUE))
  expect_false(installable("cuplyr", only_cran = FALSE))
})

fake_releases <- function(url) {
  release <- function(tag, date, prerelease = FALSE, draft = FALSE) {
    list(tag_name = tag, published_at = paste0(date, "T10:00:00Z"), prerelease = prerelease, draft = draft)
  }
  list(
    release("lib-v2.2.0", "2026-11-20"),
    release("v2.2.0-rc.1", "2026-11-15", prerelease = TRUE),
    release("v2.1.0", "2026-11-08"),
    release("lib-v2.1.0-rc.1", "2026-11-01", prerelease = TRUE),
    release("v2.0.0", "2026-10-18"),
    release("v1.16.0", "2026-09-13"),
    release("v3.0.0", "2026-12-01", draft = TRUE)
  )
}

test_that("github_release() picks the newest release, by snapshot date", {
  local_mocked_bindings(github_get = fake_releases)
  withr::defer(the$releases <- NULL)
  the$releases <- NULL
  expect_equal(github_release("owner/repo"), "v2.1.0")
  expect_equal(github_release("owner/repo", "2026-10-18"), "v2.0.0")
  expect_equal(github_release("owner/repo", "2026-10-17"), "v1.16.0")
  expect_snapshot(error = TRUE, github_release("owner/repo", "2026-01-01"))
})

test_that("install_sources() adds GitHub releases and the build environment", {
  local_mocked_bindings(binary_target = function() NULL, github_get = fake_releases)
  withr::defer(the$releases <- NULL)
  the$releases <- NULL
  sources <- install_sources(c("arrow", "tidypolars"))
  expect_named(sources$repos, "CRAN")
  expect_equal(sources$releases$polars, list(repo = "pola-rs/r-polars", tag = "v2.1.0"))
  expect_equal(
    package_refs(c("dplyr", "polars"), sources$releases),
    c("dplyr", "github::pola-rs/r-polars@v2.1.0")
  )
  expect_equal(sources$env[["NOT_CRAN"]], "true")
  expect_equal(sources$env[["LIBARROW_BINARY"]], "true")
  expect_equal(sources$env[["LIBR_POLARS_BUILD"]], "false")

  cran <- install_sources(c("arrow", "tidypolars"), only_cran = TRUE)
  expect_named(cran$repos, "CRAN")
  expect_equal(cran$releases, list())
  expect_equal(cran$env, character())

  expect_equal(install_sources("dplyr")$env, character())
})

test_that("a package from GitHub other than the release is outdated, a dev version too", {
  lib <- withr::local_tempdir()
  dir.create(file.path(lib, "fakepolars"))
  writeLines(c("Package: fakepolars", "Version: 2.1.0.9000"), file.path(lib, "fakepolars", "DESCRIPTION"))
  releases <- list(fakepolars = list(repo = "owner/repo", tag = "v2.1.0"))
  out <- outdated_packages("fakepolars", lib, character(), releases)
  expect_equal(out$installed, "2.1.0.9000")
  expect_equal(out$available, "2.1.0")
  releases$fakepolars$tag <- "v2.1.0.9000"
  expect_equal(nrow(outdated_packages("fakepolars", lib, character(), releases)), 0)
})

test_that("arena_install() with only_cran explains why tidypolars is skipped", {
  skip_if_not_installed("pak")
  local_mocked_bindings(packages_installed = function(packages, lib) rep(FALSE, length(packages)))
  messages <- capture_messages(arena_install("tidypolars", lib = withr::local_tempdir(), only_cran = TRUE))
  expect_match(paste(messages, collapse = ""), "only CRAN is allowed")
})

test_that("github_has_src() looks for compiled code at the tag", {
  local_mocked_bindings(github_get = function(url) {
    if (grepl("r-polars", url)) {
      list(list(name = "R", type = "dir"), list(name = "src", type = "dir"))
    } else {
      list(list(name = "R", type = "dir"), list(name = "DESCRIPTION", type = "file"))
    }
  })
  expect_true(github_has_src("github::pola-rs/r-polars@v2.1.0"))
  expect_false(github_has_src("github::etiennebacher/tidypolars@v0.20.0"))
})

test_that("leaving out a backend leaves out its Parquet variant", {
  backends <- tibble::tibble(
    backend = c("dplyr", "arrow", "arrow_parquet", "duckplyr", "duckplyr_parquet"),
    available = TRUE
  )
  suppressMessages(arena_config(exclude = c("arrow", "duckplyr_parquet")))
  expect_equal(default_backends(backends), c("dplyr", "duckplyr"))
  expect_equal(base_backends(c("arrow_parquet", "arrow", "dplyr")), c("arrow", "dplyr"))
  expect_false("arrow_parquet" %in% names(install_defs()))
})
