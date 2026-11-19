test_that("ppm_url() accepts latest or a date", {
  expect_equal(ppm_url(), "https://p3m.dev/cran/latest")
  expect_equal(ppm_url("2026-03-01"), "https://p3m.dev/cran/2026-03-01")
  expect_equal(
    ppm_url("latest", "manylinux_2_28"),
    "https://p3m.dev/cran/__linux__/manylinux_2_28/latest"
  )
  expect_snapshot(ppm_url("yesterday"), error = TRUE)
})

test_that("binary_target() uses manylinux only where pak has no binaries", {
  skip_if_not(Sys.info()[["sysname"]] == "Linux" && R.version$arch == "x86_64")
  local_mocked_bindings(ppm_binaries = function() TRUE)
  expect_null(binary_target())

  local_mocked_bindings(ppm_binaries = function() FALSE, glibc_version = function() numeric_version("2.44"))
  expect_equal(binary_target(), "manylinux_2_28")

  local_mocked_bindings(glibc_version = function() numeric_version("2.17"))
  expect_null(binary_target())
})

test_that("build_jobs() fits the jobs in the budget", {
  expect_equal(build_jobs(1 * 2^30), 1L)
  expect_equal(build_jobs(10 * 2^30), min(4L, ps::ps_cpu_count(logical = FALSE)))
})

test_that("arena_install() refuses to compile unless asked", {
  skip_if_not_installed("pak")
  local_mocked_bindings(
    packages_to_build = function(...) c("duckdb", "cli"),
    ppm_binaries = function() FALSE,
    binary_target = function() NULL,
    platform_label = function() "x86_64-pc-linux-gnu-somedistro",
    install_packages = function(...) stop("should not install")
  )
  lib <- withr::local_tempdir()
  expect_snapshot(arena_install("dbplyr_duckdb", lib = lib), error = TRUE)
})

test_that("arena_install() does nothing when everything is installed", {
  skip_if_not_installed("pak")
  local_mocked_bindings(packages_installed = function(packages, lib) rep(TRUE, length(packages)))
  expect_message(arena_install("dplyr", lib = withr::local_tempdir()), "installed")
})

local_status <- function(missing = character(), outdated = NULL, env = parent.frame()) {
  outdated <- outdated %||%
    data.frame(package = character(), installed = character(), available = character())
  status <- list(
    missing = missing,
    elsewhere = character(),
    outdated = outdated,
    outdated_backends = if (nrow(outdated)) "dtplyr" else character()
  )
  installs <- new.env()
  installs$calls <- list()
  local_mocked_bindings(
    backend_status = function(...) status,
    install_backends = function(backends, update = FALSE) {
      installs$calls <- c(installs$calls, list(list(backends = backends, update = update)))
    },
    .env = env
  )
  installs
}

old_dt <- data.frame(package = "data.table", installed = "1.18.4", available = "1.18.6")

test_that("offer_installs() asks before installing", {
  skip_if_not_installed("pak")
  installs <- local_status(missing = "dbplyr_duckdb", outdated = old_dt)
  answers <- c(TRUE, FALSE)
  local_mocked_bindings(
    ask_yes_no = function(...) {
      answer <- answers[[1]]
      answers <<- answers[-1]
      answer
    }
  )
  rlang::local_interactive(TRUE)
  offer_installs(NULL, "ask", .libPaths())
  expect_length(installs$calls, 1)
  expect_equal(installs$calls[[1]]$backends, "dbplyr_duckdb")
  expect_false(installs$calls[[1]]$update)
})

test_that("offer_installs() installs and updates without asking when told to", {
  skip_if_not_installed("pak")
  installs <- local_status(missing = "dbplyr_duckdb", outdated = old_dt)
  local_mocked_bindings(ask_yes_no = function(...) stop("should not ask"))
  offer_installs(NULL, "update", .libPaths())
  expect_equal(installs$calls[[1]]$backends, "dbplyr_duckdb")
  expect_equal(installs$calls[[2]]$backends, "dtplyr")
  expect_true(installs$calls[[2]]$update)
})

test_that("offer_installs() only reports when not interactive", {
  skip_if_not_installed("pak")
  installs <- local_status(missing = "dbplyr_duckdb", outdated = old_dt)
  rlang::local_interactive(FALSE)
  expect_snapshot(prefer <- offer_installs(NULL, "ask", .libPaths()))
  expect_length(installs$calls, 0)
})

test_that("offer_installs() does nothing with install = 'never'", {
  installs <- local_status(missing = "dbplyr_duckdb")
  expect_silent(offer_installs(NULL, "never", .libPaths()))
  expect_length(installs$calls, 0)
})

test_that("outdated_packages() compares with the repository", {
  local_mocked_bindings(
    available.packages = function(...) {
      matrix("99.0.0", dimnames = list("rlang", "Version"))
    },
    .package = "utils"
  )
  out <- outdated_packages(c("rlang", "nonexistent"), .libPaths(), c(CRAN = "x"))
  expect_equal(out$package, "rlang")
  expect_equal(out$available, "99.0.0")
})

test_that("updating puts tidyarena's library first", {
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())
  dir.create(arena_lib(), recursive = TRUE)
  expect_equal(default_libpath("arena")[[1]], arena_lib())
  expect_equal(utils::tail(default_libpath("session"), 1), arena_lib())
  expect_equal(default_libpath()[[1]], .libPaths()[[1]])
})

test_that("offer_installs() returns the library to prefer", {
  skip_if_not_installed("pak")
  local_status(missing = "dbplyr_duckdb", outdated = old_dt)
  local_mocked_bindings(ask_yes_no = function(...) TRUE)
  rlang::local_interactive(TRUE)
  expect_equal(offer_installs(NULL, "ask", .libPaths()), "arena")
  expect_equal(offer_installs(NULL, "missing", .libPaths()), "session")
  expect_equal(offer_installs(NULL, "update", .libPaths()), "arena")
  expect_equal(offer_installs(NULL, "never", .libPaths()), "session")

  local_mocked_bindings(ask_yes_no = function(...) FALSE)
  expect_equal(offer_installs(NULL, "ask", .libPaths()), "session")
})

test_that("arena_install() of only backends it can't install just gives the hint", {
  skip_if_not_installed("pak")
  local_mocked_bindings(
    packages_installed = function(packages, lib) rep(FALSE, length(packages)),
    platform_info = function() list(os = "linux", arch = "x86_64", r = "4.6.0", gpus = list())
  )
  messages <- capture_messages(arena_install("cuplyr", lib = withr::local_tempdir()))
  expect_match(paste(messages, collapse = ""), "Can't install")
  expect_no_match(paste(messages, collapse = ""), "up to date")
})

local_needs_build <- function(env = parent.frame()) {
  installs <- new.env()
  installs$n <- 0
  local_mocked_bindings(
    packages_to_build = function(...) c("duckdb", "cli"),
    ppm_binaries = function() FALSE,
    binary_target = function() NULL,
    platform_label = function() "x86_64-pc-linux-gnu-somedistro",
    install_packages = function(...) installs$n <- installs$n + 1,
    write_install_record = function(...) invisible(),
    .env = env
  )
  installs
}

local_answers <- function(answers, env = parent.frame()) {
  asked <- new.env()
  asked$n <- 0
  local_mocked_bindings(
    ask_yes_no = function(...) {
      asked$n <- asked$n + 1
      answers[[asked$n]]
    },
    .env = env
  )
  asked
}

test_that("arena_install() asks whether to compile, and whether to always do so", {
  skip_if_not_installed("pak")
  installs <- local_needs_build()
  lib <- withr::local_tempdir()
  rlang::local_interactive(TRUE)

  asked <- local_answers(c(TRUE, FALSE))
  suppressMessages(arena_install("dbplyr_duckdb", lib = lib))
  expect_equal(installs$n, 1)
  expect_equal(asked$n, 2)
  expect_equal(read_config()$build, "never")

  asked <- local_answers(c(TRUE, TRUE))
  suppressMessages(arena_install("dbplyr_duckdb", lib = lib))
  expect_equal(installs$n, 2)
  expect_equal(read_config()$build, "capped")

  # Saved: no more questions.
  local_mocked_bindings(ask_yes_no = function(...) stop("should not ask"))
  suppressMessages(arena_install("dbplyr_duckdb", lib = lib))
  expect_equal(installs$n, 3)
  arena_config(reset = TRUE) |> suppressMessages()
})

test_that("arena_install() stops when compiling is turned down or build is given", {
  skip_if_not_installed("pak")
  installs <- local_needs_build()
  lib <- withr::local_tempdir()
  rlang::local_interactive(TRUE)

  asked <- local_answers(FALSE)
  expect_error(suppressMessages(arena_install("dbplyr_duckdb", lib = lib)), class = "tidyarena_needs_build")
  expect_equal(asked$n, 1)

  local_mocked_bindings(ask_yes_no = function(...) stop("should not ask"))
  expect_error(
    suppressMessages(arena_install("dbplyr_duckdb", lib = lib, build = "never")),
    class = "tidyarena_needs_build"
  )
  expect_equal(installs$n, 0)
})

test_that("install_backends() asks whether to compile only once", {
  skip_if_not_installed("pak")
  installs <- local_needs_build()
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())
  rlang::local_interactive(TRUE)

  asked <- local_answers(c(TRUE, FALSE))
  suppressMessages(install_backends(c("dbplyr_duckdb", "arrow")))
  expect_equal(asked$n, 2)
  expect_equal(installs$n, 2)

  asked <- local_answers(FALSE)
  suppressMessages(install_backends(c("dbplyr_duckdb", "arrow")))
  expect_equal(asked$n, 1)
  expect_equal(installs$n, 2)
})
