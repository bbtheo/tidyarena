test_that("package_requirements() reads versioned Depends and Imports", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "Package: x",
      "Version: 1.0",
      "Depends: R (>= 4.1), methods",
      "Imports: dplyr (>= 1.2.0),\n    rlang(>=1.1.0), cli, vctrs ( == 0.7.0 )"
    ),
    file.path(dir, "DESCRIPTION")
  )
  reqs <- package_requirements(dir)
  expect_equal(vapply(reqs, `[[`, "", "package"), c("dplyr", "rlang", "vctrs"))
  expect_equal(vapply(reqs, `[[`, "", "op"), c(">=", ">=", "=="))
  expect_equal(vapply(reqs, `[[`, "", "version"), c("1.2.0", "1.1.0", "0.7.0"))
})

test_that("version_satisfies() compares versions", {
  expect_true(version_satisfies("1.2.1", ">=", "1.2.0"))
  expect_false(version_satisfies("1.1.4", ">=", "1.2.0"))
  expect_true(version_satisfies("1.10.0", ">", "1.9.0"))
  expect_false(version_satisfies("0.7.0", "!=", "0.7.0"))
})

test_that("describe_unmet() says what is wrong", {
  unmet <- data.frame(
    package = "duckplyr",
    needs = "dplyr",
    required = ">= 1.2.0",
    found = "1.1.4",
    source = "session"
  )
  expect_equal(
    describe_unmet(unmet),
    "duckplyr needs dplyr >= 1.2.0, but your library has 1.1.4."
  )
})

unmet_dplyr <- data.frame(
  package = "fake",
  needs = "dplyr",
  required = ">= 9.0",
  found = "1.2.1",
  source = "session"
)

local_preflight <- function(load_ok, retry_ok = TRUE, unmet = unmet_dplyr, env = parent.frame()) {
  local_mocked_bindings(
    load_test = function(backends, libpath) {
      stats::setNames(lapply(backends, function(b) list(ok = load_ok, error = "boom")), backends)
    },
    unmet_requirements = function(...) unmet,
    retry_with_arena = function(backend) list(ok = retry_ok, error = "still boom"),
    .env = env
  )
}

test_that("preflight() passes backends that load", {
  local_preflight(load_ok = TRUE)
  out <- suppressMessages(preflight("dplyr", "session", FALSE, .libPaths()))
  expect_equal(out$prefer, "session")
  expect_length(out$skip, 0)
})

test_that("preflight() switches to tidyarena's packages by default when not interactive", {
  local_preflight(load_ok = FALSE)
  rlang::local_interactive(FALSE)
  expect_snapshot(out <- preflight("duckplyr", "session", FALSE, .libPaths()))
  expect_equal(out$prefer, "arena")
  expect_length(out$skip, 0)
})

test_that("preflight() skips when the user says no", {
  local_preflight(load_ok = FALSE)
  local_mocked_bindings(ask_yes_no = function(...) FALSE)
  rlang::local_interactive(TRUE)
  out <- suppressMessages(preflight("duckplyr", "session", FALSE, .libPaths()))
  expect_equal(out$prefer, "session")
  expect_match(out$skip[["duckplyr"]], "needs dplyr >= 9.0")
})

test_that("preflight() skips when switching doesn't help", {
  local_preflight(load_ok = FALSE, retry_ok = FALSE)
  rlang::local_interactive(FALSE)
  out <- suppressMessages(preflight("duckplyr", "session", FALSE, .libPaths()))
  expect_equal(out$prefer, "session")
  expect_match(out$skip[["duckplyr"]], "still boom")
})

test_that("preflight() doesn't change a libpath the user gave", {
  local_preflight(load_ok = FALSE)
  rlang::local_interactive(FALSE)
  out <- suppressMessages(preflight("duckplyr", "session", TRUE, .libPaths()))
  expect_equal(out$prefer, "session")
  expect_named(out$skip, "duckplyr")
})

test_that("preflight() skips failures it can't explain without asking", {
  local_preflight(load_ok = FALSE, unmet = unmet_dplyr[0, ])
  local_mocked_bindings(ask_yes_no = function(...) stop("should not ask"))
  rlang::local_interactive(TRUE)
  out <- suppressMessages(preflight("duckplyr", "session", FALSE, .libPaths()))
  expect_match(out$skip[["duckplyr"]], "boom")
})

# A real version of the problem: tidyarena's library has a backend that
# needs a newer dependency than the one in "your" library.
install_fake <- function(name, version, lib, imports = NULL, libpath = lib) {
  src <- file.path(withr::local_tempdir(.local_envir = parent.frame(2)), name)
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(
    c(
      paste("Package:", name),
      paste("Version:", version),
      "Title: Fake",
      "Description: Fake package for tests.",
      "License: MIT",
      "Authors@R: person('A', 'B', role = c('aut', 'cre'), email = 'a@b.c')",
      if (!is.null(imports)) paste("Imports:", imports)
    ),
    file.path(src, "DESCRIPTION")
  )
  namespace <- "export(hello)"
  if (!is.null(imports)) {
    namespace <- c(namespace, paste0("import(", sub("\\s*\\(.*$", "", imports), ")"))
  }
  writeLines(namespace, file.path(src, "NAMESPACE"))
  writeLines("hello <- function() 'hi'", file.path(src, "R", "hello.R"))
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  out <- processx::run(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-test-load", paste0("--library=", lib), src),
    env = c("current", R_LIBS = paste(libpath, collapse = .Platform$path.sep)),
    error_on_status = FALSE
  )
  if (out$status != 0) stop(out$stderr)
}

test_that("a backend that needs newer packages than yours runs with tidyarena's", {
  skip_on_cran()
  withr::local_envvar(
    R_USER_DATA_DIR = withr::local_tempdir(),
    R_USER_CACHE_DIR = withr::local_tempdir()
  )
  session <- withr::local_tempdir()
  arena <- arena_lib()
  install_fake("fakeold", "1.0", session)
  install_fake("fakeold", "2.0", arena)
  install_fake("fakebackend", "1.0", arena, imports = "fakeold (>= 2.0)")

  local_mocked_bindings(backend_defs = function() {
    list(fake = list(
      packages = "fakebackend",
      gpu = FALSE,
      mem_factor = 1,
      setup = c("library(fakebackend)", ".collect <- function(x) x", ".convert <- function(x, name) x")
    ))
  })
  session_first <- c(session, .libPaths(), arena)

  unmet <- unmet_requirements("fakebackend", session_first)
  expect_equal(describe_unmet(unmet), "fakebackend needs fakeold >= 2.0, but your library has 1.0.")
  expect_false(suppressMessages(load_test("fake", session_first))$fake$ok)

  rlang::local_interactive(FALSE)
  out <- suppressMessages(preflight("fake", "session", FALSE, session_first))
  expect_equal(out$prefer, "arena")
  expect_length(out$skip, 0)
  expect_true(load_test("fake", out$libpath)$fake$ok)
})
