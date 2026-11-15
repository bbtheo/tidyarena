test_that("ppm_url() accepts latest or a date", {
  expect_equal(ppm_url(), "https://p3m.dev/cran/latest")
  expect_equal(ppm_url("2026-03-01"), "https://p3m.dev/cran/2026-03-01")
  expect_snapshot(ppm_url("yesterday"), error = TRUE)
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
    platform_label = function() "x86_64-pc-linux-gnu-somedistro",
    install_packages = function(...) stop("should not install")
  )
  lib <- withr::local_tempdir()
  expect_snapshot(arena_install("duckplyr", lib = lib), error = TRUE)
})

test_that("arena_install() does nothing when everything is installed", {
  skip_if_not_installed("pak")
  local_mocked_bindings(packages_installed = function(packages, lib) rep(TRUE, length(packages)))
  expect_message(arena_install("dplyr", lib = withr::local_tempdir()), "installed")
})
