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
  expect_snapshot(arena_install("duckplyr", lib = lib), error = TRUE)
})

test_that("arena_install() does nothing when everything is installed", {
  skip_if_not_installed("pak")
  local_mocked_bindings(packages_installed = function(packages, lib) rep(TRUE, length(packages)))
  expect_message(arena_install("dplyr", lib = withr::local_tempdir()), "installed")
})
