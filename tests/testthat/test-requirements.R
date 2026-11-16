fake_platform <- function(...) {
  gpu <- list(
    name = "Fake GPU",
    memory = 16 * 2^30,
    driver = "580.1",
    compute_capability = "8.6",
    cuda = "13.0"
  )
  gpu <- utils::modifyList(gpu, list(...))
  list(os = "linux", arch = "x86_64", r = "4.5.0", gpus = list(gpu))
}

test_that("a machine that meets cuplyr's requirements has none unmet", {
  expect_equal(unmet_platform("cuplyr", fake_platform()), character())
  expect_equal(unmet_platform("dplyr", fake_platform()), character())
})

test_that("each unmet requirement is explained", {
  expect_match(
    unmet_platform("cuplyr", fake_platform(compute_capability = "6.1")),
    "compute capability 7.0 or newer, the Fake GPU has 6.1"
  )
  expect_match(
    unmet_platform("cuplyr", fake_platform(cuda = "12.0")),
    "supports CUDA 12.2 or newer, driver 580.1 supports CUDA 12.0"
  )
  no_gpu <- fake_platform()
  no_gpu$gpus <- list()
  expect_match(unmet_platform("cuplyr", no_gpu), "needs an NVIDIA GPU")
  mac <- fake_platform()
  mac$os <- "darwin"
  mac$arch <- "aarch64"
  mac$r <- "4.2.3"
  expect_equal(
    unmet_platform("cuplyr", mac),
    c(
      "runs on Linux only, this is macOS",
      "runs on x86_64 only, this is aarch64",
      "needs R 4.3.0 or newer, this is R 4.2.3"
    )
  )
})

test_that("requirements that can't be checked count as met", {
  expect_equal(unmet_platform("cuplyr", fake_platform(cuda = NA_character_)), character())
  expect_equal(unmet_platform("cuplyr", fake_platform(compute_capability = NULL)), character())
})

test_that("arena_backends() and arena_plan() say why a GPU backend can't run", {
  no_gpu <- fake_platform()
  no_gpu$gpus <- list()
  local_mocked_bindings(platform_info = function() no_gpu)
  backends <- arena_backends()
  cuplyr <- backends[backends$backend == "cuplyr", ]
  expect_false(cuplyr$available)
  expect_match(cuplyr$reason, "needs an NVIDIA GPU")

  plan <- arena_plan("s", backends = "cuplyr", tasks = "filter_rows")
  expect_match(plan$reason[plan$backend == "cuplyr"], "needs an NVIDIA GPU")
})

test_that("the install hint explains unmet requirements instead of how to install", {
  local_mocked_bindings(platform_info = function() fake_platform(compute_capability = "6.1"))
  expect_snapshot(cli::cli_inform(install_hint("cuplyr")))
})

test_that("the install hint points to cuplyr's installer when the machine qualifies", {
  local_mocked_bindings(platform_info = function() fake_platform())
  hint <- install_hint("cuplyr")
  expect_match(hint[[1]], "./install.sh", fixed = TRUE)
  expect_no_error(cli::cli_format_method(cli::cli_bullets(hint)))
})

test_that("the driver's CUDA version is read from old and new nvidia-smi headers", {
  local_mocked_bindings(run_quiet = function(...) {
    c("| NVIDIA-SMI 535.104.05   Driver Version: 535.104.05   CUDA Version: 12.2     |")
  })
  expect_equal(driver_cuda_version(), "12.2")
  local_mocked_bindings(run_quiet = function(...) {
    c("| NVIDIA-SMI 615.78.08   KMD Version: 615.78.08   CUDA UMD Version: 13.4     |")
  })
  expect_equal(driver_cuda_version(), "13.4")
  local_mocked_bindings(run_quiet = function(...) NA_character_)
  expect_equal(driver_cuda_version(), NA_character_)
})

local_fake_diagnose <- function(diagnose, env = parent.frame()) {
  defs <- backend_defs()
  defs$fake <- list(packages = "stats", gpu = TRUE, diagnose = diagnose)
  local_mocked_bindings(backend_defs = function() defs, .env = env)
}

test_that("diagnose_backend() runs the backend's checks in a fresh process", {
  local_fake_diagnose(function() {
    list(
      ok = FALSE,
      checks = list(
        driver = list(ok = TRUE, name = "Driver", value = "580", message = "Driver 580"),
        cudf = list(ok = FALSE, name = "libcudf", value = NA_character_, message = "not found in ~/x")
      )
    )
  })
  diagnosis <- diagnose_backend("fake", .libPaths())
  expect_false(diagnosis$ok)
  expect_length(diagnosis$checks, 2)
  expect_equal(failed_checks(diagnosis), "libcudf: not found in ~/x")
})

test_that("diagnose_backend() reports checks that error", {
  local_fake_diagnose(function() stop("no libcudf.so"))
  diagnosis <- diagnose_backend("fake", .libPaths())
  expect_true(is.na(diagnosis$ok))
  expect_match(diagnosis$error, "no libcudf.so")
  expect_equal(failed_checks(diagnosis), character())
})

test_that("diagnose_backend() skips backends without diagnostics", {
  expect_null(diagnose_backend("dplyr", .libPaths()))
})

test_that("the report lists failed checks only for backends that didn't load", {
  check <- list(ok = FALSE, name = "libcudf", value = NA, message = "not found")
  diagnostics <- list(
    cuplyr = list(ok = FALSE, checks = list(check), loaded = TRUE),
    other = list(ok = FALSE, checks = list(check), loaded = FALSE)
  )
  expect_equal(diagnostic_lines(diagnostics), c("*" = "other: libcudf: not found"))
})
