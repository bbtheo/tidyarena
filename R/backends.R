#' The backends
#'
#' A backend is a small adapter: R code that turns the generated tables into
#' the backend's own in-memory form, and a function that collects a result
#' back into R. Setup is not timed; the pipeline and the collect are.
#'
#' Setup code runs with `df` and `lookup` bound to plain data frames and
#' `.budget_mb` bound to the memory budget for the task, in megabytes. It must
#' rebind `df` and `lookup` and define `.collect()`. Backends that can
#' spill set their memory limit from `.budget_mb`.
#'
#' @param libpath Library paths to look for the backend packages in. Defaults
#'   to the session's library paths, followed by [arena_lib()] when it
#'   exists.
#' @returns A tibble with one row per backend:
#'   * `backend`: backend id.
#'   * `packages`: packages the backend needs.
#'   * `gpu`: whether the backend runs on an NVIDIA GPU.
#'   * `available`: whether all `packages` are installed in `libpath` (and,
#'     for GPU backends, an NVIDIA GPU was found).
#' @export
#' @examples
#' arena_backends()
arena_backends <- function(libpath = default_libpath()) {
  defs <- backend_defs()
  gpu <- vapply(defs, `[[`, logical(1), "gpu")
  installed <- vapply(
    defs,
    function(x) all(packages_installed(x$packages, libpath)),
    logical(1)
  )
  has_gpu <- nzchar(Sys.which("nvidia-smi"))
  tibble::tibble(
    backend = names(defs),
    packages = unname(lapply(defs, `[[`, "packages")),
    gpu = unname(gpu),
    available = unname(installed & (!gpu | has_gpu))
  )
}

packages_installed <- function(packages, libpath) {
  vapply(
    packages,
    function(p) nzchar(system.file(package = p, lib.loc = libpath)),
    logical(1)
  )
}

# The anchor: every run includes it, and its output is the reference for
# correctness.
anchor_backend <- "dplyr"

backend_defs <- function() {
  collect_tibble <- ".collect <- function(x) dplyr::collect(x)"
  list(
    dplyr = list(
      packages = "dplyr",
      gpu = FALSE,
      mem_factor = 3,
      setup = c(
        "library(dplyr)",
        "df <- tibble::as_tibble(df)",
        "lookup <- tibble::as_tibble(lookup)",
        collect_tibble
      )
    ),
    dtplyr = list(
      packages = c("dplyr", "dtplyr", "data.table"),
      gpu = FALSE,
      mem_factor = 3,
      setup = c(
        "library(dplyr)",
        "df <- dtplyr::lazy_dt(data.table::as.data.table(df))",
        "lookup <- dtplyr::lazy_dt(data.table::as.data.table(lookup))",
        collect_tibble
      )
    ),
    duckplyr = list(
      packages = c("dplyr", "duckplyr"),
      gpu = FALSE,
      mem_factor = 2.5,
      fallback_pattern = "falling back|fallback",
      setup = c(
        # Report fallbacks as messages, and never upload them.
        "Sys.setenv(DUCKPLYR_FALLBACK_INFO = 'TRUE', DUCKPLYR_FALLBACK_COLLECT = '0')",
        "library(dplyr)",
        "duckplyr::db_exec(sprintf(\"SET memory_limit = '%dMB'\", .budget_mb))",
        "df <- duckplyr::as_duckdb_tibble(df)",
        "lookup <- duckplyr::as_duckdb_tibble(lookup)",
        collect_tibble
      )
    ),
    dbplyr_duckdb = list(
      packages = c("dplyr", "dbplyr", "duckdb", "DBI"),
      gpu = FALSE,
      mem_factor = 3,
      setup = c(
        "library(dplyr)",
        ".con <- DBI::dbConnect(duckdb::duckdb())",
        "DBI::dbExecute(.con, sprintf(\"SET memory_limit = '%dMB'\", .budget_mb))",
        "DBI::dbWriteTable(.con, 'df', df)",
        "DBI::dbWriteTable(.con, 'lookup', lookup)",
        "df <- dplyr::tbl(.con, 'df')",
        "lookup <- dplyr::tbl(.con, 'lookup')",
        collect_tibble
      ),
      teardown = "DBI::dbDisconnect(.con, shutdown = TRUE)"
    ),
    arrow = list(
      packages = c("dplyr", "arrow"),
      gpu = FALSE,
      mem_factor = 3,
      fallback_pattern = "pulling data into R",
      setup = c(
        "library(dplyr)",
        "df <- arrow::arrow_table(df)",
        "lookup <- arrow::arrow_table(lookup)",
        collect_tibble
      )
    ),
    tidypolars = list(
      packages = c("dplyr", "polars", "tidypolars"),
      gpu = FALSE,
      install_hint = "Install polars and tidypolars from R-universe: {.url https://rpolars.r-universe.dev}.",
      mem_factor = 3,
      setup = c(
        "library(dplyr)",
        "library(tidypolars)",
        "df <- polars::as_polars_df(df)",
        "lookup <- polars::as_polars_df(lookup)",
        ".collect <- function(x) tibble::as_tibble(as.data.frame(x))"
      )
    ),
    cuplyr = list(
      packages = c("dplyr", "cuplyr"),
      gpu = TRUE,
      install_hint = "Install cuplyr from GitHub: {.url https://github.com/bbtheo/cuplyr}.",
      mem_factor = 2,
      setup = c(
        "library(dplyr)",
        "library(cuplyr)",
        "df <- cuplyr::tbl_gpu(df)",
        "lookup <- cuplyr::tbl_gpu(lookup)",
        collect_tibble
      )
    )
  )
}
