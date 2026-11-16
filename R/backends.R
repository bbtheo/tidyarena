#' The backends
#'
#' A backend is a small adapter: R code that turns the generated tables into
#' the backend's own form, and functions that collect a result back into R
#' or compute it in the backend. Setup is not timed; the pipeline is. See
#' `vignette("extending")`.
#'
#' @param libpath Library paths to look for the backend packages in. Defaults
#'   to the session's library paths, followed by [arena_lib()] when it
#'   exists.
#' Backends that can read Parquet files themselves also come as a Parquet
#' variant, `<id>_parquet`: the same backend, given the paths of the tables'
#' Parquet files instead of data frames. It shows what an engine does with
#' data it reads itself, without the conversion from R's types (factors
#' become strings, for example).
#'
#' @returns A tibble with one row per backend:
#'   * `backend`: backend id.
#'   * `source`: where the backend gets the tables: `"memory"`, the data
#'     frames in R, or `"parquet"`, the Parquet files.
#'   * `packages`: packages the backend needs.
#'   * `gpu`: whether the backend runs on an NVIDIA GPU.
#'   * `available`: whether all `packages` are installed in `libpath` and
#'     the machine meets the backend's requirements.
#'   * `reason`: why the backend is not available. For GPU backends this
#'     says which requirement is unmet, for example the GPU's compute
#'     capability or the CUDA version the driver supports.
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
  platform <- platform_info()
  unmet <- vapply(
    names(defs),
    function(b) paste(unmet_platform(b, platform), collapse = "; "),
    character(1)
  )
  reason <- ifelse(nzchar(unmet), unmet, ifelse(installed, NA_character_, "not installed"))
  tibble::tibble(
    backend = names(defs),
    source = unname(vapply(defs, `[[`, character(1), "source")),
    packages = unname(lapply(defs, `[[`, "packages")),
    gpu = unname(gpu),
    available = unname(is.na(reason)),
    reason = unname(reason)
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
