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
