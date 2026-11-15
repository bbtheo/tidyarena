#' Install backends
#'
#' Installs the packages of the given backends, and their dependencies, into
#' a library of their own (see [arena_lib()]), so the packages being measured
#' never mix with your own library. Packages come from Posit Package Manager
#' through pak, which serves binaries for Windows, macOS and the common Linux
#' distributions. On other Linux distributions with glibc 2.28 or newer (such
#' as Arch, Fedora or Gentoo) on x86_64, it uses Package Manager's generic
#' `manylinux_2_28` binaries.
#'
#' Some packages may still have no binary for your platform. They would have
#' to be compiled, and compiling packages such as duckdb or arrow takes a long
#' time and a lot of memory. By default `arena_install()` stops and lists
#' them. With `build = "capped"` it compiles them, one package at a time, with
#' a number of parallel compile jobs chosen from free memory and, on Linux
#' with systemd, inside a memory limit with swap disabled, so a build that
#' runs out of memory is stopped instead of freezing the machine.
#'
#' @param backends Backend ids, see [arena_backends()]. Defaults to all
#'   backends except those that need a GPU.
#' @param lib Library to install into. Defaults to [arena_lib()].
#' @param snapshot Package Manager snapshot: `"latest"`, or a date such as
#'   `"2026-03-01"` to install the versions that were current then.
#' @param build What to do when a package has no binary: `"never"` to stop
#'   and list the packages, `"capped"` to compile them with the limits
#'   described above.
#' @returns The library path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' arena_install(c("dtplyr", "duckplyr"))
#' arena_run("quick", libpath = arena_lib())
#' }
arena_install <- function(
  backends = NULL,
  lib = arena_lib(),
  snapshot = "latest",
  build = c("never", "capped")
) {
  rlang::check_installed("pak", reason = "to install backends.")
  build <- rlang::arg_match(build)
  defs <- backend_defs()
  gpu <- vapply(defs, `[[`, logical(1), "gpu")
  backends <- check_ids(backends %||% names(defs)[!gpu], names(defs), "backends")
  packages <- unique(unlist(lapply(defs[backends], `[[`, "packages")))
  repos <- c(CRAN = ppm_url(snapshot, binary_target()))

  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  missing <- packages[!packages_installed(packages, lib)]
  if (length(missing) == 0) {
    cli::cli_inform("All packages for {.val {backends}} are installed in {.path {lib}}.")
    return(invisible(lib))
  }

  cli::cli_inform("Resolving {.pkg {missing}} from {.url {repos[[1]]}}.")
  needs_build <- packages_to_build(missing, lib, repos)
  if (length(needs_build) > 0 && build == "never") {
    platform <- platform_label()
    has_binaries <- ppm_binaries()
    target <- binary_target()
    them <- if (length(needs_build) == 1) "it" else "them"
    cli::cli_abort(
      c(
        "{length(needs_build)} package{?s} {?has/have} no binary for this platform and would have to be compiled: {.pkg {needs_build}}.",
        i = if (!is.null(target)) {
          "Posit Package Manager's {.val {target}} binaries, used on this platform, don't include {them}."
        } else if (!has_binaries) {
          "Posit Package Manager has no binaries for {.val {platform}}."
        },
        i = "Leave out the backends that need {them}, or compile:",
        " " = "{.code build = \"capped\"} compiles with a memory limit. This can take a long time."
      )
    )
  }

  install_packages(missing, lib, repos, needs_build)
  write_install_record(lib, repos, snapshot, missing)
  cli::cli_inform("Installed {.pkg {missing}} in {.path {lib}}.")
  invisible(lib)
}

#' The library tidyarena installs backends into
#'
#' One library per R minor version, under [tools::R_user_dir()]. Pass it to
#' `libpath` in [arena_run()] and [arena_plan()] to run the backends
#' installed by [arena_install()].
#'
#' @returns The path of the library.
#' @export
arena_lib <- function() {
  version <- paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")
  file.path(tools::R_user_dir("tidyarena", "data"), "lib", version)
}

ppm_url <- function(snapshot = "latest", target = NULL, call = rlang::caller_env()) {
  if (!identical(snapshot, "latest") && is.na(as.Date(snapshot, optional = TRUE))) {
    cli::cli_abort(
      "{.arg snapshot} must be {.val latest} or a date like {.val 2026-03-01}.",
      call = call
    )
  }
  if (is.null(target)) {
    paste0("https://p3m.dev/cran/", snapshot)
  } else {
    paste0("https://p3m.dev/cran/__linux__/", target, "/", snapshot)
  }
}

# The Package Manager binary target to ask for explicitly, or NULL to let pak
# choose. pak picks binaries itself on the platforms Package Manager builds
# for; elsewhere on Linux the generic manylinux_2_28 binaries work wherever
# glibc is 2.28 or newer.
binary_target <- function() {
  if (Sys.info()[["sysname"]] != "Linux" || ppm_binaries()) {
    return(NULL)
  }
  glibc <- glibc_version()
  if (R.version$arch == "x86_64" && !is.na(glibc) && glibc >= "2.28") {
    return("manylinux_2_28")
  }
  NULL
}

glibc_version <- function() {
  out <- run_quiet("getconf", "GNU_LIBC_VERSION")[[1]]
  version <- sub("^glibc\\s+", "", out %||% NA_character_)
  tryCatch(numeric_version(version), error = function(e) NA)
}

# Records where the packages in `lib` came from, for the run results.
write_install_record <- function(lib, repos, snapshot, packages) {
  path <- file.path(lib, "tidyarena-install.json")
  old <- if (file.exists(path)) jsonlite::read_json(path) else list()
  record <- list(
    time = format_time(Sys.time()),
    repo = unname(repos[[1]]),
    snapshot = snapshot,
    packages = as.list(packages)
  )
  jsonlite::write_json(c(old, list(record)), path, auto_unbox = TRUE, pretty = TRUE)
}

ppm_binaries <- function() {
  tryCatch(pak::ppm_has_binaries(), error = function(e) FALSE)
}

platform_label <- function() {
  tryCatch(pak::system_r_platform(), error = function(e) R.version$platform)
}

# Packages, among `packages` and their dependencies not yet in `lib`, that
# pak would install from source and that contain compiled code.
packages_to_build <- function(packages, lib, repos) {
  old <- options(repos = repos)
  on.exit(options(old), add = TRUE)
  deps <- pak::pkg_deps(packages, dependencies = NA)
  todo <- !packages_installed(deps$package, c(lib, .Library))
  compiled <- deps$platform == "source" & deps$needscompilation %in% TRUE
  sort(unique(deps$package[todo & compiled]))
}

# Runs pak in a separate R process. When something has to be compiled, the
# process gets a memory limit (a cgroup on Linux) and a number of make jobs
# that fits in free memory.
install_packages <- function(packages, lib, repos, needs_build) {
  # pak itself comes from the session's library; it installs into `lib`.
  env <- character()
  budget <- default_budget()
  if (length(needs_build) > 0) {
    jobs <- build_jobs(budget)
    env <- c(env, MAKEFLAGS = paste0("-j", jobs))
    cli::cli_inform(c(
      "Compiling {.pkg {needs_build}} with {jobs} parallel job{?s}.",
      i = "Memory limit: {format_bytes(budget)} (guard: {guard_method()})."
    ))
  }

  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(
    c(
      sprintf("options(repos = %s, Ncpus = 1L)", deparse1(repos)),
      sprintf(
        "pak::pkg_install(%s, lib = %s, upgrade = FALSE, ask = FALSE)",
        deparse1(packages),
        deparse1(lib)
      )
    ),
    script
  )
  rscript <- file.path(R.home("bin"), "Rscript")
  if (length(needs_build) > 0 && guard_method() == "cgroup") {
    command <- "systemd-run"
    args <- c(systemd_args(budget), "nice", "-n", "10", rscript, script)
  } else {
    command <- rscript
    args <- script
  }
  out <- processx::run(
    command,
    args,
    env = c("current", env),
    echo = TRUE,
    error_on_status = FALSE
  )
  if (out$status != 0) {
    killed <- out$status %in% c(-9L, 137L)
    cli::cli_abort(c(
      "Installing {.pkg {packages}} failed.",
      i = if (killed) {
        "The build ran out of its {format_bytes(budget)} memory limit and was stopped."
      } else {
        "See the output above for the error."
      }
    ))
  }
  invisible()
}

# About 2.5 GB per C++ compile job, which covers duckdb and arrow.
build_jobs <- function(budget, per_job = 2.5 * 2^30) {
  cores <- ps::ps_cpu_count(logical = FALSE) %||% 1L
  max(1L, min(as.integer(cores), as.integer(budget %/% per_job)))
}

# Default library paths for runs: tidyarena's own library first, when
# arena_install() has created it, then the session's.
default_libpath <- function() {
  lib <- arena_lib()
  if (dir.exists(lib)) c(lib, .libPaths()) else .libPaths()
}

# Install records of the libraries in `libpath` that arena_install() made.
install_records <- function(libpath) {
  paths <- file.path(libpath, "tidyarena-install.json")
  records <- lapply(paths[file.exists(paths)], jsonlite::read_json)
  unname(records)
}
