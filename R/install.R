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
#' Packages that are already installed in your library are not installed
#' again, unless `update = TRUE` and Package Manager has a newer version. Newer
#' versions go into tidyarena's library too; your own library is never
#' changed. Because tidyarena's library comes first in [arena_run()], the
#' newer versions are the ones benchmarked.
#'
#' tidypolars is not on CRAN; it is installed, together with polars, from
#' their latest GitHub releases (not from their development versions), unless
#' `only_cran = TRUE`. Some packages download a prebuilt
#' library when they are compiled (arrow's C++ library, polars' Rust
#' library), which saves building it. cuplyr needs RAPIDS and CUDA and can't
#' be installed this way; `arena_install()` says how.
#'
#' The defaults for `build` and `only_cran`, and the backends to leave out,
#' are set with [arena_config()].
#'
#' @param backends Backend ids, see [arena_backends()]. Defaults to all
#'   backends tidyarena can install, except those left out with
#'   [arena_config()].
#' @param lib Library to install into. Defaults to [arena_lib()].
#' @param snapshot Package Manager snapshot: `"latest"`, or a date such as
#'   `"2026-03-01"` to install the versions that were current then. With a
#'   date, packages in your own library are not used: everything is installed
#'   into `lib`. Packages from GitHub come from the latest release published
#'   by that date.
#' @param build What to do when a package has no binary: `"never"` to stop
#'   and list the packages, `"capped"` to compile them with the limits
#'   described above. Defaults to the setting in [arena_config()].
#' @param update If `TRUE`, also install newer versions of backend packages
#'   that are already installed.
#' @param only_cran If `TRUE`, only install from CRAN. Defaults to the
#'   setting in [arena_config()].
#' @returns The library path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' arena_install(c("dtplyr", "duckplyr"))
#' arena_install(update = TRUE)
#' }
arena_install <- function(
  backends = NULL,
  lib = arena_lib(),
  snapshot = "latest",
  build = NULL,
  update = FALSE,
  only_cran = NULL
) {
  rlang::check_installed("pak", reason = "to install backends.")
  config <- read_config()
  build <- build %||% config$build
  build <- rlang::arg_match(build, c("never", "capped"))
  only_cran <- only_cran %||% config$only_cran
  rlang::check_bool(only_cran)
  defs <- backend_defs()
  ok <- vapply(names(defs), installable, logical(1), only_cran = only_cran)
  backends <- check_ids(
    backends %||% setdiff(names(defs)[ok], config$exclude),
    names(defs),
    "backends"
  )
  inform_not_installable(backends[!ok[backends]], lib)
  backends <- backends[ok[backends]]
  if (length(backends) == 0) {
    return(invisible(lib))
  }
  packages <- unique(unlist(lapply(defs[backends], `[[`, "packages")))
  sources <- install_sources(backends, snapshot, only_cran)
  repos <- sources$repos

  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  # With a dated snapshot, packages from the user's library would defeat the
  # point, so only `lib` counts.
  search <- if (identical(snapshot, "latest")) c(lib, .libPaths()) else lib
  todo <- packages[!packages_installed(packages, search)]
  if (update) {
    outdated <- outdated_packages(setdiff(packages, todo), search, repos, sources$releases)
    todo <- union(todo, outdated$package)
  }
  if (length(todo) == 0) {
    cli::cli_inform("All packages for {.val {backends}} are installed and up to date.")
    return(invisible(lib))
  }

  refs <- package_refs(todo, sources$releases)
  cli::cli_inform("Resolving {.pkg {refs}} from {.url {repos}}.")
  needs_build <- packages_to_build(refs, lib, repos)
  if (length(needs_build) > 0 && build == "never") {
    abort_needs_build(needs_build)
  }

  install_packages(refs, lib, repos, needs_build, sources$env)
  write_install_record(lib, repos, snapshot, refs)
  cli::cli_inform("Installed {.pkg {todo}} in {.path {lib}}.")
  invisible(lib)
}

abort_needs_build <- function(needs_build, call = rlang::caller_env()) {
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
      " " = "{.code build = \"capped\"} compiles with a memory limit. This can take a long time.",
      " " = "{.code arena_config(build = \"capped\")} makes that the default."
    ),
    class = "tidyarena_needs_build",
    call = call
  )
}

inform_not_installable <- function(backends, lib) {
  defs <- backend_defs()
  for (backend in backends) {
    if (!all(packages_installed(defs[[backend]]$packages, c(lib, .libPaths())))) {
      hint <- install_hint(backend)
      cli::cli_inform(c(
        "!" = "Can't install {.val {backend}}.",
        stats::setNames(hint, rep("i", length(hint)))
      ))
    }
  }
}

# How a backend is installed: "cran" from Package Manager, "github" from
# the GitHub releases in its `install` field, "repos" from the extra
# repositories there, or "manual" when tidyarena can't install it and its
# install hint says how.
install_route <- function(backend) {
  def <- backend_defs()[[backend]]
  if (!is.null(def$install_hint)) {
    "manual"
  } else if (!is.null(def$install$github)) {
    "github"
  } else if (!is.null(def$install$repos)) {
    "repos"
  } else {
    "cran"
  }
}

installable <- function(backend, only_cran = read_config()$only_cran) {
  route <- install_route(backend)
  route == "cran" || (route %in% c("github", "repos") && !only_cran)
}

# Where to install `backends` from: repositories (extra ones first), the
# GitHub release tag of each package that comes from GitHub, and build
# environment variables. With `only_cran`, Package Manager only and no
# variables: those ask packages to download prebuilt libraries.
install_sources <- function(backends, snapshot = "latest", only_cran = FALSE) {
  cran <- c(CRAN = ppm_url(snapshot, binary_target()))
  if (only_cran) {
    return(list(repos = cran, releases = list(), env = character()))
  }
  defs <- backend_defs()[backends]
  field <- function(name) do.call(c, unname(lapply(defs, function(d) d$install[[name]])))
  repos <- field("repos")
  github <- field("github")
  github <- github[!duplicated(names(github))]
  env <- field("env")
  list(
    repos = c(repos[!duplicated(names(repos))], cran),
    releases = lapply(github, function(repo) {
      list(repo = repo, tag = github_release(repo, snapshot))
    }),
    env = env[!duplicated(names(env))] %||% character()
  )
}

# pak references: a package name, or `owner/repo@tag` for packages from a
# GitHub release.
package_refs <- function(packages, releases) {
  vapply(
    packages,
    function(p) {
      release <- releases[[p]]
      if (is.null(release)) p else paste0("github::", release$repo, "@", release$tag)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# The release to install from a GitHub repository: the newest one that is
# neither a draft nor a pre-release and has a version tag, published on or
# before `snapshot` if that is a date. Releases, unlike the default branch,
# change only when the authors decide, so results stay comparable.
github_release <- function(repo, snapshot = "latest", call = rlang::caller_env()) {
  key <- paste(repo, snapshot)
  if (!is.null(the$releases[[key]])) {
    return(the$releases[[key]])
  }
  releases <- github_get(sprintf("https://api.github.com/repos/%s/releases?per_page=100", repo))
  keep <- vapply(
    releases,
    function(r) !isTRUE(r$draft) && !isTRUE(r$prerelease) && grepl("^v?[0-9]", r$tag_name),
    logical(1)
  )
  releases <- releases[keep]
  published <- as.Date(substr(vapply(releases, function(r) r$published_at, character(1)), 1, 10))
  if (!identical(snapshot, "latest")) {
    releases <- releases[published <= as.Date(snapshot)]
    published <- published[published <= as.Date(snapshot)]
  }
  if (length(releases) == 0) {
    cli::cli_abort(
      "{.url https://github.com/{repo}} has no release{if (!identical(snapshot, 'latest')) paste(' from before', snapshot)}.",
      call = call
    )
  }
  tag <- releases[[order(published, decreasing = TRUE)[[1]]]]$tag_name
  the$releases[[key]] <- tag
  tag
}

# Whether the package at a `github::owner/repo@tag` reference has compiled
# code.
github_has_src <- function(ref) {
  parts <- regmatches(ref, regexec("^github::([^@]+)@(.+)$", ref))[[1]]
  contents <- github_get(sprintf("https://api.github.com/repos/%s/contents?ref=%s", parts[[2]], parts[[3]]))
  any(vapply(contents, function(x) identical(x$name, "src") && identical(x$type, "dir"), logical(1)))
}

github_get <- function(url) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  token <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))
  headers <- c(Accept = "application/vnd.github+json")
  if (nzchar(token)) {
    headers <- c(headers, Authorization = paste("Bearer", token))
  }
  utils::download.file(url, path, quiet = TRUE, headers = headers)
  jsonlite::read_json(path)
}

# Installed packages with a newer version in `repos`, or, for packages from
# GitHub releases, a version other than the release's (a development
# version included): a data frame with columns package, installed and
# available.
outdated_packages <- function(packages, libpath, repos, releases = list()) {
  empty <- data.frame(package = character(), installed = character(), available = character())
  if (length(packages) == 0) {
    return(empty)
  }
  from_cran <- setdiff(packages, names(releases))
  available <- if (length(from_cran) > 0) utils::available.packages(repos = repos)
  rows <- lapply(packages, function(p) {
    path <- find.package(p, lib.loc = libpath, quiet = TRUE)
    if (length(path) == 0) {
      return(NULL)
    }
    installed <- utils::packageVersion(p, lib.loc = dirname(path[[1]]))
    if (!is.null(releases[[p]])) {
      release <- package_version(sub("^v", "", releases[[p]]$tag))
      if (installed != release) {
        return(data.frame(package = p, installed = as.character(installed), available = as.character(release)))
      }
      return(NULL)
    }
    if (!p %in% rownames(available)) {
      return(NULL)
    }
    newest <- package_version(available[p, "Version"])
    if (newest > installed) {
      data.frame(package = p, installed = as.character(installed), available = as.character(newest))
    }
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  out
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
    repo = unname(repos[["CRAN"]] %||% repos[[1]]),
    repos = as.list(repos),
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
  # A DESCRIPTION in a Git repository has no NeedsCompilation field, so pak
  # assumes compiled code; look for a src directory instead.
  github <- grepl("^github::", deps$ref)
  compiled[github] <- vapply(deps$ref[github], github_has_src, logical(1))
  sort(unique(deps$package[todo & compiled]))
}

# Runs pak in a separate R process. When something has to be compiled, the
# process gets a memory limit (a cgroup on Linux) and a number of make jobs
# that fits in free memory.
install_packages <- function(packages, lib, repos, needs_build, env = character()) {
  # pak itself comes from the session's library; it installs into `lib`.
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

# Default library paths for runs. With `prefer = "session"` the session's
# libraries come first and tidyarena's library only supplies what they lack.
# With `prefer = "arena"`, chosen by updating, tidyarena's library comes
# first, so its newer versions are used.
default_libpath <- function(prefer = c("session", "arena")) {
  prefer <- rlang::arg_match(prefer)
  lib <- arena_lib()
  if (!dir.exists(lib)) {
    return(.libPaths())
  }
  if (prefer == "arena") c(lib, .libPaths()) else c(.libPaths(), lib)
}

# Install records of the libraries in `libpath` that arena_install() made.
install_records <- function(libpath) {
  paths <- file.path(libpath, "tidyarena-install.json")
  records <- lapply(paths[file.exists(paths)], jsonlite::read_json)
  unname(records)
}

# Before a run: finds missing and outdated backends and, depending on
# `install`, asks about them, installs them or just mentions them. Returns
# which library should come first: "arena" if the user chose to update,
# "session" otherwise.
offer_installs <- function(backends, install, libpath) {
  prefer <- if (install == "update") "arena" else "session"
  if (install == "never" || !rlang::is_installed("pak")) {
    return(prefer)
  }
  status <- tryCatch(
    backend_status(backends, libpath),
    error = function(e) {
      cli::cli_inform(c(
        "!" = "Couldn't check Posit Package Manager for backends, running with what is installed.",
        " " = conditionMessage(e)
      ))
      NULL
    }
  )
  if (is.null(status)) {
    return(prefer)
  }
  if (length(status$elsewhere) > 0) {
    hints <- unlist(lapply(status$elsewhere, install_hint))
    cli::cli_inform(c(
      i = "Not installed, and tidyarena can't install: {.val {status$elsewhere}}.",
      stats::setNames(hints, rep(" ", length(hints)))
    ))
  }
  has_missing <- length(status$missing) > 0
  has_outdated <- nrow(status$outdated) > 0
  if (!has_missing && !has_outdated) {
    return(prefer)
  }

  outdated_lines <- sprintf(
    "%s %s -> %s",
    status$outdated$package,
    status$outdated$installed,
    status$outdated$available
  )
  if (install == "ask" && !rlang::is_interactive()) {
    cli::cli_inform(c(
      i = if (has_missing) "Not installed: {.val {status$missing}}.",
      i = if (has_outdated) "Newer versions available: {outdated_lines}.",
      i = "Install them with {.fn arena_install}, or pass {.arg install} to {.fn arena_run}."
    ))
    return(prefer)
  }

  if (has_missing) {
    question <- "{length(status$missing)} backend{?s} {?is/are} not installed: {.val {status$missing}}. Install {?it/them}?"
    if (install %in% c("missing", "update") || ask_yes_no(question)) {
      install_backends(status$missing)
    }
  }
  if (has_outdated) {
    question <- c(
      "Newer versions are available:",
      stats::setNames(outdated_lines, rep("*", length(outdated_lines))),
      "Install them into tidyarena's library and use them for this run? Your own library is not changed."
    )
    if (install == "update" || (install == "ask" && ask_yes_no(question))) {
      install_backends(status$outdated_backends, update = TRUE)
      prefer <- "arena"
    }
  }
  prefer
}

# Missing and outdated backends among `backends` (default: all that can run
# here, except those left out with arena_config()). Backends tidyarena can't
# install are reported apart, in `elsewhere`, when they were named.
backend_status <- function(backends, libpath) {
  defs <- backend_defs()
  config <- read_config()
  platform <- platform_info()
  runnable <- Filter(function(b) length(unmet_platform(b, platform)) == 0, names(defs))
  candidates <- union(anchor_backend, backends %||% setdiff(runnable, config$exclude))
  installed <- vapply(
    candidates,
    function(b) all(packages_installed(defs[[b]]$packages, libpath)),
    logical(1)
  )
  ok <- vapply(candidates, installable, logical(1), only_cran = config$only_cran)
  present <- candidates[installed & ok]
  packages <- unique(unlist(lapply(defs[present], `[[`, "packages")))
  sources <- install_sources(present, only_cran = config$only_cran)
  outdated <- outdated_packages(packages, libpath, sources$repos, sources$releases)
  uses_outdated <- vapply(
    present,
    function(b) any(defs[[b]]$packages %in% outdated$package),
    logical(1)
  )
  list(
    missing = candidates[!installed & ok],
    elsewhere = candidates[!installed & !ok & candidates %in% (backends %||% character())],
    outdated = outdated,
    outdated_backends = present[uses_outdated]
  )
}

# Installs backends one at a time, so that one that would need compiling
# doesn't stop the others.
install_backends <- function(backends, update = FALSE) {
  for (backend in backends) {
    tryCatch(
      arena_install(backend, update = update),
      tidyarena_needs_build = function(e) {
        cli::cli_inform(c(
          "!" = "Skipping {.val {backend}}: it would have to be compiled.",
          i = "Use {.code arena_install({.str {backend}}, build = \"capped\")} to compile it with a memory limit,",
          " " = "or {.code arena_config(build = \"capped\")} to always do so."
        ))
      }
    )
  }
  invisible()
}

ask_yes_no <- function(question, .envir = parent.frame()) {
  if (!rlang::is_interactive()) {
    return(FALSE)
  }
  cli::cli_inform(question, .envir = .envir)
  utils::menu(c("Yes", "No")) == 1
}

# For each package, "arena" if it is loaded from tidyarena's library,
# "session" otherwise, and NA if it is not installed.
package_sources <- function(packages, libpath) {
  arena <- normalizePath(arena_lib(), mustWork = FALSE)
  sources <- vapply(
    packages,
    function(p) {
      path <- find.package(p, lib.loc = libpath, quiet = TRUE)
      if (length(path) == 0) {
        return(NA_character_)
      }
      lib <- normalizePath(dirname(path[[1]]), mustWork = FALSE)
      if (identical(lib, arena)) "arena" else "session"
    },
    character(1)
  )
  as.list(sources)
}
