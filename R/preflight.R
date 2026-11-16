# Checks, before a run, that every backend loads with the run's library
# order. Two checks:
#
# * A load test: the backend's setup, run by the worker on a tiny table.
#   It catches everything that would make the backend fail at setup.
# * A version check: the Depends and Imports version requirements of the
#   backend's packages, followed recursively, against the versions that come
#   first in the library order. It explains why a load test failed.

# Runs the load test on `backends`. Returns a named list of results, one per
# backend, each with `ok` and `error`.
load_test <- function(backends, libpath, guard = guard_method()) {
  data <- arena_data(100)
  dir <- withr_tempdir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  defs <- backend_defs()
  results <- lapply(backends, function(backend) {
    tmpdir <- file.path(dir, backend)
    dir.create(tmpdir)
    spec <- list(
      code = "df",
      order_key = NA_character_,
      setup = defs[[backend]]$setup,
      teardown = defs[[backend]]$teardown,
      source = defs[[backend]]$source,
      data = data$tables,
      reader_lib = reader_lib(),
      reps = 0L,
      budget_mb = 1024,
      libpath = libpath,
      tmpdir = tmpdir,
      out = file.path(dir, paste0(backend, ".rds"))
    )
    result <- run_worker(
      spec,
      budget = 2 * 2^30,
      timeout = 120,
      guard = guard,
      log = file.path(dir, paste0(backend, ".log"))
    )
    list(
      ok = identical(result$status, "ok"),
      error = scrub(result$error %||% result$status %||% "unknown error")
    )
  })
  stats::setNames(results, backends)
}

withr_tempdir <- function() {
  dir <- tempfile("tidyarena-check")
  dir.create(dir)
  dir
}

# Unmet version requirements of `packages` and their dependencies, when each
# package is taken from the first library in `libpath` that has it. Returns a
# data frame: `package` (the one with the requirement), `needs` (the
# dependency), `required` (for example ">= 1.2.0"), `found` (version found
# first, NA if none) and `source` ("arena" or "session").
unmet_requirements <- function(packages, libpath) {
  queue <- packages
  seen <- character()
  rows <- list()
  while (length(queue) > 0) {
    package <- queue[[1]]
    queue <- queue[-1]
    if (package %in% seen) {
      next
    }
    seen <- c(seen, package)
    path <- find_first(package, libpath)
    if (is.na(path)) {
      next
    }
    for (dep in package_requirements(path)) {
      found <- find_first(dep$package, libpath)
      version <- if (is.na(found)) NA_character_ else installed_version(found)
      if (is.na(found) || !version_satisfies(version, dep$op, dep$version)) {
        rows[[length(rows) + 1]] <- data.frame(
          package = package,
          needs = dep$package,
          required = paste(dep$op, dep$version),
          found = version,
          source = if (is.na(found)) NA_character_ else library_label(dirname(found))
        )
      }
      queue <- c(queue, dep$package)
    }
  }
  empty <- data.frame(
    package = character(),
    needs = character(),
    required = character(),
    found = character(),
    source = character()
  )
  do.call(rbind, c(list(empty), rows))
}

# Versioned requirements in Depends and Imports, without R itself. Packages
# required without a version are only followed when they are missing, which
# the load test reports.
package_requirements <- function(path) {
  desc <- read.dcf(file.path(path, "DESCRIPTION"), fields = c("Depends", "Imports"))
  fields <- desc[1, ]
  fields <- fields[!is.na(fields)]
  if (length(fields) == 0) {
    return(list())
  }
  entries <- trimws(unlist(strsplit(paste(fields, collapse = ","), ",")))
  entries <- entries[nzchar(entries)]
  pattern <- "^([A-Za-z0-9.]+)\\s*(\\(\\s*([<>=!]+)\\s*([^)\\s]+)\\s*\\))?$"
  out <- lapply(entries, function(entry) {
    entry <- gsub("\\s+", " ", entry)
    if (!grepl(pattern, entry, perl = TRUE)) {
      return(NULL)
    }
    package <- sub(pattern, "\\1", entry, perl = TRUE)
    op <- sub(pattern, "\\3", entry, perl = TRUE)
    if (package == "R" || !nzchar(op)) {
      return(NULL)
    }
    list(package = package, op = op, version = sub(pattern, "\\4", entry, perl = TRUE))
  })
  Filter(Negate(is.null), out)
}

find_first <- function(package, libpath) {
  path <- find.package(package, lib.loc = libpath, quiet = TRUE)
  if (length(path) == 0) NA_character_ else path[[1]]
}

installed_version <- function(path) {
  unname(read.dcf(file.path(path, "DESCRIPTION"), fields = "Version")[1, 1])
}

version_satisfies <- function(version, op, required) {
  compare <- switch(
    op,
    ">=" = `>=`,
    ">" = `>`,
    "==" = `==`,
    "<=" = `<=`,
    "<" = `<`,
    "!=" = `!=`,
    return(TRUE)
  )
  compare(package_version(version), package_version(required))
}

library_label <- function(lib) {
  arena <- normalizePath(arena_lib(), mustWork = FALSE)
  if (identical(normalizePath(lib, mustWork = FALSE), arena)) "arena" else "session"
}

describe_unmet <- function(unmet) {
  found <- ifelse(
    is.na(unmet$found),
    "it is not installed",
    paste(ifelse(unmet$source %in% "arena", "tidyarena's library has", "your library has"), unmet$found)
  )
  sprintf("%s needs %s %s, but %s.", unmet$package, unmet$needs, unmet$required, found)
}

# Runs both checks on the backends that are about to run and settles the
# failures: by switching to tidyarena's newer packages (asking first in
# interactive sessions, by default otherwise) or by skipping the backend.
# Returns `prefer`, `skip` (a named character vector of reasons), `libpath`
# and `diagnostics`, the results of the backends' own diagnostics.
preflight <- function(backends, prefer, libpath_fixed, libpath) {
  skip <- character()
  if (length(backends) == 0) {
    return(list(prefer = prefer, skip = skip, libpath = libpath, diagnostics = list()))
  }
  cli::cli_inform("Checking that {.val {backends}} {?loads/load}.")
  results <- load_test(backends, libpath)
  failed <- names(results)[!vapply(results, `[[`, logical(1), "ok")]
  diagnostics <- lapply(backends, diagnose_backend, libpath = libpath)
  names(diagnostics) <- backends
  diagnostics <- Filter(Negate(is.null), diagnostics)
  for (backend in names(diagnostics)) {
    diagnostics[[backend]]$loaded <- results[[backend]]$ok
  }

  for (backend in failed) {
    unmet <- unmet_requirements(backend_defs()[[backend]]$packages, libpath)
    can_fix <- nrow(unmet) > 0 && !libpath_fixed && prefer == "session"
    if (can_fix && agree_to_update(backend, unmet)) {
      result <- retry_with_arena(backend)
      if (result$ok) {
        prefer <- "arena"
        libpath <- default_libpath("arena")
        next
      }
      skip[[backend]] <- paste("does not load:", result$error)
    } else {
      reason <- if (nrow(unmet) > 0) describe_unmet(unmet)[[1]] else results[[backend]]$error
      skip[[backend]] <- paste("does not load:", reason)
    }
    checks <- cli_escape(failed_checks(diagnostics[[backend]]))
    cli::cli_inform(c(
      "!" = "Skipping {.val {backend}}: {skip[[backend]]}",
      if (length(checks) > 0) c(i = "Its own checks found:", stats::setNames(checks, rep("x", length(checks))))
    ))
    if (length(checks) > 0) {
      skip[[backend]] <- paste0(
        skip[[backend]],
        " (",
        paste(failed_checks(diagnostics[[backend]]), collapse = "; "),
        ")"
      )
    }
  }
  list(prefer = prefer, skip = skip, libpath = libpath, diagnostics = diagnostics)
}

# Asks whether to use tidyarena's newer packages. Without a user to ask, the
# answer is yes: a run with every backend is more useful than a run with
# your exact versions.
agree_to_update <- function(backend, unmet) {
  lines <- describe_unmet(unmet)
  if (!rlang::is_interactive()) {
    cli::cli_inform(c(
      "!" = "{.val {backend}} does not load with your packages:",
      stats::setNames(lines, rep(" ", length(lines))),
      i = "Using tidyarena's newer packages for this run, for every backend."
    ))
    return(TRUE)
  }
  ask_yes_no(c(
    "!" = "{.val {backend}} does not load with your packages:",
    stats::setNames(lines, rep(" ", length(lines))),
    "Use tidyarena's newer packages for this run? This applies to every backend, including dplyr, which the others are compared with. If not, {.val {backend}} is skipped."
  ))
}

# Makes sure tidyarena's library can load the backend on its own, by
# installing the backend and its dependencies there if it can't, and runs the
# load test again with tidyarena's library first.
retry_with_arena <- function(backend) {
  libpath <- default_libpath("arena")
  packages <- backend_defs()[[backend]]$packages
  config <- read_config()
  if (!dir.exists(arena_lib()) || nrow(unmet_requirements(packages, libpath)) > 0) {
    if (!installable(backend, config$only_cran)) {
      return(list(ok = FALSE, error = "tidyarena can't install it"))
    }
    lib <- arena_lib()
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    sources <- install_sources(backend, only_cran = config$only_cran)
    repos <- sources$repos
    refs <- package_refs(packages, sources$releases)
    needs_build <- packages_to_build(refs, lib, repos)
    if (length(needs_build) > 0 && config$build == "never") {
      return(list(ok = FALSE, error = paste("needs compiling:", paste(needs_build, collapse = ", "))))
    }
    install_packages(refs, lib, repos, needs_build, sources$env)
    write_install_record(lib, repos, "latest", refs)
    libpath <- default_libpath("arena")
  }
  load_test(backend, libpath)[[backend]]
}

cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}
