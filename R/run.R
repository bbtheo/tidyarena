#' Run the benchmark
#'
#' Runs every planned cell (see [arena_plan()]) in its own R process, smallest
#' size first, and writes results to `dir` after every cell. A run that is
#' interrupted continues where it stopped when `arena_run()` is called again
#' with the same `dir`.
#'
#' @section Safeguards:
#' Each task gets a memory budget. On Linux with systemd the budget is a hard
#' limit enforced by the kernel (a cgroup with swap disabled), so a task that
#' overruns it is killed without pushing the machine into swap. On every
#' platform a watchdog also kills a task that goes over budget, leaves less
#' than 1 GB free, makes swap grow, or runs past its timeout: 600 seconds a
#' repetition up to 1e8 rows, 1800 seconds at 1e9 rows. Loading the data into
#' the backend has a timeout of the same length (`"setup_timeout"`).
#'
#' A task that fails at one size is not run at larger sizes on the same
#' backend. Tasks predicted not to fit are skipped before they start.
#'
#' @section What is measured:
#' For each repetition, twice: wall time and peak resident memory (exact on
#' Linux, sampled every 0.1 seconds elsewhere) of the pipeline with
#' [dplyr::collect()] and every value of the result read, and of the
#' pipeline with [dplyr::compute()], the result kept in the backend. For each cell: whether it ran,
#' whether a backend fell back to dplyr, whether its result matches dplyr's,
#' how much it spilled to disk, and the versions of every loaded package.
#'
#' @section Installing backends:
#' Before a new run starts, `arena_run()` checks the requested backends (all
#' of them, if `backends` is `NULL`): which are not installed, and which have
#' newer versions on Posit Package Manager. With `install = "ask"`, the
#' default, it asks whether to install the missing ones and whether to update
#' the outdated ones. Both go into tidyarena's own library ([arena_lib()]);
#' your library is not changed. See [arena_install()]. Outside an interactive
#' session it does not ask, and installs nothing.
#'
#' The answer about updates also decides which versions the run uses. After
#' updating, tidyarena's library comes first, so the newest versions are used
#' for every package, dependencies included. Otherwise your library comes
#' first and tidyarena's library only supplies backends you don't have. A
#' backend installed by tidyarena can then need a newer dependency than the
#' one in your library; it then fails at setup, and the error is in the
#' results.
#'
#' Before the first task, every backend is loaded once on a tiny table, and
#' the version requirements of its packages are checked against the versions
#' the run would use. If a backend doesn't load because your library has too
#' old a dependency, `arena_run()` asks whether to use tidyarena's newer
#' packages for the run instead, installing them if needed. Outside an
#' interactive session it does so without asking. A backend that still
#' doesn't load is skipped, and the reason is in the results.
#'
#' @inheritParams arena_plan
#' @param install What to do about missing and outdated backends before the
#'   run: `"ask"` (only in interactive sessions), `"missing"` to install
#'   missing backends, `"update"` to install missing backends and update
#'   outdated ones, or `"never"`. Installing follows [arena_config()]:
#'   backends left out there are not installed, `only_cran` limits where
#'   packages come from, and `build` decides whether packages without a
#'   binary are compiled.
#' @param dir Directory for the results. Defaults to a new directory under
#'   [tools::R_user_dir()]. Pass the directory of an earlier run to resume
#'   it.
#' @returns The results, as returned by [arena_read()], invisibly.
#' @export
#' @examples
#' \dontrun{
#' run <- arena_run("xs")
#' arena_report(run)
#' }
arena_run <- function(
  tier = c("xs", "s", "m", "l", "xl"),
  backends = NULL,
  tasks = NULL,
  sizes = NULL,
  budget = NULL,
  libpath = default_libpath(),
  install = c("ask", "missing", "update", "never"),
  dir = NULL
) {
  tier <- rlang::arg_match(tier)
  install <- rlang::arg_match(install)
  libpath_given <- !missing(libpath)
  resuming <- !is.null(dir) && file.exists(file.path(dir, "run.json"))
  if (resuming) {
    run <- read_run_json(dir)
    check_resume(run, backends, tasks, sizes)
    check_task_hashes(run)
    prefer <- run$prefer %||% "session"
    if (!libpath_given) {
      libpath <- default_libpath(prefer)
    }
    plan <- arena_plan(
      backends = unlist(run$backends),
      tasks = unlist(run$tasks),
      sizes = unlist(run$sizes),
      budget = run$budget,
      libpath = libpath
    )
  } else {
    prefer <- "session"
    if (!libpath_given) {
      prefer <- offer_installs(backends, install, default_libpath())
      # Computed after installing, which may have created tidyarena's library.
      libpath <- default_libpath(prefer)
    } else if (arena_lib() %in% libpath) {
      offer_installs(backends, install, libpath)
    }
    plan <- arena_plan(tier, backends, tasks, sizes, budget, libpath)
    checked <- preflight(
      unique(plan$backend[plan$action == "run"]),
      prefer,
      libpath_fixed = libpath_given,
      libpath = libpath
    )
    if (!identical(checked$prefer, prefer)) {
      prefer <- checked$prefer
      libpath <- checked$libpath
      plan <- arena_plan(tier, backends, tasks, sizes, budget, libpath)
    }
    plan <- skip_backends(plan, checked$skip)
    check_disk(plan)
    dir <- dir %||% new_run_dir()
    run <- new_run(dir, plan, prefer, checked$diagnostics)
  }
  settings <- attr(plan, "settings")

  dir.create(file.path(dir, "cells"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(dir, "logs"), showWarnings = FALSE)
  results_path <- file.path(dir, "results.jsonl")
  done <- read_cells(results_path)
  refs <- anchor_refs(done)
  failed <- failed_pairs(done)
  done_keys <- cell_key(done$size, done$task, done$backend)

  if (!resuming) {
    report_preflight(run$env, plan, run$guard)
  } else {
    cli::cli_inform("Resuming the run in {.path {dir}}: {length(done_keys)} cell{?s} done.")
  }

  todo <- plan[!cell_key(plan$size, plan$task, plan$backend) %in% done_keys, ]
  task_defs <- arena_tasks()
  backend_list <- backend_defs()
  cli::cli_progress_bar(
    "Running",
    total = nrow(todo),
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cell_label}"
  )
  cell_label <- ""

  data <- NULL
  for (i in seq_len(nrow(todo))) {
    cell <- todo[i, ]
    cell_label <- paste(cell$task, cell$backend, format_n(cell$size))
    cli::cli_progress_update()
    pair <- paste(cell$task, cell$backend)

    record <- if (cell$action == "skip") {
      skipped_record(cell, cell$reason)
    } else if (pair %in% failed) {
      skipped_record(cell, "failed at a smaller size")
    } else {
      task <- task_defs[task_defs$task == cell$task, ]
      if (is.null(data) || data$size != cell$size || data$dataset != task$data) {
        data <- c(arena_data(cell$size, dataset = task$data), size = cell$size, dataset = task$data)
      }
      run_cell(
        cell,
        task,
        backend_list[[cell$backend]],
        data,
        settings,
        run,
        dir,
        libpath
      )
    }

    key <- paste(cell$task, format_n(cell$size))
    if (cell$backend == anchor_backend && !is.null(record$fingerprint)) {
      refs[[key]] <- record$fingerprint
    }
    record <- check_record(record, refs[[key]], cell$backend)
    if (record$status %in% failure_statuses) {
      failed <- c(failed, pair)
    }
    write_record(record, results_path)
  }
  cli::cli_progress_done()

  run$finished <- format_time(Sys.time())
  write_run_json(run, dir)
  unlink(file.path(dir, "cells"), recursive = TRUE)

  results <- arena_read(dir)
  cli::cli_inform("Results are in {.path {dir}}.")
  invisible(results)
}

skip_backends <- function(plan, skip) {
  rows <- plan$backend %in% names(skip) & plan$action == "run"
  plan$action[rows] <- "skip"
  plan$reason[rows] <- unname(skip[plan$backend[rows]])
  plan
}

failure_statuses <- c(
  "error",
  "setup_error",
  "setup_timeout",
  "timeout",
  "oom_guard",
  "swap_guard",
  "oom_killed",
  "crashed"
)

run_cell <- function(cell, task, backend, data, settings, run, dir, libpath) {
  name <- paste(cell$task, cell$backend, format_n(cell$size), sep = "-")
  tmpdir <- file.path(dir, "cells", name)
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  spec <- list(
    code = task$code,
    order_key = task$order_key,
    packages = unlist(task$packages),
    setup = backend$setup,
    teardown = backend$teardown,
    source = backend$source,
    data = data$tables,
    reader_lib = reader_lib(),
    reps = settings$reps,
    budget_mb = floor(settings$budget / 2^20),
    libpath = libpath,
    tmpdir = tmpdir,
    out = file.path(dir, "cells", paste0(name, ".rds"))
  )
  result <- run_worker(
    spec,
    budget = settings$budget,
    timeout = size_timeout(cell$size) * (settings$reps + 2),
    guard = run$guard,
    log = file.path(dir, "logs", paste0(name, ".log")),
    setup_timeout = size_timeout(cell$size)
  )
  unlink(c(spec$out, sub("\\.rds$", "-spec.rds", spec$out), setup_marker(spec$out)))

  fallback <- backend$fallback_pattern
  if (
    identical(result$status, "ok") &&
      !is.null(fallback) &&
      any(grepl(fallback, result$conditions, ignore.case = TRUE))
  ) {
    result$status <- "fallback"
  }

  list(
    size = cell$size,
    task = cell$task,
    task_hash = task$hash,
    backend = cell$backend,
    source = backend$source,
    status = result$status %||% "crashed",
    reason = NA_character_,
    error = truncate(scrub(result$error %||% NA_character_), 1000),
    setup_seconds = result$setup_seconds %||% NA_real_,
    reps = result$reps %||% list(),
    compute_error = truncate(scrub(result$compute_error %||% NA_character_), 1000),
    peak_method = result$peak_method %||% NA_character_,
    poll_peak_rss = result$poll_peak_rss,
    spill_bytes = result$spill_bytes,
    conditions = utils::head(truncate(scrub(result$conditions), 300), 20),
    setup_conditions = utils::head(truncate(scrub(result$setup_conditions), 300), 20),
    fingerprint = result$fingerprint,
    data_fingerprint = data$fingerprint,
    versions = as.list(result$versions[backend_versions(result$versions, backend)]),
    package_sources = package_sources(backend$packages, libpath)
  )
}

# Versions worth keeping: the backend's packages and the engines under them.
backend_versions <- function(versions, backend) {
  keep <- c(backend$packages, "dplyr", "rlang", "vctrs", "duckdb", "arrow", "polars", "data.table")
  names(versions) %in% keep
}

skipped_record <- function(cell, reason) {
  list(
    size = cell$size,
    task = cell$task,
    backend = cell$backend,
    status = "skipped",
    reason = reason,
    reps = list()
  )
}

check_record <- function(record, ref, backend) {
  if (!record$status %in% c("ok", "fallback")) {
    record$correct <- NA
  } else if (backend == anchor_backend) {
    record$correct <- TRUE
  } else {
    same <- compare_fingerprint(record$fingerprint, ref)
    record$correct <- as.logical(same)
    record$mismatch <- attr(same, "reason") %||% NA_character_
  }
  record
}

anchor_refs <- function(done) {
  refs <- list()
  anchor <- done$records[done$backend == anchor_backend]
  for (r in anchor) {
    if (!is.null(r$fingerprint)) {
      refs[[paste(r$task, format_n(r$size))]] <- r$fingerprint
    }
  }
  refs
}

failed_pairs <- function(done) {
  bad <- done$status %in% failure_statuses
  unique(paste(done$task[bad], done$backend[bad]))
}

cell_key <- function(size, task, backend) {
  paste(format_n(size), task, backend)
}

new_run_dir <- function() {
  id <- paste0(
    format(Sys.time(), "%Y%m%d-%H%M%S"),
    "-",
    paste(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = "")
  )
  file.path(runs_dir(), id)
}

new_run <- function(dir, plan, prefer = "session", diagnostics = list()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  settings <- attr(plan, "settings")
  run <- list(
    schema_version = schema_version,
    suite_version = suite_version,
    run_id = basename(dir),
    sizes = sort(unique(plan$size)),
    tasks = unique(plan$task),
    task_hashes = as.list(task_hashes(unique(plan$task))),
    backends = unique(plan$backend),
    reps = settings$reps,
    budget = settings$budget,
    guard = guard_method(),
    libraries = install_records(attr(plan, "libpath")),
    prefer = prefer,
    diagnostics = diagnostics,
    started = format_time(Sys.time()),
    finished = NULL,
    env = arena_env(dir)
  )
  write_run_json(run, dir)
  run
}

check_resume <- function(run, backends, tasks, sizes, call = rlang::caller_env()) {
  given <- list(backends = backends, tasks = tasks, sizes = sizes)
  given <- given[!vapply(given, is.null, logical(1))]
  if (length(given) > 0) {
    cli::cli_abort(
      c(
        "Can't change {.arg {names(given)}} when resuming a run.",
        i = "Resume with the same {.arg dir} only, or start a new run."
      ),
      call = call
    )
  }
  invisible()
}

task_hashes <- function(tasks) {
  all <- arena_tasks()
  stats::setNames(all$hash[match(tasks, all$task)], tasks)
}

# A run resumed with changed task code would mix results of two different
# tasks under one id.
check_task_hashes <- function(run, call = rlang::caller_env()) {
  saved <- unlist(run$task_hashes)
  if (is.null(saved)) {
    return(invisible())
  }
  now <- task_hashes(names(saved))
  changed <- names(saved)[is.na(now) | now != saved]
  if (length(changed) > 0) {
    cli::cli_abort(
      c(
        "Can't resume: the code of {length(changed)} task{?s} changed since the run started: {.val {changed}}.",
        i = "Start a new run."
      ),
      call = call
    )
  }
  invisible()
}

report_preflight <- function(env, plan, guard) {
  n_run <- sum(plan$action == "run")
  cli::cli_inform(c(
    "Running {n_run} cell{?s} on {.val {unique(plan$backend[plan$action == 'run'])}}.",
    i = "Memory budget per task: {format_bytes(attr(plan, 'settings')$budget)} (guard: {guard})."
  ))
  notes <- setting_notes(env)
  if (length(notes) > 0) {
    cli::cli_warn(c("These settings can distort the results:", notes))
  }
}

# Generated data must fit on disk with 1 GB to spare.
check_disk <- function(plan, call = rlang::caller_env()) {
  run <- plan[plan$action == "run", ]
  tasks <- arena_tasks()
  needed <- unique(data.frame(size = run$size, dataset = tasks$data[match(run$task, tasks$task)]))
  dir <- arena_cache_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cached <- file.exists(file.path(
    vapply(
      seq_len(nrow(needed)),
      function(i) dataset_dir(dir, needed$dataset[[i]], needed$size[[i]]),
      character(1)
    ),
    "fingerprint.rds"
  ))
  todo <- needed[!cached, ]
  need <- sum(data_disk_bytes(todo$size, todo$dataset)) + 2^30
  free <- ps::ps_disk_usage(dir)$available[[1]]
  if (need > free) {
    cli::cli_abort(
      c(
        "Not enough disk space for the generated data.",
        i = "Needs {format_bytes(need)} in {.path {dir}}, {format_bytes(free)} is free.",
        i = "Use smaller {.arg sizes} or a smaller {.arg tier}."
      ),
      call = call
    )
  }
  invisible()
}

format_time <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%S%z")
}

# Removes what could identify the user from messages: the home directory
# and the user name.
scrub <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  home <- normalizePath("~", winslash = "/", mustWork = FALSE)
  x <- gsub(home, "~", x, fixed = TRUE)
  x <- gsub(path.expand("~"), "~", x, fixed = TRUE)
  user <- Sys.info()[["user"]]
  if (nzchar(user) && nchar(user) > 2) {
    x <- gsub(user, "<user>", x, fixed = TRUE)
  }
  x
}

truncate <- function(x, n) {
  if (length(x) == 0) {
    return(x)
  }
  ifelse(!is.na(x) & nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}
