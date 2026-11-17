#' Run the benchmark
#'
#' Runs every planned cell (see [arena_plan()]) in its own R process, smallest
#' size first, and writes results to `dir` after every cell. A run that is
#' interrupted continues where it stopped when `arena_run()` is called again
#' with the same `dir`.
#'
#' Without `dir`, `arena_run()` first looks for a run that stopped before it
#' finished (by an error, an interrupt, or the machine going down) and would
#' run the same cells: the same tasks, backends and sizes (and `budget`, if
#' given), with unchanged task code. It asks whether to continue that run; if not, a new
#' run starts, and the old one is not offered again. Outside an interactive
#' session it does not ask and starts a new run. A run still going in another
#' R process is never resumed.
#'
#' @section Safeguards:
#' Each task gets a memory budget. On Linux with systemd the budget is a hard
#' limit enforced by the kernel (a cgroup with swap disabled), so a task that
#' overruns it is killed without pushing the machine into swap. On every
#' platform a watchdog also kills a task that goes over budget, leaves less
#' than 1 GB free, makes swap grow, or runs past its timeout, which depends on
#' the size: 120 seconds a repetition at 1e6 rows, 300 at 1e7, 600 at 1e8
#' and 1800 at 1e9. Loading the data into the backend has a timeout of the
#' same length (`"setup_timeout"`).
#'
#' A backend may also take at most 10 times as long as dplyr on the same task
#' and size, and at least 30 seconds, for each run of the pipeline. Past that
#' it is stopped, and the cell is `"too_slow"`.
#'
#' A backend that falls back to dplyr in the warm-up (see `fallback_pattern`
#' in `vignette("extending")`) fails the cell: it is `"fallback"`, and its
#' times are not measured.
#'
#' A task that fails at one size, by any of these, is not run at larger sizes
#' on the same backend. Tasks predicted not to fit are skipped before they
#' start.
#'
#' @section What is measured:
#' For each repetition, twice: wall time and peak resident memory (exact on
#' Linux, sampled every 0.1 seconds elsewhere) of the pipeline with
#' [dplyr::collect()] and every value of the result read, and of the
#' pipeline with [dplyr::compute()], the result kept in the backend (for
#' dplyr, whose computed result is the collected one, only the first). For
#' each cell: whether it ran, whether its result matches dplyr's,
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
#'   [tools::R_user_dir()], or to an unfinished run of the same cells if you
#'   choose to continue it. Pass the directory of an earlier run to resume
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
  dir = NULL,
  part = NULL
) {
  tier <- rlang::arg_match(tier)
  install <- rlang::arg_match(install)
  libpath_given <- !missing(libpath)
  offered <- FALSE
  if (is.null(dir)) {
    dir <- offer_resume(arena_plan(tier, backends, tasks, sizes, budget, libpath, part), budget)
    offered <- !is.null(dir)
  }
  resuming <- !is.null(dir) && file.exists(file.path(dir, "run.json"))
  if (resuming) {
    check_not_active(dir)
    run <- read_run_json(dir)
    if (!offered) {
      check_resume(run, backends, tasks, sizes, part)
    }
    check_task_hashes(run)
    run$abandoned <- NULL
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
    plan <- arena_plan(tier, backends, tasks, sizes, budget, libpath, part)
    checked <- preflight(
      unique(plan$backend[plan$action == "run"]),
      prefer,
      libpath_fixed = libpath_given,
      libpath = libpath
    )
    if (!identical(checked$prefer, prefer)) {
      prefer <- checked$prefer
      libpath <- checked$libpath
      plan <- arena_plan(tier, backends, tasks, sizes, budget, libpath, part)
    }
    plan <- skip_backends(plan, checked$skip)
    check_disk(plan)
    dir <- dir %||% new_run_dir()
    run <- new_run(dir, plan, prefer, checked$diagnostics)
  }
  settings <- attr(plan, "settings")
  lock_run(dir)
  finished <- FALSE
  on.exit(
    {
      unlink(file.path(dir, "pid"))
      if (!finished) {
        cli::cli_inform(c(
          "!" = "The run stopped before it finished.",
          i = "Call {.fn arena_run} again with the same settings to continue it, or with {.code dir = {.str {dir}}}."
        ))
      }
    },
    add = TRUE
  )

  dir.create(file.path(dir, "cells"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(dir, "logs"), showWarnings = FALSE)
  results_path <- file.path(dir, "results.jsonl")
  done <- read_cells(results_path)
  refs <- anchor_refs(done)
  anchor_times <- anchor_seconds(done$records)
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
        libpath,
        anchor_times[[paste(cell$task, format_n(cell$size))]]
      )
    }

    key <- paste(cell$task, format_n(cell$size))
    if (cell$backend == anchor_backend && !is.null(record$fingerprint)) {
      refs[[key]] <- record$fingerprint
      anchor_times[[key]] <- anchor_seconds(list(record))[[key]]
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
  finished <- TRUE
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
  "fallback",
  "too_slow",
  "error",
  "setup_error",
  "setup_timeout",
  "timeout",
  "oom_guard",
  "swap_guard",
  "oom_killed",
  "crashed"
)

# A backend may take `slow_factor` times as long as dplyr on the same task
# and size, and at least `slow_floor` seconds, for every run of the
# pipeline; the cell is "too_slow" past that.
slow_factor <- 10
slow_floor <- 30

run_cell <- function(cell, task, backend, data, settings, run, dir, libpath, anchor_seconds = NULL) {
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
    fallback_pattern = backend$fallback_pattern,
    time_compute = backend$time_compute,
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
    setup_timeout = size_timeout(cell$size),
    run_timeout = run_timeout(anchor_seconds, cell$size, settings$reps, backend)
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

# Seconds the pipeline may take after setup, all runs together, or Inf when
# there is no dplyr time to compare with or the size's timeout is lower.
run_timeout <- function(anchor_seconds, size, reps, backend) {
  if (is.null(anchor_seconds) || is.na(anchor_seconds)) {
    return(Inf)
  }
  per_run <- max(slow_floor, slow_factor * anchor_seconds)
  if (per_run >= size_timeout(size)) {
    return(Inf)
  }
  runs <- (reps + 1) * if (isFALSE(backend$time_compute)) 1 else 2
  per_run * runs
}

# dplyr's median time over the timed repetitions, by task and size.
anchor_seconds <- function(records) {
  out <- list()
  for (r in records) {
    if (!identical(r$backend, anchor_backend) || !identical(r$status, "ok")) {
      next
    }
    timed <- Filter(function(x) !isTRUE(x$warmup), r$reps)
    seconds <- vapply(timed, function(x) as.double(x$seconds %||% NA), numeric(1))
    if (length(seconds) > 0) {
      out[[paste(r$task, format_n(r$size))]] <- stats::median(seconds, na.rm = TRUE)
    }
  }
  out
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
  if (!identical(record$status, "ok")) {
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

check_resume <- function(run, backends, tasks, sizes, part, call = rlang::caller_env()) {
  given <- list(backends = backends, tasks = tasks, sizes = sizes, part = part)
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

# A run that stopped before it finished, by a crash, an error or an
# interrupt, and would run the same cells as `plan`: asks whether to continue
# it, and returns its directory if so. A run turned down is not offered again.
offer_resume <- function(plan, budget = NULL, dir = runs_dir()) {
  path <- find_unfinished(plan, budget, dir)
  if (is.null(path)) {
    return(NULL)
  }
  run <- read_run_json(path)
  n_done <- length(read_cells(file.path(path, "results.jsonl"))$records)
  n_total <- nrow(plan)
  started <- run$started
  if (!rlang::is_interactive()) {
    cli::cli_inform(c(
      i = "An unfinished run of the same cells, started {started}, has {n_done} of {n_total} cell{?s} done.",
      " " = "Starting a new run. To continue that one instead, call {.fn arena_run} with {.code dir = {.str {path}}}."
    ))
    return(NULL)
  }
  question <- c(
    "An unfinished run of the same cells, started {started}, has {n_done} of {n_total} cell{?s} done.",
    i = "It is in {.path {path}}.",
    "Continue it? Otherwise a new run starts."
  )
  if (ask_yes_no(question)) {
    return(path)
  }
  run$abandoned <- format_time(Sys.time())
  write_run_json(run, path)
  NULL
}

# The latest run under `dir` that didn't finish, isn't running and hasn't
# been turned down, with the same suite, task code, tasks, backends and sizes
# as `plan`, and the same budget if one is given. The default budget follows
# the memory available at the time, so it is not compared.
find_unfinished <- function(plan, budget = NULL, dir = runs_dir()) {
  runs <- list.files(dir, full.names = TRUE)
  runs <- runs[file.exists(file.path(runs, "run.json"))]
  found <- NULL
  latest <- ""
  for (path in runs) {
    run <- tryCatch(read_run_json(path), error = function(e) NULL)
    if (
      is.null(run) ||
        !is.null(run$finished) ||
        !is.null(run$abandoned) ||
        !identical(run$suite_version, suite_version) ||
        !same_cells(run, plan, budget) ||
        run_active(path)
    ) {
      next
    }
    hashes_ok <- tryCatch(
      {
        check_task_hashes(run)
        TRUE
      },
      error = function(e) FALSE
    )
    started <- run$started %||% ""
    if (hashes_ok && started >= latest) {
      found <- path
      latest <- started
    }
  }
  found
}

same_cells <- function(run, plan, budget = NULL) {
  setequal(unlist(run$tasks), unique(plan$task)) &&
    setequal(unlist(run$backends), unique(plan$backend)) &&
    setequal(as.numeric(unlist(run$sizes)), unique(plan$size)) &&
    (is.null(budget) || isTRUE(all.equal(as.numeric(run$budget), as.numeric(budget))))
}

# The R process running a run leaves its pid and start time in the run's
# directory, so that a second arena_run() doesn't resume a run that is still
# going. The start time tells the process apart from a later one with the
# same pid, after a reboot.
lock_run <- function(dir) {
  self <- ps::ps_handle()
  writeLines(
    c(as.character(ps::ps_pid(self)), format(as.numeric(ps::ps_create_time(self)), digits = 15)),
    file.path(dir, "pid")
  )
}

run_active <- function(dir) {
  path <- file.path(dir, "pid")
  if (!file.exists(path)) {
    return(FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  tryCatch(
    {
      p <- ps::ps_handle(as.integer(lines[[1]]))
      ps::ps_is_running(p) && abs(as.numeric(ps::ps_create_time(p)) - as.numeric(lines[[2]])) < 1
    },
    error = function(e) FALSE
  )
}

check_not_active <- function(dir, call = rlang::caller_env()) {
  if (run_active(dir)) {
    cli::cli_abort(
      c(
        "Can't resume {.path {dir}}: it is still running in another R process.",
        i = "Wait for it to finish, or stop that process first."
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
