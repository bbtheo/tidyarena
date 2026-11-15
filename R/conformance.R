#' Check a backend
#'
#' Runs every task on a small table with the backend, the same way
#' [arena_run()] does (each task in its own R process, under a memory
#' budget), and compares the results with dplyr's. Use it while writing a
#' new backend: it takes seconds, not the minutes of a run.
#'
#' @param backend A backend id, see [arena_backends()].
#' @param tasks Task ids to run. Defaults to all.
#' @param n Rows in the table.
#' @inheritParams arena_plan
#' @returns A tibble with one row per task, invisibly: `task`, `backend`,
#'   `status` (as in [arena_run()]), `correct` and `note`, the error or the
#'   mismatch with dplyr.
#' @export
#' @examples
#' \dontrun{
#' arena_check_backend("duckplyr")
#' }
arena_check_backend <- function(
  backend,
  tasks = NULL,
  n = 1000,
  libpath = default_libpath()
) {
  rlang::check_string(backend)
  check_ids(backend, names(backend_defs()), "backend")
  check_size(n)
  all_tasks <- arena_tasks()
  tasks <- check_ids(tasks %||% all_tasks$task, all_tasks$task, "tasks")

  cells <- conformance(tasks, union(anchor_backend, backend), n, libpath)
  cells <- cells[cells$backend == backend, ]
  cli::cli_h2("{.val {backend}} on {length(tasks)} task{?s}, {format_n(n)} rows")
  print_lines(format_conformance(cells))
  conformance_summary(cells)
  invisible(cells)
}

#' Check a task
#'
#' Checks a new or changed task before it goes into the suite:
#'
#' * it runs on dplyr, and gives the same result when run again;
#' * its memory factor, measured with dplyr at 1e6 rows, is not above the
#'   declared one;
#' * every available backend either runs it correctly or fails clearly.
#'
#' A grouped task is checked together with its `.by` form.
#'
#' @param task A task id, see [arena_tasks()].
#' @param backends Backends to run it on. Defaults to every available
#'   backend.
#' @param n Rows in the table for the backend checks.
#' @inheritParams arena_plan
#' @returns A list, invisibly: `deterministic` (logical), `mem_factor` (the
#'   measured memory factor of each task) and `cells`, as returned by
#'   [arena_check_backend()].
#' @export
#' @examples
#' \dontrun{
#' arena_check_task("summarise_lowcard")
#' }
arena_check_task <- function(
  task,
  backends = NULL,
  n = 1000,
  libpath = default_libpath()
) {
  rlang::check_string(task)
  all_tasks <- arena_tasks()
  check_ids(task, all_tasks$task, "task")
  check_size(n)
  tasks <- intersect(c(task, paste0(task, "_by")), all_tasks$task)
  all_backends <- arena_backends(libpath)
  backends <- check_ids(
    backends %||% default_backends(all_backends),
    all_backends$backend,
    "backends"
  )

  cli::cli_h2("Task {.val {tasks}}")
  first <- conformance(tasks, anchor_backend, n, libpath)
  again <- conformance(tasks, anchor_backend, n, libpath)
  ran <- first$status == "ok"
  deterministic <- all(ran) && all(mapply(
    function(a, b) isTRUE(compare_fingerprint(a, b)),
    first$fingerprint,
    again$fingerprint
  ))
  if (!all(ran)) {
    cli::cli_alert_danger("It fails on dplyr: {first$note[!ran][[1]]}")
  } else if (deterministic) {
    cli::cli_alert_success("It runs on dplyr and gives the same result every time.")
  } else {
    cli::cli_alert_danger(
      "dplyr gives a different result when it runs again. Results must not depend on chance or on row order, unless the task sets {.field order_key}."
    )
  }

  measured <- measure_mem_factor(tasks, libpath)
  declared <- all_tasks$mem_factor[match(tasks, all_tasks$task)]
  for (i in seq_along(tasks)) {
    cli::cli_alert_info(
      "Memory factor of {.val {tasks[[i]]}}: {format(measured[[i]])} measured, {format(declared[[i]])} declared."
    )
  }
  # Too low is what matters: the plan would then start sizes that don't fit,
  # and only the guard would stop them.
  low <- !is.na(measured) & measured > 1.5 * declared
  if (any(low)) {
    cli::cli_alert_warning(
      "The declared {.field mem_factor} of {.val {tasks[low]}} is too low. Raise it to at least the measured value: it decides which sizes fit in memory."
    )
  }

  others <- setdiff(backends, anchor_backend)
  cells <- conformance(tasks, union(anchor_backend, others), n, libpath)
  cells <- cells[cells$backend != anchor_backend, ]
  if (nrow(cells) > 0) {
    cli::cli_h2("On {length(others)} other backend{?s}, {format_n(n)} rows")
    print_lines(format_conformance(cells))
    conformance_summary(cells)
  }

  invisible(list(
    deterministic = deterministic,
    mem_factor = stats::setNames(measured, tasks),
    cells = cells
  ))
}

# Runs `tasks` on `backends` (the anchor first, as the reference) with one
# timed repetition. Returns a tibble: task, backend, status, correct, note,
# fingerprint and peak_rss_delta.
conformance <- function(
  tasks,
  backends,
  n,
  libpath,
  reps = 1L,
  budget = min(default_budget(), 4 * 2^30)
) {
  dir <- withr_tempdir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(file.path(dir, "cells"))
  dir.create(file.path(dir, "logs"))
  settings <- list(reps = reps, budget = budget, timeout = 60)
  run <- list(guard = guard_method())
  all_tasks <- arena_tasks()
  defs <- backend_defs()
  backends <- c(intersect(anchor_backend, backends), setdiff(backends, anchor_backend))

  rows <- list()
  for (task_id in tasks) {
    task <- all_tasks[all_tasks$task == task_id, ]
    # Some data sets don't come smaller than a minimum size.
    size <- max(n, dataset_def(task$data)$min_size)
    data <- c(arena_data(size, dataset = task$data), size = size)
    ref <- NULL
    for (backend in backends) {
      cell <- list(size = size, task = task_id, backend = backend)
      record <- run_cell(cell, task, defs[[backend]], data, settings, run, dir, libpath)
      if (backend == anchor_backend) {
        ref <- record$fingerprint
      }
      record <- check_record(record, ref, backend)
      delta <- record$reps |>
        Filter(f = function(x) !isTRUE(x$warmup)) |>
        vapply(function(x) (x$peak_rss %||% NA) - (x$base_rss %||% NA), numeric(1)) |>
        max_or_na()
      rows[[length(rows) + 1]] <- tibble::tibble(
        task = task_id,
        backend = backend,
        status = record$status,
        correct = record$correct %||% NA,
        note = conformance_note(record),
        fingerprint = list(record$fingerprint),
        peak_rss_delta = delta
      )
    }
  }
  do.call(rbind, rows)
}

conformance_note <- function(record) {
  note <- if (!is.na(record$error %||% NA)) {
    record$error
  } else if (isFALSE(record$correct)) {
    paste("differs from dplyr:", record$mismatch %||% "")
  } else if (identical(record$status, "fallback")) {
    "fell back to dplyr"
  } else {
    ""
  }
  truncate(gsub("\\s+", " ", note), 80)
}

# The memory factor of each task, as arena_plan() uses it: memory the task
# needs beyond its input, relative to the input and to dplyr's backend
# factor, measured with dplyr at 1e6 rows. NA where it couldn't be measured.
measure_mem_factor <- function(tasks, libpath, n = 1e6) {
  cells <- conformance(tasks, anchor_backend, n, libpath)
  all_tasks <- arena_tasks()
  dataset <- all_tasks$data[match(tasks, all_tasks$task)]
  size <- pmax(n, vapply(dataset, function(d) dataset_def(d)$min_size, numeric(1)))
  factor <- cells$peak_rss_delta / (backend_defs()[[anchor_backend]]$mem_factor * data_bytes(size, dataset))
  round(pmax(factor, 0.1), 1)
}

# The table of tasks, then the notes of the ones that didn't run cleanly,
# which would make the table too wide.
format_conformance <- function(cells) {
  table <- data.frame(
    task = cells$task,
    status = cells$status,
    correct = ifelse(is.na(cells$correct), "-", ifelse(cells$correct, "yes", "no"))
  )
  lines <- utils::capture.output(print(table, row.names = FALSE, right = FALSE))
  noted <- nzchar(cells$note)
  if (any(noted)) {
    lines <- c(lines, "", paste0("* ", cells$task[noted], ": ", cells$note[noted]))
  }
  lines
}

conformance_summary <- function(cells) {
  ok <- sum(cells$status == "ok" & cells$correct %in% TRUE)
  fallback <- sum(cells$status == "fallback")
  wrong <- sum(cells$correct %in% FALSE)
  failed <- sum(cells$status %in% failure_statuses)
  cli::cli_text(
    "{ok}/{nrow(cells)} ran correctly; {fallback} fell back, {wrong} wrong, {failed} failed."
  )
  invisible()
}
