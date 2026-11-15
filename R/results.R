schema_version <- 2L
suite_version <- "2026.1-dev"

#' Read the results of a run
#'
#' A run directory holds `run.json` (the settings of the run and the
#' [arena_env()] snapshot) and `results.jsonl` (one JSON record per cell,
#' with every repetition). `arena_read()` reads both and summarises them.
#'
#' @param dir A run directory, as created by [arena_run()].
#' @returns An `arena_results` object: a list with
#'   * `run`: the contents of `run.json`.
#'   * `cells`: a tibble with one row per cell: `size`, `task`, `backend`,
#'     `status`, `correct`, `seconds` (median over timed repetitions),
#'     `warmup_seconds`, `peak_rss` (bytes, highest over repetitions),
#'     `peak_rss_delta` (peak minus the memory in use before the pipeline
#'     started), `spill_bytes`, and `reason` and `error` for cells that did
#'     not run cleanly.
#'   * `reps`: a tibble with one row per repetition.
#'   * `dir`: the run directory.
#' @export
arena_read <- function(dir) {
  if (!file.exists(file.path(dir, "run.json"))) {
    cli::cli_abort("{.path {dir}} is not a tidyarena run directory.")
  }
  run <- read_run_json(dir)
  done <- read_cells(file.path(dir, "results.jsonl"))
  structure(
    list(
      run = run,
      cells = summarise_cells(done$records),
      reps = reps_table(done$records),
      dir = dir
    ),
    class = "arena_results"
  )
}

#' @export
print.arena_results <- function(x, ...) {
  cells <- x$cells
  cli::cli_text(
    "{.cls arena_results}: run {.val {x$run$run_id}}, ",
    "{nrow(cells)} cell{?s}, {sum(cells$status %in% c('ok', 'fallback'))} ran."
  )
  cli::cli_text("Use {.fn arena_report} for a summary.")
  invisible(x)
}

summarise_cells <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      size = numeric(),
      task = character(),
      task_hash = character(),
      backend = character(),
      status = character(),
      correct = logical(),
      seconds = numeric(),
      warmup_seconds = numeric(),
      peak_rss = numeric(),
      peak_rss_delta = numeric(),
      compute_seconds = numeric(),
      compute_peak_rss = numeric(),
      compute_peak_rss_delta = numeric(),
      compute_error = character(),
      spill_bytes = numeric(),
      reason = character(),
      error = character()
    ))
  }
  field <- function(name, type) {
    vapply(records, function(r) as_scalar(r[[name]], type), type)
  }
  timed <- lapply(records, function(r) Filter(function(x) !isTRUE(x$warmup), r$reps))
  rep_values <- function(reps, name) {
    as.numeric(vapply(reps, function(x) as_scalar(x[[name]], numeric(1)), numeric(1)))
  }
  seconds <- vapply(timed, function(reps) median_or_na(rep_values(reps, "seconds")), numeric(1))
  warmup <- vapply(
    records,
    function(r) {
      w <- Filter(function(x) isTRUE(x$warmup), r$reps)
      if (length(w)) as_scalar(w[[1]]$seconds, numeric(1)) else NA_real_
    },
    numeric(1)
  )
  peak <- vapply(
    seq_along(records),
    function(i) {
      exact <- max_or_na(rep_values(records[[i]]$reps, "peak_rss"))
      if (is.na(exact)) as_scalar(records[[i]]$poll_peak_rss, numeric(1)) else exact
    },
    numeric(1)
  )
  delta <- vapply(
    records,
    function(r) {
      max_or_na(rep_values(r$reps, "peak_rss") - rep_values(r$reps, "base_rss"))
    },
    numeric(1)
  )
  compute_seconds <- vapply(
    timed,
    function(reps) median_or_na(rep_values(reps, "compute_seconds")),
    numeric(1)
  )
  compute_peak <- vapply(
    records,
    function(r) max_or_na(rep_values(r$reps, "compute_peak_rss")),
    numeric(1)
  )
  compute_delta <- vapply(
    records,
    function(r) {
      max_or_na(rep_values(r$reps, "compute_peak_rss") - rep_values(r$reps, "compute_base_rss"))
    },
    numeric(1)
  )

  tibble::tibble(
    size = field("size", numeric(1)),
    task = field("task", character(1)),
    task_hash = field("task_hash", character(1)),
    backend = field("backend", character(1)),
    status = field("status", character(1)),
    correct = field("correct", logical(1)),
    seconds = seconds,
    warmup_seconds = warmup,
    peak_rss = peak,
    peak_rss_delta = delta,
    compute_seconds = compute_seconds,
    compute_peak_rss = compute_peak,
    compute_peak_rss_delta = compute_delta,
    compute_error = field("compute_error", character(1)),
    spill_bytes = field("spill_bytes", numeric(1)),
    reason = field("reason", character(1)),
    error = field("error", character(1))
  )
}

reps_table <- function(records) {
  rows <- lapply(records, function(r) {
    if (length(r$reps) == 0) {
      return(NULL)
    }
    value <- function(name) vapply(r$reps, function(x) as_scalar(x[[name]], numeric(1)), numeric(1))
    tibble::tibble(
      size = as.numeric(r$size),
      task = r$task,
      task_hash = as_scalar(r$task_hash, character(1)),
      backend = r$backend,
      rep = vapply(r$reps, function(x) as_scalar(x$rep, integer(1)), integer(1)),
      warmup = vapply(r$reps, function(x) isTRUE(x$warmup), logical(1)),
      seconds = value("seconds"),
      base_rss = value("base_rss"),
      peak_rss = value("peak_rss"),
      compute_seconds = value("compute_seconds"),
      compute_base_rss = value("compute_base_rss"),
      compute_peak_rss = value("compute_peak_rss")
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(tibble::tibble(
      size = numeric(),
      task = character(),
      task_hash = character(),
      backend = character(),
      rep = integer(),
      warmup = logical(),
      seconds = numeric(),
      base_rss = numeric(),
      peak_rss = numeric(),
      compute_seconds = numeric(),
      compute_base_rss = numeric(),
      compute_peak_rss = numeric()
    ))
  }
  do.call(rbind, rows)
}

as_scalar <- function(x, type) {
  if (length(x) == 0 || is.null(x[[1]])) {
    return(type[NA_integer_])
  }
  as.vector(x[[1]], mode = typeof(type))
}

median_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else stats::median(x)
}

max_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

read_cells <- function(path) {
  records <- if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
    lapply(lines[nzchar(lines)], jsonlite::fromJSON, simplifyVector = FALSE)
  } else {
    list()
  }
  pick <- function(name) vapply(records, function(r) as.character(r[[name]]), character(1))
  list(
    records = records,
    size = as.numeric(pick("size")),
    task = pick("task"),
    backend = pick("backend"),
    status = pick("status")
  )
}

write_record <- function(record, path) {
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  cat(line, "\n", file = path, sep = "", append = TRUE)
}

write_run_json <- function(run, dir) {
  jsonlite::write_json(
    run,
    file.path(dir, "run.json"),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )
}

read_run_json <- function(dir) {
  jsonlite::read_json(file.path(dir, "run.json"), simplifyVector = FALSE)
}
