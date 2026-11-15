#' Plan a benchmark run
#'
#' Works out what [arena_run()] would do, without running anything: which
#' cells (task × backend × size) run, which are skipped, and why. Peak memory
#' is a rough estimate from the data size; it decides which sizes fit in the
#' memory budget, and the guard in [arena_run()] catches what the estimate
#' misses.
#'
#' @section Tiers:
#' * `"quick"`: 1e6 and 1e7 rows, 3 timed repetitions. About 15 minutes.
#' * `"standard"`: 1e6, 1e7 and 1e8 rows, 5 timed repetitions.
#' * `"full"`: 1e6 to 1e9 rows, 5 timed repetitions.
#'
#' Every cell also runs one untimed warm-up first, recorded separately.
#'
#' @param tier One of `"quick"`, `"standard"` or `"full"`.
#' @param backends Backend ids to run, see [arena_backends()]. Defaults to
#'   every available backend. The anchor backend, dplyr, is always included.
#' @param tasks Task ids to run, see [arena_tasks()]. Defaults to all.
#' @param sizes Row counts. Defaults to the tier's sizes. Runs with other
#'   sizes are not comparable with other machines.
#' @param budget Most memory one task may use, in bytes. Defaults to 70% of
#'   available memory, leaving at least 2 GB free.
#' @param libpath Library paths the backends are loaded from. Defaults to
#'   the session's library paths, followed by [arena_lib()] when it exists.
#' @returns A tibble with one row per cell: `size`, `task`, `backend`,
#'   `est_peak` (bytes), `action` (`"run"` or `"skip"`) and `reason`.
#' @export
#' @examples
#' arena_plan("quick")
arena_plan <- function(
  tier = c("quick", "standard", "full"),
  backends = NULL,
  tasks = NULL,
  sizes = NULL,
  budget = NULL,
  libpath = default_libpath()
) {
  tier <- rlang::arg_match(tier)
  settings <- tier_settings(tier)
  sizes <- sort(as.double(sizes %||% settings$sizes))
  budget <- budget %||% default_budget()

  all_backends <- arena_backends(libpath)
  backends <- check_ids(
    backends %||% all_backends$backend[all_backends$available],
    all_backends$backend,
    "backends"
  )
  backends <- union(anchor_backend, backends)
  all_tasks <- arena_tasks()
  tasks <- check_ids(tasks %||% all_tasks$task, all_tasks$task, "tasks")

  cells <- expand.grid(
    backend = backends,
    task = tasks,
    size = sizes,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )[c("size", "task", "backend")]

  defs <- backend_defs()
  backend_factor <- vapply(defs[cells$backend], `[[`, numeric(1), "mem_factor")
  task_factor <- all_tasks$mem_factor[match(cells$task, all_tasks$task)]
  cells$est_peak <- unname(
    r_baseline_bytes + data_bytes(cells$size) * (1 + backend_factor * task_factor)
  )

  available <- all_backends$available[match(cells$backend, all_backends$backend)]
  cells$action <- "run"
  cells$reason <- NA_character_
  skip <- !available
  cells$reason[skip] <- "not installed"
  over <- !skip & cells$est_peak > budget
  cells$reason[over] <- "over memory budget"
  gpu <- defs[cells$backend] |> vapply(`[[`, logical(1), "gpu") |> unname()
  if (any(gpu)) {
    vram <- env_gpu()[[1]]$memory %||% 0
    gpu_peak <- data_bytes(cells$size) * (1 + backend_factor * task_factor)
    over_gpu <- !skip & !over & gpu & gpu_peak > 0.8 * vram
    cells$reason[over_gpu] <- "over GPU memory"
    over <- over | over_gpu
  }
  cells$action[skip | over] <- "skip"

  out <- tibble::as_tibble(cells)
  attr(out, "settings") <- c(settings, list(budget = budget, tier = tier))
  attr(out, "libpath") <- libpath
  out
}

r_baseline_bytes <- 300 * 2^20

tier_settings <- function(tier) {
  switch(
    tier,
    quick = list(sizes = c(1e6, 1e7), reps = 3L, timeout = 120),
    standard = list(sizes = c(1e6, 1e7, 1e8), reps = 5L, timeout = 600),
    full = list(sizes = c(1e6, 1e7, 1e8, 1e9), reps = 5L, timeout = 1800)
  )
}

check_ids <- function(x, valid, arg, call = rlang::caller_env()) {
  bad <- setdiff(x, valid)
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has unknown id{?s}: {.val {bad}}.",
        i = "Known ids: {.val {valid}}."
      ),
      call = call
    )
  }
  x
}

check_size <- function(n, call = rlang::caller_env()) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    cli::cli_abort("{.arg n} must be a single positive number.", call = call)
  }
  invisible(n)
}
