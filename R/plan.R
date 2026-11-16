#' Plan a benchmark run
#'
#' Works out what [arena_run()] would do, without running anything: which
#' cells (task × backend × size) run, which are skipped, and why. Peak memory
#' is a rough estimate from the data size; it decides which sizes fit in the
#' memory budget, and the guard in [arena_run()] catches what the estimate
#' misses.
#'
#' @section Tiers:
#' * `"s"`: 1e6 and 1e7 rows, 3 timed repetitions.
#' * `"m"`: 1e6, 1e7 and 1e8 rows, 5 timed repetitions.
#' * `"l"`: 1e6 to 1e9 rows, 5 timed repetitions.
#'
#' Every cell also runs one untimed warm-up first, recorded separately.
#'
#' @param tier The run's size: `"s"`, `"m"` or `"l"`.
#' @param backends Backend ids to run, see [arena_backends()]. Defaults to
#'   every available backend, except those left out with [arena_config()].
#'   The anchor backend, dplyr, is always included.
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
#' arena_plan("s")
arena_plan <- function(
  tier = c("s", "m", "l"),
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
    backends %||% default_backends(all_backends),
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
  task_row <- match(cells$task, all_tasks$task)
  dataset <- all_tasks$data[task_row]
  bytes <- data_bytes(cells$size, dataset)
  backend_factor <- vapply(defs[cells$backend], `[[`, numeric(1), "mem_factor")
  task_factor <- all_tasks$mem_factor[task_row]
  # The data frames in R, except for backends that read the Parquet files.
  in_r <- vapply(defs[cells$backend], function(d) d$source == "memory", logical(1))
  cells$est_peak <- unname(r_baseline_bytes + bytes * (in_r + backend_factor * task_factor))

  row <- match(cells$backend, all_backends$backend)
  cells$action <- "run"
  cells$reason <- NA_character_
  skip <- !all_backends$available[row]
  cells$reason[skip] <- all_backends$reason[row][skip]

  datasets <- dataset_defs()
  min_size <- vapply(datasets[dataset], `[[`, numeric(1), "min_size")
  too_small <- !skip & cells$size < min_size
  cells$reason[too_small] <- sprintf(
    "no %s data at this size (needs %s rows)",
    dataset[too_small],
    vapply(min_size[too_small], format_n, character(1))
  )
  skip <- skip | too_small
  missing <- vapply(
    all_tasks$packages[task_row],
    function(p) paste(p[!packages_installed(p, libpath)], collapse = ", "),
    character(1)
  )
  needs <- !skip & nzchar(missing)
  cells$reason[needs] <- paste("needs", missing[needs])
  skip <- skip | needs
  gen_factor <- vapply(datasets[dataset], `[[`, numeric(1), "gen_factor")
  too_big <- !skip & bytes * gen_factor > budget
  cells$reason[too_big] <- "data too large to generate in the memory budget"
  skip <- skip | too_big

  over <- !skip & cells$est_peak > budget
  cells$reason[over] <- "over memory budget"
  gpu <- defs[cells$backend] |> vapply(`[[`, logical(1), "gpu") |> unname()
  if (any(gpu)) {
    vram <- gpu_info()[[1]]$memory %||% 0
    gpu_peak <- bytes * (1 + backend_factor * task_factor)
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
    s = list(sizes = c(1e6, 1e7), reps = 3L, timeout = 120),
    m = list(sizes = c(1e6, 1e7, 1e8), reps = 5L, timeout = 600),
    l = list(sizes = c(1e6, 1e7, 1e8, 1e9), reps = 5L, timeout = 1800)
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
