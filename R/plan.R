#' Plan a benchmark run
#'
#' Works out what [arena_run()] would do, without running anything: which
#' cells (task × backend × size) run, which are skipped, and why. Peak memory
#' is a rough estimate from the data size; it decides which sizes fit in the
#' memory budget, and the guard in [arena_run()] catches what the estimate
#' misses.
#'
#' @section Tiers:
#' The tasks are split into four parts with similar run times and a similar
#' mix of data sets (see the `part` column of [arena_tasks()]).
#'
#' * `"xs"`: one part, at 1e6 and 1e7 rows.
#' * `"s"`: one part, at 1e6, 1e7 and 1e8 rows.
#' * `"m"`: two parts, at 1e6, 1e7 and 1e8 rows.
#' * `"l"`: every task, at 1e6, 1e7 and 1e8 rows.
#' * `"xl"`: every task, at 1e6 to 1e9 rows.
#'
#' Every cell runs one untimed warm-up and three timed repetitions, and how
#' it runs depends only on its task, backend and size: the tier only decides
#' which cells run, and is not recorded. Two m runs give the same cells as one
#' l run.
#'
#' Unless `part` is given, a run takes the parts this machine has run least
#' often at the tier's sizes, in finished runs in the default run directory,
#' so four s runs or two m runs in a row cover the tasks of one l run. The
#' order is fixed for each machine, so `arena_plan()` shows the parts
#' [arena_run()] would run, and different machines start with different
#' parts.
#'
#' @param tier The run's size: `"xs"`, `"s"`, `"m"`, `"l"` or `"xl"`.
#' @param backends Backend ids to run, see [arena_backends()]. Defaults to
#'   every available backend, except those left out with [arena_config()].
#'   The anchor backend, dplyr, is always included.
#' @param tasks Task ids to run, see [arena_tasks()]. Defaults to the
#'   tier's parts.
#' @param sizes Row counts. Defaults to the tier's sizes. Runs with other
#'   sizes are not comparable with other machines.
#' @param budget Most memory one task may use, in bytes. Defaults to 70% of
#'   available memory, leaving at least 2 GB free.
#' @param libpath Library paths the backends are loaded from. Defaults to
#'   the session's library paths, followed by [arena_lib()] when it exists.
#' @param part Parts of the suite to run, from 1 to 4, instead of the ones
#'   the tier would pick.
#' @returns A tibble with one row per cell: `size`, `task`, `backend`,
#'   `est_peak` (bytes), `action` (`"run"` or `"skip"`) and `reason`.
#' @export
#' @examples
#' arena_plan("xs")
arena_plan <- function(
  tier = c("xs", "s", "m", "l", "xl"),
  backends = NULL,
  tasks = NULL,
  sizes = NULL,
  budget = NULL,
  libpath = default_libpath(),
  part = NULL
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
  if (is.null(tasks)) {
    if (is.null(part)) {
      part <- if (settings$parts < n_parts) pick_parts(settings$parts, sizes) else seq_len(n_parts)
    }
    part <- check_part(part)
    tasks <- all_tasks$task[all_tasks$part %in% part]
  } else if (!is.null(part)) {
    cli::cli_abort("Give {.arg tasks} or {.arg part}, not both.")
  }
  tasks <- check_ids(tasks, all_tasks$task, "tasks")

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
  attr(out, "settings") <- list(reps = arena_reps, budget = budget)
  attr(out, "libpath") <- libpath
  out
}

r_baseline_bytes <- 300 * 2^20

# A tier only chooses which cells run. How a cell runs depends on nothing but
# its task, backend and size, so cells from runs of any tier can be pooled.
tier_settings <- function(tier) {
  switch(
    tier,
    xs = list(sizes = c(1e6, 1e7), parts = 1L),
    s = list(sizes = c(1e6, 1e7, 1e8), parts = 1L),
    m = list(sizes = c(1e6, 1e7, 1e8), parts = 2L),
    l = list(sizes = c(1e6, 1e7, 1e8), parts = 4L),
    xl = list(sizes = c(1e6, 1e7, 1e8, 1e9), parts = 4L)
  )
}

arena_reps <- 3L

# Seconds one repetition, or loading the data, may take.
size_timeout <- function(size) {
  if (size <= 1e6) 120 else if (size <= 1e7) 300 else if (size <= 1e8) 600 else 1800
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

check_part <- function(part, call = rlang::caller_env()) {
  if (!is.numeric(part) || length(part) == 0 || anyNA(part) || !all(part %in% seq_len(n_parts))) {
    cli::cli_abort("{.arg part} must be whole numbers from 1 to {n_parts}.", call = call)
  }
  sort(unique(as.integer(part)))
}

check_size <- function(n, call = rlang::caller_env()) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    cli::cli_abort("{.arg n} must be a single positive number.", call = call)
  }
  invisible(n)
}
