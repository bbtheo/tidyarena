# The "xs", "s" and "m" tiers run a sample of the tasks of "l". Each sample
# takes the tasks this machine has run least often, so eight xs, four s or
# two m runs cover the tasks of one l run.

sample_tasks <- function(tasks, share, counts = task_counts()) {
  n <- min(length(tasks), ceiling(length(tasks) * share))
  counts <- unname(counts[tasks])
  counts[is.na(counts)] <- 0L
  round <- min(counts)
  todo <- tasks[counts == round]
  # A fixed order for this machine and round, so that arena_plan() and
  # arena_run() pick the same tasks, and different machines different ones.
  key <- vapply(paste(sample_seed(), round, todo), cli::hash_md5, character(1))
  todo <- todo[order(key)]
  sort(utils::head(todo, n))
}

# How often each task ran, with its current code, at every size of l, in
# finished runs of any tier.
task_counts <- function(dir = runs_dir()) {
  runs <- list.files(dir, full.names = TRUE)
  runs <- runs[file.exists(file.path(runs, "run.json"))]
  hashes <- task_hashes(arena_tasks()$task)
  counts <- stats::setNames(integer(length(hashes)), names(hashes))
  for (path in runs) {
    run <- tryCatch(read_run_json(path), error = function(e) NULL)
    if (
      is.null(run$finished) ||
        !identical(run$suite_version, suite_version) ||
        !all(tier_settings("l")$sizes %in% unlist(run$sizes))
    ) {
      next
    }
    saved <- unlist(run$task_hashes)
    same <- names(saved)[names(saved) %in% names(hashes) & saved == hashes[names(saved)]]
    counts[same] <- counts[same] + 1L
  }
  counts
}

runs_dir <- function() {
  getOption("tidyarena.runs_dir", file.path(tools::R_user_dir("tidyarena", "data"), "runs"))
}

# A random number made once per machine and kept with the settings. It only
# orders the tasks; it is not recorded in results.
sample_seed <- function() {
  path <- file.path(tools::R_user_dir("tidyarena", "config"), "sample-seed")
  if (file.exists(path)) {
    return(readLines(path, n = 1, warn = FALSE))
  }
  seed <- paste(sample(c(0:9, letters[1:6]), 16, replace = TRUE), collapse = "")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(seed, path)
  seed
}
