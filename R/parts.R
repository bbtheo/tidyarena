# The suite is split into four parts with similar run times and a similar
# mix of data sets (the `part` of each task). "xs" and "s" run one part, "m"
# two, at the sizes of their tier. Each run takes the parts this machine has
# run least often at those sizes, so four s runs or two m runs cover the
# tasks of one l run.

n_parts <- 4L

pick_parts <- function(n, sizes, counts = part_counts(sizes)) {
  round <- min(counts)
  todo <- which(counts == round)
  # A fixed order for this machine and round, so that arena_plan() and
  # arena_run() pick the same parts, and different machines different ones.
  key <- vapply(paste(sample_seed(), round, todo), cli::hash_md5, character(1))
  todo <- todo[order(key)]
  picked <- utils::head(todo, n)
  if (length(picked) < n) {
    # The round ends: take the rest from the next one.
    counts[picked] <- counts[picked] + 1L
    picked <- c(picked, pick_parts(n - length(picked), sizes, counts))
  }
  sort(picked)
}

# How often each part ran in full, with the current code of its tasks and at
# least `sizes`, in finished runs.
part_counts <- function(sizes, dir = runs_dir()) {
  tasks <- arena_tasks()
  hashes <- stats::setNames(tasks$hash, tasks$task)
  counts <- integer(n_parts)
  runs <- list.files(dir, full.names = TRUE)
  runs <- runs[file.exists(file.path(runs, "run.json"))]
  for (path in runs) {
    run <- tryCatch(read_run_json(path), error = function(e) NULL)
    if (
      is.null(run$finished) ||
        !identical(run$suite_version, suite_version) ||
        !all(sizes %in% unlist(run$sizes))
    ) {
      next
    }
    saved <- unlist(run$task_hashes)
    same <- unname(saved[names(hashes)] == hashes) %in% TRUE
    for (p in seq_len(n_parts)) {
      if (all(same[tasks$part == p])) {
        counts[p] <- counts[p] + 1L
      }
    }
  }
  counts
}

runs_dir <- function() {
  getOption("tidyarena.runs_dir", file.path(tools::R_user_dir("tidyarena", "data"), "runs"))
}

# A random number made once per machine and kept with the settings. It only
# orders the parts; it is not recorded in results.
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
