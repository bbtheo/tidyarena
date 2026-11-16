# Tasks and backends are files, one per id: `inst/tasks/<id>.R` and
# `inst/backends/<id>.R`. Each file is R code whose value is a list of
# fields. Adding a task or a backend is adding a file; see
# `vignette("extending")`.

read_defs <- function(kind) {
  dir <- system.file(kind, package = "tidyarena")
  files <- sort(list.files(dir, pattern = "\\.R$", full.names = TRUE))
  defs <- lapply(files, read_def_file)
  stats::setNames(defs, tools::file_path_sans_ext(basename(files)))
}

# Evaluates a definition file. Its functions can use tidyarena's internals.
read_def_file <- function(path) {
  env <- new.env(parent = asNamespace("tidyarena"))
  value <- NULL
  for (expr in parse(path, keep.source = FALSE)) {
    value <- eval(expr, env)
  }
  value
}

task_fields <- list(
  required = c("family", "code"),
  optional = c("data", "packages", "order_key", "mem_factor", "twin")
)

backend_fields <- list(
  required = c("packages", "convert", "setup"),
  optional = c(
    "gpu",
    "mem_factor",
    "teardown",
    "fallback_pattern",
    "requires",
    "diagnose",
    "install",
    "install_hint",
    "read"
  )
)

check_def <- function(def, id, kind, fields, call = rlang::caller_env()) {
  where <- sprintf("inst/%s/%s.R", kind, id)
  if (!is.list(def) || is.null(names(def))) {
    cli::cli_abort("{.file {where}} must end with a named list.", call = call)
  }
  missing <- setdiff(fields$required, names(def))
  if (length(missing) > 0) {
    cli::cli_abort("{.file {where}} is missing {.field {missing}}.", call = call)
  }
  unknown <- setdiff(names(def), c(fields$required, fields$optional))
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.file {where}} has unknown field{?s} {.field {unknown}}.",
        i = "Known fields: {.field {c(fields$required, fields$optional)}}."
      ),
      call = call
    )
  }
  invisible(def)
}

# Backend definitions, the anchor first, with defaults filled in: not a GPU
# backend, a memory factor of 3, and dplyr's collect() and compute(). A
# backend with a `read` field also gets a Parquet variant, `<id>_parquet`,
# right after it: the same backend reading the tables from their Parquet
# files itself instead of from the data frames in R. `source` says which
# (`"memory"` or `"parquet"`) and `base` is the backend's own id.
backend_defs <- function() {
  defs <- read_defs("backends")
  defs <- defs[union(anchor_backend, names(defs))]
  out <- list()
  for (id in names(defs)) {
    def <- check_def(defs[[id]], id, "backends", backend_fields)
    if (!any(grepl("^\\s*\\.collect\\s*<-", def$setup))) {
      def$setup <- c(def$setup, ".collect <- function(x) dplyr::collect(x)")
    }
    if (!any(grepl("^\\s*\\.compute\\s*<-", def$setup))) {
      def$setup <- c(def$setup, ".compute <- function(x) dplyr::compute(x)")
    }
    def$gpu <- def$gpu %||% FALSE
    def$mem_factor <- def$mem_factor %||% 3
    def$base <- id
    read <- def$read
    def$read <- NULL
    memory <- def
    memory$source <- "memory"
    memory$setup <- c(def$setup, paste0(".convert <- function(x, name) ", def$convert))
    out[[id]] <- memory
    if (!is.null(read)) {
      parquet <- def
      parquet$source <- "parquet"
      parquet$setup <- c(def$setup, paste0(".read <- function(path, name) ", read))
      out[[paste0(id, "_parquet")]] <- parquet
    }
  }
  out
}

# Task definitions with defaults filled in, followed by the `.by` twins of
# the grouped tasks. A task without a memory factor gets 1 (arena_check_task()
# measures it), and one without a data set gets tidyarena's own.
task_defs <- function() {
  defs <- read_defs("tasks")
  twins <- list()
  for (id in names(defs)) {
    def <- check_def(defs[[id]], id, "tasks", task_fields)
    def$mem_factor <- def$mem_factor %||% 1
    def$data <- def$data %||% "arena"
    # A pipeline over several statements has no automatic .by form.
    single <- length(parse(text = def$code, keep.source = FALSE)) == 1
    grouped <- grepl("group_by(", def$code, fixed = TRUE)
    if (grouped && single && !isFALSE(def$twin)) {
      def$idiom <- "group_by"
      twin <- def
      twin$code <- by_twin(def$code, id)
      twin$idiom <- ".by"
      twins[[paste0(id, "_by")]] <- twin
    }
    def$twin <- NULL
    defs[[id]] <- def
  }
  for (id in names(twins)) {
    twins[[id]]$twin <- NULL
  }
  c(defs, twins)
}

# A short hash of a task's code. Results are only compared between runs
# whose tasks have the same hash.
task_hash <- function(code) {
  substr(cli::hash_md5(code), 1, 12)
}

# --- .by twins ---

# Verbs that take `.by`.
by_verbs <- c("summarise", "summarize", "mutate", "filter", "reframe", "slice_min", "slice_max")

# Rewrites every `group_by(g) |> verb(...)` in a pipeline into
# `verb(..., .by = g)`. The grouping must end at that verb: summarise() with
# `.groups = "drop"` (or one grouping column), or any verb followed by
# ungroup(), which is dropped.
by_twin <- function(code, id = "task", call = rlang::caller_env()) {
  fail <- function(why) {
    cli::cli_abort(
      c(
        "Can't write the {.code .by} form of task {.val {id}}: {why}.",
        i = "Set {.code twin = FALSE} in its file if it has no {.code .by} form."
      ),
      call = call
    )
  }
  chain <- flatten_pipe(parse(text = code, keep.source = FALSE)[[1]])
  steps <- chain$steps
  i <- 1
  while (i <= length(steps)) {
    if (!identical(step_name(steps[[i]]), "group_by")) {
      i <- i + 1
      next
    }
    groups <- as.list(steps[[i]])[-1]
    if (length(groups) == 0 || !is.null(names(groups)) && any(nzchar(names(groups)))) {
      fail("group_by() must list only grouping columns")
    }
    if (i == length(steps) || !step_name(steps[[i + 1]]) %in% by_verbs) {
      fail(sprintf("group_by() must be followed by one of %s", paste0(by_verbs, "()", collapse = ", ")))
    }
    verb <- steps[[i + 1]]
    verb_name <- step_name(verb)
    ends_grouping <- FALSE
    if (verb_name %in% c("summarise", "summarize")) {
      drops <- identical(verb$.groups, "drop")
      ends_grouping <- drops || length(groups) == 1
      verb$.groups <- NULL
    }
    verb$.by <- if (length(groups) == 1) groups[[1]] else as.call(c(as.name("c"), groups))
    after <- if (i + 2 <= length(steps)) steps[[i + 2]]
    if (identical(step_name(after), "ungroup")) {
      steps[[i + 2]] <- NULL
    } else if (!ends_grouping && !is.null(after)) {
      fail(sprintf("the grouping must end after %s(), with ungroup()", verb_name))
    }
    steps[[i + 1]] <- verb
    steps[[i]] <- NULL
  }
  paste(c(deparse_one(chain$base), vapply(steps, deparse_one, character(1))), collapse = " |> ")
}

# A pipeline as its first input and its steps, each step a call without its
# first argument.
flatten_pipe <- function(expr) {
  steps <- list()
  while (is.call(expr) && length(expr) >= 2 && !nzchar(names(expr)[2] %||% "")) {
    steps <- c(list(expr[-2]), steps)
    expr <- expr[[2]]
  }
  list(base = expr, steps = steps)
}

step_name <- function(step) {
  if (is.call(step) && is.name(step[[1]])) as.character(step[[1]]) else NA_character_
}

deparse_one <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}
