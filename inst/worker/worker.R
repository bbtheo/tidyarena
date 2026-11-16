# tidyarena worker: runs one task on one backend at one size.
#
# Started by tidyarena in a fresh R process as `Rscript worker.R spec.rds`.
# Uses base R only, so that nothing tidyarena depends on is loaded next to
# the packages being measured; nanoparquet reads the tables and is unloaded
# again. Writes its result to `spec$out` after setup and after every
# repetition, so a kill loses at most the repetition that was running.

local({
  spec <- readRDS(commandArgs(trailingOnly = TRUE)[[1]])
  .libPaths(spec$libpath)

  linux <- file.exists("/proc/self/status")
  `%||%` <- function(x, y) if (is.null(x)) y else x

  status_bytes <- function(field) {
    if (!linux) {
      return(NA_real_)
    }
    line <- grep(paste0("^", field, ":"), readLines("/proc/self/status"), value = TRUE)
    as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", line)) * 1024
  }

  # Writing 5 to clear_refs resets the peak RSS (VmHWM) to the current RSS.
  reset_peak <- function() {
    linux && isTRUE(tryCatch(
      {
        writeLines("5", "/proc/self/clear_refs")
        TRUE
      },
      error = function(e) FALSE
    ))
  }

  result <- list(
    status = "setup",
    setup_seconds = NA_real_,
    reps = list(),
    conditions = character(),
    error = NA_character_,
    fingerprint = NULL,
    versions = NULL,
    peak_method = if (linux) "vmhwm" else "poll"
  )
  save_result <- function() {
    tmp <- paste0(spec$out, ".tmp")
    saveRDS(result, tmp)
    file.rename(tmp, spec$out)
  }

  conditions <- character()
  capture <- function(expr) {
    withCallingHandlers(
      expr,
      message = function(m) {
        conditions <<- c(conditions, paste("message:", conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        conditions <<- c(conditions, paste("warning:", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
  }

  versions <- function() {
    namespaces <- loadedNamespaces()
    vapply(
      namespaces,
      function(p) as.character(utils::packageVersion(p)),
      character(1)
    )
  }

  fail <- function(status, e) {
    result$status <<- status
    result$error <<- conditionMessage(e)
    result$conditions <<- conditions
    result$versions <<- versions()
    save_result()
    quit(save = "no", status = 0)
  }

  # The tables are Parquet files, read into plain data frames. nanoparquet
  # is unloaded again so that only the backend's packages stay loaded.
  read_table <- function(path) {
    reader <- loadNamespace("nanoparquet", lib.loc = spec$reader_lib)
    table <- reader$read_parquet(path)
    class(table) <- "data.frame"
    table
  }
  # A backend that reads the Parquet files itself (`spec$source` is
  # "parquet") gets their paths instead.
  parquet <- identical(spec$source, "parquet")
  env <- new.env(parent = globalenv())
  if (!parquet) {
    for (name in names(spec$data)) {
      env[[name]] <- read_table(spec$data[[name]])
    }
    try(unloadNamespace("nanoparquet"), silent = TRUE)
    gc()
  }
  env$.budget_mb <- spec$budget_mb

  t0 <- Sys.time()
  # The backend's setup, then its conversion of every table into its own
  # form, or its reading of every Parquet file.
  tryCatch(
    capture({
      eval(parse(text = spec$setup), envir = env)
      for (name in names(spec$data)) {
        env[[name]] <- if (parquet) {
          env$.read(spec$data[[name]], name)
        } else {
          env$.convert(env[[name]], name)
        }
      }
      # Packages the task's code uses, attached as a user would.
      for (package in spec$packages) {
        library(package, character.only = TRUE)
      }
    }),
    error = function(e) fail("setup_error", e)
  )
  result$setup_seconds <- as.double(Sys.time() - t0, units = "secs")
  save_result()
  file.create(paste0(spec$out, ".setup-done"))

  # Messages from setup (package start-up notes) are kept apart from the
  # task's, so that only the task's are checked for fallbacks.
  result$setup_conditions <- conditions
  conditions <- character()

  task <- parse(text = spec$code)

  fingerprint <- function(x) {
    x <- as.data.frame(x)
    cols <- lapply(x, function(v) {
      type <- class(v)[[1]]
      if (is.factor(v)) v <- as.character(v)
      if (inherits(v, "integer64")) v <- as.numeric(as.character(v))
      if (inherits(v, c("Date", "POSIXt", "difftime"))) v <- as.numeric(v)
      if (is.logical(v)) v <- as.integer(v)
      na <- sum(is.na(v))
      w <- v[!is.na(v)]
      if (is.numeric(v)) {
        list(
          kind = "numeric",
          type = type,
          na = na,
          sum = sum(as.double(w)),
          sumsq = sum(as.double(w)^2),
          min = if (length(w)) min(w) else NA_real_,
          max = if (length(w)) max(w) else NA_real_
        )
      } else if (is.character(v)) {
        u <- sort(unique(w), method = "radix")
        list(
          kind = "character",
          type = type,
          na = na,
          n_distinct = length(u),
          nchar = sum(as.double(nchar(w))),
          hash = hash_lines(u)
        )
      } else {
        list(kind = "other", type = type, na = na)
      }
    })
    out <- list(nrow = nrow(x), names = names(x), cols = cols)
    if (!is.na(spec$order_key) && spec$order_key %in% names(x)) {
      out$order_hash <- hash_lines(as.character(x[[spec$order_key]]))
    }
    out
  }

  hash_lines <- function(x) {
    path <- tempfile()
    on.exit(unlink(path))
    writeLines(x, path, useBytes = TRUE)
    unname(tools::md5sum(path))
  }

  # Reads every value of a collected result, so that columns a backend
  # converts only when they are first read (ALTREP) are converted inside the
  # timing.
  read_values <- function(x) {
    for (col in x) {
      if (is.character(col)) {
        nchar(col, type = "bytes")
      } else if (is.list(col)) {
        lengths(col)
      } else if (is.factor(col)) {
        as.integer(col)
      } else {
        unclass(col) * 1
      }
    }
    invisible(x)
  }

  # Runs the task once, in its own environment so that a task with several
  # statements leaves nothing behind, and passes the result to `finish`.
  # Returns the result, its time and its peak memory.
  measure <- function(finish) {
    gc(full = TRUE)
    base_rss <- status_bytes("VmRSS")
    peak_reset <- reset_peak()
    t0 <- Sys.time()
    out <- capture(finish(eval(task, envir = new.env(parent = env))))
    seconds <- as.double(Sys.time() - t0, units = "secs")
    peak_rss <- if (peak_reset) status_bytes("VmHWM") else NA_real_
    list(out = out, seconds = seconds, base_rss = base_rss, peak_rss = peak_rss)
  }

  # Two measurements per repetition, each from the data: "compute", the
  # result computed where the backend keeps it (an Arrow table, a DuckDB
  # table, GPU memory), and "collect", the result as R values in this
  # process. The second is what correctness is checked on.
  result$status <- "running"
  for (rep in seq_len(spec$reps + 1L) - 1L) {
    computed <- tryCatch(
      measure(env$.compute),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(computed$error)) {
      result$compute_error <- computed$error
    } else if (exists(".release", envir = env, inherits = FALSE)) {
      try(capture(env$.release(computed$out)), silent = TRUE)
    }
    computed$out <- NULL
    collected <- tryCatch(
      measure(function(x) read_values(env$.collect(x))),
      error = function(e) fail("error", e)
    )
    result$reps[[rep + 1L]] <- list(
      rep = rep,
      warmup = rep == 0L,
      seconds = collected$seconds,
      base_rss = collected$base_rss,
      peak_rss = collected$peak_rss,
      compute_seconds = computed$seconds %||% NA_real_,
      compute_base_rss = computed$base_rss %||% NA_real_,
      compute_peak_rss = computed$peak_rss %||% NA_real_
    )
    if (rep == spec$reps) {
      result$fingerprint <- fingerprint(collected$out)
    }
    rm(collected, computed)
    result$conditions <- conditions
    save_result()
  }

  if (!is.null(spec$teardown)) {
    try(eval(parse(text = spec$teardown), envir = env), silent = TRUE)
  }

  result$versions <- versions()
  result$status <- "ok"
  save_result()
})
