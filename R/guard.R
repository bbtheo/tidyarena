#' Memory budget for a run
#'
#' The budget is the most memory one task may use. By default it is 70% of
#' the memory available when the run starts, while always leaving 2 GB free.
#'
#' @param fraction Share of available memory a task may use.
#' @param headroom Bytes that must stay free.
#' @returns The budget in bytes.
#' @noRd
default_budget <- function(fraction = 0.7, headroom = 2 * 2^30) {
  available <- ps::ps_system_memory()[["avail"]]
  max(0, min(available * fraction, available - headroom))
}

# How the hard memory cap is enforced: "cgroup" on Linux with systemd user
# scopes, otherwise "watchdog" only.
guard_method <- function() {
  if (!is.null(the$guard_method)) {
    return(the$guard_method)
  }
  method <- "watchdog"
  if (Sys.info()[["sysname"]] == "Linux" && nzchar(Sys.which("systemd-run"))) {
    ok <- tryCatch(
      processx::run(
        "systemd-run",
        c(systemd_args(2^30), "true"),
        error_on_status = FALSE,
        timeout = 10
      )$status ==
        0,
      error = function(e) FALSE
    )
    if (ok) method <- "cgroup"
  }
  the$guard_method <- method
  method
}

systemd_args <- function(budget) {
  c(
    "--user",
    "--scope",
    "--quiet",
    "--collect",
    "-p",
    paste0("MemoryMax=", format(ceiling(budget), scientific = FALSE)),
    "-p",
    "MemorySwapMax=0",
    "--"
  )
}

# Runs the worker on one spec and watches it. Returns the worker's result,
# with `status` set by the watchdog when it had to step in.
run_worker <- function(
  spec,
  budget,
  timeout,
  guard,
  log,
  setup_timeout = timeout,
  run_timeout = Inf
) {
  spec_path <- sub("\\.rds$", "-spec.rds", spec$out)
  saveRDS(spec, spec_path)
  rscript <- file.path(R.home("bin"), "Rscript")
  worker <- system.file("worker", "worker.R", package = "tidyarena")
  args <- c(worker, spec_path)
  if (guard == "cgroup") {
    command <- "systemd-run"
    args <- c(systemd_args(budget), rscript, args)
  } else {
    command <- rscript
  }

  p <- processx::process$new(
    command,
    args,
    stdout = log,
    stderr = "2>&1",
    wd = spec$tmpdir,
    env = c(
      "current",
      R_LIBS = paste(spec$libpath, collapse = .Platform$path.sep),
      TMPDIR = spec$tmpdir,
      TMP = spec$tmpdir,
      TEMP = spec$tmpdir,
      # Every machine runs in the same time zone. Engines that work in the
      # session's time zone (DuckDB with UTC timestamps read from Parquet)
      # would otherwise give results that depend on where the machine is.
      TZ = "UTC"
    ),
    cleanup_tree = TRUE
  )
  on.exit(if (p$is_alive()) p$kill_tree(), add = TRUE)

  watch <- watch_worker(
    p,
    budget,
    timeout,
    spec$tmpdir,
    setup_timeout = setup_timeout,
    setup_done = setup_marker(spec$out),
    run_timeout = run_timeout
  )
  p$wait()

  result <- if (file.exists(spec$out)) readRDS(spec$out) else list(reps = list())
  exit <- p$get_exit_status()
  if (!is.null(watch$reason)) {
    result$status <- watch$reason
  } else if (!result$status %in% c("ok", "fallback") && !grepl("error", result$status %||% "")) {
    # The worker died without saying why. A kill by signal 9 that the
    # watchdog did not send is the cgroup (or the kernel) running out of
    # memory.
    result$status <- if (exit %in% c(-9L, 137L)) "oom_killed" else "crashed"
    result$error <- result$error %||% paste("exit status", exit)
  }
  result$poll_peak_rss <- watch$peak_rss
  result$spill_bytes <- watch$spill_bytes
  result
}

# Stops the worker when it uses too much memory or time. Loading the data
# into the backend has a limit of its own, `setup_timeout`: a backend that
# can't load the data fails then, instead of using the time of the whole
# cell. The worker creates the file `setup_done` when setup is over. The
# time after that, running the pipeline, may take at most `run_timeout`
# seconds, or the cell is "too_slow".
watch_worker <- function(
  p,
  budget,
  timeout,
  tmpdir,
  interval = 0.1,
  setup_timeout = timeout,
  setup_done = NULL,
  run_timeout = Inf
) {
  start <- Sys.time()
  swap0 <- ps::ps_system_swap()[["used"]]
  swap_limit <- 512 * 2^20
  headroom <- 1 * 2^30
  peak <- 0
  spill <- 0
  last_spill_check <- start
  reason <- NULL
  handle <- p$as_ps_handle()
  setup_end <- NULL

  while (p$is_alive()) {
    rss <- tree_rss(handle)
    peak <- max(peak, rss)
    avail <- ps::ps_system_memory()[["avail"]]
    swap_growth <- ps::ps_system_swap()[["used"]] - swap0
    elapsed <- as.double(Sys.time() - start, units = "secs")
    if (is.null(setup_end) && !is.null(setup_done) && file.exists(setup_done)) {
      setup_end <- Sys.time()
    }
    running <- if (is.null(setup_end)) 0 else as.double(Sys.time() - setup_end, units = "secs")

    reason <- if (rss > budget) {
      "oom_guard"
    } else if (avail < headroom) {
      "oom_guard"
    } else if (swap_growth > swap_limit) {
      "swap_guard"
    } else if (elapsed > timeout) {
      "timeout"
    } else if (running > run_timeout) {
      "too_slow"
    } else if (elapsed > setup_timeout && !is.null(setup_done) && !file.exists(setup_done)) {
      "setup_timeout"
    }
    if (!is.null(reason)) {
      p$kill_tree()
      break
    }

    if (as.double(Sys.time() - last_spill_check, units = "secs") > 1) {
      spill <- max(spill, dir_bytes(tmpdir))
      last_spill_check <- Sys.time()
    }
    Sys.sleep(interval)
  }
  list(reason = reason, peak_rss = peak, spill_bytes = spill)
}

tree_rss <- function(handle) {
  procs <- c(
    list(handle),
    tryCatch(ps::ps_children(handle, recursive = TRUE), error = function(e) list())
  )
  sum(vapply(
    procs,
    function(h) {
      tryCatch(ps::ps_memory_info(h)[["rss"]], error = function(e) 0)
    },
    numeric(1)
  ))
}

dir_bytes <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  sum(file.size(files), na.rm = TRUE)
}

setup_marker <- function(out) {
  paste0(out, ".setup-done")
}
