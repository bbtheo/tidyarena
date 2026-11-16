#' Summarise a run
#'
#' Reports what runs fast on this machine: for every task, the fastest
#' backend and its speed-up over dplyr, and for every backend its coverage
#' (share of tasks it ran correctly) and its typical time and memory relative
#' to dplyr. Settings that can distort the results are listed first.
#'
#' Ratios are relative to dplyr on the same task and size, so they are
#' comparable across machines. Time and memory ratios are geometric means
#' over the cells where both the backend and dplyr ran correctly.
#'
#' @param x An `arena_results` object from [arena_run()] or [arena_read()],
#'   or a run directory.
#' @returns A list of two tibbles, invisibly: `cells`, every cell with its
#'   ratios to dplyr, and `backends`, one row per backend.
#' @export
#' @examples
#' \dontrun{
#' arena_report(arena_run("xs"))
#' }
arena_report <- function(x) {
  if (is.character(x)) {
    x <- arena_read(x)
  }
  if (!inherits(x, "arena_results")) {
    cli::cli_abort("{.arg x} must be an {.cls arena_results} object or a run directory.")
  }
  cells <- add_ratios(x$cells)
  backends <- backend_summary(cells)

  cli::cli_h1("tidyarena run {.val {x$run$run_id}}")
  cli::cli_text(machine_line(x$run$env))
  n_tasks <- length(x$run$tasks)
  if (n_tasks < nrow(arena_tasks())) {
    cli::cli_text("{n_tasks} of the {nrow(arena_tasks())} tasks.")
  }
  notes <- setting_notes(x$run$env)
  if (length(notes) > 0) {
    cli::cli_alert_warning("Settings that can distort these results:")
    cli::cli_bullets(notes)
  }

  diagnosed <- diagnostic_lines(x$run$diagnostics)
  if (length(diagnosed) > 0) {
    cli::cli_alert_warning("Backends that did not load, and what their own checks found:")
    cli::cli_bullets(diagnosed)
  }

  cli::cli_h2("Fastest backend per task")
  cli::cli_text(
    "At the largest size each task ran on, with the speed-up over dplyr. In R: the result collected into R, every value read. Compute: the result computed where the backend keeps it."
  )
  print_lines(fastest_table(cells))

  cli::cli_h2("Backends")
  cli::cli_text(
    "Coverage: share of cells run correctly. Time and memory: geometric mean ratio to dplyr (lower is better), with the result in R and computed in the backend."
  )
  print_lines(format_backends(backends))

  sources <- source_summary(cells)
  if (nrow(sources) > 0) {
    cli::cli_h2("Data in R or read from Parquet")
    cli::cli_text(
      "Each Parquet variant, which reads the tables from their Parquet files, against the same backend given data frames: geometric mean time ratio (below 1, Parquet is faster), over the cells both ran correctly. Setup, untimed, is the median time to load the data."
    )
    print_lines(format_source_summary(sources))
  }

  by_data <- dataset_summary(cells)
  if (length(unique(by_data$data)) > 1) {
    cli::cli_h2("Backends by data set")
    cli::cli_text(
      "Time in R relative to dplyr, with the share of cells run correctly in brackets."
    )
    print_lines(format_dataset_summary(by_data))
  }

  problems <- cells[!cells$status %in% c("ok", "skipped") | cells$correct %in% FALSE, ]
  if (nrow(problems) > 0) {
    cli::cli_h2("Cells that did not run cleanly")
    print_lines(format_problems(problems))
  }

  invisible(list(cells = cells, backends = backends))
}

add_ratios <- function(cells) {
  anchor <- cells[cells$backend == anchor_backend & cells$status == "ok", ]
  key <- paste(cells$task, cells$size)
  anchor_key <- paste(anchor$task, anchor$size)
  memory <- function(d) ifelse(is.na(d$peak_rss_delta), d$peak_rss, d$peak_rss_delta)
  compute_memory <- function(d) {
    ifelse(is.na(d$compute_peak_rss_delta), d$compute_peak_rss, d$compute_peak_rss_delta)
  }
  idx <- match(key, anchor_key)
  usable <- cells$status %in% c("ok", "fallback") & cells$correct %in% TRUE
  cells$time_ratio <- ifelse(usable, cells$seconds / anchor$seconds[idx], NA_real_)
  cells$mem_ratio <- ifelse(usable, memory(cells) / memory(anchor)[idx], NA_real_)
  cells$compute_ratio <- ifelse(usable, cells$compute_seconds / anchor$compute_seconds[idx], NA_real_)
  cells$compute_mem_ratio <- ifelse(
    usable,
    compute_memory(cells) / compute_memory(anchor)[idx],
    NA_real_
  )
  cells
}

backend_summary <- function(cells) {
  attempted <- cells[cells$status != "skipped" | cells$reason %in% "failed at a smaller size", ]
  rows <- lapply(split(attempted, attempted$backend), function(d) {
    good <- d$status == "ok" & d$correct %in% TRUE
    tibble::tibble(
      backend = d$backend[[1]],
      cells = nrow(d),
      coverage = mean(good),
      fallback = sum(d$status == "fallback"),
      wrong = sum(d$correct %in% FALSE),
      failed = sum(d$status %in% c(failure_statuses, "skipped")),
      time_ratio = geo_mean(d$time_ratio[good]),
      mem_ratio = geo_mean(d$mem_ratio[good]),
      compute_ratio = geo_mean(d$compute_ratio[good]),
      compute_mem_ratio = geo_mean(d$compute_mem_ratio[good])
    )
  })
  out <- do.call(rbind, rows)
  out[order(-out$coverage, out$time_ratio), ]
}

geo_mean <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (length(x) == 0) NA_real_ else exp(mean(log(x)))
}

fastest_table <- function(cells) {
  good <- cells[cells$status == "ok" & cells$correct %in% TRUE, ]
  if (nrow(good) == 0) {
    return("No cell ran correctly.")
  }
  rows <- lapply(split(good, good$task), function(d) {
    d <- d[d$size == max(d$size), ]
    anchor <- d[d$backend == anchor_backend, ]
    best <- function(col) {
      row <- d[which.min(d[[col]]), ]
      if (nrow(row) == 0) {
        return(c("-", "-"))
      }
      speed_up <- if (nrow(anchor)) sprintf("%.1fx", anchor[[col]] / row[[col]]) else "no dplyr"
      c(row$backend, speed_up)
    }
    in_r <- best("seconds")
    compute <- best("compute_seconds")
    data.frame(
      task = d$task[[1]],
      rows = format_n(d$size[[1]]),
      `in R` = in_r[[1]],
      ` ` = in_r[[2]],
      compute = compute[[1]],
      `  ` = compute[[2]],
      check.names = FALSE
    )
  })
  table <- do.call(rbind, rows)
  utils::capture.output(print(table, row.names = FALSE))
}

format_backends <- function(backends) {
  table <- data.frame(
    backend = backends$backend,
    coverage = sprintf("%3.0f%%", 100 * backends$coverage),
    `time R` = format_ratio(backends$time_ratio),
    `time compute` = format_ratio(backends$compute_ratio),
    `mem R` = format_ratio(backends$mem_ratio),
    `mem compute` = format_ratio(backends$compute_mem_ratio),
    fallback = backends$fallback,
    wrong = backends$wrong,
    failed = backends$failed,
    check.names = FALSE
  )
  utils::capture.output(print(table, row.names = FALSE))
}

format_problems <- function(problems) {
  what <- ifelse(
    problems$correct %in% FALSE,
    "wrong result",
    problems$status
  )
  detail <- ifelse(is.na(problems$error), "", paste0(": ", truncate(problems$error, 80)))
  sprintf(
    "%s / %s / %s: %s%s",
    problems$task,
    problems$backend,
    format_n(problems$size),
    what,
    gsub("\n", " ", detail)
  )
}

format_ratio <- function(x) {
  ifelse(is.na(x), "-", sprintf("%.2f", x))
}

print_lines <- function(lines) {
  cli::cli_verbatim(lines)
}

machine_line <- function(env) {
  gpu <- if (length(env$gpu) > 0) {
    paste0(", ", env$gpu[[1]]$name)
  } else {
    ""
  }
  sprintf(
    "%s, %s logical cores, %s RAM%s, %s, R %s",
    env$cpu$model %||% "unknown CPU",
    env$cpu$cores_logical %||% "?",
    format_bytes(env$memory$total %||% NA),
    gpu,
    env$os$name %||% env$os$family,
    env$r$version
  )
}

#' Settings in an environment snapshot that can distort benchmark results
#' @param env A snapshot from [arena_env()].
#' @returns A named character vector of notes, for [cli::cli_bullets()].
#' @noRd
setting_notes <- function(env) {
  notes <- character()
  # With intel_pstate and amd-pstate in active mode, "powersave" is the
  # normal governor and the energy preference is what matters.
  pstate <- grepl("pstate", env$cpu$freq_driver %||% "")
  if (identical(env$cpu$governor, "powersave") && !pstate) {
    notes <- c(notes, "The CPU frequency governor is {.val powersave}.")
  }
  if (pstate && (env$cpu$energy_preference %||% "") %in% c("power", "balance_power")) {
    notes <- c(
      notes,
      sprintf("The CPU energy preference is {.val %s}.", env$cpu$energy_preference)
    )
  }
  if (isTRUE(env$power$on_battery)) {
    notes <- c(notes, "The machine runs on battery.")
  }
  if (identical(env$power$profile, "power-saver")) {
    notes <- c(notes, "The power profile is {.val power-saver}.")
  }
  load <- env$load$loadavg_1m %||% NA
  cores <- env$cpu$cores_logical %||% NA
  if (!is.na(load) && !is.na(cores) && load > 0.25 * cores) {
    notes <- c(notes, sprintf("The machine is busy: load average %.1f on %d cores.", load, cores))
  }
  threads <- unlist(env$threads)
  if (length(threads) > 0) {
    notes <- c(
      notes,
      paste0(
        "Thread limits are set: ",
        paste0(names(threads), "=", threads, collapse = ", "),
        "."
      )
    )
  }
  names(notes) <- rep("!", length(notes))
  notes
}

format_bytes <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("?")
  }
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- max(1, min(length(units), floor(log(max(x, 1), 1024)) + 1))
  sprintf("%.1f %s", x / 1024^(i - 1), units[[i]])
}

# One bullet per failed check in the backends' own diagnostics, for the
# backends that didn't load.
diagnostic_lines <- function(diagnostics) {
  lines <- unlist(lapply(names(diagnostics), function(backend) {
    if (isTRUE(diagnostics[[backend]]$loaded)) {
      return(NULL)
    }
    checks <- failed_checks(diagnostics[[backend]])
    if (length(checks) > 0) paste0(backend, ": ", checks)
  }))
  stats::setNames(cli_escape(lines), rep("*", length(lines)))
}

# Every Parquet variant against its base backend, on the cells both ran
# correctly.
source_summary <- function(cells) {
  defs <- backend_defs()
  base <- vapply(cells$backend, function(b) defs[[b]]$base %||% b, character(1), USE.NAMES = FALSE)
  variants <- unique(cells$backend[cells$source == "parquet" & base %in% cells$backend])
  good <- cells$status == "ok" & cells$correct %in% TRUE
  rows <- lapply(variants, function(v) {
    b <- defs[[v]]$base
    p <- cells[cells$backend == v & good, ]
    m <- cells[cells$backend == b & good, ]
    i <- match(paste(p$task, p$size), paste(m$task, m$size))
    both <- !is.na(i)
    p <- p[both, ]
    m <- m[i[both], ]
    coverage <- function(backend) {
      attempted <- cells$backend == backend & cells$status != "skipped"
      if (any(attempted)) mean(good[attempted]) else NA_real_
    }
    tibble::tibble(
      backend = b,
      parquet_coverage = coverage(v),
      memory_coverage = coverage(b),
      cells = nrow(p),
      time_ratio = geo_mean(p$seconds / m$seconds),
      compute_ratio = geo_mean(p$compute_seconds / m$compute_seconds),
      setup_memory = median_or_na(m$setup_seconds),
      setup_parquet = median_or_na(p$setup_seconds)
    )
  })
  if (length(rows) == 0) {
    return(tibble::tibble())
  }
  do.call(rbind, rows)
}

format_source_summary <- function(sources) {
  coverage <- function(x) ifelse(is.na(x), "-", sprintf("%.0f%%", 100 * x))
  seconds <- function(x) ifelse(is.na(x), "-", sprintf("%.2fs", x))
  table <- data.frame(
    backend = sources$backend,
    cells = sources$cells,
    `time R` = format_ratio(sources$time_ratio),
    `time compute` = format_ratio(sources$compute_ratio),
    `coverage R` = coverage(sources$memory_coverage),
    `coverage Parquet` = coverage(sources$parquet_coverage),
    `setup R` = seconds(sources$setup_memory),
    `setup Parquet` = seconds(sources$setup_parquet),
    check.names = FALSE
  )
  utils::capture.output(print(table, row.names = FALSE))
}

# Coverage and time ratio of every backend on every data set.
dataset_summary <- function(cells) {
  tasks <- arena_tasks()
  cells$data <- tasks$data[match(cells$task, tasks$task)]
  attempted <- cells[cells$status != "skipped" | cells$reason %in% "failed at a smaller size", ]
  groups <- split(attempted, list(attempted$backend, attempted$data), drop = TRUE)
  rows <- lapply(groups, function(d) {
    good <- d$status == "ok" & d$correct %in% TRUE
    tibble::tibble(
      backend = d$backend[[1]],
      data = d$data[[1]],
      coverage = mean(good),
      time_ratio = geo_mean(d$time_ratio[good])
    )
  })
  do.call(rbind, rows)
}

format_dataset_summary <- function(by_data) {
  datasets <- intersect(names(dataset_defs()), unique(by_data$data))
  backends <- unique(by_data$backend)
  table <- data.frame(backend = backends)
  for (d in datasets) {
    rows <- by_data[by_data$data == d, ]
    i <- match(backends, rows$backend)
    table[[d]] <- ifelse(
      is.na(i),
      "-",
      sprintf("%s (%.0f%%)", format_ratio(rows$time_ratio[i]), 100 * rows$coverage[i])
    )
  }
  utils::capture.output(print(table, row.names = FALSE, right = FALSE))
}
