#' The benchmark tasks
#'
#' Every task is one dplyr pipeline, written once and run unchanged on every
#' backend. A pipeline sees two tables: `df`, the main table, and `lookup`, a
#' small dimension table keyed by `g_high`. See [arena_data()] for the
#' columns.
#'
#' @returns A tibble with one row per task:
#'   * `task`: task id.
#'   * `family`: the kind of operation, used to group results in reports.
#'   * `code`: the pipeline, as R code.
#'   * `order_key`: for tasks whose row order is part of the answer, the
#'     column whose sequence is checked; `NA` otherwise.
#'   * `mem_factor`: rough size of the intermediate data relative to `df`,
#'     used by [arena_plan()] to estimate peak memory.
#' @export
#' @examples
#' arena_tasks()
arena_tasks <- function() {
  tasks <- task_defs()
  tibble::tibble(
    task = names(tasks),
    family = vapply(tasks, `[[`, character(1), "family"),
    code = vapply(tasks, `[[`, character(1), "code"),
    order_key = vapply(
      tasks,
      function(x) x$order_key %||% NA_character_,
      character(1)
    ),
    mem_factor = vapply(tasks, `[[`, numeric(1), "mem_factor")
  )
}

task_defs <- function() {
  list(
    filter_rows = list(
      family = "filter",
      code = 'df |> filter(x > 0, g_low %in% c("k01", "k02", "k03"))',
      mem_factor = 0.3
    ),
    mutate_arith = list(
      family = "mutate",
      code = paste(
        'df |>',
        'mutate(r = x * y + z, sign = if_else(x > 0, "pos", "neg")) |>',
        'select(id, r, sign)'
      ),
      mem_factor = 1
    ),
    case_when_count = list(
      family = "mutate",
      code = paste(
        'df |>',
        'mutate(band = case_when(x < -1 ~ "low", x < 1 ~ "mid", TRUE ~ "high")) |>',
        'count(band)'
      ),
      mem_factor = 0.5
    ),
    summarise_lowcard = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_low) |>',
        'summarise(n = n(), mean_x = mean(x), sum_y = sum(y), .groups = "drop")'
      ),
      mem_factor = 0.3
    ),
    summarise_highcard = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_high) |>',
        'summarise(n = n(), mean_x = mean(x), max_y = max(y), .groups = "drop")'
      ),
      mem_factor = 0.5
    ),
    summarise_skew = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_skew) |>',
        'summarise(n = n(), sum_x = sum(x), .groups = "drop")'
      ),
      mem_factor = 0.3
    ),
    summarise_na = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_low) |>',
        'summarise(mean_z = mean(z, na.rm = TRUE), n_na = sum(is.na(z)),',
        '.groups = "drop")'
      ),
      mem_factor = 0.3
    ),
    summarise_across = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_low) |>',
        'summarise(across(c(x, y), list(mean = mean, sd = sd)), .groups = "drop")'
      ),
      mem_factor = 0.3
    ),
    join_summarise = list(
      family = "join",
      code = paste(
        'df |> inner_join(lookup, by = "g_high") |> group_by(region) |>',
        'summarise(total = sum(x * weight), .groups = "drop")'
      ),
      mem_factor = 1.5
    ),
    join_left_wide = list(
      family = "join",
      code = 'df |> left_join(lookup, by = "g_high") |> select(id, x, region)',
      mem_factor = 1.5
    ),
    window_lag = list(
      family = "window",
      code = paste(
        'df |> group_by(g_low) |> mutate(dx = x - lag(x, order_by = id)) |>',
        'ungroup() |> summarise(s = sum(dx, na.rm = TRUE))'
      ),
      mem_factor = 1.5
    ),
    window_top_n = list(
      family = "window",
      code = paste(
        'df |> group_by(g_low) |> filter(min_rank(desc(x)) <= 10) |>',
        'ungroup() |> select(g_low, id, x)'
      ),
      mem_factor = 1.5
    ),
    arrange_two = list(
      family = "arrange",
      code = 'df |> arrange(g_low, desc(x)) |> select(id, g_low, x)',
      order_key = "id",
      mem_factor = 1.2
    ),
    distinct_pairs = list(
      family = "distinct",
      code = "df |> distinct(g_low, g_skew)",
      mem_factor = 0.5
    ),
    string_detect = list(
      family = "string",
      code = 'df |> filter(grepl("7", s, fixed = TRUE)) |> count(g_low)',
      mem_factor = 0.5
    )
  )
}
