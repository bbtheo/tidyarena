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
#'   * `idiom`: for grouped tasks, how the grouping is written: `"group_by"`
#'     or `".by"`. Every grouped task exists in both forms (the `.by` one
#'     has the suffix `_by`), because some backends run only one of them.
#'     `NA` for tasks without grouping.
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
    mem_factor = vapply(tasks, `[[`, numeric(1), "mem_factor"),
    idiom = vapply(tasks, function(x) x$idiom %||% NA_character_, character(1))
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
      idiom = "group_by",
      mem_factor = 0.3
    ),
    summarise_highcard = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_high) |>',
        'summarise(n = n(), mean_x = mean(x), max_y = max(y), .groups = "drop")'
      ),
      idiom = "group_by",
      mem_factor = 0.5
    ),
    summarise_skew = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_skew) |>',
        'summarise(n = n(), sum_x = sum(x), .groups = "drop")'
      ),
      idiom = "group_by",
      mem_factor = 0.3
    ),
    summarise_na = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_low) |>',
        'summarise(mean_z = mean(z, na.rm = TRUE), n_na = sum(is.na(z)),',
        '.groups = "drop")'
      ),
      idiom = "group_by",
      mem_factor = 0.3
    ),
    summarise_across = list(
      family = "summarise",
      code = paste(
        'df |> group_by(g_low) |>',
        'summarise(across(c(x, y), list(mean = mean, sd = sd)), .groups = "drop")'
      ),
      idiom = "group_by",
      mem_factor = 0.3
    ),
    join_summarise = list(
      family = "join",
      code = paste(
        'df |> inner_join(lookup, by = "g_high") |> group_by(region) |>',
        'summarise(total = sum(x * weight), .groups = "drop")'
      ),
      idiom = "group_by",
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
      idiom = "group_by",
      mem_factor = 1.5
    ),
    window_top_n = list(
      family = "window",
      code = paste(
        'df |> group_by(g_low) |> filter(min_rank(desc(x)) <= 10) |>',
        'ungroup() |> select(g_low, id, x)'
      ),
      idiom = "group_by",
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
    ),

    # The grouped tasks again, grouped with `.by` instead of group_by().
    summarise_lowcard_by = list(
      family = "summarise",
      code = paste(
        "df |>",
        "summarise(n = n(), mean_x = mean(x), sum_y = sum(y), .by = g_low)"
      ),
      idiom = ".by",
      mem_factor = 0.3
    ),
    summarise_highcard_by = list(
      family = "summarise",
      code = paste(
        "df |>",
        "summarise(n = n(), mean_x = mean(x), max_y = max(y), .by = g_high)"
      ),
      idiom = ".by",
      mem_factor = 0.5
    ),
    summarise_skew_by = list(
      family = "summarise",
      code = "df |> summarise(n = n(), sum_x = sum(x), .by = g_skew)",
      idiom = ".by",
      mem_factor = 0.3
    ),
    summarise_na_by = list(
      family = "summarise",
      code = paste(
        "df |>",
        "summarise(mean_z = mean(z, na.rm = TRUE), n_na = sum(is.na(z)), .by = g_low)"
      ),
      idiom = ".by",
      mem_factor = 0.3
    ),
    summarise_across_by = list(
      family = "summarise",
      code = paste(
        "df |>",
        "summarise(across(c(x, y), list(mean = mean, sd = sd)), .by = g_low)"
      ),
      idiom = ".by",
      mem_factor = 0.3
    ),
    join_summarise_by = list(
      family = "join",
      code = paste(
        'df |> inner_join(lookup, by = "g_high") |>',
        "summarise(total = sum(x * weight), .by = region)"
      ),
      idiom = ".by",
      mem_factor = 1.5
    ),
    window_lag_by = list(
      family = "window",
      code = paste(
        "df |> mutate(dx = x - lag(x, order_by = id), .by = g_low) |>",
        "summarise(s = sum(dx, na.rm = TRUE))"
      ),
      idiom = ".by",
      mem_factor = 1.5
    ),
    window_top_n_by = list(
      family = "window",
      code = paste(
        "df |> filter(min_rank(desc(x)) <= 10, .by = g_low) |>",
        "select(g_low, id, x)"
      ),
      idiom = ".by",
      mem_factor = 1.5
    )
  )
}
