#' The benchmark tasks
#'
#' Every task is one dplyr pipeline, written once and run unchanged on every
#' backend. A pipeline sees two tables: `df`, the main table, and `lookup`, a
#' small dimension table keyed by `g_high`. See [arena_data()] for the
#' columns. Each task is a file in the package's `tasks` folder; see
#' `vignette("extending")` to add one.
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
#'     `NA` for tasks without grouping. The `.by` forms are written by
#'     tidyarena from the `group_by()` ones.
#'   * `hash`: a hash of `code`. A task id always means the same code:
#'     results with different hashes are never compared.
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
    idiom = vapply(tasks, function(x) x$idiom %||% NA_character_, character(1)),
    hash = vapply(tasks, function(x) task_hash(x$code), character(1))
  )
}
