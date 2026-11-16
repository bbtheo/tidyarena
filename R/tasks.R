#' The benchmark tasks
#'
#' Every task is one dplyr pipeline, written once and run unchanged on every
#' backend, over the tables of one data set (see [arena_data()]): tidyarena's
#' own `df` and `lookup`, db-benchmark's, or the TPC-H schema. Each task is a file in the package's `tasks` folder; see
#' `vignette("extending")` to add one.
#'
#' @returns A tibble with one row per task:
#'   * `task`: task id.
#'   * `family`: the kind of operation, used to group results in reports.
#'     The tasks from db-benchmark have the families `"dbbench-groupby"` and
#'     `"dbbench-join"`, the ones derived from TPC-H `"tpch"`.
#'   * `part`: which of the four parts of the suite the task is in, 1 to 4.
#'     The `"xs"`, `"s"` and `"m"` tiers of [arena_plan()] run one or two
#'     parts. The parts have similar run times and a similar mix of data
#'     sets; a `.by` form is in the part of its `group_by()` form.
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
#'   * `data`: the data set the task runs on.
#'   * `packages`: packages the task's code uses besides those of the
#'     backend, such as lubridate.
#'   * `hash`: a hash of `code`. A task id always means the same code:
#'     results with different hashes are never compared.
#' @export
#' @examples
#' arena_tasks()
arena_tasks <- function() {
  tasks <- unname_list(task_defs())
  tibble::tibble(
    task = attr(tasks, "ids"),
    family = vapply(tasks, `[[`, character(1), "family"),
    part = vapply(tasks, `[[`, integer(1), "part"),
    code = vapply(tasks, `[[`, character(1), "code"),
    order_key = vapply(
      tasks,
      function(x) x$order_key %||% NA_character_,
      character(1)
    ),
    mem_factor = vapply(tasks, `[[`, numeric(1), "mem_factor"),
    idiom = vapply(tasks, function(x) x$idiom %||% NA_character_, character(1)),
    data = vapply(tasks, `[[`, character(1), "data"),
    packages = unname(lapply(tasks, function(x) x$packages %||% character())),
    hash = vapply(tasks, function(x) task_hash(x$code), character(1))
  )
}

# The task list without names, so that the columns built from it have none.
unname_list <- function(tasks) {
  ids <- names(tasks)
  structure(unname(tasks), ids = ids)
}
