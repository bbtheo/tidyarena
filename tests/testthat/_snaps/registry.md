# definition files are validated

    Code
      check_def(list(code = "df"), "bad", "tasks", task_fields)
    Condition
      Error:
      ! 'inst/tasks/bad.R' is missing family.
    Code
      check_def(list(family = "f", code = "df", memfactor = 1), "bad", "tasks",
      task_fields)
    Condition
      Error:
      ! 'inst/tasks/bad.R' has unknown field memfactor.
      i Known fields: family, code, order_key, mem_factor, and twin.
    Code
      check_def("df", "bad", "tasks", task_fields)
    Condition
      Error:
      ! 'inst/tasks/bad.R' must end with a named list.

# by_twin() refuses pipelines it can't rewrite

    Code
      by_twin("df |> group_by(g) |> mutate(y = x) |> filter(y > 0)", "keeps_grouping")
    Condition
      Error:
      ! Can't write the `.by` form of task "keeps_grouping": the grouping must end after mutate(), with ungroup().
      i Set `twin = FALSE` in its file if it has no `.by` form.
    Code
      by_twin("df |> group_by(g) |> arrange(x)", "no_by_verb")
    Condition
      Error:
      ! Can't write the `.by` form of task "no_by_verb": group_by() must be followed by one of summarise(), summarize(), mutate(), filter(), reframe(), slice_min(), slice_max().
      i Set `twin = FALSE` in its file if it has no `.by` form.
    Code
      by_twin("df |> group_by(g, .add = TRUE) |> summarise(n = n())", "named")
    Condition
      Error:
      ! Can't write the `.by` form of task "named": group_by() must list only grouping columns.
      i Set `twin = FALSE` in its file if it has no `.by` form.
    Code
      by_twin("df |> group_by(a, b) |> summarise(n = n()) |> mutate(m = n)",
        "partial_drop")
    Condition
      Error:
      ! Can't write the `.by` form of task "partial_drop": the grouping must end after summarise(), with ungroup().
      i Set `twin = FALSE` in its file if it has no `.by` form.

