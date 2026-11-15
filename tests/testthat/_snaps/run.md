# resuming refuses changed settings

    Code
      arena_run(dir = dir, tasks = "filter_rows")
    Condition
      Error in `arena_run()`:
      ! Can't change `tasks` when resuming a run.
      i Resume with the same `dir` only, or start a new run.

# resuming refuses tasks whose code changed

    Code
      arena_run(dir = res$dir)
    Condition
      Error in `arena_run()`:
      ! Can't resume: the code of 1 task changed since the run started: "filter_rows".
      i Start a new run.

