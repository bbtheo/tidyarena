# arena_plan() rejects unknown ids

    Code
      arena_plan("quick", tasks = "nope")
    Condition
      Error in `arena_plan()`:
      ! `tasks` has unknown id: "nope".
      i Known ids: "arrange_two", "case_when_count", "distinct_pairs", "filter_rows", "join_left_wide", "join_summarise", "mutate_arith", "string_detect", "summarise_across", "summarise_highcard", "summarise_lowcard", "summarise_na", "summarise_skew", "window_lag", "window_top_n", "join_summarise_by", "summarise_across_by", "summarise_highcard_by", ..., "window_lag_by", and "window_top_n_by".

