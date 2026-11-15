# arena_plan() rejects unknown ids

    Code
      arena_plan("quick", tasks = "nope")
    Condition
      Error in `arena_plan()`:
      ! `tasks` has unknown id: "nope".
      i Known ids: "filter_rows", "mutate_arith", "case_when_count", "summarise_lowcard", "summarise_highcard", "summarise_skew", "summarise_na", "summarise_across", "join_summarise", "join_left_wide", "window_lag", "window_top_n", "arrange_two", "distinct_pairs", "string_detect", "summarise_lowcard_by", "summarise_highcard_by", "summarise_skew_by", ..., "window_lag_by", and "window_top_n_by".

