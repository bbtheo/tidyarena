# arena_plan() rejects unknown ids

    Code
      arena_plan("quick", tasks = "nope")
    Condition
      Error in `arena_plan()`:
      ! `tasks` has unknown id: "nope".
      i Known ids: "arrange_two", "case_when_count", "datetime_add_days", "datetime_add_month", "datetime_add_month_rollback", "datetime_ceiling_month", "datetime_duration", "datetime_filter_range", "datetime_floor_day", "datetime_floor_hour", "datetime_floor_month", "datetime_floor_week", "datetime_parse", "datetime_parts", "datetime_timezone", "dbbench_groupby_q01", "dbbench_groupby_q02", "dbbench_groupby_q03", ..., "window_lag_by", and "window_top_n_by".

