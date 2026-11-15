# data sets refuse sizes they don't have

    Code
      arena_data(1e+06, withr::local_tempdir(), "dbbench_join")
    Condition
      Error in `arena_data()`:
      ! The "dbbench_join" data set needs at least 1e7 rows.

---

    Code
      arena_data(1e+06, withr::local_tempdir(), "nope")
    Condition
      Error in `arena_data()`:
      ! `dataset` has unknown id: "nope".
      i Known ids: "arena", "dbbench_groupby", "dbbench_join", "joins", "strings", "datetimes", and "tpch".

