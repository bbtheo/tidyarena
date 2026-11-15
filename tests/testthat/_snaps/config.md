# arena_config() starts from the defaults and saves changes

    Code
      arena_config(exclude = c("cuplyr", "dtplyr"), build = "capped")
    Message
      * Only CRAN: FALSE
      * Left out: "cuplyr" and "dtplyr"
      * Packages without binaries: "capped"

# arena_config() checks its arguments

    Code
      arena_config(exclude = "dplyr")
    Condition
      Error in `arena_config()`:
      ! `exclude` can't include "dplyr": it is the reference every backend is compared with.
    Code
      arena_config(exclude = "nope")
    Condition
      Error in `arena_config()`:
      ! `exclude` has unknown id: "nope".
      i Known ids: "dplyr", "arrow", "cuplyr", "dbplyr_duckdb", "dtplyr", "duckplyr", and "tidypolars".
    Code
      arena_config(build = "always")
    Condition
      Error in `arena_config()`:
      ! `build` must be one of "never" or "capped", not "always".
    Code
      arena_config(only_cran = "yes")
    Condition
      Error in `arena_config()`:
      ! `only_cran` must be `TRUE` or `FALSE`, not the string "yes".

