# duckplyr: dplyr verbs run by DuckDB, falling back to dplyr where it can't.
list(
  packages = c("dplyr", "duckplyr"),
  mem_factor = 2.5,
  fallback_pattern = "falling back|fallback",
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "duckplyr::as_duckdb_tibble(x)",
  setup = c(
    # Report fallbacks as messages, and never upload them.
    "Sys.setenv(DUCKPLYR_FALLBACK_INFO = 'TRUE', DUCKPLYR_FALLBACK_COLLECT = '0')",
    "library(dplyr)",
    "duckplyr::db_exec(sprintf(\"SET memory_limit = '%dMB'\", .budget_mb))",
    # compute() writes a temporary table; .release() drops it again, outside
    # the timing, so that repetitions don't pile up tables in memory.
    ".computed <- 0L",
    ".compute <- function(x) {",
    "  .computed <<- .computed + 1L",
    "  dplyr::compute(x, name = paste0('tidyarena_', .computed))",
    "}",
    ".release <- function(x) duckplyr::db_exec(sprintf('DROP TABLE IF EXISTS tidyarena_%d', .computed))"
  )
)
