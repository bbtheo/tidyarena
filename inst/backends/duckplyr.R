# duckplyr: dplyr verbs run by DuckDB, falling back to dplyr where it can't.
list(
  packages = c("dplyr", "duckplyr"),
  mem_factor = 2.5,
  fallback_pattern = "falling back|fallback",
  setup = c(
    # Report fallbacks as messages, and never upload them.
    "Sys.setenv(DUCKPLYR_FALLBACK_INFO = 'TRUE', DUCKPLYR_FALLBACK_COLLECT = '0')",
    "library(dplyr)",
    "duckplyr::db_exec(sprintf(\"SET memory_limit = '%dMB'\", .budget_mb))",
    "df <- duckplyr::as_duckdb_tibble(df)",
    "lookup <- duckplyr::as_duckdb_tibble(lookup)"
  )
)
