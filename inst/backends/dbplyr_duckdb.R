# dbplyr: dplyr code translated to SQL, run by an in-process DuckDB.
list(
  packages = c("dplyr", "dbplyr", "duckdb", "DBI"),
  setup = c(
    "library(dplyr)",
    ".con <- DBI::dbConnect(duckdb::duckdb())",
    "DBI::dbExecute(.con, sprintf(\"SET memory_limit = '%dMB'\", .budget_mb))",
    "DBI::dbWriteTable(.con, 'df', df)",
    "DBI::dbWriteTable(.con, 'lookup', lookup)",
    "df <- dplyr::tbl(.con, 'df')",
    "lookup <- dplyr::tbl(.con, 'lookup')"
  ),
  teardown = "DBI::dbDisconnect(.con, shutdown = TRUE)"
)
