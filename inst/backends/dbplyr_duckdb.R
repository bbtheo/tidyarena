# dbplyr: dplyr code translated to SQL, run by an in-process DuckDB.
list(
  packages = c("dplyr", "dbplyr", "duckdb", "DBI"),
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "{ DBI::dbWriteTable(.con, name, x); dplyr::tbl(.con, name) }",
  # Reads the Parquet file at `path` itself: a view, so every query scans
  # the file. Parquet stores R's UTC times as TIMESTAMP WITH TIME ZONE, whose
  # functions need DuckDB's icu extension; the view casts them to TIMESTAMP,
  # the type DuckDB gives the same columns written from R.
  read = paste(
    "{ .cols <- DBI::dbGetQuery(.con, sprintf(\"DESCRIBE SELECT * FROM read_parquet('%s')\", path));",
    ".tz <- .cols$column_name[.cols$column_type == 'TIMESTAMP WITH TIME ZONE'];",
    ".select <- if (length(.tz) > 0) sprintf('* REPLACE (%s)', paste(sprintf('\"%s\"::TIMESTAMP AS \"%s\"', .tz, .tz), collapse = ', ')) else '*';",
    "DBI::dbExecute(.con, sprintf(\"CREATE VIEW %s AS SELECT %s FROM read_parquet('%s')\", name, .select, path));",
    "dplyr::tbl(.con, name) }"
  ),
  setup = c(
    "library(dplyr)",
    ".con <- DBI::dbConnect(duckdb::duckdb())",
    "DBI::dbExecute(.con, sprintf(\"SET memory_limit = '%dMB'\", .budget_mb))",
    # compute() writes a temporary table; .release() drops it again, outside
    # the timing, so that repetitions don't pile up tables in memory.
    ".computed <- 0L",
    ".compute <- function(x) {",
    "  .computed <<- .computed + 1L",
    "  dplyr::compute(x, name = paste0('tidyarena_', .computed))",
    "}",
    ".release <- function(x) DBI::dbExecute(.con, sprintf('DROP TABLE IF EXISTS tidyarena_%d', .computed))"
  ),
  teardown = "DBI::dbDisconnect(.con, shutdown = TRUE)"
)
