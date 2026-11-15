#' Generate the benchmark data
#'
#' The tables are generated from a fixed seed with fixed random number
#' generators, so every machine runs on the same data. Generation runs in a
#' separate R process, so the memory it takes is released when it is done.
#' Tables are cached as uncompressed Parquet files, which load faster than
#' `.rds` files, and reused when their fingerprint matches.
#'
#' `df` has these columns:
#' * `id`: row number, integer.
#' * `g_low`: 10 groups, character (`"k01"` to `"k10"`).
#' * `g_high`: `n / 100` groups, integer, uniform.
#' * `g_skew`: 10,000 groups, integer, Zipf distributed.
#' * `x`: standard normal, double.
#' * `y`: uniform on (0, 1), double.
#' * `z`: integer from 1 to 100, 5% missing.
#' * `s`: 1,000 distinct strings, character.
#'
#' `lookup` has one row per `g_high` value with columns `g_high`, `region`
#' (five regions) and `weight`.
#'
#' @param n Number of rows in `df`.
#' @param dir Directory for the cached tables.
#' @returns A list with the paths of the two tables (`df`, `lookup`) and the
#'   `fingerprint` of `df`.
#' @export
#' @examples
#' \dontrun{
#' arena_data(1e5, tempfile())
#' }
arena_data <- function(n, dir = arena_cache_dir()) {
  check_size(n)
  dir <- file.path(dir, paste0("gen", generator_version), format_n(n))
  paths <- list(
    df = file.path(dir, "df.parquet"),
    lookup = file.path(dir, "lookup.parquet"),
    fingerprint = file.path(dir, "fingerprint.rds")
  )
  if (all(file.exists(unlist(paths)))) {
    return(list(
      df = paths$df,
      lookup = paths$lookup,
      fingerprint = readRDS(paths$fingerprint)
    ))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # Tables cached as .rds by earlier versions.
  unlink(file.path(dir, c("df.rds", "lookup.rds")))
  generate <- generate_tables
  environment(generate) <- globalenv()
  fingerprint <- callr::r(
    generate,
    args = list(n = n, seed = generator_seed, paths = paths)
  )
  list(df = paths$df, lookup = paths$lookup, fingerprint = fingerprint)
}

generator_version <- 1L
generator_seed <- 20261114L

# Runs in a fresh R process: base R, and nanoparquet to write the tables.
generate_tables <- function(n, seed, paths) {
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  n <- as.integer(n)
  n_high <- max(1L, n %/% 100L)
  n_skew <- 10000L
  strings <- sprintf("s%04d", seq_len(1000L) * 7L)

  z <- sample.int(100L, n, replace = TRUE)
  z[stats::runif(n) < 0.05] <- NA_integer_
  df <- data.frame(
    id = seq_len(n),
    g_low = sprintf("k%02d", sample.int(10L, n, replace = TRUE)),
    g_high = sample.int(n_high, n, replace = TRUE),
    g_skew = sample.int(
      n_skew,
      n,
      replace = TRUE,
      prob = 1 / seq_len(n_skew)^1.1
    ),
    x = stats::rnorm(n),
    y = stats::runif(n),
    z = z,
    s = sample(strings, n, replace = TRUE)
  )
  lookup <- data.frame(
    g_high = seq_len(n_high),
    region = sample(c("north", "south", "east", "west", "centre"), n_high, TRUE),
    weight = stats::runif(n_high)
  )

  fingerprint <- list(
    nrow = n,
    sum_x = sum(df$x),
    sum_y = sum(df$y),
    sum_z = sum(as.double(df$z), na.rm = TRUE),
    n_na_z = sum(is.na(df$z)),
    sum_skew = sum(as.double(df$g_skew))
  )
  nanoparquet::write_parquet(df, paths$df, compression = "uncompressed")
  nanoparquet::write_parquet(lookup, paths$lookup, compression = "uncompressed")
  saveRDS(fingerprint, paths$fingerprint)
  fingerprint
}

# In-memory size of `df` and `lookup`, in bytes: 4 integer columns, 2
# double columns and 2 character columns (pointers into the string cache).
data_bytes <- function(n) {
  n * (4 * 4 + 2 * 8 + 2 * 8) * 1.05
}

# Bytes the cached tables take on disk: Parquet stores the repeated
# strings once per page.
data_disk_bytes <- function(n) {
  data_bytes(n) * 0.7
}

#' Where tidyarena keeps its files
#'
#' Generated data and run results live in the user cache directory given by
#' [tools::R_user_dir()]. `arena_cache_clear()` deletes the generated data;
#' run results are kept.
#'
#' @returns `arena_cache_dir()` returns the path of the data cache.
#'   `arena_cache_clear()` returns it invisibly.
#' @export
arena_cache_dir <- function() {
  file.path(tools::R_user_dir("tidyarena", "cache"), "data")
}

#' @rdname arena_cache_dir
#' @export
arena_cache_clear <- function() {
  dir <- arena_cache_dir()
  unlink(dir, recursive = TRUE)
  cli::cli_inform("Deleted the generated data in {.path {dir}}.")
  invisible(dir)
}

format_n <- function(n) {
  out <- format(as.double(n), scientific = TRUE)
  sub("e\\+0?", "e", sub("\\.0+e", "e", out))
}

# Where the worker loads nanoparquet from. It is tidyarena's own
# dependency, so it may not be in the library paths the backends run with.
reader_lib <- function() {
  dirname(find.package("nanoparquet"))
}
