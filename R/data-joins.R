# tidyarena's join data: a fact table and the tables it is joined with, with
# keys of every kind. Runs in a fresh R process with base R and nanoparquet.
#
# * `events`, n rows: `id`; `key_int`, n / 10 keys; `key_chr`, the same key
#   as a string; `key_fct`, 1,000 levels; `t`, a time in seconds; `x`, a
#   value from 0 to 1000.
# * `dim`: one row per key, 90% of the keys of `events` and 10% that are not
#   there, with `key_int` and `key_chr`, a `category` (20) and a `score`.
# * `cats`: 900 of the levels of `key_fct` and 100 others, as a factor whose
#   levels are in another order, and a `weight`.
# * `prices`: for every level of `key_fct`, prices that change at random
#   times (`t_change`), n / 10 rows, for rolling joins.
# * `bands`: 100 adjacent value bands `[lo, hi)`, for a non-equi join on `x`.
# * `windows`: for every level, 5 overlapping time windows `[start, end)`,
#   for a non-equi join on `key_fct` and `t`.
generate_joins <- function(n, seed, paths, options = list()) {
  # No min/max statistics: nanoparquet 0.5.2 writes wrong ones for columns
  # of more than one page, and readers that trust them, DuckDB among them,
  # then skip rows that match.
  no_minmax <- nanoparquet::parquet_options(write_minmax_values = FALSE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  n <- as.integer(n)
  m <- max(10L, n %/% 10L)
  levels <- sprintf("f%04d", seq_len(1000L))
  horizon <- 1e7

  key_int <- sample.int(m, n, replace = TRUE)
  events <- data.frame(
    id = seq_len(n),
    key_int = key_int,
    key_chr = sprintf("c%08d", key_int),
    key_fct = factor(levels[sample.int(1000L, n, replace = TRUE)], levels = levels),
    t = sample.int(horizon, n, replace = TRUE),
    x = round(stats::runif(n, 0, 1000), 3)
  )
  rm(key_int)

  keys <- c(sample.int(m, as.integer(0.9 * m)), m + seq_len(as.integer(0.1 * m)))
  keys <- keys[sample.int(length(keys))]
  dim <- data.frame(
    key_int = keys,
    key_chr = sprintf("c%08d", keys),
    category = sample(sprintf("cat%02d", 1:20), length(keys), replace = TRUE),
    score = round(stats::runif(length(keys)), 4)
  )

  cat_levels <- c(sample(levels, 900L), sprintf("g%04d", 1:100))
  cats <- data.frame(
    key_fct = factor(cat_levels, levels = sample(cat_levels)),
    weight = round(stats::runif(length(cat_levels)), 4)
  )

  per_level <- max(1L, n %/% 10000L)
  prices <- data.frame(
    key_fct = factor(rep(levels, each = per_level), levels = levels),
    t_change = sample.int(horizon, 1000L * per_level, replace = TRUE),
    price = round(stats::runif(1000L * per_level, 1, 100), 2)
  )
  prices <- prices[order(prices$key_fct, prices$t_change), ]
  rownames(prices) <- NULL

  bands <- data.frame(
    band = sprintf("b%03d", 1:100),
    lo = (0:99) * 10,
    hi = (1:100) * 10
  )

  windows <- data.frame(
    window_id = seq_len(5000L),
    key_fct = factor(rep(levels, each = 5L), levels = levels),
    start = sample.int(8e6, 5000L, replace = TRUE)
  )
  windows$end <- windows$start + 2000000L

  tables <- list(
    events = events,
    dim = dim,
    cats = cats,
    prices = prices,
    bands = bands,
    windows = windows
  )
  fingerprint <- list()
  for (name in names(tables)) {
    d <- tables[[name]]
    nanoparquet::write_parquet(d, paths[[name]], compression = "uncompressed", options = no_minmax)
    numeric <- vapply(d, is.numeric, logical(1))
    sums <- lapply(d[numeric], function(v) sum(as.double(v)))
    fingerprint[[name]] <- c(list(nrow = nrow(d)), sums)
  }
  fingerprint
}
