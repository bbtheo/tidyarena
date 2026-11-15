# The data of db-benchmark, <https://github.com/duckdblabs/db-benchmark>,
# generated as its `_data/groupby-datagen.R` and `_data/join-datagen.R` do
# (at commit 2f05a6f), for 100 groups, no missing values and unsorted rows:
# the same random numbers drawn in the same order, so the tables equal
# db-benchmark's. Ported from data.table to base R; character columns become
# factors, as db-benchmark's solutions read them (`stringsAsFactors = TRUE`).
#
# db-benchmark is licensed under the Mozilla Public License 2.0. Copyright
# H2O.ai, DuckDB Labs and the db-benchmark contributors. See
# inst/COPYRIGHTS.
#
# Both run in a fresh R process with base R and nanoparquet only.

generate_dbbench_groupby <- function(n, seed, paths, options = list()) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  N <- as.integer(n)
  K <- 100L
  x <- list()
  x[["id1"]] <- sample(sprintf("id%03d", 1:K), N, TRUE)
  x[["id2"]] <- sample(sprintf("id%03d", 1:K), N, TRUE)
  x[["id3"]] <- sample(sprintf("id%010d", 1:(N / K)), N, TRUE)
  x[["id4"]] <- sample(K, N, TRUE)
  x[["id5"]] <- sample(K, N, TRUE)
  x[["id6"]] <- sample(N / K, N, TRUE)
  x[["v1"]] <- sample(5, N, TRUE)
  x[["v2"]] <- sample(15, N, TRUE)
  x[["v3"]] <- round(stats::runif(N, max = 100), 6)
  for (col in c("id1", "id2", "id3")) {
    x[[col]] <- factor(x[[col]])
  }
  x <- as.data.frame(x)
  nanoparquet::write_parquet(x, paths$x, compression = "uncompressed")

  summarise_table <- function(d) {
    numeric <- vapply(d, is.numeric, logical(1))
    c(list(nrow = nrow(d)), lapply(d[numeric], function(v) sum(as.double(v))))
  }
  list(x = summarise_table(x))
}

generate_dbbench_join <- function(n, seed, paths, options = list()) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  N <- as.integer(n)

  # Sample ensuring none is missing.
  sample_all <- function(x, size) {
    y <- c(x, sample(x, size = max(size - length(x), 0), replace = TRUE))
    sample(y)
  }
  # Split into common (0.9), left (0.1) and right (0.1) keys.
  split_xlr <- function(n) {
    key <- sample.int(n * 1.1)
    list(
      x = key[seq.int(1, n * 0.9)],
      l = key[seq.int(n * 0.9 + 1, n)],
      r = key[seq.int(n + 1, n * 1.1)]
    )
  }
  # As db-benchmark writes a table: character copies of the integer keys,
  # and the value columns last.
  finish <- function(d) {
    if ("id1" %in% names(d)) d$id4 <- factor(sprintf("id%.0f", d$id1))
    if ("id2" %in% names(d)) d$id5 <- factor(sprintf("id%.0f", d$id2))
    if ("id3" %in% names(d)) d$id6 <- factor(sprintf("id%.0f", d$id3))
    d[c(setdiff(names(d), c("v1", "v2")), intersect(c("v1", "v2"), names(d)))]
  }
  summarise_table <- function(d) {
    numeric <- vapply(d, is.numeric, logical(1))
    c(list(nrow = nrow(d)), lapply(d[numeric], function(v) sum(as.double(v))))
  }
  fingerprint <- list()
  write <- function(d, name) {
    d <- finish(d)
    nanoparquet::write_parquet(d, paths[[name]], compression = "uncompressed")
    fingerprint[[name]] <<- summarise_table(d)
  }

  key1 <- split_xlr(N / 1e6)
  key2 <- split_xlr(N / 1e3)
  key3 <- split_xlr(N)

  lhs <- c("x", "l")
  x <- data.frame(
    id1 = sample_all(unlist(key1[lhs], use.names = FALSE), N),
    id2 = sample_all(unlist(key2[lhs], use.names = FALSE), N),
    id3 = sample_all(unlist(key3[lhs], use.names = FALSE), N)
  )
  x$v1 <- round(stats::runif(nrow(x), max = 100), 6)
  write(x, "x")
  rm(x)

  rhs <- c("x", "r")
  size <- N / 1e6
  small <- data.frame(id1 = sample_all(unlist(key1[rhs], use.names = FALSE), size))
  small$v2 <- round(stats::runif(nrow(small), max = 100), 6)
  write(small, "small")

  size <- N / 1e3
  medium <- data.frame(
    id1 = sample_all(unlist(key1[rhs], use.names = FALSE), size),
    id2 = sample_all(unlist(key2[rhs], use.names = FALSE), size)
  )
  medium$v2 <- round(stats::runif(nrow(medium), max = 100), 6)
  write(medium, "medium")

  big <- data.frame(
    id1 = sample_all(unlist(key1[rhs], use.names = FALSE), N),
    id2 = sample_all(unlist(key2[rhs], use.names = FALSE), N),
    id3 = sample_all(unlist(key3[rhs], use.names = FALSE), N)
  )
  rm(key1, key2, key3)
  big$v2 <- round(stats::runif(nrow(big), max = 100), 6)
  write(big, "big")

  fingerprint
}
