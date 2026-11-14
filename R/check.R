# Compares a result fingerprint with the anchor's. Returns TRUE or FALSE,
# with the first difference found in the "reason" attribute.
#
# Fingerprints do not depend on row order (except for tasks with an
# order key), and numeric columns are compared with a relative tolerance:
# backends sum in different orders. Integer and double columns are treated
# alike, because backends differ in the type `n()` and `sum()` return.
compare_fingerprint <- function(x, ref, tolerance = 1e-6) {
  differ <- function(reason) structure(FALSE, reason = reason)
  if (is.null(x) || is.null(ref)) {
    return(structure(NA, reason = "no fingerprint"))
  }
  if (!identical(as.numeric(x$nrow), as.numeric(ref$nrow))) {
    return(differ(sprintf("%s rows, expected %s", x$nrow, ref$nrow)))
  }
  if (!identical(unlist(x$names), unlist(ref$names))) {
    return(differ("column names differ"))
  }
  for (i in seq_along(ref$cols)) {
    a <- x$cols[[i]]
    b <- ref$cols[[i]]
    name <- unlist(ref$names)[[i]]
    if (!identical(a$kind, b$kind)) {
      return(differ(sprintf("column %s is %s, expected %s", name, a$type, b$type)))
    }
    if (!identical(as.numeric(a$na), as.numeric(b$na))) {
      return(differ(sprintf("column %s has %s missing, expected %s", name, a$na, b$na)))
    }
    same <- switch(
      a$kind,
      numeric = all(mapply(
        near,
        a[c("sum", "sumsq", "min", "max")],
        b[c("sum", "sumsq", "min", "max")],
        MoreArgs = list(tolerance = tolerance)
      )),
      character = identical(a$n_distinct, b$n_distinct) && identical(a$hash, b$hash),
      TRUE
    )
    if (!same) {
      return(differ(sprintf("column %s has different values", name)))
    }
  }
  if (!identical(x$order_hash, ref$order_hash)) {
    return(differ("rows are in a different order"))
  }
  TRUE
}

near <- function(a, b, tolerance) {
  a <- as.numeric(a %||% NA)
  b <- as.numeric(b %||% NA)
  if (is.na(a) || is.na(b)) {
    return(is.na(a) && is.na(b))
  }
  abs(a - b) <= tolerance * max(1, abs(b))
}
