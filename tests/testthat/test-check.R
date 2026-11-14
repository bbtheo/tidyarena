fp <- function(sum = 10, n = 3, names = c("a", "b"), hash = "h") {
  list(
    nrow = n,
    names = names,
    cols = list(
      list(kind = "numeric", type = "integer", na = 0, sum = sum, sumsq = 30, min = 1, max = 4),
      list(kind = "character", type = "character", na = 0, n_distinct = 2, hash = hash)
    )
  )
}

test_that("fingerprints match within tolerance", {
  expect_true(compare_fingerprint(fp(), fp()))
  expect_true(compare_fingerprint(fp(sum = 10 + 1e-9), fp()))
})

test_that("fingerprint differences are reported", {
  expect_match(attr(compare_fingerprint(fp(n = 4), fp()), "reason"), "rows")
  expect_match(attr(compare_fingerprint(fp(sum = 11), fp()), "reason"), "column a")
  expect_match(attr(compare_fingerprint(fp(hash = "x"), fp()), "reason"), "column b")
  expect_match(attr(compare_fingerprint(fp(names = c("a", "c")), fp()), "reason"), "names")
  expect_true(is.na(compare_fingerprint(NULL, fp())))
})

test_that("integer and double results compare equal", {
  a <- fp()
  a$cols[[1]]$type <- "numeric"
  expect_true(compare_fingerprint(a, fp()))
})
