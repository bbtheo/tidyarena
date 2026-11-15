# A tiny run with dplyr only, shared by the tests that need results.
local_tiny_run <- function(tasks = c("filter_rows", "summarise_lowcard"), env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env), .local_envir = env)
  suppressMessages(arena_run(
    "quick",
    backends = "dplyr",
    tasks = tasks,
    sizes = 1e3,
    install = "never",
    dir = file.path(dir, "run")
  ))
}

small_tables <- function() {
  env <- new.env(parent = asNamespace("dplyr"))
  df <- tibble::tibble(
    id = 1:20,
    g_low = rep(c("k01", "k02"), 10),
    g_high = rep(1:4, 5),
    g_skew = rep(1:2, 10),
    x = seq(-1, 1, length.out = 20),
    y = 1:20 / 20,
    z = c(NA, 2:20),
    s = rep(c("s0007", "s0014"), 10)
  )
  lookup <- tibble::tibble(g_high = 1:4, region = c("a", "b", "a", "b"), weight = 1:4)
  env$df <- df
  env$lookup <- lookup
  env
}
