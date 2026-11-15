# Tests never read or write the user's settings.
withr::local_envvar(
  R_USER_CONFIG_DIR = withr::local_tempdir(.local_envir = testthat::teardown_env()),
  .local_envir = testthat::teardown_env()
)
