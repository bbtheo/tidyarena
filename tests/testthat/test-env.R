test_that("arena_env() records the machine without identifying it", {
  env <- arena_env()
  expect_named(env, c("os", "cpu", "memory", "gpu", "disk", "r", "threads", "power", "load"))
  text <- jsonlite::toJSON(env, auto_unbox = TRUE)
  info <- Sys.info()
  # The host name can legitimately equal the distribution name, which is
  # part of the OS name and kernel release.
  no_os <- jsonlite::toJSON(env[names(env) != "os"], auto_unbox = TRUE)
  expect_false(grepl(info[["nodename"]], no_os, fixed = TRUE))
  expect_false(grepl(info[["user"]], text, fixed = TRUE))
  expect_false(grepl(path.expand("~"), text, fixed = TRUE))
})

test_that("setting_notes() flags distorting settings", {
  env <- list(
    cpu = list(governor = "powersave", freq_driver = "acpi-cpufreq", cores_logical = 4L),
    power = list(on_battery = TRUE),
    load = list(loadavg_1m = 3),
    threads = list(OMP_NUM_THREADS = "1")
  )
  notes <- setting_notes(env)
  expect_length(notes, 4)
  expect_match(notes, "governor|battery|busy|Thread", all = FALSE)
})

test_that("powersave is normal under intel_pstate", {
  env <- list(cpu = list(governor = "powersave", freq_driver = "intel_pstate"))
  expect_length(setting_notes(env), 0)
})
