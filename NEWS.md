# tidyarena (development version)

* First version: `arena_run()` runs 15 dplyr tasks on up to seven backends,
  each in its own R process under a memory budget, and records time, peak
  memory, coverage and correctness with a snapshot of the machine
  (`arena_env()`). `arena_plan()` shows what a run would do, `arena_read()`
  reads the results and `arena_report()` summarises them.

* `arena_install()` installs backends into tidyarena's own library
  (`arena_lib()`) as binaries from Posit Package Manager, optionally from a
  dated snapshot. It does not compile packages unless asked to with
  `build = "capped"`, which limits compile jobs and memory.
