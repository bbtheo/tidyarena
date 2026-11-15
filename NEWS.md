# tidyarena (development version)

* First version: `arena_run()` runs 15 dplyr tasks on up to seven backends,
  each in its own R process under a memory budget, and records time, peak
  memory, coverage and correctness with a snapshot of the machine
  (`arena_env()`). `arena_plan()` shows what a run would do, `arena_read()`
  reads the results and `arena_report()` summarises them.

* `arena_install()` installs backends into tidyarena's own library
  (`arena_lib()`) as binaries from Posit Package Manager, optionally from a
  dated snapshot. It does not compile packages unless asked to with
  `build = "capped"`, which limits compile jobs and memory. On Linux
  distributions without their own Package Manager builds, it uses the
  generic `manylinux_2_28` binaries. Where the packages came from is
  recorded in the library and in every run's `run.json`.

* Every grouped task now also exists with `.by` grouping (suffix `_by`), and
  `arena_tasks()` has an `idiom` column, because some backends run only one
  of the two forms. The suite has 23 tasks.

* `arena_run()` checks the requested backends before a new run and, in
  interactive sessions, asks whether to install missing ones and update
  outdated ones (`install = "ask"`). `install = "missing"`, `"update"` and
  `"never"` decide without asking. Backends that would have to be compiled
  are skipped with a hint; backends not on CRAN get an install hint.
* `arena_install()` no longer reinstalls backends that are in your own
  library, and `update = TRUE` installs newer versions into tidyarena's
  library. Every result records whether each backend package came from
  tidyarena's library or yours.

* The answer to the update question decides which versions a run uses:
  after updating, tidyarena's library comes first; otherwise your library
  comes first and tidyarena's only supplies missing backends. The choice is
  stored in `run.json` and kept when a run is resumed.

* Before the first task, `arena_run()` loads every backend once on a tiny
  table and checks the version requirements of its packages against the
  versions the run would use. If a backend doesn't load because your library
  has too old a dependency, it asks whether to use tidyarena's newer
  packages instead (installing them if needed); outside an interactive
  session it does so without asking. Backends that still don't load are
  skipped, with the reason in the results.
