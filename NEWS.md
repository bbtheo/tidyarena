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

* GPU backends declare what they need from the machine: an NVIDIA GPU, its
  compute capability, the CUDA version the driver supports, OS,
  architecture and R version (for cuplyr, from its README). These are
  checked with nvidia-smi before anything is installed, and
  `arena_backends()` and `arena_plan()` say which one is unmet. When a
  machine qualifies but cuplyr is not installed, tidyarena points to
  cuplyr's `install.sh` and says whether pixi, conda or a CUDA toolkit is
  there; it never builds cuplyr itself. Before a run, cuplyr's own
  `check_deps()` runs too, its results are saved in `run.json`, and its
  failed checks explain a cuplyr that does not load.

* Tasks and backends are files, one per id, in `inst/tasks/` and
  `inst/backends/`, with defaults for optional fields and checks for
  unknown ones. The `.by` form of each grouped task is now written from its
  `group_by()` form. `arena_check_backend()` and `arena_check_task()` check
  a new backend or task on a small table in seconds; `arena_check_task()`
  also measures its memory factor. `arena_tasks()` has a `hash` of each
  task's code, which results record and resumed runs check.
  `vignette("extending")` explains how to add a task or a backend.

* The generated tables are cached as uncompressed Parquet files, written and
  read with nanoparquet, instead of `.rds` files. At 1e7 rows a worker loads
  them in 0.3 s instead of 1.4 s, and they take about half the disk space.
  The worker unloads nanoparquet before the backend's setup. Existing
  `.rds` caches are replaced the next time a size is used.

* `arena_config()` saves settings for installing and running: `only_cran`
  to install only from CRAN, `exclude` to leave backends out of installs and
  runs, and `build` to choose whether packages without a binary are
  compiled. `arena_install()` gains `only_cran` and takes its defaults from
  there.

* tidypolars and polars are installed from their latest GitHub releases,
  or the latest before a dated `snapshot`, not from development versions,
  with polars' prebuilt Rust library. arrow, where it has to be compiled,
  uses Apache's prebuilt C++ library. Backends declare such sources in a new
  `install` field.
