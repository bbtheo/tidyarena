# tidyarena (development version)

* The tiers are now t-shirt sizes, all with three timed repetitions.
  `"l"` runs every task at 1e6 to 1e8 rows, as `"standard"` did, and
  `"xl"` adds 1e9, as `"full"` did. `"m"`, `"s"` and `"xs"` run a half, a
  quarter and an eighth of the tasks as in `"l"`, taking the tasks the
  machine has run least often in finished runs, so two m, four s or eight
  xs runs cover one l run. They replace `"quick"`.

* Backends that read Parquet files themselves also run as Parquet variants:
  `dbplyr_duckdb_parquet`, `duckplyr_parquet`, `arrow_parquet`,
  `tidypolars_parquet` and `cuplyr_parquet` read the tables from their
  cached Parquet files (a DuckDB view, `read_parquet_duckdb()`,
  `open_dataset()`, `scan_parquet()`, `read_parquet_gpu()`) instead of being given data frames from R. A backend's
  new `read` field defines its variant. `arena_report()` compares each
  variant with its backend, and results have a `source` column and the
  untimed `setup_seconds`. `arena_config(exclude = "arrow")` leaves out
  the variant too.

* cuplyr's CPU fallbacks, silent by default, now warn and count as
  fallbacks, as duckplyr's and arrow's do.

* Every task runs with `TZ=UTC`, so that results don't depend on the
  machine's time zone.

* The cached Parquet files are written without min/max statistics, and are
  generated again. nanoparquet 0.5.2 writes wrong statistics for columns of
  more than one page, and DuckDB, which trusts them, then dropped matching
  rows from joins on the files (140 of 9 million in db-benchmark's join
  q5). The fix is in nanoparquet's development version.

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

* Every repetition is now measured twice, each time from the data: *in R*
  (the pipeline, `collect()` and reading every value of the result, so that
  columns converted only when read are converted inside the timing) and
  *compute* (the pipeline with `compute()`, the result kept in the backend).
  Results have `compute_seconds` and `compute_peak_rss` next to `seconds`
  and `peak_rss`, and `arena_report()` shows both. Backends can define
  `.compute()` and `.release()` in their setup. Results from earlier
  versions timed `collect()` without reading the values, so the suite
  version is now 2026.1-dev.

* Two suites from established benchmarks join tidyarena's own tasks:
  db-benchmark's 10 groupby and 5 join questions, on data generated as
  db-benchmark generates it (ported to base R; the tables are identical),
  and the 22 TPC-H-derived queries of arrowbench, on the TPC-H tables
  generated by DuckDB. Tasks now run on a data set (`arena_data()` gains
  `dataset`), whose tables can be any number; backends give a `convert`
  expression applied to each table instead of converting `df` and `lookup`
  in their setup. Tasks can name `packages` they need (lubridate). The
  report adds a table by data set.

* tidyarena is now licensed under the MPL-2.0, the license of db-benchmark;
  `inst/COPYRIGHTS` credits db-benchmark and arrowbench.

* Three more data sets of tidyarena's own, with tasks: `"joins"` (13 joins
  on integer, string and factor keys, semi, anti and full joins, rolling
  joins with `closest()` and non-equi joins on value bands and time
  windows), `"strings"` (15 string operations such as grouping on
  `str_sub()`, regular expressions, cleaning, extracting and sorting) and
  `"datetimes"` (13 date and time operations: `floor_date()` by month, week,
  hour and day, `ceiling_date()`, adding months with `+` and `%m+%`,
  durations, parsing, time zones). The worker attaches the packages a task
  names, so task code reads as users write it. There are 119 tasks.

* Loading the data into a backend has the tier's timeout of its own: a
  backend that can't load the data stops with `"setup_timeout"` instead of
  using the time of the whole cell.
