
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyarena

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

tidyarena benchmarks dplyr backends against each other with the same
dplyr code. Every task is one pipeline, written once and run unchanged
on dplyr, dtplyr, duckplyr, dbplyr with duckdb, arrow, tidypolars and
cuplyr. For each task it records:

- whether the backend ran it, failed, or fell back to dplyr,
- whether the result matches dplyr’s,
- how long it took, including collecting the result into R,
- how much memory it used at its peak.

It also records the machine and its settings (OS, CPU, memory, GPU,
disk, BLAS, thread limits, power settings), so results from many
machines can be compared.

## Installation

You can install the development version of tidyarena from GitHub with:

``` r
# install.packages("pak")
pak::pak("bbtheo/tidyarena")
```

Then install the backends you want to compare. `arena_install()` puts
them in a library of their own, so they never mix with your packages:

``` r
tidyarena::arena_install(c("dtplyr", "duckplyr", "dbplyr_duckdb", "arrow"))
```

Packages come as binaries from Posit Package Manager, which builds them
for Windows, macOS and the common Linux distributions (Ubuntu, Debian,
RHEL/Rocky, openSUSE and others). On other x86_64 Linux distributions
with glibc 2.28 or newer, such as Arch or Fedora, it uses Package
Manager’s generic `manylinux_2_28` binaries. Those don’t cover every
package (arrow is missing, for example). When a package would have to be
compiled, `arena_install()` stops and lists it. Compiling duckdb or
arrow takes a long time and several GB of memory per compile job. If you
want to do it anyway, `build = "capped"` compiles them with a number of
jobs that fits in free memory and, on Linux with systemd, inside a
memory limit with swap disabled.

You don’t have to do this first: when a run starts, `arena_run()` checks
the backends and asks whether to install missing ones and whether to
update outdated ones. If you update, the run uses the newest versions of
everything from tidyarena’s library. If you don’t, it uses the packages
in your own library and takes only the backends you lack from
tidyarena’s. Backends that are not installed are skipped.

## Usage

See what a run would do, then run it:

``` r
library(tidyarena)

arena_plan("xs")
run <- arena_run("xs")
arena_report(run)
```

Runs come in t-shirt sizes. The tasks are split into four parts with
similar run times and a similar mix of data sets. `"xs"` runs one part at
1e6 rows, `"s"` one part and `"m"` two parts at 1e6 and 1e7 rows, `"l"`
every task at 1e6 and 1e7 rows, and `"xl"` every task at up to 1e8 rows.
Each run takes the parts this machine has run least often, so four s runs
or two m runs give the same cells as one l run: every cell runs one
warm-up and three timed repetitions, the same in every tier.

`arena_report()` shows the fastest backend for every task, and for every
backend its coverage and its time and memory relative to dplyr.
`arena_read()` gives the raw measurements.

## Safe to run

Each task runs in its own R process with a memory budget, by default 70%
of available memory with at least 2 GB left free:

- On Linux with systemd, the budget is a cgroup limit with swap
  disabled. A task that overruns it is killed by the kernel before the
  machine starts swapping.
- On every platform, a watchdog kills a task that goes over budget,
  leaves less than 1 GB free, makes swap grow, or runs too long.
- Sizes that are not expected to fit are skipped before they start, and
  a task that fails at one size is not retried at larger sizes.

Results are written after every task. If a run is interrupted, call
`arena_run(dir = <the run directory>)` to continue it.

## What is measured, and how

- **Data** is generated from a fixed seed, so every machine runs on
  identical tables (see `?arena_data`), cached as Parquet files. It is
  loaded into each backend's own format before timing starts. Backends
  that read Parquet themselves (DuckDB through dbplyr and duckplyr, arrow,
  Polars and cuplyr) also run as Parquet variants, `<id>_parquet`, which read
  the files instead of being given data frames from R: the data then keeps
  the engine's own types, and the report compares the two. Every task
  runs with the time zone set to UTC, so that results don't depend on
  where the machine is.
- **Time** is measured twice, each from the data, because both answers
  are useful:
  - *in R*: the pipeline, `collect()`, and reading every value of the
    result, so that backends that convert columns only when they are
    read pay for it inside the timing;
  - *compute*: the pipeline with `compute()`, the result computed where
    the backend keeps it (a DuckDB table, an Arrow table, GPU memory),
    with nothing transferred to R.
- **Memory** is the peak resident memory of the process, for both. On
  Linux it is exact (the kernel’s high-water mark, reset before every
  repetition); elsewhere it is sampled every 0.1 seconds.
- **Correctness** compares a fingerprint of each result with dplyr’s:
  row count, column names, missing values, sums and ranges of numeric
  columns (with a relative tolerance), and the distinct values of
  character columns.

Nothing that identifies you or your machine is recorded: no host name,
user name or home directory paths.

## The suites

- **tidyarena’s own tasks**, 23 of them, on a generated table with low,
  high and skewed group cardinalities, missing values and strings.
- **db-benchmark** (<https://github.com/duckdblabs/db-benchmark>): its
  10 groupby and 5 join questions on its own data, generated as it does,
  so results can be set next to its published ones.
- **Joins, strings, dates and times**, three more data sets of
  tidyarena’s own: joins on integer, string and factor keys, rolling and
  non-equi joins; string operations such as grouping on `str_sub()`,
  regular expressions and cleaning; `floor_date()`, adding months, time
  zones and parsing.
- **TPC-H-derived queries**: the 22 queries of TPC-H written in dplyr
  (from arrowbench), over the eight TPC-H tables generated by DuckDB.
  They are not the TPC-H benchmark, and their results are not comparable
  with published TPC-H results.

## Adding tasks and backends

Each task and each backend is a short file in `inst/tasks/` or
`inst/backends/`. `arena_check_backend()` and `arena_check_task()` check
a new one in seconds: they run it on a small table and compare it with
dplyr. See `vignette("extending")`.

## License

tidyarena is licensed under the Mozilla Public License 2.0. It contains
code from db-benchmark (MPL-2.0) and arrowbench (MIT); see
`inst/COPYRIGHTS`.
