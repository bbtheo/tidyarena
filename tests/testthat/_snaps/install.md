# ppm_url() accepts latest or a date

    Code
      ppm_url("yesterday")
    Condition
      Error:
      ! `snapshot` must be "latest" or a date like "2026-03-01".

# arena_install() refuses to compile unless asked

    Code
      arena_install("duckplyr", lib = lib)
    Message
      Resolving dplyr and duckplyr from <https://p3m.dev/cran/latest>.
    Condition
      Error in `arena_install()`:
      ! 2 packages have no binary for this platform and would have to be compiled: duckdb and cli.
      i Posit Package Manager has no binaries for "x86_64-pc-linux-gnu-somedistro".
      i Leave out the backends that need them, or compile:
        `build = "capped"` compiles with a memory limit. This can take a long time.

