# cuplyr: dplyr verbs run on an NVIDIA GPU by RAPIDS libcudf.
list(
  packages = c("dplyr", "cuplyr"),
  gpu = TRUE,
  mem_factor = 2,
  # cuplyr runs what it can't do on the GPU on the CPU instead, silently by
  # default; setup asks it to warn, so that such cells count as fallbacks.
  fallback_pattern = "fell back to CPU",
  # From cuplyr's README.
  requires = list(
    os = "linux",
    arch = "x86_64",
    r = "4.3.0",
    gpu = "nvidia",
    compute_capability = "7.0",
    cuda = "12.2"
  ),
  # Turns each table, `x`, named `name`, into the backend's form.
  convert = "cuplyr::tbl_gpu(x)",
  # Reads the Parquet file at `path` itself, decoded on the GPU by libcudf: a
  # lazy scan, so every query reads the columns it uses from the file. Since
  # cuplyr 0.1.1.9000 (d838d5f).
  read = "cuplyr::read_parquet_gpu(path, lazy = TRUE)",
  setup = c(
    "options(cuplyr.fallback = 'warn')",
    "library(dplyr)",
    "library(cuplyr)"
  ),
  # cuplyr is built from source against libcudf and the CUDA toolkit. Its
  # install.sh sets those up with pixi or conda, or uses the system's. That
  # is a long, heavy build, so tidyarena only says how.
  install_hint = function() {
    command <- "git clone https://github.com/bbtheo/cuplyr && cd cuplyr && ./install.sh"
    found <- if (nzchar(Sys.which("pixi"))) {
      "pixi"
    } else if (nzchar(Sys.which("mamba")) || nzchar(Sys.which("conda"))) {
      "conda"
    } else if (nzchar(Sys.which("nvcc"))) {
      "your CUDA toolkit"
    }
    c(
      paste0(
        "This machine meets cuplyr's GPU requirements. cuplyr is built from source with RAPIDS libcudf; in a shell, run {.code ",
        command,
        "}."
      ),
      if (is.null(found)) {
        "It needs pixi ({.url https://pixi.sh}), conda, or a CUDA toolkit with libcudf; pixi is the simplest."
      } else {
        paste0("It will use ", found, ", which is installed here.")
      }
    )
  },
  # cuplyr's check of the driver, the CUDA toolkit, libcudf, its R packages
  # and GPU access. Runs in a fresh R process, so it is self-contained.
  diagnose = function() {
    utils::capture.output(result <- cuplyr::check_deps())
    result
  }
)
