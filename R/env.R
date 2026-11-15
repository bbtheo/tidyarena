#' Snapshot of the machine and its settings
#'
#' Records what can change benchmark results: operating system, CPU, memory,
#' GPU, disk, the R build and its BLAS, thread settings, power and CPU
#' frequency settings, and how busy the machine is. Nothing that identifies
#' the machine or the user is recorded: no host name, user name or paths in
#' the home directory.
#'
#' Fields that cannot be read on a platform are `NA`.
#'
#' @param dir Directory whose disk is described in `disk`.
#' @returns A named list.
#' @export
#' @examples
#' str(arena_env())
arena_env <- function(dir = tempdir()) {
  list(
    os = env_os(),
    cpu = env_cpu(),
    memory = env_memory(),
    gpu = env_gpu(),
    disk = env_disk(dir),
    r = env_r(),
    threads = env_threads(),
    power = env_power(),
    load = env_load()
  )
}

env_os <- function() {
  sysname <- Sys.info()[["sysname"]]
  list(
    family = tolower(sysname),
    name = switch(
      sysname,
      Linux = os_release("PRETTY_NAME"),
      Darwin = paste("macOS", run_quiet("sw_vers", "-productVersion")),
      Windows = utils::win.version(),
      NA_character_
    ),
    kernel = Sys.info()[["release"]],
    arch = R.version$arch
  )
}

os_release <- function(field) {
  lines <- read_quiet("/etc/os-release")
  line <- grep(paste0("^", field, "="), lines, value = TRUE)
  if (length(line) == 0) {
    return(NA_character_)
  }
  gsub('^[^=]+="?|"$', "", line[[1]])
}

env_cpu <- function() {
  sysname <- Sys.info()[["sysname"]]
  model <- switch(
    sysname,
    Linux = {
      line <- grep("^model name", read_quiet("/proc/cpuinfo"), value = TRUE)
      if (length(line)) trimws(sub("^[^:]+:", "", line[[1]])) else NA
    },
    Darwin = run_quiet("sysctl", c("-n", "machdep.cpu.brand_string")),
    Windows = Sys.getenv("PROCESSOR_IDENTIFIER", NA),
    NA
  )
  cpufreq <- "/sys/devices/system/cpu/cpu0/cpufreq"
  list(
    model = as.character(model),
    cores_logical = ps::ps_cpu_count(logical = TRUE),
    cores_physical = ps::ps_cpu_count(logical = FALSE),
    freq_driver = read_first(file.path(cpufreq, "scaling_driver")),
    governor = read_first(file.path(cpufreq, "scaling_governor")),
    energy_preference = read_first(
      file.path(cpufreq, "energy_performance_preference")
    ),
    max_mhz = as.numeric(
      read_first(file.path(cpufreq, "cpuinfo_max_freq"))
    ) /
      1000
  )
}

env_memory <- function() {
  mem <- ps::ps_system_memory()
  swap <- ps::ps_system_swap()
  list(
    total = mem[["total"]],
    available = mem[["avail"]],
    swap_total = swap[["total"]],
    swap_used = swap[["used"]],
    transparent_hugepages = thp_setting()
  )
}

thp_setting <- function() {
  line <- read_first("/sys/kernel/mm/transparent_hugepage/enabled")
  if (is.na(line)) {
    return(NA_character_)
  }
  sub("^.*\\[([a-z]+)\\].*$", "\\1", line)
}

env_gpu <- function() {
  if (!nzchar(Sys.which("nvidia-smi"))) {
    return(list())
  }
  out <- run_quiet(
    "nvidia-smi",
    c(
      "--query-gpu=name,memory.total,driver_version,compute_cap",
      "--format=csv,noheader,nounits"
    )
  )
  if (is.na(out[[1]])) {
    return(list())
  }
  cuda <- driver_cuda_version()
  lapply(strsplit(out, ",\\s*"), function(f) {
    list(
      name = f[[1]],
      memory = as.numeric(f[[2]]) * 2^20,
      driver = f[[3]],
      compute_capability = f[[4]],
      cuda = cuda
    )
  })
}

# The newest CUDA version the driver supports, from the nvidia-smi header.
# Newer drivers label it "CUDA UMD Version".
driver_cuda_version <- function() {
  header <- run_quiet("nvidia-smi")
  line <- grep("CUDA (UMD )?Version:", header, value = TRUE)
  if (length(line) == 0) {
    return(NA_character_)
  }
  sub(".*CUDA (UMD )?Version:\\s*([0-9.]+).*", "\\2", line[[1]])
}

env_disk <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  usage <- tryCatch(ps::ps_disk_usage(dir), error = function(e) NULL)
  list(
    free = if (is.null(usage)) NA_real_ else usage$available[[1]],
    rotational = disk_rotational(dir)
  )
}

disk_rotational <- function(dir) {
  if (Sys.info()[["sysname"]] != "Linux" || !nzchar(Sys.which("findmnt"))) {
    return(NA)
  }
  source <- run_quiet("findmnt", c("-n", "-o", "SOURCE", "--target", dir))
  device <- sub("\\[.*$", "", source[[1]])
  if (is.na(device) || !startsWith(device, "/dev/") || !nzchar(Sys.which("lsblk"))) {
    return(NA)
  }
  rota <- run_quiet("lsblk", c("-ndo", "ROTA", device))
  if (is.na(rota[[1]])) NA else trimws(rota[[1]]) == "1"
}

env_r <- function() {
  info <- utils::sessionInfo()
  list(
    version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    blas = basename(info$BLAS %||% NA_character_),
    lapack = basename(info$LAPACK %||% NA_character_),
    tidyarena = as.character(utils::packageVersion("tidyarena"))
  )
}

thread_env_vars <- c(
  "OMP_NUM_THREADS",
  "OMP_THREAD_LIMIT",
  "OPENBLAS_NUM_THREADS",
  "MKL_NUM_THREADS",
  "R_DATATABLE_NUM_THREADS",
  "POLARS_MAX_THREADS",
  "ARROW_IO_THREADS"
)

env_threads <- function() {
  values <- Sys.getenv(thread_env_vars, unset = NA)
  as.list(values[!is.na(values)])
}

env_power <- function() {
  sysname <- Sys.info()[["sysname"]]
  on_battery <- switch(
    sysname,
    Linux = linux_on_battery(),
    Darwin = {
      out <- run_quiet("pmset", c("-g", "batt"))
      if (is.na(out[[1]])) NA else any(grepl("Battery Power", out))
    },
    NA
  )
  list(
    on_battery = on_battery,
    profile = run_quiet("powerprofilesctl", "get")[[1]]
  )
}

linux_on_battery <- function() {
  supplies <- Sys.glob("/sys/class/power_supply/*")
  types <- vapply(file.path(supplies, "type"), read_first, character(1))
  mains <- supplies[types %in% "Mains"]
  if (length(mains) == 0) {
    return(if (any(types %in% "Battery")) NA else FALSE)
  }
  online <- vapply(file.path(mains, "online"), read_first, character(1))
  !any(online %in% "1")
}

env_load <- function() {
  load <- tryCatch(ps::ps_loadavg(), error = function(e) rep(NA_real_, 3))
  list(loadavg_1m = unname(load[[1]]), loadavg_5m = unname(load[[2]]))
}

# Helpers that never fail: a missing file or command gives NA.

read_quiet <- function(path) {
  tryCatch(
    suppressWarnings(readLines(path, warn = FALSE)),
    error = function(e) character()
  )
}

read_first <- function(path) {
  lines <- read_quiet(path)
  if (length(lines) == 0) NA_character_ else trimws(lines[[1]])
}

run_quiet <- function(command, args = character()) {
  if (!nzchar(Sys.which(command))) {
    return(NA_character_)
  }
  out <- tryCatch(
    processx::run(command, args, error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(out) || out$status != 0) {
    return(NA_character_)
  }
  lines <- strsplit(trimws(out$stdout), "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) NA_character_ else lines
}
