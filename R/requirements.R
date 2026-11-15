# What a backend needs from the machine, apart from its packages. GPU
# backends declare it in `requires`, a list with any of:
#
# * `os`, `arch`: the only platform it runs on, as in `Sys.info()` (lower
#   case) and `R.version$arch`.
# * `r`: the oldest R version.
# * `gpu`: `"nvidia"` if it needs an NVIDIA GPU.
# * `compute_capability`: the oldest GPU architecture.
# * `cuda`: the oldest CUDA version the driver must support.
#
# Checking them needs nothing but nvidia-smi, so it works before the backend
# is installed: it decides whether to offer the backend at all, and explains
# why not.

# The machine, as the requirements see it.
platform_info <- function() {
  list(
    os = tolower(Sys.info()[["sysname"]]),
    arch = R.version$arch,
    r = as.character(getRversion()),
    gpus = gpu_info()
  )
}

# GPUs, from nvidia-smi. Cached for the session: they don't change, and
# nvidia-smi is slow to start.
gpu_info <- function() {
  if (is.null(the$gpus)) {
    the$gpus <- env_gpu()
  }
  the$gpus
}

# One sentence per requirement of `backend` that this machine doesn't meet.
# Empty when it meets them all. A requirement that can't be checked, such as
# a CUDA version nvidia-smi doesn't report, counts as met: the load test
# catches it.
unmet_platform <- function(backend, platform = platform_info()) {
  req <- backend_defs()[[backend]]$requires
  if (is.null(req)) {
    return(character())
  }
  out <- character()
  if (!is.null(req$os) && !identical(platform$os, req$os)) {
    out <- c(out, sprintf("runs on %s only, this is %s", os_label(req$os), os_label(platform$os)))
  }
  if (!is.null(req$arch) && !identical(platform$arch, req$arch)) {
    out <- c(out, sprintf("runs on %s only, this is %s", req$arch, platform$arch))
  }
  if (!is.null(req$r) && package_version(platform$r) < req$r) {
    out <- c(out, sprintf("needs R %s or newer, this is R %s", req$r, platform$r))
  }
  if (identical(req$gpu, "nvidia")) {
    if (length(platform$gpus) == 0) {
      return(c(out, "needs an NVIDIA GPU, and nvidia-smi found none"))
    }
    gpu <- platform$gpus[[1]]
    if (!is.null(req$compute_capability) && older(gpu$compute_capability, req$compute_capability)) {
      out <- c(out, sprintf(
        "needs a GPU with compute capability %s or newer, the %s has %s",
        req$compute_capability,
        gpu$name,
        gpu$compute_capability
      ))
    }
    if (!is.null(req$cuda) && older(gpu$cuda, req$cuda)) {
      out <- c(out, sprintf(
        "needs a driver that supports CUDA %s or newer, driver %s supports CUDA %s",
        req$cuda,
        gpu$driver,
        gpu$cuda
      ))
    }
  }
  out
}

# Whether version `x` is older than `required`. Unknown versions are not.
older <- function(x, required) {
  x <- package_version(x %||% NA_character_, strict = FALSE)
  !is.na(x) && x < required
}

os_label <- function(os) {
  switch(os, linux = "Linux", darwin = "macOS", windows = "Windows", os)
}

# How to install a backend tidyarena can't install: why it can't run here,
# if it can't, why it isn't allowed, or its install hint.
install_hint <- function(backend) {
  unmet <- unmet_platform(backend)
  if (length(unmet) > 0) {
    return(sprintf("{.val %s} %s.", backend, cli_escape(paste(unmet, collapse = "; "))))
  }
  def <- backend_defs()[[backend]]
  if (is.null(def$install_hint) && !is.null(def$install$github %||% def$install$repos)) {
    sources <- c(
      if (!is.null(def$install$github)) paste0("https://github.com/", def$install$github),
      def$install$repos
    )
    urls <- paste0("{.url ", sources, "}", collapse = ", ")
    return(paste0(
      "{.val ", backend, "} comes from ", urls, ", not CRAN, and only CRAN is allowed. ",
      "Allow it with {.code arena_config(only_cran = FALSE)}."
    ))
  }
  hint <- def$install_hint
  if (is.function(hint)) hint() else hint
}

# The backend's own diagnostics, for backends that have them: a function run
# in a fresh R process with `libpath`, returning a list of checks, each with
# `ok` (`NA` when not evaluated), `name`, `value` and `message`. Run during
# preflight and saved with the run, so that a GPU backend that fails says
# why. Their failures only count when the backend failed the load test too:
# cuplyr's check of libcudf, for one, looks for build-time files that a
# working install may not have. NULL if the backend has no diagnostics or
# isn't installed.
diagnose_backend <- function(backend, libpath, timeout = 60) {
  def <- backend_defs()[[backend]]
  if (is.null(def$diagnose) || !all(packages_installed(def$packages, libpath))) {
    return(NULL)
  }
  proc <- callr::r_bg(def$diagnose, libpath = libpath, supervise = TRUE)
  proc$wait(timeout * 1000)
  if (proc$is_alive()) {
    proc$kill()
    return(list(ok = NA, checks = list(), error = "timed out"))
  }
  result <- tryCatch(proc$get_result(), error = function(e) e)
  if (inherits(result, "error")) {
    message <- conditionMessage(result$parent %||% result)
    return(list(ok = NA, checks = list(), error = scrub(truncate(message, 500))))
  }
  checks <- lapply(result$checks, function(check) {
    list(
      ok = check$ok,
      name = check$name,
      value = scrub(check$value),
      message = scrub(check$message)
    )
  })
  list(ok = result$ok, checks = unname(checks), error = NULL)
}

# Messages of the checks that failed.
failed_checks <- function(diagnosis) {
  failed <- Filter(function(check) identical(check$ok, FALSE), diagnosis$checks)
  vapply(failed, function(check) paste0(check$name, ": ", check$message), character(1))
}
