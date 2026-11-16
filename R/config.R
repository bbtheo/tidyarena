#' Configure how tidyarena installs and runs backends
#'
#' Settings saved for future sessions, in [tools::R_user_dir()]. They are the
#' defaults of [arena_install()], [arena_run()] and [arena_plan()]; arguments
#' given to those functions take precedence.
#'
#' Call it without arguments to see the current settings.
#'
#' @param only_cran If `TRUE`, packages are only installed from CRAN, through
#'   Posit Package Manager. Backends published elsewhere (tidypolars, on
#'   R-universe) are then not installed, and packages built from source are
#'   not asked to download prebuilt libraries (arrow's C++ library, polars'
#'   Rust library).
#' @param exclude Backend ids to leave out: they are not installed, offered
#'   or run unless you name them. Leaving out a backend leaves out its
#'   Parquet variant too; `"arrow_parquet"` leaves out only the variant. dplyr, the reference for the others, can't
#'   be left out. Use `character()` to include every backend again.
#' @param build What [arena_install()] does with packages that have no
#'   binary for your platform: `"never"` stops and lists them, `"capped"`
#'   compiles them with a memory limit.
#' @param reset If `TRUE`, go back to the defaults first.
#' @returns The settings, a list; invisibly when they were changed.
#' @export
#' @examples
#' \dontrun{
#' arena_config()
#' arena_config(exclude = "cuplyr", build = "capped")
#' arena_config(only_cran = TRUE)
#' arena_config(reset = TRUE)
#' }
arena_config <- function(only_cran = NULL, exclude = NULL, build = NULL, reset = FALSE) {
  changing <- !is.null(only_cran) || !is.null(exclude) || !is.null(build) || reset
  config <- if (reset) config_defaults() else read_config()
  if (!is.null(only_cran)) {
    rlang::check_bool(only_cran)
    config$only_cran <- only_cran
  }
  if (!is.null(exclude)) {
    if (!is.character(exclude)) {
      cli::cli_abort("{.arg exclude} must be a character vector of backend ids.")
    }
    check_ids(exclude, names(backend_defs()), "exclude")
    if (anchor_backend %in% exclude) {
      cli::cli_abort(
        "{.arg exclude} can't include {.val {anchor_backend}}: it is the reference every backend is compared with."
      )
    }
    config$exclude <- unique(exclude)
  }
  if (!is.null(build)) {
    config$build <- rlang::arg_match(build, c("never", "capped"))
  }
  if (!changing) {
    print_config(config)
    return(config)
  }
  write_config(config)
  print_config(config)
  invisible(config)
}

config_defaults <- function() {
  list(only_cran = FALSE, exclude = character(), build = "never")
}

config_path <- function() {
  file.path(tools::R_user_dir("tidyarena", "config"), "config.json")
}

read_config <- function() {
  defaults <- config_defaults()
  path <- config_path()
  if (!file.exists(path)) {
    return(defaults)
  }
  saved <- tryCatch(jsonlite::read_json(path), error = function(e) {
    cli::cli_warn("Ignoring {.path {path}}, which can't be read: {conditionMessage(e)}")
    list()
  })
  config <- utils::modifyList(defaults, saved[intersect(names(saved), names(defaults))])
  config$exclude <- as.character(unlist(config$exclude))
  config
}

write_config <- function(config) {
  path <- config_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  config$exclude <- as.list(config$exclude)
  jsonlite::write_json(config, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

print_config <- function(config) {
  exclude <- if (length(config$exclude) > 0) "{.val {config$exclude}}" else "none"
  cli::cli_bullets(c(
    "*" = "Only CRAN: {config$only_cran}",
    "*" = paste("Left out:", exclude),
    "*" = "Packages without binaries: {.val {config$build}}"
  ))
}

# The backends used when none are named: those available, minus the ones
# left out.
default_backends <- function(backends) {
  exclude <- read_config()$exclude
  base <- vapply(backend_defs()[backends$backend], function(d) d$base %||% "", character(1))
  excluded <- backends$backend %in% exclude | base %in% exclude
  backends$backend[backends$available & !excluded]
}
