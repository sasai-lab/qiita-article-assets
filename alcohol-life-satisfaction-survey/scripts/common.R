project_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("Run with Rscript scripts/<script>.R")
  script <- sub("^--file=", "", file_arg)
  normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
}

root <- project_root()
setwd(root)
Sys.setenv(
  RENV_PATHS_CACHE = file.path(root, "renv", "cache"),
  RENV_PATHS_ROOT = file.path(root, "renv", "root"),
  RENV_CONFIG_CACHE_SYMLINKS = "FALSE",
  RENV_CONFIG_AUTO_SNAPSHOT = "FALSE",
  RENV_CONFIG_SANDBOX_ENABLED = "FALSE"
)
project_library <- normalizePath(file.path(root, "renv", "library"), mustWork = FALSE)
already_active <- any(startsWith(normalizePath(.libPaths(), mustWork = FALSE), project_library))
if (!already_active && basename(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))) != "00_setup.R" && file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

write_csv <- function(value, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(value, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
