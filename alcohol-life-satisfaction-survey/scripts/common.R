project_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("Run with Rscript scripts/<script>.R")
  script <- sub("^--file=", "", file_arg)
  normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
}

root <- project_root()
setwd(root)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

write_csv <- function(value, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(value, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
