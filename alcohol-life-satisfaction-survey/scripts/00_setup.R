# Run once with: Rscript --vanilla scripts/00_setup.R
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- sub("^--file=", "", file_arg)
source(file.path(dirname(script), "common.R"))

options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600, Ncpus = 2)
Sys.setenv(
  RENV_PATHS_CACHE = file.path(root, "renv", "cache"),
  RENV_PATHS_ROOT = file.path(root, "renv", "root"),
  RENV_CONFIG_CACHE_SYMLINKS = "FALSE",
  RENV_CONFIG_AUTO_SNAPSHOT = "FALSE",
  RENV_CONFIG_SANDBOX_ENABLED = "FALSE"
)

bootstrap <- file.path(root, ".bootstrap-library")
dir.create(bootstrap, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(bootstrap, .Library))
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", lib = bootstrap)
}

if (file.exists("renv.lock")) {
  renv::load(project = root)
  renv::restore(project = root, prompt = FALSE)
} else {
  renv::init(project = root, bare = TRUE, restart = FALSE)
  renv::install(c("survey", "haven", "digest"), project = root)
  renv::snapshot(project = root, type = "implicit", prompt = FALSE)
}

renv::status(project = root)
message("Environment setup complete: ", R.version.string)
