file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
if (!requireNamespace("digest", quietly = TRUE)) stop("Run scripts/00_setup.R first")

curl <- Sys.which("curl")
if (!nzchar(curl)) stop("curl is required for downloading official assets")
assets <- read.csv("config/assets.csv", stringsAsFactors = FALSE)
manifest_path <- "outputs/assets_manifest.csv"
manifest <- if (file.exists(manifest_path)) {
  read.csv(manifest_path, stringsAsFactors = FALSE)
} else NULL

validate_file <- function(path) {
  if (!file.exists(path) || file.info(path)$size <= 0) stop("Empty asset: ", path)
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    status <- system2(Sys.which("unzip"), c("-tq", shQuote(path)), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(status, "status"))) stop("ZIP integrity check failed: ", path)
  } else if (grepl("\\.pdf$", path, ignore.case = TRUE)) {
    if (!identical(rawToChar(readBin(path, "raw", n = 4)), "%PDF")) {
      stop("Downloaded file is not PDF: ", path)
    }
  } else if (grepl("\\.html$", path, ignore.case = TRUE)) {
    head <- rawToChar(readBin(path, "raw", n = 4096))
    if (!grepl("<html|<!doctype", head, ignore.case = TRUE)) stop("Not HTML: ", path)
  }
}

for (i in seq_len(nrow(assets))) {
  asset <- assets[i, ]
  existing <- if (!is.null(manifest)) match(asset$id, manifest$id) else NA_integer_
  if (file.exists(asset$path)) {
    if (is.na(existing)) stop("Unrecorded file exists; inspect before replacing: ", asset$path)
    if (!identical(asset$url, manifest$url[existing])) stop("Source URL has changed: ", asset$id)
    if (!identical(digest::digest(file = asset$path, algo = "sha256"), manifest$sha256[existing])) {
      stop("Existing asset differs from recorded SHA-256: ", asset$path)
    }
    validate_file(asset$path)
    message("Verified cached asset: ", asset$id)
    next
  }

  dir.create(dirname(asset$path), recursive = TRUE, showWarnings = FALSE)
  dir.create(".cache/downloads", recursive = TRUE, showWarnings = FALSE)
  partial <- paste0(asset$path, ".partial")
  headers <- file.path(".cache/downloads", paste0(asset$id, ".headers"))
  message("Downloading: ", asset$id)
  result <- system2(curl, c(
    "--fail", "--location", "--silent", "--show-error", "--retry", "3",
    "--connect-timeout", "20", "--max-time", "300",
    "--dump-header", shQuote(headers), "--output", shQuote(partial),
    "--write-out", shQuote("%{url_effective}"), shQuote(asset$url)
  ), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status"))) stop(paste(result, collapse = "\n"))
  # Validate the temporary file with its original extension before renaming it.
  staged <- file.path(".cache/downloads", basename(asset$path))
  if (!file.rename(partial, staged)) stop("Could not stage asset: ", asset$id)
  validate_file(staged)
  if (!file.rename(staged, asset$path)) stop("Could not save asset: ", asset$id)

  header_lines <- readLines(headers, warn = FALSE)
  header_value <- function(name) {
    matches <- grep(paste0("^", name, ":"), header_lines, value = TRUE, ignore.case = TRUE)
    if (!length(matches)) return("")
    trimws(sub("^[^:]+:", "", tail(matches, 1)))
  }
  record <- data.frame(
    id = asset$id, kind = asset$kind, path = asset$path, url = asset$url,
    effective_url = tail(result, 1), retrieved_at_utc = utc_now(),
    bytes = file.info(asset$path)$size,
    sha256 = digest::digest(file = asset$path, algo = "sha256"),
    last_modified = header_value("last-modified"), etag = header_value("etag"),
    stringsAsFactors = FALSE
  )
  if (!is.na(existing)) manifest <- manifest[-existing, , drop = FALSE]
  manifest <- rbind(manifest, record)
  write_csv(manifest, manifest_path)
}

message("All official assets prepared: ", nrow(assets))
