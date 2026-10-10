file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))

assets <- read.csv("config/assets.csv", stringsAsFactors = FALSE)
manifest <- read.csv("outputs/assets_manifest.csv", stringsAsFactors = FALSE)
sources <- read.csv("config/module_sources.csv", stringsAsFactors = FALSE)
stopifnot(setequal(assets$id, manifest$id), length(unique(sources$state_code)) == 40L)

for (i in seq_len(nrow(manifest))) {
  stopifnot(file.exists(manifest$path[i]))
  if (digest::digest(file = manifest$path[i], algo = "sha256") != manifest$sha256[i]) {
    stop("SHA-256 mismatch: ", manifest$path[i])
  }
}

required <- c("_STATE", "SEQNO", "_STSTR", "_PSU", "LSATISFY", "ALCDAY4", "DRNK3GE5",
              "_AGE80", "SEXVAR", "EDUCA", "INCOME3", "MARITAL", "_RACEGR3", "_SMOKER3")
checks <- list()
for (id in assets$id[assets$kind == "data"]) {
  asset <- assets[assets$id == id, ]
  members <- unzip(asset$path, list = TRUE)$Name
  # The official main archive uses "LLCP2023.XPT " with a trailing space.
  xpt_member <- members[grepl("\\.xpt[[:space:]]*$", members, ignore.case = TRUE)]
  if (length(xpt_member) != 1L || basename(xpt_member) != xpt_member) stop("Unexpected XPT member: ", id)
  extraction <- file.path(".cache/xpt", id)
  dir.create(extraction, recursive = TRUE, showWarnings = FALSE)
  xpt_path <- file.path(extraction, trimws(xpt_member))
  if (!file.exists(xpt_path)) {
    unzip(asset$path, files = xpt_member, exdir = extraction)
    original_path <- file.path(extraction, xpt_member)
    if (original_path != xpt_path && !file.rename(original_path, xpt_path)) stop("Could not normalize extracted XPT name: ", id)
  }

  columns <- names(haven::read_xpt(xpt_path, n_max = 1))
  weight <- unique(sources$weight[sources$source == id])
  missing <- setdiff(c(required, weight), columns)
  if (length(missing)) stop("Missing required columns in ", id, ": ", paste(missing, collapse = ", "))
  data <- haven::read_xpt(xpt_path, col_select = tidyselect::all_of(c("_STATE", "LSATISFY", weight)))
  selected <- sources[sources$source == id, ]
  for (j in seq_len(nrow(selected))) {
    in_state <- as.numeric(data[["_STATE"]]) == selected$state_code[j]
    values <- as.numeric(data$LSATISFY[in_state])
    weights <- as.numeric(data[[weight]][in_state])
    valid <- values %in% 1:4
    checks[[length(checks) + 1L]] <- data.frame(
      source = id, state_code = selected$state_code[j], state_name = selected$state_name[j],
      weight_variable = weight, archive_member = xpt_member,
      file_columns = length(columns), file_records = nrow(data), state_records = sum(in_state),
      life_satisfaction_valid_records = sum(valid),
      valid_records_with_positive_weight = sum(valid & is.finite(weights) & weights > 0),
      checked_at_utc = utc_now()
    )
    if (!any(valid & is.finite(weights) & weights > 0)) stop("No usable life-satisfaction records: ", id, "/", selected$state_name[j])
  }
  rm(data)
  gc(verbose = FALSE)
  message("XPT import and required columns verified: ", id)
}

write_csv(do.call(rbind, checks), "outputs/asset_checks.csv")

codebook_archive <- assets$path[assets$id == "codebook"]
codebook_member <- unzip(codebook_archive, list = TRUE)$Name
if (length(codebook_member) != 1L || basename(codebook_member) != codebook_member || !grepl("\\.html$", codebook_member, ignore.case = TRUE)) {
  stop("Unexpected codebook archive contents")
}
codebook_directory <- "assets/references/codebook"
dir.create(codebook_directory, recursive = TRUE, showWarnings = FALSE)
unzip(codebook_archive, files = codebook_member, exdir = codebook_directory)
codebook_html <- file.path(codebook_directory, codebook_member)
write_csv(data.frame(
  source_archive = codebook_archive, archive_member = codebook_member, path = codebook_html,
  sha256 = digest::digest(file = codebook_html, algo = "sha256")
), "outputs/reference_files.csv")
message("Official asset checks passed. No regression on BRFSS has been run.")
