# Source after common.R. Raw ZIPs are verified and extracted afresh on each run.
analysis_raw_variables <- c(
  "_STATE", "SEQNO", "_STSTR", "_PSU", "LSATISFY", "ALCDAY4", "DRNK3GE5",
  "_AGE80", "SEXVAR", "EDUCA", "INCOME3", "MARITAL", "_RACEGR3", "_SMOKER3", "_RFBING6"
)

verified_xpt <- function(source_id) {
  assets <- read.csv("config/assets.csv", stringsAsFactors = FALSE)
  manifest <- read.csv("outputs/assets_manifest.csv", stringsAsFactors = FALSE)
  a <- assets[assets$id == source_id & assets$kind == "data", , drop = FALSE]
  m <- manifest[manifest$id == source_id, , drop = FALSE]
  stopifnot(nrow(a) == 1L, nrow(m) == 1L, a$path == m$path,
            digest::digest(file = a$path, algo = "sha256") == m$sha256)
  members <- unzip(a$path, list = TRUE)
  member <- members$Name[grepl("\\.xpt[[:space:]]*$", members$Name, ignore.case = TRUE)]
  stopifnot(length(member) == 1L, basename(member) == member)
  directory <- file.path(".cache/xpt", source_id)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  unzip(a$path, files = member, exdir = directory, overwrite = TRUE)
  path <- file.path(directory, trimws(member))
  original <- file.path(directory, member)
  if (original != path && !file.rename(original, path)) stop("XPT name normalization failed")
  list(path = path, archive = a$path, archive_sha256 = m$sha256,
       member = member, member_date = as.character(members$Date[members$Name == member]))
}

read_raw <- function(path, columns) {
  x <- as.data.frame(haven::read_xpt(path, col_select = tidyselect::all_of(columns)))
  for (v in setdiff(columns, "SEQNO")) x[[v]] <- as.numeric(x[[v]])
  x$SEQNO <- trimws(as.character(x$SEQNO))
  x
}

record_key <- function(x) paste(x[["_STATE"]], x$SEQNO, sep = ":")

drinking_status <- function(x) {
  out <- rep("unknown", length(x))
  out[x %in% c(101:107, 201:230)] <- "current"
  out[x %in% 888] <- "none_30_days"
  out
}

drinking_days <- function(x) {
  out <- rep(NA_real_, length(x))
  weekly <- x %in% 101:107
  monthly <- x %in% 201:230
  out[weekly] <- (x[weekly] - 100) * 30 / 7
  out[monthly] <- x[monthly] - 200
  out
}

binary_binge <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% 1:76] <- 1L
  out[x %in% 88] <- 0L
  out
}

recode_group <- function(x, groups, labels) {
  out <- rep(NA_character_, length(x))
  for (i in seq_along(groups)) out[x %in% groups[[i]]] <- labels[i]
  factor(out, levels = labels)
}

raw_response_status <- function(x, valid, dont_know = numeric(), refused = numeric(),
                                unknown_combined = numeric(), skip = rep(FALSE, length(x))) {
  out <- rep("unexpected_code", length(x))
  out[is.na(x)] <- "blank"
  out[x %in% dont_know] <- "dont_know"
  out[x %in% refused] <- "refused"
  out[x %in% unknown_combined] <- "unknown_combined"
  out[x %in% valid] <- "valid"
  out[skip & is.na(x)] <- "structural_skip"
  out
}

missing_rules <- list(
  LSATISFY = list(valid = 1:4, dont_know = 7, refused = 9),
  ALCDAY4 = list(valid = c(101:107, 201:230, 888), dont_know = 777, refused = 999),
  DRNK3GE5 = list(valid = c(1:76, 88), dont_know = 77, refused = 99),
  `_AGE80` = list(valid = 18:80), SEXVAR = list(valid = 1:2),
  EDUCA = list(valid = 1:6, refused = 9),
  INCOME3 = list(valid = 1:11, dont_know = 77, refused = 99),
  MARITAL = list(valid = 1:6, refused = 9),
  `_RACEGR3` = list(valid = 1:5, unknown_combined = 9),
  `_SMOKER3` = list(valid = 1:4, unknown_combined = 9),
  `_RFBING6` = list(valid = 1:2, unknown_combined = 9)
)

make_design <- function(x) {
  options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
  survey::svydesign(ids = ~psu_id, strata = ~stratum_id, weights = ~weight,
                    data = x, nest = TRUE)
}

model_formula <- life_dissatisfied ~ binge + age_group + sex + state + education + income +
  marital + race_ethnicity + smoking +
  splines::ns(drink_days_30, knots = c(7, 14, 21), Boundary.knots = c(1, 30))
