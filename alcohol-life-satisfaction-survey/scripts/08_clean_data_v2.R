file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
source("scripts/cleaning_v2_helpers.R")
dir.create("outputs/v2", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed/v2", recursive = TRUE, showWarnings = FALSE)

# Keep the old cleaning code, data, and records intact; record hashes before processing.
preservation_path <- v2_out("v1_preservation_manifest.csv")
if (!file.exists(preservation_path)) {
  old_paths <- c(list.files("data/processed", pattern = "\\.rds$", full.names = TRUE),
    list.files("outputs", pattern = "\\.(csv|txt)$", full.names = TRUE),
    "docs/analysis-plan-v1.3.md", "docs/cleaning-report.md",
    "scripts/data_helpers.R", "scripts/04_audit_sources.R", "scripts/05_clean_data.R",
    "scripts/06_verify_cleaning.R", "scripts/07_profile_redesign.R")
  write_csv(data.frame(path = old_paths,
    sha256 = vapply(old_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1))),
    preservation_path)
}
old_manifest <- read.csv(preservation_path, stringsAsFactors = FALSE)
stopifnot(all(vapply(seq_len(nrow(old_manifest)), function(i)
  digest::digest(file = old_manifest$path[i], algo = "sha256") == old_manifest$sha256[i], logical(1))))

audit <- read.csv("outputs/source_variable_comparison.csv", stringsAsFactors = FALSE)
quantity_audit <- read.csv("outputs/redesign_source_comparison.csv", stringsAsFactors = FALSE)
stopifnot(all(audit$different_records[audit$variable != "LSATISFY"] == 0),
          all(audit$both_observed_different == 0), all(quantity_audit$different_records == 0))
schema <- read.csv("outputs/source_schema.csv", stringsAsFactors = FALSE)
sources <- read.csv("config/module_sources.csv", stringsAsFactors = FALSE)
stopifnot(nrow(sources) == 43L, length(unique(sources$state_code)) == 40L,
          !anyDuplicated(sources[c("source", "state_code")]))
parts <- list()
for (id in unique(sources$source)) {
  info <- verified_xpt(id)
  stopifnot(info$archive_sha256 == schema$archive_sha256[schema$source == id],
    digest::digest(file = info$path, algo = "sha256") == schema$xpt_sha256[schema$source == id])
  use <- sources[sources$source == id, , drop = FALSE]
  weight_var <- unique(use$weight)
  stopifnot(length(weight_var) == 1L)
  x <- read_raw(info$path, c(v2_raw_variables, weight_var))
  x <- x[x[["_STATE"]] %in% use$state_code, , drop = FALSE]
  x$source <- id; x$weight_variable <- weight_var
  x$weight_original <- x[[weight_var]]; x[[weight_var]] <- NULL
  parts[[id]] <- x
  message("Selected v2 module records: ", id, " / ", nrow(x))
}
d <- do.call(rbind, parts); rownames(d) <- NULL
d$record_id <- record_key(d)
stopifnot(!anyNA(d[["_STATE"]]), !anyNA(d$SEQNO), all(nzchar(d$SEQNO)), !anyDuplicated(d$record_id))
pooling <- aggregate(list(records = rep(1L, nrow(d))),
  d[c("source", "_STATE", "weight_variable")], sum)
names(pooling)[names(pooling) == "_STATE"] <- "state_code"
pooling$total_state_records <- ave(pooling$records, pooling$state_code, FUN = sum)
pooling$pooling_fraction <- pooling$records / pooling$total_state_records
pooling$state_name <- sources$state_name[match(paste(pooling$source, pooling$state_code),
                                              paste(sources$source, sources$state_code))]
d$pooling_fraction <- pooling$pooling_fraction[match(paste(d$source, d[["_STATE"]]),
                                                    paste(pooling$source, pooling$state_code))]
d$weight <- d$weight_original * d$pooling_fraction
d$stratum_id <- paste(d[["_STATE"]], format(d[["_STSTR"]], scientific = FALSE, trim = TRUE), sep = ":")
d$psu_id <- paste(d[["_STATE"]], format(d[["_PSU"]], scientific = FALSE, trim = TRUE), sep = ":")
valid_id <- function(x) is.finite(x) & x > 0 & x == floor(x)
d$design_eligible <- is.finite(d$weight) & d$weight > 0 & valid_id(d[["_STSTR"]]) & valid_id(d[["_PSU"]])
d$adult_eligible <- d[["_AGE80"]] %in% 18:80
d <- cbind(d, derive_exposures_v2(d$ALCDAY4, d$AVEDRNK3, d$DRNK3GE5, d$MAXDRNKS))
d$life_dissatisfied <- ifelse(d$LSATISFY %in% 1:2, 0L,
                             ifelse(d$LSATISFY %in% 3:4, 1L, NA_integer_))
d$life_satisfaction <- recode_group(d$LSATISFY, as.list(1:4),
  c("Very_satisfied", "Satisfied", "Dissatisfied", "Very_dissatisfied"))
states <- unique(sources[c("state_code", "state_name")]); states <- states[order(states$state_code), ]
d$state <- factor(d[["_STATE"]], levels = states$state_code, labels = states$state_name)
d$complete_case <- complete.cases(d[v2_analysis_variables])
d$analysis_eligible <- d$design_eligible & d$adult_eligible & !d$alcohol_contradictory & d$complete_case
conditions <- list(invalid_design = !d$design_eligible, invalid_age = !d$adult_eligible,
  contradictory_alcohol = d$alcohol_contradictory,
  unknown_alcohol_frequency = d$drinking_status == "unknown",
  drinker_quantity_88_hold = d$quantity_classification_status == "drinker_reported_none_hold",
  missing_drinker_quantity = d$quantity_classification_status == "missing_drinker_quantity",
  missing_life_satisfaction = is.na(d$life_dissatisfied))
d$exclusion_reason <- "included"
remaining <- rep(TRUE, nrow(d))
flow <- list(data.frame(step = 0L, reason = "selected_module", entering = nrow(d), excluded = 0L, remaining = nrow(d)))
for (i in seq_along(conditions)) {
  at <- remaining & conditions[[i]]
  d$exclusion_reason[at] <- names(conditions)[i]
  entering <- sum(remaining); remaining[at] <- FALSE
  flow[[i + 1L]] <- data.frame(step = i, reason = names(conditions)[i], entering = entering,
                             excluded = sum(at), remaining = sum(remaining))
}
stopifnot(identical(remaining, d$analysis_eligible), !anyNA(d$analysis_eligible))
write_csv(do.call(rbind, flow), v2_out("cleaning_flow.csv"))
write_csv(pooling, v2_out("weight_pooling.csv"))

statuses <- list(); unexpected <- list(); raw_counts <- list()
populations <- list(full_module = rep(TRUE, nrow(d)), current_drinkers = d$drinking_status == "current",
                    none_30_days = d$drinking_status == "none_30_days")
for (v in names(v2_missing_rules)) {
  extra <- list()
  if (v %in% c("AVEDRNK3", "DRNK3GE5", "MAXDRNKS"))
    extra$skip <- d$ALCDAY4 %in% c(777, 888, 999) | is.na(d$ALCDAY4)
  status <- do.call(raw_response_status, c(list(x = d[[v]]), v2_missing_rules[[v]], extra))
  if (v == "AVEDRNK3") status[d[[v]] %in% 88] <- "reported_none"
  if (v == "MAXDRNKS") status[d[[v]] %in% 88] <- "invalid_response"
  for (p in names(populations)) {
    at <- populations[[p]]
    counts <- as.data.frame(table(status[at]), stringsAsFactors = FALSE)
    names(counts) <- c("response_status", "records")
    counts$percent <- 100 * counts$records / sum(at)
    statuses[[length(statuses) + 1L]] <- data.frame(population = p, variable = v, denominator = sum(at), counts)
  }
  at <- which(status == "unexpected_code")
  if (length(at)) unexpected[[v]] <- data.frame(record_id = d$record_id[at], source = d$source[at],
                                               variable = v, raw_value = d[[v]][at])
  values <- ifelse(is.na(d[[v]]), "blank", as.character(d[[v]]))
  raw_counts[[v]] <- data.frame(variable = v, as.data.frame(table(raw_value = values), stringsAsFactors = FALSE))
}
write_csv(do.call(rbind, statuses), v2_out("raw_response_status.csv"))
write_csv(do.call(rbind, raw_counts), v2_out("raw_code_counts.csv"))
unexpected <- if (length(unexpected)) do.call(rbind, unexpected) else
  data.frame(record_id = character(), source = character(), variable = character(), raw_value = numeric())
write_csv(unexpected, v2_out("unexpected_codes.csv"))
if (nrow(unexpected)) stop("Unexpected raw codes found; inspect outputs/v2/unexpected_codes.csv")

s <- d[d$analysis_eligible, , drop = FALSE]
stopifnot(nrow(s) > 0L, all(complete.cases(s[v2_analysis_variables])),
          all(s$drink_days_30 >= 0 & s$drink_days_30 <= 30))
group_counts <- list(); support <- list(); dictionary <- list()
for (v in c("frequency_group", "quantity_group")) {
  group_counts[[v]] <- data.frame(variable = v, label = v2_labels[[v]],
                                 as.data.frame(table(level = s[[v]]), stringsAsFactors = FALSE))
  support[[v]] <- data.frame(variable = v,
    as.data.frame(table(level = s[[v]], life_dissatisfied = s$life_dissatisfied), stringsAsFactors = FALSE))
  dictionary[[v]] <- data.frame(variable = v, level_order = seq_along(levels(d[[v]])),
    level = levels(d[[v]]), label = v2_labels[[v]], regression_reference = seq_along(levels(d[[v]])) == 1L)
}
write_csv(do.call(rbind, group_counts), v2_out("analysis_group_counts.csv"))
write_csv(do.call(rbind, support), v2_out("outcome_support.csv"))
write_csv(do.call(rbind, dictionary), v2_out("factor_levels.csv"))
write_csv(as.data.frame(table(frequency = s$frequency_group, quantity = s$quantity_group)),
          v2_out("frequency_quantity_cells.csv"))
state_counts <- aggregate(list(module_records = rep(1L, nrow(d)),
  current_drinkers = as.integer(d$drinking_status == "current"),
  none_30_days = as.integer(d$drinking_status == "none_30_days"), analysis_records = as.integer(d$analysis_eligible)),
  d[c("source", "state")], sum)
write_csv(state_counts, v2_out("cleaning_state_counts.csv"))
missingness <- list()
for (p in names(populations)) for (v in c(v2_analysis_variables, "average_drinks_drinking_day")) {
  at <- populations[[p]]; n_missing <- sum(is.na(d[[v]][at]))
  missingness[[length(missingness) + 1L]] <- data.frame(population = p, variable = v,
    denominator = sum(at), missing_records = n_missing, missing_percent = 100 * n_missing / sum(at),
    note = if (v == "average_drinks_drinking_day" && p != "current_drinkers")
      "Includes undefined numeric average for non-drinkers; quantity category is defined" else "")
}
write_csv(do.call(rbind, missingness), v2_out("clean_variable_missingness.csv"))

# These checks document patterns, without inventing exclusion rules for unequal question units.
valid_q <- d$drinking_status == "current" & d$AVEDRNK3 %in% 1:76
valid_max <- d$MAXDRNKS %in% 1:76
patterns <- list(none_with_positive_answer = d$alcohol_contradictory,
  drinker_quantity_88 = d$drinking_status == "current" & d$AVEDRNK3 %in% 88,
  drinker_quantity_20_or_more = valid_q & d$AVEDRNK3 >= 20,
  daily_average_above_max_per_occasion = valid_q & valid_max & d$AVEDRNK3 > d$MAXDRNKS,
  binge_times_above_drinking_days = d$drinking_status == "current" & d$DRNK3GE5 %in% 1:76 & d$DRNK3GE5 > d$drink_days_30,
  computed_weekly_zero_current = d$drinking_status == "current" & d[["_DRNKWK2"]] %in% 0,
  current_missing_binge = d$drinking_status == "current" & !d$DRNK3GE5 %in% c(1:76, 88),
  background_item_missing = !d$EDUCA %in% 1:6 | !d$INCOME3 %in% 1:11 |
    !d$MARITAL %in% 1:6 | !d[["_RACEGR3"]] %in% 1:5 | !d[["_SMOKER3"]] %in% 1:4)
write_csv(do.call(rbind, lapply(names(patterns), function(p) {
  at <- patterns[[p]]; at[is.na(at)] <- FALSE
  data.frame(pattern = p, module_records = sum(at), retained_analysis_records = sum(at & d$analysis_eligible))
})), v2_out("consistency_patterns.csv"))
old_s <- readRDS("data/processed/analysis_complete_cases.rds")
write_csv(as.data.frame(table(old_v1_included = d$record_id %in% old_s$record_id,
                              new_v2_included = d$analysis_eligible)), v2_out("v1_v2_membership.csv"))

# Construct on the full module before restricting to the common analysis domain.
full <- make_design(d[d$design_eligible, , drop = FALSE])
domain <- subset(full, analysis_eligible)
stopifnot(identical(s, domain$variables))
psu_strata <- unique(full$variables[c("stratum_id", "psu_id")])
psu_n <- table(psu_strata$stratum_id)
domain_psu_n <- table(unique(s[c("stratum_id", "psu_id")])$stratum_id)
write_csv(data.frame(full_records = nrow(full$variables), full_strata = length(psu_n),
  full_psus = nrow(psu_strata), full_lonely_strata = sum(psu_n == 1L), analysis_records = nrow(s),
  analysis_nonempty_strata = length(domain_psu_n), analysis_single_psu_strata = sum(domain_psu_n == 1L),
  analysis_degrees_of_freedom = survey::degf(domain), minimum_weight = min(full$variables$weight),
  maximum_weight = max(full$variables$weight), same_glm_survey_variables = identical(s, domain$variables)),
  v2_out("cleaning_design_checks.csv"))
paths <- v2_data(c("module_clean.rds", "analysis_complete_cases.rds", "survey_design_full.rds"))
saveRDS(d, paths[1], compress = "gzip"); saveRDS(s, paths[2], compress = "gzip"); saveRDS(full, paths[3], compress = "gzip")
write_csv(data.frame(path = paths, records = c(nrow(d), nrow(s), nrow(full$variables)),
  sha256 = vapply(paths, function(p) digest::digest(file = p, algo = "sha256"), character(1)),
  bytes = file.info(paths)$size, generated_at_utc = utc_now()), v2_out("cleaned_data_manifest.csv"))
inputs <- c("config/assets.csv", "config/module_sources.csv", "renv.lock", "scripts/common.R",
  "scripts/data_helpers.R", "scripts/cleaning_v2_helpers.R", "scripts/08_clean_data_v2.R",
  "scripts/09_verify_cleaning_v2.R", "outputs/assets_manifest.csv", "outputs/source_schema.csv",
  "outputs/source_variable_comparison.csv", "outputs/redesign_source_comparison.csv",
  "assets/references/codebook/USCODE23_LLCP_021924.HTML", schema$archive)
write_csv(data.frame(path = inputs,
  sha256 = vapply(inputs, function(p) digest::digest(file = p, algo = "sha256"), character(1))),
  v2_out("cleaning_run_inputs.csv"))
message("V2 cleaning complete: module = ", nrow(d), "; common analysis = ", nrow(s),
        "; non-drinkers included = ", sum(s$drinking_status == "none_30_days"), ". No regression run.")
