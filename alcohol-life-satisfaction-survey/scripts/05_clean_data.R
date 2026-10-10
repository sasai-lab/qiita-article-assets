file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")

# Do not proceed if the source audit is missing or relevant values disagree.
audit <- read.csv("outputs/source_variable_comparison.csv", stringsAsFactors = FALSE)
stopifnot(all(audit$different_records[audit$variable != "LSATISFY"] == 0),
          all(audit$both_observed_different == 0))
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
  weight_variable <- unique(use$weight)
  stopifnot(length(weight_variable) == 1L)
  x <- read_raw(info$path, c(analysis_raw_variables, weight_variable))
  x <- x[x[["_STATE"]] %in% use$state_code, , drop = FALSE]
  x$source <- id
  x$weight_variable <- weight_variable
  x$weight_original <- x[[weight_variable]]
  x[[weight_variable]] <- NULL
  parts[[id]] <- x
  message("Selected module records: ", id, " / ", nrow(x))
}
d <- do.call(rbind, parts)
rownames(d) <- NULL
d$record_id <- record_key(d)
stopifnot(!anyNA(d[["_STATE"]]), !anyNA(d$SEQNO), !anyDuplicated(d$record_id))

# Full state-version record counts, before any item/drinking/complete-case filter.
pooling <- aggregate(list(records = rep(1L, nrow(d))),
                     d[c("source", "_STATE", "weight_variable")], sum)
names(pooling)[names(pooling) == "_STATE"] <- "state_code"
pooling$total_state_records <- ave(pooling$records, pooling$state_code, FUN = sum)
pooling$pooling_fraction <- pooling$records / pooling$total_state_records
pooling$state_name <- sources$state_name[match(
  paste(pooling$source, pooling$state_code), paste(sources$source, sources$state_code))]
pool_key <- paste(pooling$source, pooling$state_code)
d$pooling_fraction <- pooling$pooling_fraction[match(paste(d$source, d[["_STATE"]]), pool_key)]
d$weight <- d$weight_original * d$pooling_fraction
d$stratum_id <- paste(d[["_STATE"]], format(d[["_STSTR"]], scientific = FALSE, trim = TRUE), sep = ":")
d$psu_id <- paste(d[["_STATE"]], format(d[["_PSU"]], scientific = FALSE, trim = TRUE), sep = ":")
valid_id <- function(x) is.finite(x) & x > 0 & x == floor(x)
d$design_eligible <- is.finite(d$weight) & d$weight > 0 &
  valid_id(d[["_STSTR"]]) & valid_id(d[["_PSU"]])

# Retain raw fields alongside clean variables. No item-missing imputation.
d$adult_eligible <- d[["_AGE80"]] %in% 18:80
d$drinking_status <- drinking_status(d$ALCDAY4)
d$alcohol_contradictory <- d$ALCDAY4 %in% 888 & d$DRNK3GE5 %in% 1:76
d$drinking_status[d$alcohol_contradictory] <- "contradictory"
d$drink_days_30 <- drinking_days(d$ALCDAY4)
d$life_dissatisfied <- rep(NA_integer_, nrow(d))
d$life_dissatisfied[d$LSATISFY %in% 1:2] <- 0L
d$life_dissatisfied[d$LSATISFY %in% 3:4] <- 1L
d$life_satisfaction <- recode_group(d$LSATISFY, as.list(1:4),
  c("Very_satisfied", "Satisfied", "Dissatisfied", "Very_dissatisfied"))
b <- binary_binge(d$DRNK3GE5)
b[d$drinking_status != "current"] <- NA_integer_
d$binge <- factor(b, levels = 0:1, labels = c("No", "Yes"))
age_breaks <- c(18, seq(25, 80, by = 5), 81)
age_labels <- c("18-24", paste(seq(25, 75, 5), seq(29, 79, 5), sep = "-"), "80+")
d$age_group <- cut(d[["_AGE80"]], breaks = age_breaks, right = FALSE,
                   labels = age_labels, include.lowest = TRUE)
d$sex <- recode_group(d$SEXVAR, as.list(1:2), c("Male", "Female"))
states <- unique(sources[c("state_code", "state_name")])
states <- states[order(states$state_code), , drop = FALSE]
d$state <- factor(d[["_STATE"]], levels = states$state_code, labels = states$state_name)
d$education <- recode_group(d$EDUCA, list(1:3, 4, 5, 6),
  c("Below_high_school", "High_school", "Some_college", "College_graduate"))
d$income <- recode_group(d$INCOME3, list(1:4, 5:6, 7:8, 9:11),
  c("Under_25000", "25000_to_49999", "50000_to_99999", "100000_or_more"))
d$marital <- recode_group(d$MARITAL, list(1, 2:4, 5, 6),
  c("Married", "Divorced_widowed_separated", "Never_married", "Unmarried_couple"))
d$race_ethnicity <- recode_group(d[["_RACEGR3"]], as.list(1:5),
  c("White_NH", "Black_NH", "Other_NH", "Multiracial_NH", "Hispanic"))
d$smoking <- recode_group(d[["_SMOKER3"]], list(4, 3, 1:2), c("Never", "Former", "Current"))
covariates <- c("age_group", "sex", "state", "education", "income", "marital", "race_ethnicity", "smoking")
analysis_vars <- c("life_dissatisfied", "binge", "drink_days_30", covariates)
d$complete_case <- complete.cases(d[analysis_vars])
d$analysis_eligible <- d$design_eligible & d$adult_eligible &
  d$drinking_status == "current" & d$complete_case

# Ordered, mutually exclusive exclusion reasons; item-missing summaries below are separate.
conditions <- list(
  invalid_design = !d$design_eligible,
  invalid_age = !d$adult_eligible,
  contradictory_alcohol = d$alcohol_contradictory,
  no_alcohol_30_days = d$drinking_status == "none_30_days",
  unknown_alcohol_frequency = d$drinking_status == "unknown",
  missing_binge = is.na(d$binge),
  missing_life_satisfaction = is.na(d$life_dissatisfied),
  missing_covariates = !complete.cases(d[covariates]))
d$exclusion_reason <- rep("included", nrow(d))
remaining <- rep(TRUE, nrow(d)); flow <- list()
flow[[1L]] <- data.frame(step = 0L, reason = "selected_module", entering = nrow(d), excluded = 0L, remaining = nrow(d))
for (i in seq_along(conditions)) {
  excluded <- remaining & conditions[[i]]
  d$exclusion_reason[excluded] <- names(conditions)[i]
  entering <- sum(remaining)
  remaining[excluded] <- FALSE
  flow[[i + 1L]] <- data.frame(step = i, reason = names(conditions)[i], entering = entering,
                              excluded = sum(excluded), remaining = sum(remaining))
}
stopifnot(identical(remaining, d$analysis_eligible), !anyNA(d$analysis_eligible))
flow <- do.call(rbind, flow)
write_csv(flow, "outputs/cleaning_flow.csv")
write_csv(pooling, "outputs/weight_pooling.csv")

# Distinguish legitimate alcohol-question skips from refusals/unknowns and ordinary blanks.
status_counts <- list(); unexpected <- list()
for (v in names(missing_rules)) {
  extra <- list()
  if (v == "DRNK3GE5") extra$skip <- d$ALCDAY4 %in% c(777, 888, 999) | is.na(d$ALCDAY4)
  status <- do.call(raw_response_status, c(list(x = d[[v]]), missing_rules[[v]], extra))
  for (population in c("full_module", "current_drinkers")) {
    in_population <- if (population == "full_module") rep(TRUE, nrow(d)) else d$drinking_status == "current"
    counts <- as.data.frame(table(status[in_population]), stringsAsFactors = FALSE)
    names(counts) <- c("response_status", "records")
    counts$percent <- 100 * counts$records / sum(in_population)
    status_counts[[length(status_counts) + 1L]] <- data.frame(population = population, variable = v,
      denominator = sum(in_population), counts)
  }
  at <- which(status == "unexpected_code")
  if (length(at)) unexpected[[v]] <- data.frame(record_id = d$record_id[at], source = d$source[at],
                                               variable = v, raw_value = d[[v]][at])
}
write_csv(do.call(rbind, status_counts), "outputs/raw_response_status.csv")
unexpected <- if (length(unexpected)) do.call(rbind, unexpected) else
  data.frame(record_id = character(), source = character(), variable = character(), raw_value = numeric())
write_csv(unexpected, "outputs/unexpected_codes.csv")
if (nrow(unexpected)) stop("Unexpected raw codes found; inspect before finalizing cleaned data")

missingness <- list()
for (population in c("full_module", "current_drinkers")) {
  at <- if (population == "full_module") rep(TRUE, nrow(d)) else d$drinking_status == "current"
  for (v in analysis_vars) {
    n_missing <- sum(is.na(d[[v]][at]))
    missingness[[length(missingness) + 1L]] <- data.frame(population = population, variable = v,
      denominator = sum(at), missing_records = n_missing, missing_percent = 100 * n_missing / sum(at))
  }
}
write_csv(do.call(rbind, missingness), "outputs/clean_variable_missingness.csv")

# Computed binge indicator is a consistency check, never a replacement for raw answers.
expected_binge <- rep(NA_integer_, nrow(d))
expected_binge[d$drinking_status == "none_30_days"] <- 1L
current_valid <- d$drinking_status == "current" & !is.na(d$binge)
expected_binge[current_valid] <- as.integer(d$binge[current_valid])
comparable <- !is.na(expected_binge) & d[["_RFBING6"]] %in% 1:2
binge_mismatch <- comparable & !is.na(expected_binge) & d[["_RFBING6"]] != expected_binge
binge_mismatch[is.na(binge_mismatch)] <- FALSE
write_csv(data.frame(record_id = d$record_id[binge_mismatch], source = d$source[binge_mismatch],
                    ALCDAY4 = d$ALCDAY4[binge_mismatch], DRNK3GE5 = d$DRNK3GE5[binge_mismatch],
                    calculated_binge = d[["_RFBING6"]][binge_mismatch]), "outputs/binge_indicator_mismatches.csv")

s <- d[d$analysis_eligible, , drop = FALSE]
stopifnot(nrow(s) > 0L, all(complete.cases(s[analysis_vars])), all(s$drink_days_30 >= 1 & s$drink_days_30 <= 30))
support <- list()
for (v in c("binge", covariates)) {
  counts <- as.data.frame(table(level = s[[v]], binge = s$binge), stringsAsFactors = FALSE)
  support[[v]] <- data.frame(variable = v, counts)
}
write_csv(do.call(rbind, support), "outputs/covariate_support.csv")
write_csv(as.data.frame(table(binge = s$binge, life_dissatisfied = s$life_dissatisfied)), "outputs/analysis_group_counts.csv")
state_counts <- aggregate(list(module_records = rep(1L, nrow(d)), current_drinkers = as.integer(d$drinking_status == "current"),
                              analysis_records = as.integer(d$analysis_eligible)), d[c("source", "state")], sum)
write_csv(state_counts, "outputs/cleaning_state_counts.csv")

# Construct on all design-valid module records, then subset the analysis domain.
full <- make_design(d[d$design_eligible, , drop = FALSE])
domain <- subset(full, analysis_eligible)
stopifnot(identical(s$record_id, domain$variables$record_id))
psu_strata <- unique(d[d$design_eligible, c("stratum_id", "psu_id")])
psu_n <- table(psu_strata$stratum_id)
domain_psu_n <- table(unique(s[c("stratum_id", "psu_id")])$stratum_id)
design_checks <- data.frame(
  full_records = nrow(full$variables), full_strata = length(psu_n), full_psus = nrow(psu_strata),
  full_lonely_strata = sum(psu_n == 1L), analysis_records = nrow(s),
  analysis_nonempty_strata = length(domain_psu_n), analysis_single_psu_strata = sum(domain_psu_n == 1L),
  analysis_degrees_of_freedom = survey::degf(domain),
  positive_finite_weights = all(is.finite(s$weight) & s$weight > 0), same_analysis_ids = TRUE,
  minimum_weight = min(d$weight[d$design_eligible]), maximum_weight = max(d$weight[d$design_eligible]))
write_csv(design_checks, "outputs/cleaning_design_checks.csv")

factor_variables <- c("life_satisfaction", "binge", covariates)
dictionary <- do.call(rbind, lapply(factor_variables, function(v) {
  data.frame(variable = v, level_order = seq_along(levels(d[[v]])), level = levels(d[[v]]),
             regression_reference = seq_along(levels(d[[v]])) == 1L & v != "life_satisfaction")
}))
write_csv(dictionary, "outputs/factor_levels.csv")

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(d, "data/processed/module_clean.rds", compress = "gzip")
saveRDS(s, "data/processed/analysis_complete_cases.rds", compress = "gzip")
saveRDS(full, "data/processed/survey_design_full.rds", compress = "gzip")
paths <- c("data/processed/module_clean.rds", "data/processed/analysis_complete_cases.rds", "data/processed/survey_design_full.rds")
write_csv(data.frame(path = paths, records = c(nrow(d), nrow(s), nrow(full$variables)),
  sha256 = vapply(paths, function(p) digest::digest(file = p, algo = "sha256"), character(1)),
  bytes = file.info(paths)$size, generated_at_utc = utc_now()), "outputs/cleaned_data_manifest.csv")

checks <- data.frame(check = c("source_audit", "unique_record_ids", "pooling_fractions_sum_to_one",
  "no_unexpected_codes", "analysis_complete_cases", "same_glm_survey_ids", "no_computed_binge_mismatch"),
  passed = c(TRUE, !anyDuplicated(d$record_id), all(abs(tapply(pooling$pooling_fraction, pooling$state_code, sum) - 1) < 1e-12),
             nrow(unexpected) == 0, all(complete.cases(s[analysis_vars])), identical(s$record_id, domain$variables$record_id), !any(binge_mismatch)))
write_csv(checks, "outputs/cleaning_checks.csv")
stopifnot(all(checks$passed))
input_paths <- c("config/module_sources.csv", "renv.lock", "scripts/data_helpers.R",
                 "scripts/04_audit_sources.R", "scripts/05_clean_data.R", schema$archive)
write_csv(data.frame(path = input_paths,
  sha256 = vapply(input_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1))),
  "outputs/cleaning_run_inputs.csv")
message("Cleaning complete: module = ", nrow(d), "; complete cases = ", nrow(s),
        "; binge No = ", sum(s$binge == "No"), "; binge Yes = ", sum(s$binge == "Yes"), ". No regression run.")
