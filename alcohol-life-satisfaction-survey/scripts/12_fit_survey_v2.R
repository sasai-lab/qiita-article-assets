file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
source("scripts/cleaning_v2_helpers.R")
source("scripts/regression_v2_helpers.R")
source("scripts/survey_v2_helpers.R")
library(survey)
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
dir.create("outputs/v2/step3", recursive = TRUE, showWarnings = FALSE)

for (path in c(v2_out("cleaned_data_manifest.csv"), v2_out("cleaning_run_inputs.csv"),
  v2_out("v1_preservation_manifest.csv"), step2_out("run_inputs.csv"), step2_out("output_manifest.csv"),
  step2_out("step1_preservation_manifest.csv"), step2_out("verification_output_manifest.csv"))) {
  verify_manifest_v2(path)
}
stopifnot(all(read.csv(v2_out("cleaning_verification.csv"))$passed),
  all(read.csv(step2_out("verification.csv"))$passed))
preserve_paths <- c(read.csv(v2_out("cleaned_data_manifest.csv"))$path,
  list.files("outputs/v2", pattern = "\\.(csv|txt)$", full.names = TRUE),
  list.files("outputs/v2/step2", full.names = TRUE), "docs/cleaning-report-v2.md",
  "docs/ordinary-regression-report-v2.md")
write_csv(hash_manifest_v2(preserve_paths), step3_out("prior_steps_preservation_manifest.csv"))
if (!file.exists(step3_out("analysis-plan-at-fit.md"))) {
  stopifnot(file.copy("docs/analysis-plan.md", step3_out("analysis-plan-at-fit.md"), overwrite = FALSE))
}
S <- readRDS(v2_data("analysis_complete_cases.rds"))
full <- readRDS(v2_data("survey_design_full.rds"))
domain <- subset(full, analysis_eligible)
ordinary <- read.csv(step2_out("estimates.csv"), stringsAsFactors = FALSE)
ordinary_models <- readRDS(step2_out("glm_models.rds"))
stopifnot(identical(S, domain$variables), identical(S$record_id, ordinary_models$analysis_record_ids),
  all(complete.cases(S[v2_analysis_variables])), !anyDuplicated(S$record_id))
design_df <- survey::degf(domain)
critical <- stats::qt(0.975, df = design_df)

models <- list(); estimates <- list(); diagnostics <- list(); coefficients <- list()
covariances <- list(); warnings <- list(); calculations <- list(); weighted_counts <- list()
for (name in names(v2_model_formulas)) {
  formula <- v2_model_formulas[[name]]; predictor <- all.vars(formula)[2]
  category_levels <- levels(S[[predictor]])
  contrast <- setNames(list(stats::contr.treatment(category_levels, base = 1L)), predictor)
  # svyglm rebuilds a glm call inside its namespace. Embed the contrast list
  # rather than leaving the symbol 'contrast', which also names a function
  # there and can be resolved incorrectly when the call is evaluated.
  fit_call <- substitute(survey::svyglm(formula, design = domain, family = stats::quasibinomial(),
    na.action = stats::na.fail, contrasts = CONTRASTS, rescale = TRUE,
    control = stats::glm.control(epsilon = 1e-10)), list(CONTRASTS = contrast))
  fit <- withCallingHandlers(eval(fit_call), warning = function(w) {
      text <- conditionMessage(w)
      warnings[[length(warnings) + 1L]] <<- data.frame(model = name, warning = text,
        expected_lonely_domain_adjustment = grepl("^Stratum \\(.+\\) has only one PSU at stage 1$", text))
      invokeRestart("muffleWarning")
    })
  stopifnot(fit$converged, !fit$boundary, fit$rank == 5L, stats::nobs(fit) == nrow(S),
    all(is.finite(stats::coef(fit))), all(is.finite(stats::vcov(fit))))
  result <- categorical_estimates_v2(fit, predictor, category_levels, v2_labels[[predictor]], critical)
  counts <- table(S[[predictor]], S$life_dissatisfied)
  w <- as.numeric(stats::weights(domain))
  W <- vapply(category_levels, function(g) sum(w[S[[predictor]] == g]), numeric(1))
  K <- vapply(category_levels, function(g) sum(w[S[[predictor]] == g] * S$life_dissatisfied[S[[predictor]] == g]), numeric(1))
  e <- result$estimates
  e$model <- name; e$predictor <- predictor; e$method <- "svyglm_design_weighted"
  e$records <- rowSums(counts); e$dissatisfied_records <- as.integer(counts[, "1"])
  e$satisfied_records <- as.integer(counts[, "0"])
  e$weight_sum <- W; e$dissatisfied_weight_sum <- K
  e$direct_weighted_probability <- K / W
  e$design_df <- design_df; e$critical_value <- critical
  stopifnot(max(abs(e$probability - e$direct_weighted_probability)) < 1e-10)
  estimates[[name]] <- e
  diagnostics[[name]] <- data.frame(model = name, formula = paste(deparse(formula), collapse = " "),
    records = stats::nobs(fit), converged = fit$converged, boundary = fit$boundary, rank = fit$rank,
    iterations = fit$iter, design_df = design_df, glm_residual_df = fit$df.residual,
    minimum_covariance_eigenvalue = min(eigen(stats::vcov(fit), symmetric = TRUE, only.values = TRUE)$values),
    maximum_probability_aggregation_error = max(abs(e$probability - e$direct_weighted_probability)),
    warning_count = sum(vapply(warnings, function(w) w$model == name, logical(1))))
  coefficients[[name]] <- data.frame(model = name, term = names(stats::coef(fit)),
    logit_coefficient = stats::coef(fit), standard_error = sqrt(diag(stats::vcov(fit))))
  cov_long <- as.data.frame(as.table(stats::vcov(fit)), stringsAsFactors = FALSE)
  names(cov_long) <- c("term_1", "term_2", "covariance")
  covariances[[name]] <- data.frame(model = name, cov_long)
  plot_comparison_v2(ordinary[ordinary$model == name, ], e, name,
    step3_out(paste0(name, "_comparison.png")), nrow(S))
  weighted_counts[[name]] <- e[c("model", "level", "label", "records", "dissatisfied_records",
    "weight_sum", "dissatisfied_weight_sum", "direct_weighted_probability")]
  models[[name]] <- fit; calculations[[name]] <- result
  message("Survey model complete: ", name, "; n = ", stats::nobs(fit), "; design df = ", design_df)
}
warning_table <- if (length(warnings)) do.call(rbind, warnings) else data.frame(model = character(),
  warning = character(), expected_lonely_domain_adjustment = logical())
write_csv(warning_table, step3_out("warnings.csv"))
if (any(!warning_table$expected_lonely_domain_adjustment)) stop("Inspect unexpected survey warnings before finalizing")
weighted <- do.call(rbind, estimates)
comparison <- compare_estimates_v2(ordinary, weighted)
write_csv(weighted, step3_out("estimates.csv"))
write_csv(comparison, step3_out("comparison.csv"))
write_csv(do.call(rbind, weighted_counts), step3_out("weighted_aggregation.csv"))
write_csv(do.call(rbind, diagnostics), step3_out("model_diagnostics.csv"))
write_csv(do.call(rbind, coefficients), step3_out("coefficients.csv"))
write_csv(do.call(rbind, covariances), step3_out("coefficient_covariances.csv"))
saveRDS(list(models = models, calculations = calculations, analysis_record_ids = S$record_id,
  definition_version = "2.0", design_df = design_df, critical_value = critical,
  options = list(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE),
  generated_at_utc = utc_now()), step3_out("survey_models.rds"), compress = "gzip")
writeLines(capture.output(sessionInfo()), step3_out("sessionInfo.txt"), useBytes = TRUE)
write_csv(data.frame(setting = c("design_version", "outcome", "frequency_formula", "quantity_formula",
  "family", "background_adjustment", "design", "domain", "lonely_psu", "adjust_domain_lonely",
  "finite_population_correction", "probability_interval", "difference_interval", "multiplicity", "design_df", "critical_value"),
  value = c("2.0", "LSATISFY 3 or 4 = 1; 1 or 2 = 0", "life_dissatisfied ~ frequency_group",
    "life_dissatisfied ~ quantity_group", "quasibinomial(logit)", "none",
    "saved full design: state+STSTR strata; state+PSU ids; pooled questionnaire-version weights",
    "subset(full, analysis_eligible)", "adjust", "TRUE", "none",
    "t Wald on logit, transformed to probability", "t Wald, delta method using full design coefficient covariance",
    "pointwise, no simultaneous coverage", as.character(design_df), sprintf("%.16f", critical))),
  step3_out("analysis_specification.csv"))
input_paths <- c(v2_data(c("analysis_complete_cases.rds", "survey_design_full.rds")),
  v2_out("cleaned_data_manifest.csv"), v2_out("cleaning_verification.csv"), step2_out("estimates.csv"),
  step2_out("glm_models.rds"), step3_out("analysis-plan-at-fit.md"), "renv.lock",
  "scripts/common.R", "scripts/data_helpers.R", "scripts/cleaning_v2_helpers.R",
  "scripts/regression_v2_helpers.R", "scripts/survey_v2_helpers.R", "scripts/12_fit_survey_v2.R",
  "scripts/13_verify_survey_v2.R")
write_csv(hash_manifest_v2(input_paths), step3_out("run_inputs.csv"))
output_paths <- step3_out(c("estimates.csv", "comparison.csv", "weighted_aggregation.csv", "model_diagnostics.csv",
  "coefficients.csv", "coefficient_covariances.csv", "survey_models.rds", "frequency_comparison.png",
  "quantity_comparison.png", "warnings.csv", "sessionInfo.txt", "analysis_specification.csv", "run_inputs.csv",
  "prior_steps_preservation_manifest.csv", "analysis-plan-at-fit.md"))
m <- hash_manifest_v2(output_paths)
m$bytes <- file.info(output_paths)$size; m$generated_at_utc <- utc_now()
write_csv(m, step3_out("output_manifest.csv"))
message("Step 3 estimates and comparison saved. Run 13_verify_survey_v2.R to verify and produce the report.")
