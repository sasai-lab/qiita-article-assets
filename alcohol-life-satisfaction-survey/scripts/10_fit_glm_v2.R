file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
source("scripts/cleaning_v2_helpers.R")
source("scripts/regression_v2_helpers.R")
dir.create("outputs/v2/step2", recursive = TRUE, showWarnings = FALSE)

# Validate step 1 without regenerating or changing its data, flags, or records.
for (name in c("cleaned_data_manifest.csv", "cleaning_run_inputs.csv", "v1_preservation_manifest.csv")) {
  m <- read.csv(v2_out(name), stringsAsFactors = FALSE)
  stopifnot(all(vapply(seq_len(nrow(m)), function(i)
    digest::digest(file = m$path[i], algo = "sha256") == m$sha256[i], logical(1))))
}
verification <- read.csv(v2_out("cleaning_verification.csv"))
stopifnot(nrow(verification) > 0, all(verification$passed))
manifest <- read.csv(v2_out("cleaned_data_manifest.csv"), stringsAsFactors = FALSE)
preserve_paths <- c(manifest$path, list.files("outputs/v2", pattern = "\\.(csv|txt)$", full.names = TRUE),
                    "docs/cleaning-report-v2.md")
write_csv(data.frame(path = preserve_paths,
  sha256 = vapply(preserve_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1))),
  step2_out("step1_preservation_manifest.csv"))
if (!file.exists(step2_out("analysis-plan-at-fit.md"))) {
  stopifnot(file.copy("docs/analysis-plan.md", step2_out("analysis-plan-at-fit.md"), overwrite = FALSE))
}
S <- readRDS(v2_data("analysis_complete_cases.rds"))
stopifnot(nrow(S) == manifest$records[manifest$path == v2_data("analysis_complete_cases.rds")],
          all(S$analysis_eligible), all(complete.cases(S[v2_analysis_variables])), !anyDuplicated(S$record_id))

models <- list(); estimates <- list(); diagnostics <- list(); coefficients <- list()
covariances <- list(); warnings <- list(); calculations <- list()
for (name in names(v2_model_formulas)) {
  formula <- v2_model_formulas[[name]]; predictor <- all.vars(formula)[2]
  category_levels <- levels(S[[predictor]])
  contrast <- setNames(list(stats::contr.treatment(category_levels, base = 1L)), predictor)
  fit <- withCallingHandlers(stats::glm(formula, data = S, family = stats::binomial(),
    na.action = stats::na.fail, contrasts = contrast,
    control = stats::glm.control(epsilon = 1e-10)), warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- data.frame(model = name, warning = conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  stopifnot(fit$converged, !fit$boundary, fit$rank == 5L, stats::nobs(fit) == nrow(S),
            all(is.finite(stats::coef(fit))), all(is.finite(stats::vcov(fit))))
  result <- categorical_estimates_v2(fit, predictor, category_levels, v2_labels[[predictor]])
  counts <- table(S[[predictor]], S$life_dissatisfied)
  e <- result$estimates
  e$model <- name; e$predictor <- predictor; e$method <- "glm_unweighted"
  e$records <- rowSums(counts); e$dissatisfied_records <- as.integer(counts[, "1"])
  e$satisfied_records <- as.integer(counts[, "0"])
  e$direct_unweighted_probability <- e$dissatisfied_records / e$records
  stopifnot(max(abs(e$probability - e$direct_unweighted_probability)) < 1e-10)
  estimates[[name]] <- e
  diagnostics[[name]] <- data.frame(model = name, formula = paste(deparse(formula), collapse = " "),
    records = stats::nobs(fit), converged = fit$converged, boundary = fit$boundary, rank = fit$rank,
    iterations = fit$iter, residual_deviance = fit$deviance, residual_df = fit$df.residual,
    minimum_covariance_eigenvalue = min(eigen(stats::vcov(fit), symmetric = TRUE, only.values = TRUE)$values),
    maximum_probability_aggregation_error = max(abs(e$probability - e$direct_unweighted_probability)),
    warning_count = sum(vapply(warnings, function(w) w$model == name, logical(1))))
  coefficients[[name]] <- data.frame(model = name, term = names(stats::coef(fit)),
                                     logit_coefficient = stats::coef(fit), standard_error = sqrt(diag(stats::vcov(fit))))
  cov_long <- as.data.frame(as.table(stats::vcov(fit)), stringsAsFactors = FALSE)
  names(cov_long) <- c("term_1", "term_2", "covariance")
  covariances[[name]] <- data.frame(model = name, cov_long)
  plot_glm_v2(e, name, step2_out(paste0(name, "_glm.png")), nrow(S))
  models[[name]] <- fit; calculations[[name]] <- result
  message("Ordinary glm complete: ", name, "; n = ", stats::nobs(fit), "; converged = ", fit$converged)
}
warning_table <- if (length(warnings)) do.call(rbind, warnings) else data.frame(model = character(), warning = character())
write_csv(warning_table, step2_out("warnings.csv"))
if (nrow(warning_table)) stop("Inspect glm warnings before finalizing step 2")
write_csv(do.call(rbind, estimates), step2_out("estimates.csv"))
write_csv(do.call(rbind, diagnostics), step2_out("model_diagnostics.csv"))
write_csv(do.call(rbind, coefficients), step2_out("coefficients.csv"))
write_csv(do.call(rbind, covariances), step2_out("coefficient_covariances.csv"))
saveRDS(list(models = models, calculations = calculations, analysis_record_ids = S$record_id,
  definition_version = "2.0", critical_value = stats::qnorm(0.975), generated_at_utc = utc_now()),
  step2_out("glm_models.rds"), compress = "gzip")
writeLines(capture.output(sessionInfo()), step2_out("sessionInfo.txt"), useBytes = TRUE)
write_csv(data.frame(setting = c("design_version", "outcome", "frequency_formula", "quantity_formula",
  "weighting", "background_adjustment", "probability_interval", "difference_interval", "multiplicity", "critical_value"),
  value = c("2.0", "LSATISFY 3 or 4 = 1; 1 or 2 = 0", "life_dissatisfied ~ frequency_group",
    "life_dissatisfied ~ quantity_group", "none", "none", "normal Wald on logit, transformed to probability",
    "normal Wald, delta method using full coefficient covariance", "pointwise, no simultaneous coverage",
    sprintf("%.16f", stats::qnorm(0.975)))), step2_out("analysis_specification.csv"))
input_paths <- c(v2_data("analysis_complete_cases.rds"), v2_out("cleaned_data_manifest.csv"),
  v2_out("cleaning_verification.csv"), step2_out("analysis-plan-at-fit.md"), "renv.lock",
  "scripts/common.R", "scripts/data_helpers.R", "scripts/cleaning_v2_helpers.R",
  "scripts/regression_v2_helpers.R", "scripts/10_fit_glm_v2.R", "scripts/11_verify_glm_v2.R")
write_csv(data.frame(path = input_paths,
  sha256 = vapply(input_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1))), step2_out("run_inputs.csv"))
output_paths <- step2_out(c("estimates.csv", "model_diagnostics.csv", "coefficients.csv", "coefficient_covariances.csv",
  "glm_models.rds", "frequency_glm.png", "quantity_glm.png", "warnings.csv", "sessionInfo.txt",
  "analysis_specification.csv", "run_inputs.csv", "step1_preservation_manifest.csv", "analysis-plan-at-fit.md"))
write_csv(data.frame(path = output_paths,
  sha256 = vapply(output_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1)),
  bytes = file.info(output_paths)$size, generated_at_utc = utc_now()), step2_out("output_manifest.csv"))
message("Step 2 estimates, model objects, figures, and provenance saved. Survey estimation has not been run.")
