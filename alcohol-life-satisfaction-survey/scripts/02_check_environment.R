file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))

packages <- c("renv", "survey", "haven", "digest")
versions <- vapply(packages, function(package) as.character(packageVersion(package)), character(1))
write_csv(data.frame(package = packages, version = versions), "outputs/package_versions.csv")

# Generated data are used solely for checking the execution environment.
# They are unrelated to BRFSS and are never used as article results.
set.seed(20261010)
sample_data <- data.frame(
  stratum = rep(1:4, each = 60), psu = rep(1:48, each = 5),
  weight = runif(240, 0.5, 3), age = runif(240, 18, 80),
  binge = factor(rbinom(240, 1, 0.4), levels = c(0, 1))
)
sample_data$unhappy <- rbinom(240, 1, plogis(-1.8 + 0.4 * as.numeric(sample_data$binge == "1") + 0.01 * sample_data$age))

design <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~weight, data = sample_data, nest = TRUE)
ordinary <- glm(unhappy ~ binge + age, data = sample_data, family = binomial())
weighted <- survey::svyglm(unhappy ~ binge + age, design = design, family = quasibinomial())
adjustment <- survey::svyglm(unhappy ~ age, design = design, family = quasibinomial())
predictions <- survey::svypredmeans(adjustment, ~binge)
difference <- survey::svycontrast(predictions, c(-1, 1))

stopifnot(
  ordinary$converged, weighted$converged,
  all(is.finite(coef(ordinary))), all(is.finite(coef(weighted))),
  all(is.finite(vcov(weighted))),
  all(is.finite(coef(predictions))), all(is.finite(vcov(predictions))),
  all(is.finite(coef(difference))), all(is.finite(survey::SE(difference)))
)

# Check XPT reader/writer independently of the official dataset.
temporary_xpt <- tempfile(fileext = ".xpt")
haven::write_xpt(data.frame(check_id = 1:3, check_value = c(0, 1, 1)), temporary_xpt)
round_trip <- haven::read_xpt(temporary_xpt)
stopifnot(nrow(round_trip) == 3L, identical(as.numeric(round_trip$check_value), c(0, 1, 1)))
unlink(temporary_xpt)

writeLines(capture.output(sessionInfo()), "outputs/sessionInfo.txt")
write_csv(data.frame(
  check = c("ordinary_glm", "survey_glm", "survey_predictive_margins", "survey_contrast", "haven_xpt"),
  status = "passed", checked_at_utc = utc_now(), data_type = "generated_environment_check_only"
), "outputs/environment_checks.csv")
message("Environment checks passed. No BRFSS associations have been estimated.")
