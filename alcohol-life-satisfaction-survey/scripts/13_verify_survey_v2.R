file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
source("scripts/cleaning_v2_helpers.R")
source("scripts/regression_v2_helpers.R")
source("scripts/survey_v2_helpers.R")
library(survey)
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
results <- list()
check <- function(name, condition) {
  passed <- isTRUE(condition)
  results[[length(results) + 1L]] <<- data.frame(check = name, passed = passed)
  write_csv(do.call(rbind, results), step3_out("verification.csv"))
  if (!passed) stop("Step 3 verification failed: ", name)
}
near <- function(a, b, tolerance = 1e-7) isTRUE(all.equal(a, b, tolerance = tolerance, check.attributes = FALSE))
for (path in step3_out(c("run_inputs.csv", "output_manifest.csv", "prior_steps_preservation_manifest.csv"))) {
  verify_manifest_v2(path)
  check(paste0("hashes_", basename(path)), TRUE)
}
verify_manifest_v2(v2_out("v1_preservation_manifest.csv"))
check("old_v1_preserved", TRUE)
S <- readRDS(v2_data("analysis_complete_cases.rds"))
full <- readRDS(v2_data("survey_design_full.rds"))
domain <- subset(full, analysis_eligible)
saved <- readRDS(step3_out("survey_models.rds"))
ordinary_saved <- readRDS(step2_out("glm_models.rds"))
e <- read.csv(step3_out("estimates.csv"), stringsAsFactors = FALSE)
ordinary <- read.csv(step2_out("estimates.csv"), stringsAsFactors = FALSE)
comparison <- read.csv(step3_out("comparison.csv"), stringsAsFactors = FALSE)
warning_table <- read.csv(step3_out("warnings.csv"), stringsAsFactors = FALSE)
check("same_all_variables_in_domain_and_glm_sample", identical(S, domain$variables))
check("same_analysis_ids_all_models", identical(saved$analysis_record_ids, S$record_id) &&
  identical(ordinary_saved$analysis_record_ids, S$record_id))
check("definition_version_2", saved$definition_version == "2.0")
check("both_preplanned_models_present", identical(names(saved$models), names(v2_model_formulas)))
check("specified_lonely_psu_options", identical(saved$options,
  list(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)))
check("only_expected_lonely_domain_warnings", all(warning_table$expected_lonely_domain_adjustment))
check("design_df_and_t_critical", saved$design_df == survey::degf(domain) &&
  near(saved$critical_value, stats::qt(0.975, survey::degf(domain)), 1e-12))
check("full_design_one_psu_per_respondent", ncol(full$cluster) == 1L && !anyDuplicated(full$cluster[, 1]))
check("full_design_no_fpc_or_post_calibration", is.null(full$fpc$popsize) && is.null(full$postStrata))
check("weights_match_saved_clean_data", near(as.numeric(stats::weights(domain)), S$weight, 1e-12))

# Independent stratified Taylor variance of weighted group proportions.
# Each full-design record is one unique PSU. Omitted domain records contribute
# zero influence but remain in the original stratum sample size. A lonely
# domain/full stratum is centred at the grand influence mean (zero here), as
# specified by the preplanned 'adjust' option. No survey variance function or
# fitted-model covariance is used in this calculation.
manual_group_covariance <- function(predictor, probability, weight_sums) {
  d <- full$variables
  eligible <- d$analysis_eligible
  lv <- levels(S[[predictor]])
  influence <- matrix(0, nrow(d), length(lv))
  for (j in seq_along(lv)) {
    members <- eligible & !is.na(d[[predictor]]) & d[[predictor]] == lv[j]
    influence[members, j] <- d$weight[members] *
      (d$life_dissatisfied[members] - probability[j]) / weight_sums[j]
  }
  stopifnot(max(abs(colSums(influence))) < 1e-10)
  covariance <- matrix(0, length(lv), length(lv))
  indices <- split(seq_len(nrow(d)), d$stratum_id)
  for (ix in indices) {
    n_full <- length(ix); n_domain <- sum(eligible[ix])
    if (n_domain == 0L) next
    u <- influence[ix, , drop = FALSE]
    if (n_full > 1L && n_domain > 1L) u <- sweep(u, 2, colMeans(u), "-")
    multiplier <- if (n_full > 1L) n_full / (n_full - 1) else 1
    covariance <- covariance + multiplier * crossprod(u)
  }
  covariance
}

manual_results <- list(); covariance_results <- list(); verification_warnings <- list()
for (name in names(v2_model_formulas)) {
  fit <- saved$models[[name]]; calculation <- saved$calculations[[name]]
  formula <- v2_model_formulas[[name]]; predictor <- all.vars(formula)[2]
  lv <- levels(S[[predictor]]); x <- e[e$model == name, ]
  counts <- table(S[[predictor]], S$life_dissatisfied)
  W <- vapply(lv, function(g) sum(S$weight[S[[predictor]] == g]), numeric(1))
  K <- vapply(lv, function(g) sum(S$weight[S[[predictor]] == g] * S$life_dissatisfied[S[[predictor]] == g]), numeric(1))
  p <- K / W; critical <- saved$critical_value
  C <- manual_group_covariance(predictor, p, W)
  contrast <- diag(length(lv)); contrast[, 1] <- contrast[, 1] - 1
  delta_covariance <- contrast %*% C %*% t(contrast)
  eta_covariance <- C / outer(p * (1 - p), p * (1 - p))
  se_logit <- sqrt(diag(eta_covariance))
  delta <- p - p[1]; se_delta <- sqrt(pmax(0, diag(delta_covariance)))
  manual_results[[name]] <- data.frame(model = name, level = lv, probability = p,
    probability_se = sqrt(diag(C)), logit_se = se_logit,
    probability_ci_low = stats::plogis(stats::qlogis(p) - critical * se_logit),
    probability_ci_high = stats::plogis(stats::qlogis(p) + critical * se_logit),
    difference_pp = 100 * delta, difference_se_pp = 100 * se_delta,
    difference_ci_low_pp = 100 * (delta - critical * se_delta),
    difference_ci_high_pp = 100 * (delta + critical * se_delta))
  cov_long <- as.data.frame(as.table(C), stringsAsFactors = FALSE)
  names(cov_long) <- c("group_1", "group_2", "covariance")
  covariance_results[[name]] <- data.frame(model = name, cov_long)
  check(paste0("same_rows_outcome_and_predictor_", name), identical(rownames(fit$model), rownames(S)) &&
    identical(fit$model$life_dissatisfied, S$life_dissatisfied) && identical(fit$model[[predictor]], S[[predictor]]))
  check(paste0("same_formula_as_ordinary_no_background_predictors_", name),
    identical(all.vars(stats::formula(fit)), all.vars(formula)) &&
    identical(all.vars(stats::formula(ordinary_saved$models[[name]])), all.vars(formula)) &&
    identical(setdiff(names(fit$model), "(weights)"), c("life_dissatisfied", predictor)))
  check(paste0("specified_family_and_no_offset_", name), fit$family$family == "quasibinomial" &&
    fit$family$link == "logit" && is.null(fit$offset))
  check(paste0("no_additional_exclusion_converged_full_rank_", name), stats::nobs(fit) == nrow(S) &&
    fit$converged && !fit$boundary && fit$rank == 5L && is.null(fit$na.action))
  check(paste0("fixed_categories_and_non_drinker_reference_", name), identical(x$level, lv) &&
    identical(fit$xlevels[[predictor]], lv) && lv[1] == "None_30_days")
  check(paste0("design_model_uses_same_weights_strata_psus_", name),
    identical(as.character(fit$survey.design$cluster[, 1]), as.character(domain$cluster[, 1])) &&
    identical(as.character(fit$survey.design$strata[, 1]), as.character(domain$strata[, 1])) &&
    identical(rownames(fit$survey.design$cluster), rownames(domain$cluster)) &&
    near(fit$survey.design$fpc, domain$fpc, 1e-12) &&
    near(as.numeric(stats::weights(fit$survey.design)), S$weight, 1e-12))
  check(paste0("working_weights_only_rescale_design_weights_", name),
    near(fit$prior.weights, S$weight / mean(S$weight), 1e-12))
  check(paste0("counts_match_ordinary_and_both_outcomes_present_", name), all(counts > 0) &&
    near(x$records, rowSums(counts), 1e-12) && near(x$dissatisfied_records, counts[, "1"], 1e-12))
  check(paste0("probabilities_equal_direct_weighted_counts_", name), max(abs(x$probability - p)) < 1e-10)
  check(paste0("weight_sums_correct_", name), near(x$weight_sum, W, 1e-12) && near(x$dissatisfied_weight_sum, K, 1e-12))
  check(paste0("probability_covariance_matches_manual_stratified_variance_", name), near(calculation$probability_covariance, C))
  check(paste0("logit_covariance_matches_manual_stratified_variance_", name), near(calculation$logit_covariance, eta_covariance))
  check(paste0("difference_covariance_includes_group_covariance_", name), near(calculation$difference_covariance, delta_covariance))
  check(paste0("probability_intervals_match_manual_taylor_variance_", name),
    near(x$probability_ci_low, manual_results[[name]]$probability_ci_low) &&
    near(x$probability_ci_high, manual_results[[name]]$probability_ci_high))
  check(paste0("difference_and_intervals_match_manual_taylor_variance_", name),
    near(x$difference_pp, manual_results[[name]]$difference_pp) &&
    near(x$difference_se_pp, manual_results[[name]]$difference_se_pp) &&
    near(x$difference_ci_low_pp, manual_results[[name]]$difference_ci_low_pp) &&
    near(x$difference_ci_high_pp, manual_results[[name]]$difference_ci_high_pp))
  predicted <- stats::predict(fit, newdata = calculation$newdata, type = "link", se.fit = TRUE, vcov = TRUE)
  check(paste0("predictions_and_full_covariance_match_R_predict_", name),
    near(as.numeric(predicted), x$logit) && near(stats::vcov(predicted), calculation$logit_covariance))
  # A second route uses survey means of ten numerator/denominator indicators
  # on the same domain, then the ratio gradient. It avoids group subsetting,
  # which would change the definition of lonely domains.
  indicator <- stats::model.matrix(~group - 1, data.frame(group = S[[predictor]]))
  moments <- cbind(indicator, indicator * S$life_dissatisfied)
  colnames(moments) <- paste0("moment_", seq_len(ncol(moments)))
  moment_design <- domain
  moment_design$variables[colnames(moments)] <- as.data.frame(moments)
  means <- withCallingHandlers(survey::svymean(stats::reformulate(colnames(moments)), moment_design),
    warning = function(w) {
      verification_warnings[[length(verification_warnings) + 1L]] <<- data.frame(model = name, warning = conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  denominators <- as.numeric(stats::coef(means))[1:5]
  numerators <- as.numeric(stats::coef(means))[6:10]
  J <- cbind(diag(-numerators / denominators^2), diag(1 / denominators))
  mean_ratio_covariance <- J %*% stats::vcov(means) %*% t(J)
  check(paste0("weighted_proportions_match_survey_means_ratio_", name), near(p, numerators / denominators, 1e-10))
  check(paste0("manual_covariance_matches_survey_means_ratio_", name), near(C, mean_ratio_covariance))
  check(paste0("valid_probability_intervals_", name), all(x$probability_ci_low > 0 & x$probability_ci_high < 1 &
    x$probability_ci_low < x$probability & x$probability < x$probability_ci_high))
  check(paste0("reference_difference_zero_", name), all(unlist(x[1,
    c("difference_pp", "difference_se_pp", "difference_ci_low_pp", "difference_ci_high_pp")]) == 0))
  signature <- readBin(step3_out(paste0(name, "_comparison.png")), "raw", n = 8L)
  check(paste0("comparison_png_saved_", name), identical(signature, as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))))
  message("Verified survey model using direct weights, independent stratified variance, survey means, and R predict: ", name)
}
check("comparison_table_reproduces_both_methods", near(comparison, compare_estimates_v2(ordinary, e), 1e-12))
frequency <- e[e$model == "frequency", ]; quantity <- e[e$model == "quantity", ]
check("same_non_drinker_estimate_and_variance_in_both_models", near(frequency$probability[1], quantity$probability[1], 1e-10) &&
  near(frequency$probability_se[1], quantity$probability_se[1]))
write_csv(do.call(rbind, manual_results), step3_out("independent_taylor_check.csv"))
write_csv(do.call(rbind, covariance_results), step3_out("independent_probability_covariances.csv"))
write_csv(if (length(verification_warnings)) do.call(rbind, verification_warnings) else
  data.frame(model = character(), warning = character()), step3_out("verification_warnings.csv"))

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
md_table <- function(x, headers = names(x)) c(paste0("| ", paste(headers, collapse = " | "), " |"),
  paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
  apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
result_table <- function(x) {
  format_delta <- function(method) {
    text <- sprintf("%+.1f（%+.1f〜%+.1f）", x[[paste0(method, "_difference_pp")]],
      x[[paste0(method, "_difference_ci_low_pp")]], x[[paste0(method, "_difference_ci_high_pp")]])
    text[x$reference] <- "基準：0.0"
    text
  }
  md_table(data.frame(group = x$label, ordinary = sprintf("%.1f（%.1f〜%.1f）", x$glm_dissatisfaction_percent,
    x$glm_dissatisfaction_ci_low_percent, x$glm_dissatisfaction_ci_high_percent),
    weighted = sprintf("%.1f（%.1f〜%.1f）", x$survey_dissatisfaction_percent,
    x$survey_dissatisfaction_ci_low_percent, x$survey_dissatisfaction_ci_high_percent),
    ordinary_delta = format_delta("glm"), weighted_delta = format_delta("survey")),
    c("カテゴリ", "glm 不満割合 %（95%CI）", "survey 不満割合 %（95%CI）",
      "glm 飲酒なしとの差 pp（95%CI）", "survey 飲酒なしとの差 pp（95%CI）"))
}
changed <- comparison[!comparison$reference &
  comparison$glm_difference_ci_excludes_zero != comparison$survey_difference_ci_excludes_zero, ]
changed_description <- if (nrow(changed)) paste0("非飲酒カテゴリとの差について、点ごとの95%区間が0を含むかどうかが変わったのは、",
  paste(paste0(ifelse(changed$model == "frequency", "頻度：", "量："), changed$label), collapse = "、"), "です。") else
  "非飲酒カテゴリとの差について、点ごとの95%区間が0を含むかどうかは、全カテゴリで通常の回帰と同じでした。"
diag <- read.csv(step3_out("model_diagnostics.csv"))
report <- c("# ステップ3：surveyの回帰と通常の回帰の比較（設計2.0）", "",
  paste0("確認日時：", format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Tokyo"), "（日本時間）。ステップ3の推定・比較・検証が完了。"), "",
  paste0("ステップ2と同じ", fmt_n(nrow(S)), "人、同じ生活への不満という目的変数、同じ飲酒頻度・飲酒日の平均量のカテゴリで、2つのsurvey::svyglmを実行しました。年齢・性別・所得などの背景変数は追加していません。"), "",
  "## 1. 比較で分かったこと", "",
  paste0("過去30日に飲酒なしの不満割合は、通常の回帰の", sprintf("%.1f%%", comparison$glm_dissatisfaction_percent[6]),
    "からsurveyの", sprintf("%.1f%%", quantity$dissatisfaction_percent[1]), "になりました。飲酒日に平均1ドリンクは",
    sprintf("%.1f%% → %.1f%%", comparison$glm_dissatisfaction_percent[7], quantity$dissatisfaction_percent[2]),
    "、平均5以上は", sprintf("%.1f%% → %.1f%%", comparison$glm_dissatisfaction_percent[10], quantity$dissatisfaction_percent[5]), "です。"), "",
  paste0("全10行で、不満割合の95%区間の幅のsurvey/glm比は", sprintf("%.2f〜%.2f", min(comparison$probability_ci_width_ratio),
    max(comparison$probability_ci_width_ratio)), "倍でした。同じ対象者数でも、各回答の重みと抽出区分を反映すると、不確かさの評価が変わりました。"), "",
  changed_description, "",
  "割合の変化は重み付き集計への変更で確認できます。信頼区間の変化は重み・層・PSUなどを含む設計分散への変更です。変化全体を年齢、重みのばらつき、層化など1つの原因に定量的に分解した分析ではありません。", "",
  "## 2. 飲酒頻度だけのモデル", "", result_table(comparison[comparison$model == "frequency", ]), "",
  paste0("15日超〜30日の非飲酒カテゴリとの差の95%区間上限は、丸める前では",
    sprintf("%+.3f", frequency$difference_ci_high_pp[5]),
    "ppです。表では−0.0と表示されますが、0よりわずかに小さい値です。区間が0を含むかどうかは丸める前の値で判定しています。"), "",
  paste0("![頻度：glmとsurveyの比較](", file.path(root, step3_out("frequency_comparison.png")), ")"), "",
  "週回答は×30/7で換算し、分類前に丸めていません。飲酒量をそろえた頻度の差ではありません。", "",
  "## 3. 飲酒日の平均量だけのモデル", "", result_table(comparison[comparison$model == "quantity", ]), "",
  paste0("![量：glmとsurveyの比較](", file.path(root, step3_out("quantity_comparison.png")), ")"), "",
  "ドリンク数は飲酒日の平均量で、飲酒機会1回の量・飲まない日を含む1日平均・30日の総量とは区別します。頻度をそろえた量の差でもありません。", "",
  "表は小数1桁、CSVは丸めていない値です。ppはパーセントポイントです。区間は点ごとの95%信頼区間で、全比較を同時に保証する区間ではありません。区間が0を含むかどうかは差の区間で判断し、2つの割合の区間の重なりで判断しません。", "",
  "## 4. 計算方法", "", "```r", "library(survey)",
  "options(survey.lonely.psu = \"adjust\", survey.adjust.domain.lonely = TRUE)",
  "full <- readRDS(\"data/processed/v2/survey_design_full.rds\")",
  "design <- subset(full, analysis_eligible)",
  "fit_frequency <- svyglm(life_dissatisfied ~ frequency_group, design = design, family = quasibinomial())",
  "fit_quantity <- svyglm(life_dissatisfied ~ quantity_group, design = design, family = quasibinomial())", "```", "",
  "全303,632件で作った設計から解析対象をドメインとして指定しました。層は州＋_STSTR、PSUは州＋_PSU、重みは公式指定の質問票版の重みを用います。複数版の州では、欠測などで絞る前の州・版の人数比で重みを調整したステップ1の値を引き継いでいます。", "",
  "この保存データではPSUは回答者ごとに一意です。複数回答者を同じ家庭や地域クラスターとしてまとめた解析ではありません。有限母集団補正は指定していません。", "",
  paste0("不確実性にはvcov(svyglm)を使い、設計自由度", fmt_n(saved$design_df),
    "のt分布（臨界値", sprintf("%.8f", saved$critical_value), "）で区間を求めました。割合はロジット尺度の区間を確率へ戻し、割合の差は全係数の共分散を含むデルタ法で求めました。svyglmのsummaryの既定残差自由度ではなく、計画どおり解析ドメインのdegfを使っています。"), "",
  "## 5. 検証結果", "",
  md_table(data.frame(model = c("頻度", "量"), n = fmt_n(diag$records), converged = ifelse(diag$converged, "はい", "いいえ"),
    iterations = diag$iterations, error = sprintf("%.2e", diag$maximum_probability_aggregation_error)),
    c("モデル", "対象者数", "収束", "反復回数", "重み付き集計との最大誤差")), "",
  paste0("別プロセスで", length(results), "項目の検証がすべて通りました。各カテゴリの予測割合を重み付き不満者数÷重みの合計と照合しました。さらに、モデルやsurveyの分散関数を使わずに、回答者の影響量と元の層別人数からTaylor分散を計算し、割合・差の共分散と区間に一致することを確認しました。surveyの指標平均から比を作る別経路とRのpredictにも照合しました。"), "",
  paste0("全体の単独PSUの層61、解析ドメインで1PSUだけになる非空層25について、計画したadjustの設定を使いました。モデル実行時の警告は", nrow(warning_table),
    "件で、", if (nrow(warning_table)) "すべてこの設定に関する既知のドメイン警告でした。" else "追加のモデル警告はありませんでした。",
    "単独PSUの扱いも独立計算に含めています。"), "",
  "ステップ1・2の入力データ、分類・除外記録、モデル・推定値・図・報告書と、旧版の保存物はSHA-256が一致しました。Rや追加パッケージの導入は行っていません。", "",
  "## 6. 言える範囲", "",
  "これは選定した39州とDCの、解析項目が有効な回答者についての重み付き関連です。任意モジュールの実施地域と完全ケースに制約されるため、全米・日本の成人の代表値とは扱いません。重みは項目欠測や群間の交絡を自動で取り除きません。", "",
  "横断調査なので、飲酒・禁酒による生活満足度の変化、推奨量や安全量は判断できません。背景の違い、逆方向の関連、自己申告の誤差が残ります。「過去30日に飲酒なし」では、生涯非飲酒者と以前飲んでいた人などを区別できません。", "",
  "## 7. 成果物・再実行・進捗", "",
  "- outputs/v2/step3/estimates.csv：surveyの全カテゴリの割合・差・区間。",
  "- comparison.csv、frequency_comparison.png、quantity_comparison.png：通常の回帰との比較。",
  "- weighted_aggregation.csv：実人数と重み付き集計。重みの合計を実際の回答者数と扱わない。",
  "- survey_models.rds、model_diagnostics.csv、coefficients.csv、coefficient_covariances.csv：モデルと計算記録。",
  "- verification.csv、independent_taylor_check.csv、independent_probability_covariances.csv：別プロセスの数値検証。",
  "- analysis-plan-at-fit.md、analysis_specification.csv、run_inputs.csv、output_manifest.csv、prior_steps_preservation_manifest.csv：計画・方法・入出力の保存記録。", "",
  "```sh", "Rscript scripts/12_fit_survey_v2.R", "Rscript scripts/13_verify_survey_v2.R", "```", "",
  "| ステップ | 状態 |", "| --- | --- |", "| 1. クリーニング | 完了（設計2.0） |",
  "| 2. 通常の回帰 | 完了 |", "| 3. surveyとの比較 | 完了（2モデル・比較表・図・検証） |", "| 4. 記事 | 未着手 |", "",
  "次は今回の比較をもとにQiita原稿を作成します。記事作成・投稿はこのステップでは行っていません。", "",
  "出典：[CDCのモジュール分析ガイド](https://www.cdc.gov/brfss/annual_data/2023/pdf/Complex-Sampling-Weights-and-Preparing-Module-Data-for-Analysis-2023-508.pdf)、[survey公式マニュアル](https://cran.r-project.org/web/packages/survey/refman/survey.html)。",
  "対象と分類は[分析設計書](analysis-plan.md)、通常の回帰は[ステップ2報告](ordinary-regression-report-v2.md)を参照してください。")
writeLines(report, "docs/survey-comparison-report-v2.md", useBytes = TRUE)
paths <- c(step3_out(c("verification.csv", "independent_taylor_check.csv",
  "independent_probability_covariances.csv", "verification_warnings.csv")), "docs/survey-comparison-report-v2.md")
m <- hash_manifest_v2(paths); m$generated_at_utc <- utc_now()
write_csv(m, step3_out("verification_output_manifest.csv"))
message("All ", length(results), " Step 3 verification checks passed. Comparison report saved.")
