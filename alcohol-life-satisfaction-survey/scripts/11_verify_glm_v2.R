file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
source("scripts/cleaning_v2_helpers.R")
source("scripts/regression_v2_helpers.R")
results <- list()
check <- function(name, condition) {
  passed <- isTRUE(condition)
  results[[length(results) + 1L]] <<- data.frame(check = name, passed = passed)
  write_csv(do.call(rbind, results), step2_out("verification.csv"))
  if (!passed) stop("Step 2 verification failed: ", name)
}
near <- function(a, b, tolerance = 1e-7) isTRUE(all.equal(a, b, tolerance = tolerance, check.attributes = FALSE))
for (name in c("run_inputs.csv", "output_manifest.csv", "step1_preservation_manifest.csv")) {
  m <- read.csv(step2_out(name), stringsAsFactors = FALSE)
  check(paste0("hashes_", name), all(vapply(seq_len(nrow(m)), function(i)
    digest::digest(file = m$path[i], algo = "sha256") == m$sha256[i], logical(1))))
}
old <- read.csv(v2_out("v1_preservation_manifest.csv"), stringsAsFactors = FALSE)
check("old_v1_records_preserved", all(vapply(seq_len(nrow(old)), function(i)
  digest::digest(file = old$path[i], algo = "sha256") == old$sha256[i], logical(1))))
S <- readRDS(v2_data("analysis_complete_cases.rds"))
saved <- readRDS(step2_out("glm_models.rds"))
e <- read.csv(step2_out("estimates.csv"), stringsAsFactors = FALSE)
diagnostics <- read.csv(step2_out("model_diagnostics.csv"), stringsAsFactors = FALSE)
check("saved_model_definition_version", saved$definition_version == "2.0")
check("same_common_analysis_ids", identical(saved$analysis_record_ids, S$record_id))
check("both_planned_models_present", identical(names(saved$models), names(v2_model_formulas)))
check("no_fit_warnings", nrow(read.csv(step2_out("warnings.csv"))) == 0L)
check("diagnostics_all_converged_without_boundary", all(diagnostics$converged & !diagnostics$boundary))
z <- stats::qnorm(0.975)
check("normal_critical_value", identical(saved$critical_value, z))
independent <- list()
for (name in names(v2_model_formulas)) {
  fit <- saved$models[[name]]; calculation <- saved$calculations[[name]]
  formula <- v2_model_formulas[[name]]; predictor <- all.vars(formula)[2]
  at <- e$model == name; model_estimates <- e[at, ]
  levels_expected <- levels(S[[predictor]])
  counts <- table(S[[predictor]], S$life_dissatisfied)
  n <- as.numeric(rowSums(counts)); k <- as.numeric(counts[, "1"])
  probability <- k / n
  link <- stats::qlogis(probability)
  # Independent closed-form results for a binomial model with one categorical predictor.
  # These formulas are specific to the unweighted model, not a survey variance estimate.
  link_variance <- 1 / k + 1 / (n - k)
  probability_variance <- probability * (1 - probability) / n
  delta <- probability - probability[1L]
  delta_variance <- probability_variance + probability_variance[1L]
  delta_variance[1L] <- 0
  independent[[name]] <- data.frame(model = name, level = levels_expected, records = n, dissatisfied_records = k,
    probability = probability, logit_variance = link_variance,
    probability_ci_low = stats::plogis(link - z * sqrt(link_variance)),
    probability_ci_high = stats::plogis(link + z * sqrt(link_variance)),
    difference_pp = 100 * delta, difference_se_pp = 100 * sqrt(delta_variance),
    difference_ci_low_pp = 100 * (delta - z * sqrt(delta_variance)),
    difference_ci_high_pp = 100 * (delta + z * sqrt(delta_variance)))
  check(paste0("same_model_rows_", name), identical(rownames(fit$model), rownames(S)))
  check(paste0("same_outcome_and_predictor_", name), identical(fit$model$life_dissatisfied, S$life_dissatisfied) &&
    identical(fit$model[[predictor]], S[[predictor]]))
  check(paste0("one_unadjusted_predictor_", name), identical(all.vars(stats::formula(fit)), all.vars(formula)) &&
    identical(names(fit$model), c("life_dissatisfied", predictor)))
  check(paste0("no_survey_weights_or_offset_", name), all(fit$prior.weights == 1) && is.null(fit$offset) &&
    fit$family$family == "binomial" && fit$family$link == "logit")
  check(paste0("common_n_complete_rank_", name), stats::nobs(fit) == nrow(S) && fit$rank == 5L && is.null(fit$na.action))
  check(paste0("every_category_both_outcomes_no_separation_", name), all(counts > 0))
  check(paste0("fixed_reference_and_category_order_", name), identical(model_estimates$level, levels_expected) &&
    identical(fit$xlevels[[predictor]], levels_expected) && levels_expected[1L] == "None_30_days")
  check(paste0("raw_counts_preserved_", name), near(model_estimates$records, n, 1e-12) &&
    near(model_estimates$dissatisfied_records, k, 1e-12))
  check(paste0("predicted_probabilities_equal_counts_", name), max(abs(model_estimates$probability - probability)) < 1e-10)
  expected_coefficients <- c(link[1L], link[-1L] - link[1L])
  check(paste0("coefficients_equal_closed_form_", name), near(unname(stats::coef(fit)), expected_coefficients))
  check(paste0("logit_covariance_equal_closed_form_", name), near(calculation$logit_covariance, diag(link_variance)))
  check(paste0("probability_variance_equal_closed_form_", name), near(diag(calculation$probability_covariance), probability_variance))
  check(paste0("delta_covariance_equal_closed_form_", name), near(diag(calculation$difference_covariance), delta_variance))
  check(paste0("probability_intervals_equal_closed_form_", name),
    near(model_estimates$probability_ci_low, independent[[name]]$probability_ci_low) &&
    near(model_estimates$probability_ci_high, independent[[name]]$probability_ci_high))
  check(paste0("difference_and_intervals_equal_closed_form_", name),
    near(model_estimates$difference_pp, independent[[name]]$difference_pp) &&
    near(model_estimates$difference_se_pp, independent[[name]]$difference_se_pp) &&
    near(model_estimates$difference_ci_low_pp, independent[[name]]$difference_ci_low_pp) &&
    near(model_estimates$difference_ci_high_pp, independent[[name]]$difference_ci_high_pp))
  predicted <- stats::predict(fit, newdata = calculation$newdata, type = "link", se.fit = TRUE)
  check(paste0("predictions_match_R_predict_", name), near(unname(predicted$fit), model_estimates$logit) &&
    near(unname(predicted$se.fit), model_estimates$logit_se))
  check(paste0("probability_intervals_within_zero_one_", name),
    all(model_estimates$probability_ci_low > 0 & model_estimates$probability_ci_high < 1 &
        model_estimates$probability_ci_low < model_estimates$probability &
        model_estimates$probability < model_estimates$probability_ci_high))
  check(paste0("reference_difference_exactly_zero_", name), all(unlist(model_estimates[1L,
    c("difference_pp", "difference_se_pp", "difference_ci_low_pp", "difference_ci_high_pp")]) == 0))
  image_path <- step2_out(paste0(name, "_glm.png"))
  signature <- readBin(image_path, "raw", n = 8L)
  check(paste0("png_figure_saved_", name), identical(signature, as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))) &&
    file.info(image_path)$size > 10000)
  message("Verified ordinary glm against binomial counts, covariance, intervals, and R predict: ", name)
}
freq <- e[e$model == "frequency", ]; quantity <- e[e$model == "quantity", ]
check("identical_non_drinker_reference_in_both_models", abs(freq$probability[1] - quantity$probability[1]) < 1e-10 &&
  near(freq$probability_ci_low[1], quantity$probability_ci_low[1], 1e-7) &&
  near(freq$probability_ci_high[1], quantity$probability_ci_high[1], 1e-7))
write_csv(do.call(rbind, independent), step2_out("independent_binomial_check.csv"))

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
md_table <- function(x, headers = names(x)) c(
  paste0("| ", paste(headers, collapse = " | "), " |"),
  paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
  apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
result_table <- function(x) {
  delta_interval <- sprintf("%+.1f（%+.1f〜%+.1f）", x$difference_pp, x$difference_ci_low_pp, x$difference_ci_high_pp)
  delta_interval[x$reference] <- "基準：0.0"
  md_table(data.frame(group = x$label, n = fmt_n(x$records),
    dissatisfied = fmt_n(x$dissatisfied_records),
    percent = sprintf("%.1f（%.1f〜%.1f）", x$dissatisfaction_percent,
      x$dissatisfaction_ci_low_percent, x$dissatisfaction_ci_high_percent), difference = delta_interval),
    c("カテゴリ", "人数", "不満あり", "不満割合 %（95%CI）", "飲酒なしとの差 pp（95%CI）"))
}
freq_description <- if (all(freq$difference_pp[-1] < 0))
  "今回の回答者では、飲酒頻度の4カテゴリはいずれも過去30日に飲酒なしのカテゴリより、不満割合が低くなりました。飲酒頻度が増えるほど不満割合が一方向に増えるという並びにはなっていません。" else
  "飲酒頻度ごとの不満割合と非飲酒カテゴリとの差は、予定した全カテゴリを表と図に示しました。"
quantity_description <- if (all(diff(quantity$probability[-1]) > 0))
  "飲酒者のカテゴリでは、飲酒日の平均量が大きいカテゴリほど不満割合が高くなりました。これは平均量の異なる人たちの比較です。" else
  "飲酒日の平均量ごとの不満割合と非飲酒カテゴリとの差は、予定した全カテゴリを表と図に示しました。"
diag_table <- data.frame(model = c("頻度", "量"), n = fmt_n(diagnostics$records),
  converged = ifelse(diagnostics$converged, "はい", "いいえ"), iterations = diagnostics$iterations,
  rank = diagnostics$rank, warnings = diagnostics$warning_count,
  error = sprintf("%.2e", diagnostics$maximum_probability_aggregation_error))
report <- c(
  "# ステップ2：通常の回帰の結果（設計2.0）", "",
  paste0("確認日時：", format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Tokyo"), "（日本時間）。surveyによる実データの推定は未実行。"), "",
  paste0("ステップ1で確定した共通対象", fmt_n(nrow(S)), "人について、飲酒頻度だけ・飲酒日の平均量だけを説明変数とする通常のロジスティック回帰を実行しました。各回答者を等しく扱い、年齢・性別・所得などの背景変数と調査重みは使っていません。対象者・分類・式は変更していません。"), "",
  "## 1. 今回の結果の読み方", "",
  "評価項目は、生活満足度の「不満・非常に不満」を合わせた不満割合です。数値が高いほど、不満と回答した人の割合が高いことを意味します。", "",
  freq_description, "", quantity_description, "",
  paste0("過去30日に飲酒なしの不満割合は", sprintf("%.1f%%", quantity$dissatisfaction_percent[1]),
    "、飲酒日に平均1ドリンクは", sprintf("%.1f%%", quantity$dissatisfaction_percent[2]),
    "、平均5ドリンク以上は", sprintf("%.1f%%", quantity$dissatisfaction_percent[5]),
    "でした。平均5以上と非飲酒カテゴリとの差は", sprintf("%+.1f", quantity$difference_pp[5]), "パーセントポイントです。"), "",
  "単純な回帰なので、これは群の背景の違いを含む関連です。「飲酒すると満足度が上がる」「量を減らすと改善する」という因果関係は、この結果から判断できません。", "",
  "## 2. 飲酒頻度だけのモデル", "", "```r",
  "glm(life_dissatisfied ~ frequency_group, data = S, family = binomial())", "```", "",
  result_table(freq), "",
  "週の日数は×30/7で換算し、丸めずに分類しています。週1日の回答は「3日超〜7日」です。", "",
  paste0("![通常の回帰：頻度](", file.path(root, step2_out("frequency_glm.png")), ")"), "",
  "## 3. 飲酒日の平均量だけのモデル", "", "```r",
  "glm(life_dissatisfied ~ quantity_group, data = S, family = binomial())", "```", "",
  result_table(quantity), "",
  "量は飲酒日に平均何ドリンク飲んだかという指標です。1回の飲酒機会の量や、30日間の総量とは区別します。非飲酒者には飲酒日の平均量を0と補完せず、独立したカテゴリを使っています。", "",
  paste0("![通常の回帰：量](", file.path(root, step2_out("quantity_glm.png")), ")"), "",
  "## 4. 単純集計と回帰の関係・信頼区間", "",
  "1つのカテゴリ変数のみのモデルなので、予測不満割合は各カテゴリの不満者数÷人数と一致します。今回は全カテゴリで一致を確認しました。回帰が背景の違いを取り除いた推定値を作ったわけではありません。", "",
  "割合の95%信頼区間は、係数と共分散からロジット尺度で正規近似の区間を求め、確率へ戻しました。割合の差は、各群と基準群の勾配の差と係数の共分散を使うデルタ法です。基準群自身との差は定義上0で、差の区間も0です。", "",
  "表示は小数1桁、CSVは計算精度のままです。差の単位ppはパーセントポイントです。例えば6.3%と8.6%の差は約2.3ppで、相対的な増加率とは異なります。区間は点ごとの95%区間で、全比較を同時に保証しません。", "",
  "これらの区間は回答者を独立とみなす通常のモデルに基づきます。BRFSSの層・PSU・調査重みを考慮した区間は、次のステップ3で同じ対象者・式について求めます。", "",
  "## 5. モデルと数値の検証", "",
  md_table(diag_table, c("モデル", "対象者数", "収束", "反復回数", "ランク", "警告", "単純集計との最大誤差")), "",
  "各カテゴリに不満あり・なしの両方があり、このカテゴリだけのモデルに完全分離はありませんでした。有限の係数と正定値の共分散、固定した参照カテゴリ、同じ対象者・目的変数、追加の欠測除外がないことを確認しました。", "",
  paste0("別プロセスで", length(results), "項目の検証がすべて通りました。回帰の係数・分散・割合・差・区間を、カテゴリ別人数と不満者数から求めた独立の二項モデルの式、およびRのpredictと照合しました。ステップ1と旧版の保存物もハッシュが一致しました。"), "",
  "## 6. 保存した成果物と再実行", "",
  "- `outputs/v2/step2/estimates.csv`：予定した全10カテゴリの人数、不満者数、推定割合、非飲酒カテゴリとの差、95%区間。",
  "- `glm_models.rds`：2モデル、対象者ID、予測行列と共分散。",
  "- `model_diagnostics.csv`、`coefficients.csv`、`coefficient_covariances.csv`：モデルの状態と係数。",
  "- `frequency_glm.png`、`quantity_glm.png`：各群の割合と非飲酒カテゴリとの差。",
  "- `verification.csv`、`independent_binomial_check.csv`：別プロセスでの照合記録。",
  "- `analysis-plan-at-fit.md`：計算前に保存した設計書。",
  "- `run_inputs.csv`、`output_manifest.csv`、`step1_preservation_manifest.csv`：コード・環境・入力と出力のハッシュ。", "",
  "新版のステップ1が完了したフォルダで：", "", "```sh",
  "Rscript scripts/10_fit_glm_v2.R", "Rscript scripts/11_verify_glm_v2.R", "```", "",
  "元データ、分類、対象者は変更せず、ステップ2の結果を再作成します。追加パッケージは導入していません。", "",
  "## 7. 言える範囲と進捗", "",
  "結果は選定した39州とDCの、解析項目が有効な回答者の関連です。全米・日本の成人の代表値とは扱いません。年齢・性別・所得などの構成の違い、逆方向の関連、自己申告、欠測による選択は残ります。「過去30日に飲酒なし」から、生涯非飲酒・禁酒・以前は飲んでいた人を区別できません。", "",
  "量と頻度を別々のモデルにしたため、頻度をそろえた量の関連や、量をそろえた頻度の関連も示していません。カテゴリ間の違いを個人の飲酒行動を変えた際の変化と読み替えません。", "",
  "| ステップ | 状態 |", "| --- | --- |", "| 1. クリーニング | 完了（設計2.0） |",
  "| 2. 通常の回帰 | 完了（2モデル・表・図・検証） |", "| 3. surveyとの比較 | 未着手 |", "| 4. 記事 | 未着手 |", "",
  "次は同じ対象者・式・参照カテゴリでsurveyの回帰を行い、推定割合と信頼区間を並べて比較します。",
  "データ・評価項目・解釈の基準は[分析設計書](analysis-plan.md)、ステップ1は[クリーニング報告](cleaning-report-v2.md)を参照してください。"
)
writeLines(report, "docs/ordinary-regression-report-v2.md", useBytes = TRUE)
verification_paths <- c(step2_out("verification.csv"), step2_out("independent_binomial_check.csv"),
                        "docs/ordinary-regression-report-v2.md")
write_csv(data.frame(path = verification_paths,
  sha256 = vapply(verification_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1)),
  generated_at_utc = utc_now()), step2_out("verification_output_manifest.csv"))
message("All ", length(results), " step 2 verification checks passed. Ordinary regression report saved.")
