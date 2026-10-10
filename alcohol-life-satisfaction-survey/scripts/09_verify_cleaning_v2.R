file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
source("scripts/cleaning_v2_helpers.R")
results <- list()
check <- function(name, condition) {
  passed <- isTRUE(condition)
  results[[length(results) + 1L]] <<- data.frame(check = name, passed = passed)
  write_csv(do.call(rbind, results), v2_out("cleaning_verification.csv"))
  if (!passed) stop("V2 verification failed: ", name)
}
for (name in c("cleaned_data_manifest.csv", "cleaning_run_inputs.csv", "v1_preservation_manifest.csv")) {
  m <- read.csv(v2_out(name), stringsAsFactors = FALSE)
  check(paste0("hashes_", name), all(vapply(seq_len(nrow(m)), function(i)
    digest::digest(file = m$path[i], algo = "sha256") == m$sha256[i], logical(1))))
}
d <- readRDS(v2_data("module_clean.rds")); s <- readRDS(v2_data("analysis_complete_cases.rds"))
full <- readRDS(v2_data("survey_design_full.rds"))
invisible(loadNamespace("survey"))
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
domain <- subset(full, analysis_eligible)
check("full_design_before_domain", identical(d$record_id[d$design_eligible], full$variables$record_id))
check("saved_analysis_equals_survey_domain", identical(s, domain$variables))
check("unique_state_seqno", !anyDuplicated(d$record_id))
check("common_analysis_no_missing_predictor_or_outcome", all(complete.cases(s[v2_analysis_variables])))

# Examples independently specify expected answers at every frequency and quantity boundary.
f <- c(888, 101, 107, 201, 203, 204, 207, 208, 215, 216, 230, 777, 999, NA, 100, 108, 231)
example <- derive_exposures_v2(f, rep(1, length(f)) * ifelse(f %in% 888, NA, 1),
                              rep(NA_real_, length(f)), rep(NA_real_, length(f)))
check("frequency_boundary_numeric_examples", isTRUE(all.equal(example$drink_days_30,
  c(0, 30/7, 30, 1, 3, 4, 7, 8, 15, 16, 30, rep(NA_real_, 6)))))
check("frequency_boundary_category_examples", identical(as.character(example$frequency_group),
  c("None_30_days", "over3_to7", "over15_to30", "1_to_3", "1_to_3", "over3_to7",
    "over3_to7", "over7_to15", "over7_to15", "over15_to30", "over15_to30", rep(NA_character_, 6))))
q <- c(1, 2, 3, 4, 5, 76, 77, 88, 99, NA, NA, 1, NA, 88)
example <- derive_exposures_v2(c(rep(201, 10), 888, 888, 888, 777), q,
  c(rep(NA_real_, 12), 1, NA), rep(NA_real_, length(q)))
check("quantity_boundary_category_examples", identical(as.character(example$quantity_group),
  c("1", "2", "3_to_4", "3_to_4", "5_or_more", "5_or_more", rep(NA_character_, 4),
    "None_30_days", NA_character_, NA_character_, NA_character_)))
check("non_drinker_numeric_average_undefined", all(is.na(example$average_drinks_drinking_day[11:14])))
check("quantity_88_held_without_reclassifying_drinker",
  example$drinking_status[8] == "current" && example$quantity_classification_status[8] == "drinker_reported_none_hold")
check("positive_quantity_or_binge_conflicts_with_non_drinker", identical(which(example$alcohol_contradictory), c(12L, 13L)))
max_example <- derive_exposures_v2(c(888, 201), c(NA, 4), c(NA, 88), c(1, 2))
check("maximum_conflict_only_for_non_drinker", identical(max_example$alcohol_contradictory, c(TRUE, FALSE)) &&
        as.character(max_example$quantity_group[2]) == "3_to_4")
check("structural_skip_distinguished_from_asked_missing", identical(
  raw_response_status(c(NA, NA, 77, 99, 88), valid = c(1:76, 88), dont_know = 77, refused = 99,
    skip = c(TRUE, FALSE, FALSE, FALSE, FALSE)), c("structural_skip", "blank", "dont_know", "refused", "valid")))

# Re-read all selected raw fields and weights. Also repeat version/main comparisons on all version records.
sources <- read.csv("config/module_sources.csv", stringsAsFactors = FALSE)
source_comparisons <- list()
for (id in unique(sources$source)) {
  info <- verified_xpt(id)
  use <- sources[sources$source == id, , drop = FALSE]; weight_var <- unique(use$weight)
  raw_all <- read_raw(info$path, c(v2_raw_variables, weight_var))
  if (id == "llcp_main") {
    main <- raw_all; main_key <- record_key(main)
  } else {
    at <- match(record_key(raw_all), main_key)
    check(paste0("all_version_ids_in_main_", id), !anyNA(at) && !anyDuplicated(record_key(raw_all)))
    for (v in setdiff(v2_raw_variables, c("_STATE", "SEQNO"))) {
      a <- raw_all[[v]]; b <- main[[v]][at]
      same <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
      both <- !is.na(a) & !is.na(b)
      source_comparisons[[length(source_comparisons) + 1L]] <- data.frame(source = id, variable = v,
        compared_records = nrow(raw_all), different_records = sum(!same), both_observed_different = sum(a[both] != b[both]))
      check(paste0("version_values_", id, "_", v), if (v == "LSATISFY")
        all(same | (is.na(b) & !is.na(a))) else all(same))
    }
  }
  raw <- raw_all[raw_all[["_STATE"]] %in% use$state_code, , drop = FALSE]
  clean <- d[d$source == id, , drop = FALSE]
  check(paste0("raw_records_", id), identical(record_key(raw), clean$record_id))
  check(paste0("raw_fields_", id), all(vapply(v2_raw_variables, function(v)
    identical(raw[[v]], clean[[v]]), logical(1))))
  check(paste0("raw_weights_", id), identical(raw[[weight_var]], clean$weight_original))
  message("V2 verified against original ZIP and main/version: ", id)
}
write_csv(do.call(rbind, source_comparisons), v2_out("source_variable_comparison.csv"))

check("outcome_mapping", identical(d$life_dissatisfied,
  ifelse(d$LSATISFY %in% 1:2, 0L, ifelse(d$LSATISFY %in% 3:4, 1L, NA_integer_))))
week <- d$ALCDAY4 %in% 101:107; month <- d$ALCDAY4 %in% 201:230
none <- d$ALCDAY4 %in% 888 & !d$alcohol_contradictory
current <- d$ALCDAY4 %in% c(101:107, 201:230)
check("weekly_days_no_rounding", all(abs(d$drink_days_30[week] - (d$ALCDAY4[week] - 100) * 30 / 7) < 1e-12))
check("monthly_days", all(d$drink_days_30[month] == d$ALCDAY4[month] - 200))
check("none_days_zero_both_categories_defined", all(d$drink_days_30[none] == 0) &&
  all(as.character(d$frequency_group[none]) == "None_30_days") &&
  all(as.character(d$quantity_group[none]) == "None_30_days"))
check("none_numeric_quantity_undefined", all(is.na(d$average_drinks_drinking_day[none])))
check("unknown_frequency_not_non_drinker", all(is.na(d$frequency_group[d$drinking_status == "unknown"])) &&
  all(is.na(d$quantity_group[d$drinking_status == "unknown"])))
check("valid_numeric_quantity_unchanged", identical(d$average_drinks_drinking_day[current & d$AVEDRNK3 %in% 1:76],
                                                    d$AVEDRNK3[current & d$AVEDRNK3 %in% 1:76]))
check("quantity_88_excluded_not_non_drinker", all(!d$analysis_eligible[current & d$AVEDRNK3 %in% 88]) &&
  all(is.na(d$quantity_group[current & d$AVEDRNK3 %in% 88])))
check("fixed_category_order_and_reference", identical(levels(s$frequency_group), v2_frequency_levels) &&
  identical(levels(s$quantity_group), v2_quantity_levels))
expected <- d$design_eligible & d$adult_eligible & !d$alcohol_contradictory & d$LSATISFY %in% 1:4 &
  (none | (current & d$AVEDRNK3 %in% 1:76))
check("common_eligibility_without_background_or_binge_requirement", identical(d$analysis_eligible, expected))
check("exclusion_reasons_match_eligibility", identical(d$exclusion_reason == "included", d$analysis_eligible))
flow <- read.csv(v2_out("cleaning_flow.csv"), stringsAsFactors = FALSE)
check("exclusion_totals", sum(flow$excluded) + nrow(s) == nrow(d) && tail(flow$remaining, 1) == nrow(s))
check("no_unknown_codes", nrow(read.csv(v2_out("unexpected_codes.csv"))) == 0L)
pooling <- read.csv(v2_out("weight_pooling.csv"), stringsAsFactors = FALSE)
for (i in seq_len(nrow(pooling))) {
  state_at <- d[["_STATE"]] == pooling$state_code[i]; version_at <- state_at & d$source == pooling$source[i]
  fraction <- sum(version_at) / sum(state_at)
  check(paste0("full_counts_weight_pooling_", pooling$source[i], "_", pooling$state_code[i]),
    abs(pooling$pooling_fraction[i] - fraction) < 1e-12 &&
      all(abs(d$weight[version_at] / d$weight_original[version_at] - fraction) < 1e-12))
}
check("state_fractions_sum_to_one", all(abs(tapply(pooling$pooling_fraction, pooling$state_code, sum) - 1) < 1e-12))
check("positive_finite_design_weights", all(is.finite(full$variables$weight) & full$variables$weight > 0))
check("psu_ids_do_not_span_strata", !anyDuplicated(unique(full$variables[c("psu_id", "stratum_id")])$psu_id))
old_d <- readRDS("data/processed/module_clean.rds")
check("same_full_population_and_weights_as_v1", identical(d$record_id, old_d$record_id) && identical(d$weight, old_d$weight))

# Check model support without fitting either regression or calculating group percentages.
matrix_checks <- list()
for (name in names(v2_model_formulas)) {
  formula <- v2_model_formulas[[name]]
  x <- model.matrix(stats::delete.response(stats::terms(formula)), data = s)
  rank <- qr(x)$rank
  predictor <- all.vars(formula)[2]
  check(paste0("single_predictor_full_rank_", name), length(all.vars(formula)) == 2L && ncol(x) == 5L && rank == ncol(x))
  check(paste0("both_outcomes_every_category_", name), all(table(s[[predictor]], s$life_dissatisfied) > 0))
  matrix_checks[[name]] <- data.frame(model = name, records = nrow(x), columns = ncol(x), rank = rank)
}
write_csv(do.call(rbind, matrix_checks), v2_out("model_matrix_check.csv"))
writeLines(capture.output(sessionInfo()), v2_out("cleaning_sessionInfo.txt"), useBytes = TRUE)

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
md_table <- function(x, headers = names(x)) c(
  paste0("| ", paste(headers, collapse = " | "), " |"),
  paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
  apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
reason_ja <- c(selected_module = "選定した州・質問票版の全レコード", invalid_design = "重み・層・PSUが無効",
  invalid_age = "年齢が対象外・不明", contradictory_alcohol = "飲酒なしと正の量・回数の不整合",
  unknown_alcohol_frequency = "飲酒頻度が不明・拒否・空欄", drinker_quantity_88_hold = "飲酒日数あり・平均量はなし（88）：分類保留",
  missing_drinker_quantity = "飲酒者の平均量が不明・拒否・空欄", missing_life_satisfaction = "生活満足度が不明・拒否・空欄")
flow_table <- data.frame(reason = unname(reason_ja[flow$reason]), excluded = fmt_n(flow$excluded), remaining = fmt_n(flow$remaining))
groups <- read.csv(v2_out("analysis_group_counts.csv"), stringsAsFactors = FALSE)
group_table <- function(v) {
  x <- groups[groups$variable == v, ]
  md_table(data.frame(group = x$label, records = fmt_n(x$Freq)), c("カテゴリ", "解析対象の人数"))
}
status <- read.csv(v2_out("raw_response_status.csv"), stringsAsFactors = FALSE)
quantity_status <- status[status$variable == "AVEDRNK3" & status$population == "current_drinkers", ]
status_ja <- c(valid = "1〜76の有効値", reported_none = "88：なし（分類保留）", dont_know = "77：不明",
               refused = "99：拒否", blank = "空欄")
quantity_table <- data.frame(status = unname(status_ja[quantity_status$response_status]), records = fmt_n(quantity_status$records))
patterns <- read.csv(v2_out("consistency_patterns.csv"), stringsAsFactors = FALSE)
pattern_n <- function(name, column = "module_records") patterns[[column]][match(name, patterns$pattern)]
design_info <- read.csv(v2_out("cleaning_design_checks.csv"))
multi <- pooling[pooling$pooling_fraction < 1, ]; multi <- multi[order(multi$state_code, multi$source), ]
pool_table <- data.frame(state = multi$state_name, source = multi$source, records = fmt_n(multi$records), fraction = sprintf("%.8f", multi$pooling_fraction))
report <- c(
  "# ステップ1：新設計2.0のデータ精査・クリーニング結果", "",
  paste0("確認日時：", format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Tokyo"), "（日本時間）。回帰は未実行。"), "",
  paste0("BRFSS 2023の生活満足度モジュールを使い、飲酒頻度と飲酒日の平均量を別々の単純なモデルで比較するデータを確定しました。39州とDC、43の州・版の組み合わせから", fmt_n(nrow(d)),
    "件を保持し、共通の解析対象は", fmt_n(nrow(s)), "人です。旧版1.3の111,202人と同じ対象ではありません。"), "",
  paste0("共通対象には過去30日に飲酒なしの", fmt_n(sum(s$drinking_status == "none_30_days")), "人を含みます。飲酒者は", fmt_n(sum(s$drinking_status == "current")),
    "人です。人数はすべて重み付け前の実レコード数です。両モデル・両推定方法でこの同じ対象者を使います。"), "",
  "## 1. この問いに使える項目", "",
  "| 役割 | 項目 | 読み方 |", "| --- | --- | --- |",
  "| 評価項目 | LSATISFY | 非常に満足・満足=0、不満・非常に不満=1。4段階の元回答も保存 |",
  "| 頻度 | ALCDAY4 | 週1〜7日の回答を×30/7、月1〜30日の回答はそのまま。888は過去30日に飲酒なし |",
  "| 量 | AVEDRNK3 | 過去30日の飲酒日に平均何ドリンク飲んだか。1回の機会の量・全日平均・30日の総量とは異なる |", "",
  "非飲酒者の量の空欄は正常な質問スキップです。量カテゴリを「過去30日に飲酒なし」とし、飲酒日の数値平均には0を補いません。頻度不明を0日へ置き換えません。背景変数と多量飲酒回数の欠測は主分析の除外条件にしていません。", "",
  "[CDC公式質問票](https://www.cdc.gov/brfss/questionnaires/pdf-ques/2023-BRFSS-Questionnaire-508.pdf#page=40)と[コードブック](https://www.cdc.gov/brfss/annual_data/2023/zip/codebook23_llcp-v2-508.zip)で質問文・特殊コード・スキップを確認しました。ドリンク数を日本のグラス数に置き換えません。", "",
  "## 2. 確定した対象と除外", "",
  md_table(flow_table, c("処理・理由", "この段階で除外", "残った人数")), "",
  "上から順に適用し、最初に該当した理由を1人につき1つ記録しています。欠測が重なるため、個別項目の欠測件数の合計とは異なります。頻度だけのモデルも量を分類できる共通対象に限定します。", "",
  paste0("現在飲酒者", fmt_n(sum(current)), "人の平均量の元回答は次のとおりです。生活満足度の有効回答を要求する前の人数です。"), "",
  md_table(quantity_table, c("平均量の元回答", "人数")), "",
  "88はコードブックにある「なし」という回答です。飲酒日数がある回答者を非飲酒者へ振り分ける根拠にはせず、量の分類を保留して共通対象から除きました。明確な誤回答と断定したわけではありません。", "",
  paste0("所得などの背景項目に欠測がある", fmt_n(pattern_n("background_item_missing", "retained_analysis_records")),
    "人、飲酒者で多量飲酒回数が欠測の", fmt_n(pattern_n("current_missing_binge", "retained_analysis_records")), "人も、必要な生活満足度・量・頻度が有効なら含まれています。両者には重なりがあります。"), "",
  "## 3. 結果を見る前に固定した分類と人数", "",
  "### 頻度", "", group_table("frequency_group"), "",
  "週回答は丸めずに分類しました。週1日の回答は30/7日となり、「3日超〜7日」に入ります。", "",
  "### 飲酒日の平均量", "", group_table("quantity_group"), "",
  "参照カテゴリはいずれも「過去30日に飲酒なし」です。分類は表示上の区切りであり、安全・危険の基準ではありません。不満割合や関連の方向・大きさはこの段階で推定していません。", "",
  "## 4. 整合性と有効な大きい値の扱い", "",
  paste0("飲酒なしと正の平均量・最大量・多量飲酒回数が同時に記録された例は", fmt_n(pattern_n("none_with_positive_answer")), "件でした。未知コードと回答者IDの重複も0件でした。"), "",
  paste0("平均量20〜76ドリンクの有効な元回答は", fmt_n(pattern_n("drinker_quantity_20_or_more")), "件あり、生活満足度などの条件を満たす", fmt_n(pattern_n("drinker_quantity_20_or_more", "retained_analysis_records")),
    "件を残しました。極端な値の自己申告精度を保証するものではありませんが、コードブックの有効範囲内の値を結果に合わせて除外・上限置換しません。平均5以上のカテゴリには広い量の範囲が含まれます。"), "",
  paste0("飲酒日の平均量が1回の最大量を上回る例は", fmt_n(pattern_n("daily_average_above_max_per_occasion")), "件、多量飲酒回数が飲酒日数を上回る例は", fmt_n(pattern_n("binge_times_above_drinking_days")),
    "件でした。日数・1日平均・1回の機会・回数は異なる単位なので、この大小関係だけを除外理由にはしません。件数を`outputs/v2/consistency_patterns.csv`に残しました。"), "",
  paste0("算出済み週飲酒量_DRNKWK2が0でも飲酒日数のある回答が", fmt_n(pattern_n("computed_weekly_zero_current")), "件ありました。公式算出規則はAVEDRNK3=88も0とします。算出変数の0だけで非飲酒者と分類せず、ALCDAY4を使います。"), "",
  "## 5. ファイル・重み・surveyの準備", "",
  "元ZIPのSHA-256と展開XPTを再確認し、新版に残した全回答者の元項目・元重みを再読込と照合しました。全版別回答者についても主ファイルと再照合し、飲酒量・頻度・設計情報は一致しました。生活満足度の差は「主ファイルが空欄、版別に値がある」ものだけでした。公式の州・版・重みの指定に従って採用しています。", "",
  "主ファイル350列と版別345列、公開ページの更新表記とのずれは[旧報告](cleaning-report.md)に記録しています。差の原因や更新版の統一を確認できたとは扱いません。使用ファイルは保存したURL・取得日時・ハッシュで固定しています。", "",
  "複数版を採用する州では、欠測や飲酒で絞る前の人数の比率を元重みに掛けました。州を等しく重み付けしていません。", "",
  md_table(pool_table, c("州", "採用元", "全件数", "元重みに掛ける比率")), "",
  paste0("全", fmt_n(nrow(full$variables)), "件でsurveyの設計を作り、`subset(full, analysis_eligible)`で共通対象を指定します。通常の回帰用データとsurveyのドメインは、対象者IDと全変数が一致しました。全体は", fmt_n(design_info$full_strata),
    "層、単独PSUの層は", fmt_n(design_info$full_lonely_strata), "です。共通対象の非空層は", fmt_n(design_info$analysis_nonempty_strata), "、1PSUだけになる層は", fmt_n(design_info$analysis_single_psu_strata), "、設計自由度は", fmt_n(design_info$analysis_degrees_of_freedom), "です。"), "",
  "層・PSUは州コードと結合し、ネストを指定しました。有限母集団補正は指定せず、`survey.lonely.psu=\"adjust\"`と`survey.adjust.domain.lonely=TRUE`を使います。", "",
  "## 6. 成果物と再実行", "",
  "- `data/processed/v2/module_clean.rds`：全レコード、元回答、分類、解析フラグ、除外理由。",
  "- `data/processed/v2/analysis_complete_cases.rds`：2モデルと通常の回帰で使う共通対象。",
  "- `data/processed/v2/survey_design_full.rds`：全対象から作ったsurvey設計。surveyをロードしてからsubsetする。",
  "- `outputs/v2/`：除外、元コード、質問スキップ、カテゴリ人数、州・版、重み、整合性、モデルの事前確認、SHA-256。",
  "- `scripts/cleaning_v2_helpers.R`、`08_clean_data_v2.R`、`09_verify_cleaning_v2.R`：新版の再現コード。", "",
  paste0("別プロセスで", length(results), "項目の検証がすべて通りました。固定した2モデルの説明変数行列は各5列でフルランク、全カテゴリに不満あり・なしの両方がありました。回帰の推定は行っていません。旧版のコード・データ・確認記録はハッシュも変わっていません。"), "",
  "既存の環境・公式アセット・採用元の確認記録がある場合：", "", "```sh",
  "Rscript scripts/08_clean_data_v2.R", "Rscript scripts/09_verify_cleaning_v2.R", "```", "",
  "初めて準備する場合はREADMEの環境・取得・確認手順に続けて、`04_audit_sources.R`、旧版保存用の`05_clean_data.R`・`06_verify_cleaning.R`、`07_profile_redesign.R`を実行してから新版へ進みます。未知コード、元ファイルのハッシュ不一致、版間の想定外の差は停止条件です。", "",
  "## 7. 次のステップと解釈の範囲", "",
  "ステップ1は新版2.0で完了しました。ステップ2の通常の回帰、ステップ3のsurveyによる推定・比較、ステップ4の記事は未着手です。次は共通対象を使い、頻度のみ・量のみの2つのglmを実行します。", "",
  "このデータは群による生活への不満割合の違いを調べるために使えます。年齢などの構成の違いは残ります。禁酒・生涯非飲酒・以前は飲んでいた人を区別できず、飲酒行動を変えた個人の変化や飲酒の因果効果、量と頻度の独立した効果は示しません。選定した39州とDCと項目欠測のない対象に限定され、全米・日本の代表値とは扱いません。"
)
writeLines(report, "docs/cleaning-report-v2.md", useBytes = TRUE)
message("All ", length(results), " v2 verification checks passed. Report saved; no regression run.")
