file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")

results <- list()
check <- function(name, condition) {
  passed <- isTRUE(condition)
  results[[length(results) + 1L]] <<- data.frame(check = name, passed = passed)
  write_csv(do.call(rbind, results), "outputs/cleaning_verification.csv")
  if (!passed) stop("Cleaning verification failed: ", name)
}

manifest <- read.csv("outputs/cleaned_data_manifest.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(manifest))) {
  check(paste0("saved_hash_", basename(manifest$path[i])),
        digest::digest(file = manifest$path[i], algo = "sha256") == manifest$sha256[i])
}
d <- readRDS("data/processed/module_clean.rds")
s <- readRDS("data/processed/analysis_complete_cases.rds")
full <- readRDS("data/processed/survey_design_full.rds")
invisible(loadNamespace("survey")) # Register S3 methods before subsetting a saved design.
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
domain <- subset(full, analysis_eligible)
check("full_design_retains_pre_domain_records", identical(d$record_id[d$design_eligible], full$variables$record_id))
check("saved_complete_cases_equal_domain", identical(s, domain$variables))
check("unique_state_seqno", !anyDuplicated(d$record_id))

# Fixed examples catch weekly/monthly unit confusion, missing codes, and large valid binge counts.
check("frequency_boundary_examples", isTRUE(all.equal(
  drinking_days(c(101, 107, 201, 230, 777, 888, 999, NA, 100, 108, 231)),
  c(30/7, 30, 1, 30, rep(NA_real_, 7)), check.attributes = FALSE)))
check("binge_boundary_examples", identical(binary_binge(c(1, 31, 76, 77, 88, 99, NA)),
                                          c(1L, 1L, 1L, NA_integer_, 0L, NA_integer_, NA_integer_)))
check("structural_skip_not_zero", identical(
  raw_response_status(c(NA, NA, 77, 99, 88), valid = c(1:76, 88), dont_know = 77, refused = 99,
                      skip = c(TRUE, FALSE, FALSE, FALSE, FALSE)),
  c("structural_skip", "blank", "dont_know", "refused", "valid")))

# Re-read all selected records from the original ZIPs; compare every retained raw field and weight.
sources <- read.csv("config/module_sources.csv", stringsAsFactors = FALSE)
for (id in unique(sources$source)) {
  info <- verified_xpt(id)
  use <- sources[sources$source == id, , drop = FALSE]
  weight_var <- unique(use$weight)
  raw <- read_raw(info$path, c(analysis_raw_variables, weight_var))
  raw <- raw[raw[["_STATE"]] %in% use$state_code, , drop = FALSE]
  clean <- d[d$source == id, , drop = FALSE]
  check(paste0("raw_records_", id), identical(record_key(raw), clean$record_id))
  check(paste0("raw_values_", id), all(vapply(analysis_raw_variables, function(v) {
    identical(raw[[v]], clean[[v]])
  }, logical(1))))
  check(paste0("raw_weights_", id), identical(raw[[weight_var]], clean$weight_original))
  message("Verified cleaned records against original ZIP: ", id)
}

# Check the transformations on the actual saved records, independently of helper implementations.
check("outcome_mapping", identical(d$life_dissatisfied,
  ifelse(d$LSATISFY %in% 1:2, 0L, ifelse(d$LSATISFY %in% 3:4, 1L, NA_integer_))))
check("weekly_conversion", all(abs(d$drink_days_30[d$ALCDAY4 %in% 101:107] -
  (d$ALCDAY4[d$ALCDAY4 %in% 101:107] - 100) * 30 / 7) < 1e-12))
check("monthly_conversion", all(d$drink_days_30[d$ALCDAY4 %in% 201:230] ==
  d$ALCDAY4[d$ALCDAY4 %in% 201:230] - 200))
check("noncurrent_binge_remains_missing", all(is.na(d$binge[d$drinking_status != "current"])))
check("income_code_9_is_valid", all(as.character(d$income[d$INCOME3 %in% 9]) == "100000_or_more"))
check("income_special_codes_are_missing", all(is.na(d$income[d$INCOME3 %in% c(77, 99) | is.na(d$INCOME3)])))
check("age_has_fixed_13_categories", length(levels(d$age_group)) == 13L &&
  all(as.character(d$age_group[d[["_AGE80"]] == 80]) == "80+"))
check("valid_large_binge_counts_retained", all(d$binge[d$drinking_status == "current" & d$DRNK3GE5 %in% 31:76] == "Yes"))

pooling <- read.csv("outputs/weight_pooling.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(pooling))) {
  state_at <- d[["_STATE"]] == pooling$state_code[i]
  version_at <- state_at & d$source == pooling$source[i]
  expected <- sum(version_at) / sum(state_at)
  check(paste0("pooling_full_counts_", pooling$source[i], "_", pooling$state_code[i]),
        abs(pooling$pooling_fraction[i] - expected) < 1e-12 &&
        all(abs(d$weight[version_at] / d$weight_original[version_at] - expected) < 1e-12))
}
check("all_weights_positive_finite", all(is.finite(full$variables$weight) & full$variables$weight > 0))
check("psus_do_not_span_strata", !anyDuplicated(unique(full$variables[c("psu_id", "stratum_id")])$psu_id))

# Estimability check only: build the fixed predictor matrix, without fitting a regression.
predictor_formula <- stats::delete.response(stats::terms(model_formula))
x <- model.matrix(predictor_formula, data = s)
matrix_rank <- qr(x)$rank
check("fixed_model_matrix_full_rank", matrix_rank == ncol(x))
write_csv(data.frame(records = nrow(x), columns = ncol(x), rank = matrix_rank,
  zero_support_covariate_levels = sum(read.csv("outputs/covariate_support.csv")$Freq == 0 &
    read.csv("outputs/covariate_support.csv")$variable != "binge")), "outputs/model_matrix_check.csv")
check("both_groups_have_both_outcomes", all(table(s$binge, s$life_dissatisfied) > 0))
writeLines(capture.output(sessionInfo()), "outputs/cleaning_sessionInfo.txt", useBytes = TRUE)

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
md_table <- function(x, headers = names(x)) {
  c(paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
}
flow <- read.csv("outputs/cleaning_flow.csv", stringsAsFactors = FALSE)
reason_ja <- c(selected_module = "州・質問票版を選定した全レコード", invalid_design = "重み・層・PSUが無効",
  invalid_age = "年齢が対象外・不明", contradictory_alcohol = "飲酒回答が矛盾",
  no_alcohol_30_days = "過去30日に飲酒なし", unknown_alcohol_frequency = "飲酒頻度が不明・拒否・空欄",
  missing_binge = "多量飲酒の回答が欠測", missing_life_satisfaction = "生活満足度が欠測",
  missing_covariates = "調整変数が1つ以上欠測")
flow_table <- data.frame(reason = unname(reason_ja[flow$reason]), excluded = fmt_n(flow$excluded), remaining = fmt_n(flow$remaining))
missing <- read.csv("outputs/clean_variable_missingness.csv", stringsAsFactors = FALSE)
missing <- missing[missing$population == "current_drinkers", , drop = FALSE]
missing_table <- data.frame(variable = missing$variable, records = fmt_n(missing$missing_records),
                            percent = sprintf("%.1f%%", missing$missing_percent))
multi <- pooling[pooling$pooling_fraction < 1, , drop = FALSE]
multi <- multi[order(multi$state_code, multi$source), , drop = FALSE]
pool_table <- data.frame(state = multi$state_name, source = multi$source, records = fmt_n(multi$records),
                         fraction = sprintf("%.8f", multi$pooling_fraction))
design_info <- read.csv("outputs/cleaning_design_checks.csv")
matrix_info <- read.csv("outputs/model_matrix_check.csv")
schema <- read.csv("outputs/source_schema.csv", stringsAsFactors = FALSE)
schema_table <- data.frame(source = schema$source, records = fmt_n(schema$records), columns = schema$columns, zip_member_date = schema$member_date)
input_checks <- read.csv("outputs/cleaning_checks.csv")
report <- c(
  "# ステップ1：データクリーニングの結果", "",
  paste0("作成日時：", utc_now(), "（UTC）。設計版1.3。回帰はまだ実行していません。"), "",
  paste0("生活満足度モジュールの39州とDC、43の州・版の組み合わせから", fmt_n(nrow(d)),
    "件を取り込みました。過去30日の現在飲酒者は", fmt_n(sum(d$drinking_status == "current")),
    "人、解析に必要な項目がすべて有効な完全ケースは", fmt_n(nrow(s)), "人です。"), "",
  paste0("完全ケースの多量飲酒なしは", fmt_n(sum(s$binge == "No")), "人、ありは", fmt_n(sum(s$binge == "Yes")),
         "人です。人数は重み付け前の実レコード数です。関連の方向や大きさはこの段階で推定していません。"), "",
  "## 1. 採用した元ファイルと版の確認", "",
  md_table(schema_table, c("取得元ID", "全レコード数", "列数", "ZIP内の日時")), "",
  "[CDC公開ページ](https://www.cdc.gov/brfss/annual_data/annual_2023.html)は主XPTを345列・2025年2月更新と説明していますが、公式リンクから保存した実ファイルは350列です。版別XPTは345列です。ページとファイルの更新表記のずれは残り、同じ更新版に統一できたとは主張しません。元ZIPは変更せず、取得URL・日時・SHA-256で固定しています。", "",
  "主ファイルだけにある列はLNDSXBRT、CELSXBRT、BIRTHSEX、TRNSGNDR、RCSGEND1、RCSXBRTH、_CLLCPWT、_LLCPWTです。各版には、主ファイルにない版専用の2つの重みとRCSBORG1があります。今回使う列は4ファイルすべてにあります。", "",
  "版別ファイルの全回答者を_STATEとSEQNOで主ファイルに照合しました。飲酒、調整変数、層、PSUと整合性点検用の_RFBING6は完全一致しました。LSATISFYの差はすべて「主ファイルが空欄、版別に回答あり」で、双方に値がある回答の不一致はありませんでした。この差は今回版別を採用する州に限られています。", "",
  "この照合と[公式の州・版・重みの指定](https://www.cdc.gov/brfss/questionnaires/modules/category2023.htm)を採用根拠にします。主ファイルで版別州の空欄を欠測として処理せず、指定された版別ファイルから回答を取り込みました。公開ページとの列数・日時の差の原因を、この照合だけで確定したとは扱いません。", "",
  "詳細：`outputs/source_schema.csv`、`source_column_differences.csv`、`source_variable_comparison.csv`、`life_satisfaction_source_routing.csv`。", "",
  "## 2. 除外の流れ", "",
  md_table(flow_table, c("処理・理由", "この段階で除外", "残った人数")), "",
  "この表は上から順に適用するため、除外理由は重複しません。複数項目に欠測がある人も、その人が初めて除外された理由だけを数えています。", "",
  "_STATEとSEQNOによる重複、想定外のコード、飲酒なしと正の多量飲酒回数の矛盾、元回答と_RFBING6の分類の不一致はいずれも0件でした。", "",
  "## 3. 変数の整形と欠測", "",
  "評価項目は生活満足度だけです。LSATISFYの1・2を不満なし=0、3・4を不満あり=1にしました。7・9・空欄を補完していません。4段階の元回答も保持しています。", "",
  "ALCDAY4の101〜107は週の日数×30/7、201〜230は月の日数に変換しました。888は過去30日に飲酒なしであり、禁酒・生涯非飲酒とは分類していません。777・999・空欄は飲酒頻度不明です。現在飲酒者のDRNK3GE5は1〜76を多量飲酒あり、88をなし、77・99・空欄を欠測にしました。31〜76回の有効な回答も残しています。", "",
  "飲酒なし・飲酒頻度不明によるDRNK3GE5の正常な質問スキップと、現在飲酒者の欠測を分けて記録しました。現在飲酒者の空欄を0回に置き換えていません。特殊コードは変数ごとに扱い、INCOME3の9は10万ドル以上の有効カテゴリに含めました。", "",
  "調整変数の分類・飲酒頻度のスプラインは設計書どおりです。_AGE80にはCDCが補完した年齢を使用しています。今回の処理では追加の項目補完を行っていません。元の値、カテゴリの順序、回帰の参照カテゴリを保存しています。", "",
  paste0("次の欠測表の分母は現在飲酒者", fmt_n(sum(d$drinking_status == "current")), "人です。複数項目の欠測が重なるため、人数を合計して除外人数とはしません。"), "",
  md_table(missing_table, c("解析用変数", "欠測人数", "欠測割合")), "",
  sprintf("完全ケースは現在飲酒者の%.1f%%です。欠測者の除外による選択は残り、調査重みでこの欠測が自動的に補正されるわけではありません。", 100 * nrow(s) / sum(d$drinking_status == "current")), "",
  "詳細：`outputs/raw_code_counts.csv`、`raw_response_status.csv`、`clean_variable_missingness.csv`、`factor_levels.csv`。", "",
  "## 4. 重みと調査設計", "",
  "同じ州の複数版は、飲酒・欠測で絞る前の州・版別全レコード数の比率で元の版別重みを調整しました。その他の州の重みはそのままです。州ごとに合計1へ正規化していません。", "",
  md_table(pool_table, c("州", "採用元", "全レコード数", "元の重みに掛ける比率")), "",
  paste0("重み・層・PSUが有効な全", fmt_n(nrow(full$variables)), "件から調査設計を作り、その後に解析対象フラグでドメインを指定しました。通常の回帰用データとsurveyのドメインは、対象者IDだけでなく保存した全変数も一致しています。"), "",
  paste0("層は_STATE＋_STSTR、PSUは_STATE＋_PSUです。全体は", fmt_n(design_info$full_strata), "層で、単独PSUの層が", fmt_n(design_info$full_lonely_strata),
    "あります。解析対象が1つのPSUだけになる非空層は", fmt_n(design_info$analysis_single_psu_strata), "です。`survey.lonely.psu=\"adjust\"`、`survey.adjust.domain.lonely=TRUE`を使います。解析ドメインの設計自由度は", fmt_n(design_info$analysis_degrees_of_freedom), "です。"), "",
  "詳細：`outputs/weight_pooling.csv`、`cleaning_design_checks.csv`、`cleaning_state_counts.csv`。", "",
  "## 5. 保存した成果物と確認", "",
  "- `data/processed/module_clean.rds`：選定した全レコード。元の値、整形した変数、解析対象フラグ、除外理由、重み・設計情報を保持。",
  "- `data/processed/analysis_complete_cases.rds`：通常のglmで使用する完全ケース。",
  "- `data/processed/survey_design_full.rds`：全レコードから作成したsurveyの設計。読み込む前にsurveyパッケージをロードし、その後に`subset(design, analysis_eligible)`を使う。",
  "- `outputs/cleaned_data_manifest.csv`：保存したデータの件数・サイズ・SHA-256。",
  "- `outputs/cleaning_run_inputs.csv`：使用した元ZIP、設定、クリーニングコード、renv.lockのSHA-256。", "",
  paste0("処理中の", sum(input_checks$passed), "項目と、別スクリプトの", length(results),
    "項目の検証がすべて通りました。元ZIPから再読込した全対象レコードの元変数・元重み、境界値、週／月の変換、特殊コード、重み調整、保存したデータのハッシュを照合しました。"), "",
  paste0("固定した回帰式の説明変数行列は", matrix_info$columns, "列で、ランクは", matrix_info$rank,
    "です。両群に両方の目的変数値があり、共変量の全カテゴリが両群に含まれています。これは回帰前の確認であり、収束・完全分離・推定精度の確認はステップ2・3で行います。"), "",
  "確認記録：`outputs/cleaning_checks.csv`、`cleaning_verification.csv`、`model_matrix_check.csv`、`covariate_support.csv`、`analysis_group_counts.csv`、`cleaning_sessionInfo.txt`。", "",
  "## 6. 再実行", "", "```sh", "Rscript scripts/04_audit_sources.R", "Rscript scripts/05_clean_data.R", "Rscript scripts/06_verify_cleaning.R", "```", "",
  "元データのハッシュが一致しない場合や、未知コード・重複・使用変数の不一致が見つかった場合は停止します。生成データと記録は再作成しますが、元ZIPは変更しません。R・surveyのバージョンはrenv.lockで固定しています。", "",
  "ステップ1は完了です。次は同じ完全ケース・固定した回帰式で通常のglmを実行するステップ2です。"
)
writeLines(report, "docs/cleaning-report.md", useBytes = TRUE)
message("All ", length(results), " cleaning verification checks passed. No regression run.")
