# 飲酒の頻度・量と生活満足度：glmとsurveyの比較（BRFSS 2023）

Qiitaの記事2本で使ったRのコード、図、結果の表です。

- 記事1：禁酒で生活は劇的に良くなる？ 公開データをRで調べて、言えることを考えた
- 記事2：Rのglmとsurveyを同じデータ・同じ式で比べてみた：飲酒と生活満足度のBRFSSデータで

（記事のURLは公開後に追記します）

## やっていること

アメリカのBRFSS 2023のうち、生活満足度の質問を実施した39州とワシントンDCのデータを使い、過去30日の飲酒頻度と、飲酒日の平均量のカテゴリごとに、生活に不満と答えた人の割合を比べています。

同じ255,168人・同じ式で、重みを使わない`glm`と、調査の重み・層・PSUを反映した`survey::svyglm`を実行し、各カテゴリの不満割合と、「過去30日に飲酒なし」との差を95%信頼区間つきで並べています。背景変数は調整しておらず、飲み方の違う人たちの間の関連を見るものです。

## フォルダ構成

```text
scripts/   Rスクリプト（00〜13を番号順に実行）
config/    データの取得元（assets.csv）と、州・質問票の版・重みの対応（module_sources.csv）
docs/      設計書（analysis-plan.md）。実行すると、検証レポートもここに作られます
figures/   記事で使った図
results/   記事の表の元になった数値（comparison.csv。丸めていない値）
renv.lock  パッケージのバージョン
data/      実行時にデータが保存されます（リポジトリには含めていません）
```

## データ

データはリポジトリに含めていません。`scripts/01_fetch_assets.R`が、CDCの公開ページから次のファイルを取得します（取得元のURLは`config/assets.csv`にあります）。

- 2023年版の主ファイル（`LLCP2023XPT.zip`）と、質問票の版ごとの3ファイル（`LLCP23V1_XPT.zip`〜`LLCP23V3_XPT.zip`）
- 質問票、コードブック、モジュールの重みの説明資料など

データのページ：

- [BRFSS 2023年版](https://www.cdc.gov/brfss/annual_data/annual_2023.html)
- [質問票の版ごとのデータ](https://www.cdc.gov/brfss/annual_data/2023/llcp_multiq.html)

## 実行環境

R 4.6.1、survey 4.5、haven 2.5.5、renv 1.3.1（macOSのHomebrew版Rで実行しました）。パッケージは`renv`で、このフォルダの中に入ります。図の日本語フォントには、macOSのヒラギノを使っています。

## 実行手順

このフォルダをカレントディレクトリにして、番号順に実行します。

```sh
Rscript --vanilla scripts/00_setup.R    # renvで環境を用意し、パッケージのバージョンを固定
Rscript scripts/01_fetch_assets.R       # CDCの公式データ・資料を取得し、取得元とハッシュを記録
Rscript scripts/02_check_environment.R  # 生成データでglm・svyglmなどの動作を確認
Rscript scripts/03_check_assets.R       # 必要な変数、対象州・版の生活満足度と重みの収録を確認
Rscript scripts/04_audit_sources.R      # 主ファイルと版別ファイルの照合
Rscript scripts/05_clean_data.R         # 初期設計での整形（08がこの保存物を参照）
Rscript scripts/06_verify_cleaning.R    # 05の検証
Rscript scripts/07_profile_redesign.R   # 飲酒量の項目の収録を点検
Rscript scripts/08_clean_data_v2.R      # 頻度・量の分類、共通対象、survey設計の作成
Rscript scripts/09_verify_cleaning_v2.R # クリーニングの検証
Rscript scripts/10_fit_glm_v2.R         # 通常のglm（2モデル）と図
Rscript scripts/11_verify_glm_v2.R      # glmの結果の検証
Rscript scripts/12_fit_survey_v2.R      # svyglm（2モデル）と比較表・図
Rscript scripts/13_verify_survey_v2.R   # surveyの結果の検証
```

05・06は、分析の初期設計（`docs/analysis-plan-v1.3.md`）で作った整形と検証です。記事の結果には使っていませんが、08がこの保存物のハッシュを確認するため、今のスクリプトでは実行が必要です。

## 記事の数値・図と出力の対応

- 記事の表：`results/comparison.csv`。実行すると、同じものが`outputs/v2/step3/comparison.csv`に作られます。
- 図：`figures/`。実行すると、`outputs/v2/step2/`と`outputs/v2/step3/`に作られます。
