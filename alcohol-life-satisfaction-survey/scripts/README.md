# 飲酒の頻度・量と生活満足度：glmとsurveyの分析スクリプト

アメリカの公開調査データBRFSS 2023を使って、飲酒の頻度・量と生活満足度の関係を、普通のロジスティック回帰（`glm`）と、調査の重み・層・PSUを反映した`survey::svyglm`で比べたときのRスクリプトです。

この分析の内容は、Qiitaの記事2本で紹介する予定です（公開したらここにリンクを追加します）。

- 記事1：禁酒で生活は劇的に良くなる？ 公開データをRで調べて、言えることを考えた
- 記事2：Rのglmとsurveyは、何をしているのか？ 同じデータ・同じ式で比べてみた

## スクリプト

| ファイル | 内容 |
| --- | --- |
| `10_fit_glm_v2.R` | 通常のglm（頻度モデル・量モデル）と図 |
| `11_verify_glm_v2.R` | glmの結果の検証（人数から計算した二項モデルの式、`predict`との照合） |
| `12_fit_survey_v2.R` | svyglm（頻度モデル・量モデル）と、glmとの比較表・図 |
| `13_verify_survey_v2.R` | surveyの結果の検証（重み付き集計、surveyの関数を使わずに計算した分散などとの照合） |
| `regression_v2_helpers.R` | 各カテゴリの不満割合と、基準カテゴリとの差の信頼区間 |
| `survey_v2_helpers.R` | glmとsurveyの比較表と図 |
| `cleaning_v2_helpers.R` | 頻度・量のカテゴリとモデル式の定義 |
| `data_helpers.R` | 頻度の日数換算、survey設計の作成など |
| `common.R` | 各スクリプトが最初に読み込む共通の処理（作業フォルダの設定、CSVの書き出しなど） |

モデルは次の2つです。どちらも、カテゴリ変数を1つだけ入れたロジスティック回帰で、背景変数は調整していません。

```r
life_dissatisfied ~ frequency_group  # 過去30日の飲酒日数
life_dissatisfied ~ quantity_group   # 飲酒日の平均ドリンク数
```

## データについて

データ本体と、データの取得・クリーニングのスクリプトはここに含めていません。元のデータはCDCのページから取得できます。

- [BRFSS 2023年版](https://www.cdc.gov/brfss/annual_data/annual_2023.html)
- [質問票の版ごとのデータ](https://www.cdc.gov/brfss/annual_data/2023/llcp_multiq.html)

実行環境は R 4.6.1、survey 4.5 です。
