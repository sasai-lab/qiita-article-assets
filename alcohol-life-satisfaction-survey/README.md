# 飲酒の頻度・量と生活満足度：glmとsurveyの比較（BRFSS 2023）

Qiitaの記事2本で使った分析のRコード、図、結果の表です。

- 記事1：禁酒で生活は劇的に良くなる？ 公開データをRで調べて、言えることを考えた
- 記事2：Rのglmとsurveyを同じデータ・同じ式で比べてみた：飲酒と生活満足度のBRFSSデータで

（記事のURLは公開後に追記します）

## やっていること

アメリカのBRFSS 2023のうち、生活満足度の質問を実施した39州とワシントンDCのデータを使い、過去30日の飲酒頻度と、飲酒日の平均量のカテゴリごとに、生活に不満と答えた人の割合を比べています。

同じ255,168人・同じ式で、重みを使わない`glm`と、調査の重み・層・PSUを反映した`survey::svyglm`を実行し、各カテゴリの不満割合と、「過去30日に飲酒なし」との差を95%信頼区間つきで並べています。背景変数は調整しておらず、飲み方の違う人たちの間の関連を見るものです。

## フォルダ構成

```text
scripts/   分析のRスクリプト
docs/      設計書（analysis-plan.md）
figures/   記事で使った図
results/   記事の表の元になった数値（comparison.csv。丸めていない値）
renv.lock  パッケージのバージョン
```

置いているのは、glmとsurveyの推定・検証のコードです。データの取得とクリーニングのコードは含めていないので、このフォルダだけで公開データから記事の数字までを再現することはできません。

| ファイル | 中身 |
| --- | --- |
| `scripts/10_fit_glm_v2.R` | 通常のglm（頻度モデル・量モデル）と図 |
| `scripts/11_verify_glm_v2.R` | glmの結果の検証（人数から計算した二項モデルの式、`predict`との照合） |
| `scripts/12_fit_survey_v2.R` | svyglm（頻度モデル・量モデル）と、glmとの比較表・図 |
| `scripts/13_verify_survey_v2.R` | surveyの結果の検証（重み付き集計、surveyの関数を使わずに計算した分散などとの照合） |
| `scripts/regression_v2_helpers.R` | 各カテゴリの割合と、基準カテゴリとの差の信頼区間（記事2のコードの抜粋元） |
| `scripts/survey_v2_helpers.R` | glmとsurveyの比較表と図 |
| `scripts/cleaning_v2_helpers.R` | 頻度・量のカテゴリとモデル式の定義（記事1のコードの抜粋元） |
| `scripts/data_helpers.R` | 頻度の日数換算、survey設計の作成など（記事のコードの抜粋元） |
| `scripts/common.R` | 共通の設定 |
| `scripts/00_setup.R`、`02_check_environment.R` | renvによる環境の準備と、生成データでの動作確認 |

## データ

データはリポジトリに含めていません。CDCの次のページから取得できます。

- [BRFSS 2023年版](https://www.cdc.gov/brfss/annual_data/annual_2023.html)：主ファイル（`LLCP2023XPT.zip`）
- [質問票の版ごとのデータ](https://www.cdc.gov/brfss/annual_data/2023/llcp_multiq.html)：版ごとの3ファイル（`LLCP23V1_XPT.zip`〜`LLCP23V3_XPT.zip`）

## 実行環境

R 4.6.1、survey 4.5、haven 2.5.5、renv 1.3.1（macOSのHomebrew版Rで実行しました）。図の日本語フォントには、macOSのヒラギノを使っています。
