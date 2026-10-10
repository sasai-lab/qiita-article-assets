# Step 3 keeps the Step 2 calculations and files unchanged.
step3_out <- function(name) file.path("outputs/v2/step3", name)

hash_manifest_v2 <- function(paths) {
  paths <- unique(paths)
  data.frame(path = paths, sha256 = vapply(paths, function(p)
    digest::digest(file = p, algo = "sha256"), character(1)))
}

verify_manifest_v2 <- function(path) {
  m <- read.csv(path, stringsAsFactors = FALSE)
  stopifnot(nrow(m) > 0L, all(file.exists(m$path)), all(vapply(seq_len(nrow(m)), function(i)
    digest::digest(file = m$path[i], algo = "sha256") == m$sha256[i], logical(1))))
  invisible(m)
}

compare_estimates_v2 <- function(ordinary, weighted) {
  rownames(ordinary) <- NULL
  rownames(weighted) <- NULL
  # CSV imports may represent counts as integers while rowSums returns doubles.
  stopifnot(identical(ordinary$model, weighted$model), identical(ordinary$level, weighted$level),
    identical(as.numeric(ordinary$records), as.numeric(weighted$records)))
  columns <- c("dissatisfaction_percent", "dissatisfaction_ci_low_percent",
    "dissatisfaction_ci_high_percent", "difference_pp", "difference_ci_low_pp",
    "difference_ci_high_pp", "logit_se", "difference_se_pp")
  ans <- ordinary[c("model", "predictor", "level", "label", "reference", "records", "dissatisfied_records")]
  ans[paste0("glm_", columns)] <- ordinary[columns]
  ans[paste0("survey_", columns)] <- weighted[columns]
  ans$percentage_shift_pp <- weighted$dissatisfaction_percent - ordinary$dissatisfaction_percent
  ans$glm_probability_ci_width_pp <- ordinary$dissatisfaction_ci_high_percent - ordinary$dissatisfaction_ci_low_percent
  ans$survey_probability_ci_width_pp <- weighted$dissatisfaction_ci_high_percent - weighted$dissatisfaction_ci_low_percent
  ans$probability_ci_width_ratio <- ans$survey_probability_ci_width_pp / ans$glm_probability_ci_width_pp
  ans$glm_difference_ci_excludes_zero <- ordinary$difference_ci_low_pp > 0 | ordinary$difference_ci_high_pp < 0
  ans$survey_difference_ci_excludes_zero <- weighted$difference_ci_low_pp > 0 | weighted$difference_ci_high_pp < 0
  ans
}

plot_comparison_v2 <- function(ordinary, weighted, name, path, records) {
  if (capabilities("cairo") && file.exists("/opt/homebrew/etc/fonts/fonts.conf")) {
    font_cache <- file.path(root, ".cache/fontconfig/cache")
    dir.create(font_cache, recursive = TRUE, showWarnings = FALSE)
    font_config <- file.path(root, ".cache/fontconfig/fonts.conf")
    writeLines(c("<?xml version=\"1.0\"?>", "<fontconfig>",
      paste0("<cachedir>", font_cache, "</cachedir>"),
      "<include ignore_missing=\"yes\">/opt/homebrew/etc/fonts/fonts.conf</include>",
      "</fontconfig>"), font_config)
    Sys.setenv(FONTCONFIG_FILE = font_config)
  }
  grDevices::png(path, width = 2400, height = 1200, res = 180,
    type = if (capabilities("cairo")) "cairo" else "quartz",
    family = if (capabilities("aqua")) "Hiragino Sans" else "sans")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(1, 2), oma = c(4.2, 0, 3.3, 0), bg = "white")
  at <- rev(seq_len(nrow(ordinary)))
  colours <- c("#24588A", "#C56518")
  graphics::par(mar = c(4.8, 10.6, 2.8, 1.2))
  graphics::plot(NA, xlim = c(0, max(ordinary$dissatisfaction_ci_high_percent,
    weighted$dissatisfaction_ci_high_percent) * 1.10), ylim = c(0.5, length(at) + 0.5),
    xlab = "生活への不満の割合（%）", ylab = "", yaxt = "n", main = "群ごとの不満割合", bty = "n", xaxs = "i")
  graphics::abline(h = at, col = "#ECEFF3")
  graphics::axis(2, at = at, labels = ordinary$label, las = 1, tick = FALSE, cex.axis = 0.85)
  for (i in 1:2) {
    e <- if (i == 1L) ordinary else weighted
    yy <- at + if (i == 1L) 0.13 else -0.13
    graphics::arrows(e$dissatisfaction_ci_low_percent, yy, e$dissatisfaction_ci_high_percent,
      yy, angle = 90, code = 3, length = 0.04, col = colours[i], lwd = 2)
    graphics::points(e$dissatisfaction_percent, yy, pch = c(19, 17)[i], cex = 1.1, col = colours[i])
  }
  graphics::par(mar = c(4.8, 1.5, 2.8, 1.8))
  delta_range <- range(ordinary$difference_ci_low_pp, ordinary$difference_ci_high_pp,
    weighted$difference_ci_low_pp, weighted$difference_ci_high_pp, 0)
  padding <- max(diff(delta_range) * 0.10, 0.1)
  graphics::plot(NA, xlim = delta_range + c(-padding, padding), ylim = c(0.5, length(at) + 0.5),
    xlab = "飲酒なしとの差（パーセントポイント）", ylab = "", yaxt = "n",
    main = "非飲酒カテゴリとの差", bty = "n", xaxs = "i")
  graphics::abline(h = at, col = "#ECEFF3")
  graphics::abline(v = 0, lty = 2, col = "#8A929B")
  for (i in 1:2) {
    e <- if (i == 1L) ordinary else weighted
    yy <- at + if (i == 1L) 0.13 else -0.13
    nonref <- !e$reference
    graphics::arrows(e$difference_ci_low_pp[nonref], yy[nonref], e$difference_ci_high_pp[nonref],
      yy[nonref], angle = 90, code = 3, length = 0.04, col = colours[i], lwd = 2)
    graphics::points(e$difference_pp, yy, pch = c(19, 17)[i], cex = 1.1, col = colours[i])
  }
  title <- if (name == "frequency") "飲酒頻度と生活への不満：glmとsurvey" else "飲酒日の平均量と生活への不満：glmとsurvey"
  graphics::mtext(title, side = 3, outer = TRUE, line = 1.6, cex = 1.25, font = 2)
  graphics::mtext(paste0("BRFSS 2023・39州とDC・共通対象 ", format(records, big.mark = ","),
    "人 | 同じ対象者・カテゴリ・回帰式、背景調整なし"), side = 3, outer = TRUE, line = 0.1, cex = 0.8)
  graphics::mtext("青 ●：通常のglm　橙 ▲：survey（調査重み・層・PSUを使用）", side = 1, outer = TRUE, line = 0.6, cex = 0.9)
  graphics::mtext("横線：点ごとの95%信頼区間。glmはモデルに基づく区間、surveyは調査設計に基づく区間。",
    side = 1, outer = TRUE, line = 1.8, cex = 0.8)
  graphics::mtext("「飲酒なし」は過去30日に飲酒なし。群間の関連であり、飲酒・禁酒の効果を示す図ではありません。",
    side = 1, outer = TRUE, line = 3.0, cex = 0.75)
}
