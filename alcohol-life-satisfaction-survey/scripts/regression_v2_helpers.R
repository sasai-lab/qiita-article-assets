# Shared calculation for categorical logistic models. The caller supplies the
# covariance matrix and critical value appropriate to its estimation method.
step2_out <- function(name) file.path("outputs/v2/step2", name)

categorical_estimates_v2 <- function(fit, predictor, category_levels, labels,
                                     critical = stats::qnorm(0.975)) {
  stopifnot(length(category_levels) == length(labels), length(category_levels) > 1L,
            identical(fit$xlevels[[predictor]], category_levels), critical > 0)
  newdata <- setNames(data.frame(factor(category_levels, levels = category_levels)), predictor)
  x <- stats::model.matrix(stats::delete.response(stats::terms(fit)), newdata,
                           contrasts.arg = fit$contrasts, xlev = fit$xlevels)
  beta <- stats::coef(fit); covariance <- stats::vcov(fit)
  stopifnot(identical(colnames(x), names(beta)), all(is.finite(beta)), all(is.finite(covariance)))
  eta <- drop(x %*% beta)
  eta_covariance <- x %*% covariance %*% t(x)
  eta_se <- sqrt(diag(eta_covariance))
  probability <- stats::plogis(eta)
  gradient <- sweep(x, 1, probability * (1 - probability), "*")
  probability_covariance <- gradient %*% covariance %*% t(gradient)
  # Subtract the reference gradient before computing the variance of a difference.
  difference_gradient <- gradient - matrix(gradient[1L, ], nrow = nrow(gradient),
                                            ncol = ncol(gradient), byrow = TRUE)
  difference_covariance <- difference_gradient %*% covariance %*% t(difference_gradient)
  difference <- probability - probability[1L]
  difference_se <- sqrt(pmax(0, diag(difference_covariance)))
  estimates <- data.frame(level = category_levels, label = labels,
    reference = seq_along(category_levels) == 1L,
    logit = eta, logit_se = eta_se, probability = probability,
    probability_se = sqrt(diag(probability_covariance)),
    probability_ci_low = stats::plogis(eta - critical * eta_se),
    probability_ci_high = stats::plogis(eta + critical * eta_se),
    dissatisfaction_percent = 100 * probability,
    dissatisfaction_ci_low_percent = 100 * stats::plogis(eta - critical * eta_se),
    dissatisfaction_ci_high_percent = 100 * stats::plogis(eta + critical * eta_se),
    difference_pp = 100 * difference, difference_se_pp = 100 * difference_se,
    difference_ci_low_pp = 100 * (difference - critical * difference_se),
    difference_ci_high_pp = 100 * (difference + critical * difference_se))
  list(estimates = estimates, newdata = newdata, predictor_matrix = x,
       logit_covariance = eta_covariance, probability_covariance = probability_covariance,
       difference_covariance = difference_covariance)
}

plot_glm_v2 <- function(estimates, name, path, records) {
  # Cairo resolves the Japanese font correctly here. Keep its cache inside the
  # project so rendering never needs writable system or user cache directories.
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
  grDevices::png(path, width = 2200, height = 1100, res = 180,
                 type = if (capabilities("cairo")) "cairo" else "quartz",
                 family = if (capabilities("aqua")) "Hiragino Sans" else "sans")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(1, 2), oma = c(3.0, 0, 3.3, 0), bg = "white", cex = 1)
  at <- rev(seq_len(nrow(estimates)))
  graphics::par(mar = c(4.8, 10.6, 2.8, 1.2))
  graphics::plot(NA, xlim = c(0, max(estimates$dissatisfaction_ci_high_percent) * 1.12),
    ylim = c(0.5, length(at) + 0.5), xlab = "生活への不満の割合（%）", ylab = "", yaxt = "n",
    main = "群ごとの不満割合", bty = "n", xaxs = "i")
  graphics::abline(h = at, col = "#ECEFF3", lwd = 1)
  graphics::axis(2, at = at, labels = estimates$label, las = 1, tick = FALSE, cex.axis = 0.9)
  graphics::arrows(estimates$dissatisfaction_ci_low_percent, at,
    estimates$dissatisfaction_ci_high_percent, at, angle = 90, code = 3, length = 0.04,
    col = "#24588A", lwd = 2)
  graphics::points(estimates$dissatisfaction_percent, at, pch = 19, cex = 1.25, col = "#24588A")
  graphics::par(mar = c(4.8, 1.5, 2.8, 1.8))
  delta_range <- range(c(estimates$difference_ci_low_pp, estimates$difference_ci_high_pp, 0))
  padding <- max(diff(delta_range) * 0.12, 0.1)
  graphics::plot(NA, xlim = delta_range + c(-padding, padding), ylim = c(0.5, length(at) + 0.5),
    xlab = "飲酒なしとの差（パーセントポイント）", ylab = "", yaxt = "n",
    main = "非飲酒カテゴリとの差", bty = "n", xaxs = "i")
  graphics::abline(h = at, col = "#ECEFF3", lwd = 1)
  graphics::abline(v = 0, lty = 2, col = "#8A929B")
  nonref <- !estimates$reference
  graphics::arrows(estimates$difference_ci_low_pp[nonref], at[nonref],
    estimates$difference_ci_high_pp[nonref], at[nonref], angle = 90, code = 3, length = 0.04,
    col = "#16766D", lwd = 2)
  graphics::points(estimates$difference_pp, at, pch = 19, cex = 1.25, col = "#16766D")
  title <- if (name == "frequency") "飲酒頻度と生活への不満：通常の回帰" else "飲酒日の平均量と生活への不満：通常の回帰"
  graphics::mtext(title, side = 3, outer = TRUE, line = 1.6, cex = 1.25, font = 2)
  graphics::mtext(paste0("BRFSS 2023・39州とDC・共通対象 ", format(records, big.mark = ","),
    "人 | 調査重みなし・背景調整なし"), side = 3, outer = TRUE, line = 0.1, cex = 0.8)
  graphics::mtext("点：推定値　横線：点ごとの95%信頼区間（通常のモデルに基づく）", side = 1, outer = TRUE, line = 0.7, cex = 0.85)
  graphics::mtext("「飲酒なし」は過去30日に飲酒なし。群間の関連を示し、飲酒・禁酒の効果を示す図ではありません。",
                  side = 1, outer = TRUE, line = 1.9, cex = 0.75)
}
