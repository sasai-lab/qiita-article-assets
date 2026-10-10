# Design 2.0: one outcome, two separate unadjusted categorical predictors.
# Source after data_helpers.R for verified ZIP reading and survey design construction.
v2_raw_variables <- unique(c(analysis_raw_variables, "AVEDRNK3", "MAXDRNKS", "_DRNKWK2"))
v2_analysis_variables <- c("life_dissatisfied", "frequency_group", "quantity_group")
v2_frequency_levels <- c("None_30_days", "1_to_3", "over3_to7", "over7_to15", "over15_to30")
v2_quantity_levels <- c("None_30_days", "1", "2", "3_to_4", "5_or_more")
v2_model_formulas <- list(frequency = life_dissatisfied ~ frequency_group,
                         quantity = life_dissatisfied ~ quantity_group)

derive_exposures_v2 <- function(frequency, quantity, binge, maximum) {
  stopifnot(length(frequency) == length(quantity), length(quantity) == length(binge),
            length(binge) == length(maximum))
  status <- drinking_status(frequency)
  current <- status == "current"
  none <- status == "none_30_days"
  contradictory <- none & (quantity %in% 1:76 | binge %in% 1:76 | maximum %in% 1:76)
  days <- drinking_days(frequency)
  days[none & !contradictory] <- 0
  days[contradictory] <- NA_real_
  frequency_group <- rep(NA_character_, length(days))
  frequency_group[!is.na(days) & days == 0] <- v2_frequency_levels[1]
  frequency_group[!is.na(days) & days > 0 & days <= 3] <- v2_frequency_levels[2]
  frequency_group[!is.na(days) & days > 3 & days <= 7] <- v2_frequency_levels[3]
  frequency_group[!is.na(days) & days > 7 & days <= 15] <- v2_frequency_levels[4]
  frequency_group[!is.na(days) & days > 15 & days <= 30] <- v2_frequency_levels[5]

  average <- rep(NA_real_, length(days)) # Undefined for non-drinkers, never impute zero.
  average[current & quantity %in% 1:76] <- quantity[current & quantity %in% 1:76]
  quantity_group <- rep(NA_character_, length(days))
  quantity_group[none & !contradictory] <- v2_quantity_levels[1]
  quantity_group[current & quantity %in% 1] <- v2_quantity_levels[2]
  quantity_group[current & quantity %in% 2] <- v2_quantity_levels[3]
  quantity_group[current & quantity %in% 3:4] <- v2_quantity_levels[4]
  quantity_group[current & quantity %in% 5:76] <- v2_quantity_levels[5]
  quantity_status <- rep("unknown_frequency", length(days))
  quantity_status[current] <- "missing_drinker_quantity"
  quantity_status[current & quantity %in% 88] <- "drinker_reported_none_hold"
  quantity_status[current & quantity %in% 1:76] <- "valid_drinker_quantity"
  quantity_status[none & !contradictory] <- "non_drinker_not_required"
  quantity_status[contradictory] <- "contradictory_non_drinker"
  data.frame(drinking_status = status, alcohol_contradictory = contradictory,
    drink_days_30 = days, average_drinks_drinking_day = average,
    frequency_group = factor(frequency_group, levels = v2_frequency_levels),
    quantity_group = factor(quantity_group, levels = v2_quantity_levels),
    quantity_classification_status = quantity_status)
}

v2_missing_rules <- missing_rules
v2_missing_rules$AVEDRNK3 <- list(valid = c(1:76, 88), dont_know = 77, refused = 99)
v2_missing_rules$MAXDRNKS <- list(valid = c(1:76, 88), dont_know = 77, refused = 99)
v2_missing_rules[["_DRNKWK2"]] <- list(valid = 0:98999, unknown_combined = 99900)

v2_labels <- list(
  frequency_group = c("過去30日に飲酒なし", "1〜3日", "3日超〜7日", "7日超〜15日", "15日超〜30日"),
  quantity_group = c("過去30日に飲酒なし", "飲酒日に平均1ドリンク", "平均2ドリンク",
                     "平均3〜4ドリンク", "平均5ドリンク以上"))

v2_out <- function(name) file.path("outputs/v2", name)
v2_data <- function(name) file.path("data/processed/v2", name)
