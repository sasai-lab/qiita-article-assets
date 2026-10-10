file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")

sources <- read.csv("config/module_sources.csv", stringsAsFactors = FALSE)
main_info <- verified_xpt("llcp_main")
main_columns <- names(haven::read_xpt(main_info$path, n_max = 1))
main <- read_raw(main_info$path, analysis_raw_variables)
main_key <- record_key(main)
stopifnot(!anyNA(main_key), !anyDuplicated(main_key), all(nzchar(main$SEQNO)))
schemas <- list(); differences <- list(); comparisons <- list(); profiles <- list(); routing <- list()

for (id in unique(sources$source)) {
  info <- if (id == "llcp_main") main_info else verified_xpt(id)
  columns <- names(haven::read_xpt(info$path, n_max = 1))
  weight <- unique(sources$weight[sources$source == id])
  stopifnot(length(weight) == 1L, all(c(analysis_raw_variables, weight) %in% columns))
  x <- read_raw(info$path, c(analysis_raw_variables, weight))
  key <- record_key(x)
  stopifnot(!anyDuplicated(key), all(nzchar(x$SEQNO)), !anyNA(x[["_STATE"]]))
  schemas[[id]] <- data.frame(source = id, archive = info$archive,
    archive_sha256 = info$archive_sha256, xpt_sha256 = digest::digest(file = info$path, algo = "sha256"),
    member = info$member, member_date = info$member_date, records = nrow(x),
    columns = length(columns), duplicate_keys = sum(duplicated(key)), weight_variable = weight)
  if (id != "llcp_main") {
    differences[[id]] <- rbind(
      data.frame(source = id, location = "main_only", variable = setdiff(main_columns, columns)),
      data.frame(source = id, location = "version_only", variable = setdiff(columns, main_columns)))
    at <- match(key, main_key)
    stopifnot(!anyNA(at))
    for (v in setdiff(analysis_raw_variables, c("_STATE", "SEQNO"))) {
      left <- x[[v]]; right <- main[[v]][at]
      same <- (is.na(left) & is.na(right)) | (!is.na(left) & !is.na(right) & left == right)
      comparisons[[length(comparisons) + 1L]] <- data.frame(
        source = id, variable = v, matched_records = length(at), different_records = sum(!same),
        both_observed_different = sum(!is.na(left) & !is.na(right) & left != right),
        main_blank_version_observed = sum(is.na(right) & !is.na(left)),
        main_observed_version_blank = sum(!is.na(right) & is.na(left)))
      if (v == "LSATISFY") {
        states_selected <- x[["_STATE"]] %in% sources$state_code[sources$source == id]
        routing[[id]] <- as.data.frame(table(
          source = rep(id, nrow(x)), state_code = x[["_STATE"]],
          selected_state = states_selected,
          main_value = ifelse(is.na(right), "blank", as.character(right)),
          version_value = ifelse(is.na(left), "blank", as.character(left))))
        routing[[id]] <- routing[[id]][routing[[id]]$Freq > 0, , drop = FALSE]
      }
    }
  }
  selected <- x[["_STATE"]] %in% sources$state_code[sources$source == id]
  for (v in names(missing_rules)) {
    values <- x[[v]][selected]
    value_text <- ifelse(is.na(values), "blank", format(values, scientific = FALSE, trim = TRUE))
    counts <- as.data.frame(table(value_text), stringsAsFactors = FALSE)
    names(counts) <- c("raw_value", "records")
    profiles[[length(profiles) + 1L]] <- data.frame(source = id, variable = v, counts)
  }
  message("Audited ", id, ": ", nrow(x), " records, ", length(columns), " columns")
  rm(x); gc(verbose = FALSE)
}

write_csv(do.call(rbind, schemas), "outputs/source_schema.csv")
write_csv(do.call(rbind, differences), "outputs/source_column_differences.csv")
comparison <- do.call(rbind, comparisons)
write_csv(comparison, "outputs/source_variable_comparison.csv")
write_csv(do.call(rbind, routing), "outputs/life_satisfaction_source_routing.csv")
write_csv(do.call(rbind, profiles), "outputs/raw_code_counts.csv")
if (any(comparison$different_records[comparison$variable != "LSATISFY"] != 0) ||
    any(comparison$both_observed_different != 0)) {
  stop("Core/design variables or observed LSATISFY answers differ; inspect source_variable_comparison.csv before cleaning")
}
message("Source audit passed: core/design variables match; LSATISFY blank routing is recorded separately. No regression run.")
