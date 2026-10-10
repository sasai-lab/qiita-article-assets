# Availability audit for the revised question only. No regression or dataset replacement.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("^--file=", "", file_arg)), "common.R"))
source("scripts/data_helpers.R")
sources <- read.csv("config/module_sources.csv", stringsAsFactors = FALSE)
variables <- c("_STATE", "SEQNO", "LSATISFY", "ALCDAY4", "AVEDRNK3", "MAXDRNKS", "DRNK3GE5", "_DRNKWK2")
main_info <- verified_xpt("llcp_main")
main <- read_raw(main_info$path, variables)
main_keys <- record_key(main)
counts <- list(); codes <- list(); comparisons <- list()
for (id in unique(sources$source)) {
  info <- if (id == "llcp_main") main_info else verified_xpt(id)
  columns <- names(haven::read_xpt(info$path, n_max = 1))
  stopifnot(all(variables %in% columns))
  x <- read_raw(info$path, variables)
  if (id != "llcp_main") {
    at <- match(record_key(x), main_keys)
    stopifnot(!anyNA(at))
    for (v in c("ALCDAY4", "AVEDRNK3", "MAXDRNKS", "DRNK3GE5", "_DRNKWK2")) {
      a <- x[[v]]; b <- main[[v]][at]
      same <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
      comparisons[[length(comparisons) + 1L]] <- data.frame(source = id, variable = v,
        compared_records = nrow(x), different_records = sum(!same))
    }
  }
  x <- x[x[["_STATE"]] %in% sources$state_code[sources$source == id], , drop = FALSE]
  current <- x$ALCDAY4 %in% c(101:107, 201:230)
  none <- x$ALCDAY4 %in% 888
  valid_quantity <- x$AVEDRNK3 %in% 1:76
  none_inconsistent <- none & (valid_quantity | x$DRNK3GE5 %in% 1:76 | x$MAXDRNKS %in% 1:76)
  new_base <- (none & !none_inconsistent | current & valid_quantity) & x$LSATISFY %in% 1:4
  counts[[id]] <- data.frame(source = id, module_records = nrow(x),
    current_drinkers = sum(current), none_30_days = sum(none),
    current_valid_quantity = sum(current & valid_quantity),
    current_quantity_88 = sum(current & x$AVEDRNK3 %in% 88),
    current_quantity_unknown = sum(current & !valid_quantity & !x$AVEDRNK3 %in% 88),
    none_inconsistent = sum(none_inconsistent),
    proposed_unadjusted_complete_records = sum(new_base))
  for (population in c("current_drinkers", "none_30_days")) {
    at <- if (population == "current_drinkers") current else none
    values <- ifelse(is.na(x$AVEDRNK3[at]), "blank", as.character(x$AVEDRNK3[at]))
    tab <- as.data.frame(table(values), stringsAsFactors = FALSE)
    names(tab) <- c("raw_value", "records")
    codes[[length(codes) + 1L]] <- data.frame(source = id, population = population, variable = "AVEDRNK3", tab)
  }
  message("Quantity/frequency availability audited: ", id)
}
write_csv(do.call(rbind, counts), "outputs/redesign_sample_availability.csv")
write_csv(do.call(rbind, codes), "outputs/redesign_quantity_codes.csv")
comparison <- do.call(rbind, comparisons)
write_csv(comparison, "outputs/redesign_source_comparison.csv")
stopifnot(all(comparison$different_records == 0))
message("Redesign profiling complete. Saved cleaned data and regression results have not been changed.")
