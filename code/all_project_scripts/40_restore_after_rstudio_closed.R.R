# ============================================================
# 40_restore_after_rstudio_closed.R
# Restore project environment after accidental RStudio closure
# Project: Sepsis_CrossCohort_scRNA / BMC Genomics manuscript
# Author: Felix / Xinzhuo Li
# Purpose:
#   1. Restore project working directory
#   2. Reload required packages
#   3. Reconnect all key frozen outputs and manuscript files
#   4. Rebuild a lightweight project state object
#   5. Verify whether the project can continue from manuscript revision stage
# ============================================================

options(stringsAsFactors = FALSE)
options(width = 180)
options(max.print = 200)

cat("\n============ 40 Restore after RStudio closed ============\n")

# -----------------------------
# 0. Project path
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

if (!dir.exists(project_dir)) {
  stop("Project directory not found: ", project_dir)
}

setwd(project_dir)

cat("\nWorking directory restored:\n")
cat(getwd(), "\n")

# -----------------------------
# 1. Directory structure
# -----------------------------
dir_scripts <- file.path(project_dir, "03_scripts")
dir_results <- file.path(project_dir, "04_results")
dir_freeze  <- file.path(dir_results, "final_freeze")
dir_tables  <- file.path(dir_freeze, "tables")
dir_figures <- file.path(dir_freeze, "figures")
dir_logs    <- file.path(dir_results, "logs")

dir_manuscript <- file.path(project_dir, "07_manuscript")
dir_bmc_draft  <- file.path(dir_manuscript, "BMC_Genomics_draft")
dir_bmc_package <- file.path(dir_manuscript, "BMC_Genomics_final_submission_package")
dir_ready <- file.path(dir_bmc_package, "00_READY_TO_UPLOAD")
dir_ready_fig <- file.path(dir_ready, "figures")
dir_ready_add <- file.path(dir_ready, "additional_files")
dir_ready_checks <- file.path(dir_ready, "checks")

dir_restore <- file.path(dir_results, "restore_after_rstudio_closed")
if (!dir.exists(dir_restore)) dir.create(dir_restore, recursive = TRUE)

# -----------------------------
# 2. Helper functions
# -----------------------------
safe_exists <- function(path) {
  file.exists(path) | dir.exists(path)
}

file_size_mb <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    warning("CSV not found: ", path)
    return(NULL)
  }
  read.csv(path, check.names = FALSE)
}

safe_read_xlsx <- function(path, sheet = NULL) {
  if (!file.exists(path)) {
    warning("XLSX not found: ", path)
    return(NULL)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    warning("Package readxl not installed. Cannot read: ", path)
    return(NULL)
  }
  if (is.null(sheet)) {
    readxl::read_xlsx(path)
  } else {
    readxl::read_xlsx(path, sheet = sheet)
  }
}

status_df <- function(items) {
  data.frame(
    item = names(items),
    path = unname(items),
    exists = vapply(items, safe_exists, logical(1)),
    size_mb = vapply(items, file_size_mb, numeric(1)),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# 3. Load packages
# -----------------------------
required_pkgs <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "ggplot2",
  "pROC",
  "openxlsx",
  "officer",
  "flextable",
  "readxl"
)

pkg_status <- data.frame(
  package = required_pkgs,
  installed = vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE),
  loaded = FALSE,
  stringsAsFactors = FALSE
)

missing_pkgs <- pkg_status$package[!pkg_status$installed]

if (length(missing_pkgs) > 0) {
  cat("\nMissing packages detected:\n")
  print(missing_pkgs)
  cat("\nInstall them manually if needed:\n")
  cat('install.packages(c("', paste(missing_pkgs, collapse = '", "'), '"))\n', sep = "")
}

for (p in required_pkgs[pkg_status$installed]) {
  suppressPackageStartupMessages(
    library(p, character.only = TRUE)
  )
  pkg_status$loaded[pkg_status$package == p] <- TRUE
}

# -----------------------------
# 4. Key frozen output paths
# -----------------------------
key_files <- list(
  # Frozen manuscript-ready tables
  table1_cohort_context = file.path(dir_tables, "T20_table1_T20_table1_cohort_context.csv"),
  table2_nested_lodo = file.path(dir_tables, "T20_table2_T20_table2_nested_LODO_validation.csv"),
  table3_calibration_threshold = file.path(dir_tables, "T20_table3_T20_table3_calibration_threshold_transport.csv"),
  table4_scrna_localization = file.path(dir_tables, "T20_table4_T20_table4_scRNA_localization.csv"),
  key_numbers = file.path(dir_tables, "T20_key_numbers_T20_key_results_numbers.csv"),
  claims_restrictions = file.path(dir_tables, "T21_claims_T21_recommended_claims_and_restrictions.csv"),
  consistency_checks = file.path(dir_tables, "T21_consistency_T21_key_consistency_checks.csv"),
  risk_flags = file.path(dir_tables, "T21_risk_flags_T21_manuscript_risk_flags.csv"),
  
  # Nested LODO source outputs
  nested_metrics = file.path(dir_tables, "T14_nested_metrics_T14_nested_LODO_validation_metrics.csv"),
  nested_predictions = file.path(dir_tables, "T14_nested_predictions_T14_nested_LODO_predictions.csv"),
  gene_frequency = file.path(dir_tables, "T14_gene_frequency_T14_nested_LODO_gene_selection_frequency.csv"),
  
  # Calibration and threshold outputs
  calibration_metrics = file.path(dir_tables, "T17_calibration_T17_nested_LODO_calibration_metrics_final.csv"),
  threshold_drift = file.path(dir_tables, "T17_threshold_T17_nested_LODO_threshold_drift_final.csv"),
  dca_summary = file.path(dir_tables, "T17_dca_summary_T17_nested_LODO_DCA_summary.csv"),
  pooled_performance = file.path(dir_tables, "T17_pooled_perf_T17_pooled_nested_LODO_performance.csv"),
  
  # scRNA outputs
  scrna_celltype_score = file.path(dir_tables, "T19_scRNA_celltype_T19_scRNA_signature_score_by_celltype.csv"),
  scrna_cluster_annotation = file.path(dir_tables, "T19_scRNA_cluster_annotation_T19_scRNA_cluster_annotation.csv"),
  scrna_summary = file.path(dir_tables, "T19_scRNA_summary_T19_scRNA_signature_localization_summary.csv"),
  
  # Submission files
  ready_main_manuscript = file.path(dir_ready, "Main_manuscript.docx"),
  ready_cover_letter = file.path(dir_ready, "Cover_letter.docx"),
  ready_manifest = file.path(dir_ready, "BMC_Genomics_FINAL_UPLOAD_CHECKLIST_v1.3.xlsx"),
  
  # Figures
  figure1_pdf = file.path(dir_ready_fig, "Figure_1.pdf"),
  figure2_pdf = file.path(dir_ready_fig, "Figure_2.pdf"),
  figure3_pdf = file.path(dir_ready_fig, "Figure_3.pdf"),
  figure4_pdf = file.path(dir_ready_fig, "Figure_4.pdf"),
  figure5_pdf = file.path(dir_ready_fig, "Figure_5.pdf"),
  
  # Additional files
  additional_file_1 = file.path(dir_ready_add, "Additional_file_1.xlsx"),
  additional_file_2 = file.path(dir_ready_add, "Additional_file_2.xlsx"),
  additional_file_3 = file.path(dir_ready_add, "Additional_file_3.xlsx"),
  additional_file_4 = file.path(dir_ready_add, "Additional_file_4.xlsx"),
  additional_file_5 = file.path(dir_ready_add, "Additional_file_5.xlsx"),
  additional_file_6 = file.path(dir_ready_add, "Additional_file_6.xlsx")
)

key_file_status <- status_df(key_files)

# -----------------------------
# 5. Read core tables into memory
# -----------------------------
cat("\nReading core frozen outputs...\n")

T20_table1 <- safe_read_csv(key_files$table1_cohort_context)
T20_table2 <- safe_read_csv(key_files$table2_nested_lodo)
T20_table3 <- safe_read_csv(key_files$table3_calibration_threshold)
T20_table4 <- safe_read_csv(key_files$table4_scrna_localization)
T20_key_numbers <- safe_read_csv(key_files$key_numbers)

T14_metrics <- safe_read_csv(key_files$nested_metrics)
T14_predictions <- safe_read_csv(key_files$nested_predictions)
T14_gene_frequency <- safe_read_csv(key_files$gene_frequency)

T17_calibration <- safe_read_csv(key_files$calibration_metrics)
T17_threshold <- safe_read_csv(key_files$threshold_drift)
T17_dca <- safe_read_csv(key_files$dca_summary)
T17_pooled <- safe_read_csv(key_files$pooled_performance)

T19_celltype_score <- safe_read_csv(key_files$scrna_celltype_score)
T19_cluster_annotation <- safe_read_csv(key_files$scrna_cluster_annotation)
T19_summary <- safe_read_csv(key_files$scrna_summary)

# -----------------------------
# 6. Verify nested LODO algorithm metadata
# -----------------------------
algorithm_check <- data.frame(
  check_item = c(
    "T14 metrics file exists",
    "best_alpha column exists",
    "lambda_rule_used column exists",
    "n_selected_genes column exists",
    "all lambda_rule_used are lambda.1se",
    "alpha grid includes 0.50, 0.75 or 1.00"
  ),
  observed = NA,
  status = NA,
  stringsAsFactors = FALSE
)

if (!is.null(T14_metrics)) {
  algorithm_check$observed[1] <- TRUE
  algorithm_check$observed[2] <- "best_alpha" %in% names(T14_metrics)
  algorithm_check$observed[3] <- "lambda_rule_used" %in% names(T14_metrics)
  algorithm_check$observed[4] <- "n_selected_genes" %in% names(T14_metrics)
  
  if ("lambda_rule_used" %in% names(T14_metrics)) {
    algorithm_check$observed[5] <- all(T14_metrics$lambda_rule_used == "lambda.1se", na.rm = TRUE)
  } else {
    algorithm_check$observed[5] <- FALSE
  }
  
  if ("best_alpha" %in% names(T14_metrics)) {
    algorithm_check$observed[6] <- all(T14_metrics$best_alpha %in% c(0.50, 0.75, 1.00), na.rm = TRUE)
  } else {
    algorithm_check$observed[6] <- FALSE
  }
} else {
  algorithm_check$observed <- FALSE
}

algorithm_check$status <- ifelse(algorithm_check$observed == TRUE, "PASS", "CHECK")

# -----------------------------
# 7. Verify final manuscript upload file set
# -----------------------------
expected_upload_files <- list(
  Main_manuscript = file.path(dir_ready, "Main_manuscript.docx"),
  Cover_letter = file.path(dir_ready, "Cover_letter.docx"),
  Figure_1 = file.path(dir_ready_fig, "Figure_1.pdf"),
  Figure_2 = file.path(dir_ready_fig, "Figure_2.pdf"),
  Figure_3 = file.path(dir_ready_fig, "Figure_3.pdf"),
  Figure_4 = file.path(dir_ready_fig, "Figure_4.pdf"),
  Figure_5 = file.path(dir_ready_fig, "Figure_5.pdf"),
  Additional_file_1 = file.path(dir_ready_add, "Additional_file_1.xlsx"),
  Additional_file_2 = file.path(dir_ready_add, "Additional_file_2.xlsx"),
  Additional_file_3 = file.path(dir_ready_add, "Additional_file_3.xlsx"),
  Additional_file_4 = file.path(dir_ready_add, "Additional_file_4.xlsx"),
  Additional_file_5 = file.path(dir_ready_add, "Additional_file_5.xlsx"),
  Additional_file_6 = file.path(dir_ready_add, "Additional_file_6.xlsx")
)

upload_status <- status_df(expected_upload_files)

upload_status$upload_class <- c(
  "Main manuscript",
  "Cover letter",
  rep("Main figure PDF", 5),
  rep("Additional file XLSX", 6)
)

upload_status$size_limit_mb <- c(
  20, 20,
  rep(10, 5),
  rep(20, 6)
)

upload_status$size_ok <- ifelse(
  upload_status$exists,
  upload_status$size_mb <= upload_status$size_limit_mb,
  FALSE
)

upload_status$status <- ifelse(
  upload_status$exists & upload_status$size_ok,
  "READY",
  "CHECK"
)

# -----------------------------
# 8. Reconstruct key numeric snapshot
# -----------------------------
extract_key_value <- function(df, possible_name_patterns) {
  if (is.null(df)) return(NA_character_)
  if (ncol(df) < 2) return(NA_character_)
  
  # Common format: metric / value
  metric_col <- names(df)[1]
  value_col <- names(df)[2]
  
  idx <- rep(FALSE, nrow(df))
  for (pat in possible_name_patterns) {
    idx <- idx | grepl(pat, df[[metric_col]], ignore.case = TRUE)
  }
  
  if (any(idx, na.rm = TRUE)) {
    return(as.character(df[[value_col]][which(idx)[1]]))
  }
  NA_character_
}

key_snapshot <- data.frame(
  item = c(
    "Median AUROC",
    "Pooled AUROC",
    "Median absolute threshold shift",
    "Failed/low-performance cohort",
    "scRNA dataset",
    "Main sample size",
    "Sepsis samples",
    "Control samples"
  ),
  value = c(
    extract_key_value(T20_key_numbers, c("median.*AUROC", "median_auroc")),
    extract_key_value(T20_key_numbers, c("pooled.*AUROC", "pooled_auroc")),
    extract_key_value(T20_key_numbers, c("threshold.*shift", "absolute.*shift")),
    "GSE54514 if confirmed by T14 metrics",
    "GSE167363 if confirmed by manuscript",
    "484",
    "333",
    "151"
  ),
  stringsAsFactors = FALSE
)

if (!is.null(T14_metrics) && all(c("validation_dataset", "AUROC") %in% names(T14_metrics))) {
  low_idx <- which.min(T14_metrics$AUROC)
  key_snapshot$value[key_snapshot$item == "Failed/low-performance cohort"] <-
    paste0(T14_metrics$validation_dataset[low_idx], " AUROC=", round(T14_metrics$AUROC[low_idx], 3))
}

# -----------------------------
# 9. Determine next safe continuation point
# -----------------------------
n_missing_upload <- sum(upload_status$status != "READY")
n_missing_key <- sum(!key_file_status$exists)

can_continue_submission_review <- n_missing_upload == 0

overall_status <- data.frame(
  metric = c(
    "restore_timestamp",
    "project_dir",
    "working_directory",
    "n_required_packages",
    "n_packages_loaded",
    "n_key_files_missing",
    "n_expected_upload_files",
    "n_upload_files_not_ready",
    "can_continue_from_submission_review_stage",
    "recommended_next_step"
  ),
  value = c(
    as.character(Sys.time()),
    project_dir,
    getwd(),
    length(required_pkgs),
    sum(pkg_status$loaded),
    n_missing_key,
    nrow(upload_status),
    n_missing_upload,
    ifelse(can_continue_submission_review, "YES", "NO_FIX_MISSING_FILES_FIRST"),
    ifelse(
      can_continue_submission_review,
      "Continue manuscript polishing, figure/table visual review, and BMC Genomics submission package check. Do not rerun early preprocessing unless manuscript numbers conflict.",
      "Fix missing READY_TO_UPLOAD files or rerun the relevant later-stage script only."
    )
  ),
  stringsAsFactors = FALSE
)

# -----------------------------
# 10. Save restore outputs
# -----------------------------
overall_path <- file.path(dir_restore, "T40_restore_overall_status.csv")
pkg_status_path <- file.path(dir_restore, "T40_package_status.csv")
key_file_status_path <- file.path(dir_restore, "T40_key_file_status.csv")
upload_status_path <- file.path(dir_restore, "T40_ready_upload_file_status.csv")
algorithm_check_path <- file.path(dir_restore, "T40_nested_lodo_algorithm_metadata_check.csv")
key_snapshot_path <- file.path(dir_restore, "T40_key_result_snapshot_after_restore.csv")
rdata_path <- file.path(dir_restore, "T40_restored_project_state.RData")

write.csv(overall_status, overall_path, row.names = FALSE)
write.csv(pkg_status, pkg_status_path, row.names = FALSE)
write.csv(key_file_status, key_file_status_path, row.names = FALSE)
write.csv(upload_status, upload_status_path, row.names = FALSE)
write.csv(algorithm_check, algorithm_check_path, row.names = FALSE)
write.csv(key_snapshot, key_snapshot_path, row.names = FALSE)

save(
  project_dir,
  dir_scripts,
  dir_results,
  dir_freeze,
  dir_tables,
  dir_figures,
  dir_manuscript,
  dir_bmc_draft,
  dir_bmc_package,
  dir_ready,
  dir_ready_fig,
  dir_ready_add,
  dir_ready_checks,
  T20_table1,
  T20_table2,
  T20_table3,
  T20_table4,
  T20_key_numbers,
  T14_metrics,
  T14_predictions,
  T14_gene_frequency,
  T17_calibration,
  T17_threshold,
  T17_dca,
  T17_pooled,
  T19_celltype_score,
  T19_cluster_annotation,
  T19_summary,
  overall_status,
  pkg_status,
  key_file_status,
  upload_status,
  algorithm_check,
  key_snapshot,
  file = rdata_path
)

# -----------------------------
# 11. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nPackage status:\n")
print(pkg_status)

cat("\nNested LODO algorithm metadata check:\n")
print(algorithm_check)

cat("\nKey numeric snapshot:\n")
print(key_snapshot)

cat("\nREADY_TO_UPLOAD file status:\n")
print(upload_status)

cat("\nMissing key files, if any:\n")
missing_key_files <- key_file_status[!key_file_status$exists, , drop = FALSE]
if (nrow(missing_key_files) > 0) {
  print(missing_key_files)
} else {
  cat("No missing key files detected.\n")
}

cat("\n关键输出：\n")
cat("1) ", overall_path, "\n", sep = "")
cat("2) ", key_file_status_path, "\n", sep = "")
cat("3) ", upload_status_path, "\n", sep = "")
cat("4) ", algorithm_check_path, "\n", sep = "")
cat("5) ", key_snapshot_path, "\n", sep = "")
cat("6) ", rdata_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Nested LODO algorithm metadata check、Key numeric snapshot、READY_TO_UPLOAD file status 贴给我。\n")
cat("如果 can_continue_from_submission_review_stage = YES，我们就从英文 Word 投稿稿的最终审查和 BMC Genomics 格式补全继续。\n")

cat("\n============ 40 restore complete ============\n")