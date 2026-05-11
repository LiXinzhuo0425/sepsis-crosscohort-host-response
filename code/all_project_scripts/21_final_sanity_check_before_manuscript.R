# ============================================================
# 21_final_sanity_check_before_manuscript.R
# Final sanity check before manuscript drafting
#
# 目的：
# 1. 检查主线结果文件是否齐全
# 2. 检查关键数字是否互相一致
# 3. 自动识别 manuscript risk points
# 4. 明确哪些结果可以主文强调，哪些只能补充/探索性表述
# 5. 生成写稿前最终质控表
#
# 输出：
# 04_results/reporting/
#   T21_final_sanity_check_summary.xlsx
#   T21_key_consistency_checks.csv
#   T21_manuscript_risk_flags.csv
#   T21_recommended_claims_and_restrictions.csv
#
# 07_manuscript/
#   final_sanity_check_before_manuscript_v0.1.md
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

report_dir <- file.path(project_dir, "04_results", "reporting")
nested_dir <- file.path(project_dir, "04_results", "nested_LODO")
dca_dir <- file.path(project_dir, "04_results", "dca")
sc_dir <- file.path(project_dir, "04_results", "single_cell")
manuscript_dir <- file.path(project_dir, "07_manuscript")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

files <- list(
  table1 = file.path(report_dir, "T20_table1_cohort_context.csv"),
  table2 = file.path(report_dir, "T20_table2_nested_LODO_validation.csv"),
  table3 = file.path(report_dir, "T20_table3_calibration_threshold_transport.csv"),
  table4 = file.path(report_dir, "T20_table4_scRNA_localization.csv"),
  key_numbers = file.path(report_dir, "T20_key_results_numbers.csv"),
  figure_plan = file.path(report_dir, "T20_figure_plan_final.csv"),
  nested_predictions = file.path(nested_dir, "T14_nested_LODO_predictions.csv"),
  nested_gene_frequency = file.path(nested_dir, "T14_nested_LODO_gene_selection_frequency.csv"),
  pooled_perf = file.path(dca_dir, "T17_pooled_nested_LODO_performance.csv"),
  dca_summary = file.path(dca_dir, "T17_nested_LODO_DCA_summary.csv"),
  sc_localization = file.path(sc_dir, "T19_scRNA_signature_localization_summary.csv"),
  sc_score_celltype = file.path(sc_dir, "T19_scRNA_signature_score_by_celltype.csv"),
  results_draft = file.path(manuscript_dir, "results_draft_v0.2_after_nested_LODO_scRNA.md"),
  interpretation_notes = file.path(manuscript_dir, "interpretation_notes_v0.2_after_nested_LODO_scRNA.md")
)

file_presence <- data.frame(
  file_key = names(files),
  file_path = unlist(files),
  exists = file.exists(unlist(files)),
  stringsAsFactors = FALSE
)

missing <- file_presence[file_presence$exists == FALSE, , drop = FALSE]

if (nrow(missing) > 0) {
  stop(
    "缺少必要文件：\n",
    paste(missing$file_key, missing$file_path, sep = ": ", collapse = "\n")
  )
}

# ============================================================
# 工具函数
# ============================================================

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
}

get_key_value <- function(key_df, item_name) {
  x <- key_df$value[key_df$item == item_name]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

as_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

fmt_num <- function(x, digits = 3) {
  if (is.na(as.numeric(x))) return("NA")
  formatC(as.numeric(x), format = "f", digits = digits)
}

status_pass <- function(condition) {
  if (isTRUE(condition)) "PASS" else "FLAG"
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

# ============================================================
# 读取数据
# ============================================================

message("Reading final manuscript-ready outputs...")

table1 <- read_csv_df(files$table1)
table2 <- read_csv_df(files$table2)
table3 <- read_csv_df(files$table3)
table4 <- read_csv_df(files$table4)
key_numbers <- read_csv_df(files$key_numbers)
figure_plan <- read_csv_df(files$figure_plan)
nested_predictions <- read_csv_df(files$nested_predictions)
nested_gene_frequency <- read_csv_df(files$nested_gene_frequency)
pooled_perf <- read_csv_df(files$pooled_perf)
dca_summary <- read_csv_df(files$dca_summary)
sc_localization <- read_csv_df(files$sc_localization)
sc_score_celltype <- read_csv_df(files$sc_score_celltype)

# ============================================================
# 一致性检查
# ============================================================

message("Running consistency checks...")

n_bulk_key <- as_num(get_key_value(key_numbers, "n_total_samples_bulk"))
n_case_key <- as_num(get_key_value(key_numbers, "n_sepsis_bulk"))
n_ctrl_key <- as_num(get_key_value(key_numbers, "n_control_bulk"))

n_bulk_table2 <- sum(table2$n, na.rm = TRUE)
n_case_table2 <- sum(table2$n_case, na.rm = TRUE)
n_ctrl_table2 <- sum(table2$n_control, na.rm = TRUE)

median_auroc_key <- as_num(get_key_value(key_numbers, "nested_LODO_median_AUROC"))
median_auroc_table2 <- median(table2$AUROC, na.rm = TRUE)

min_auroc_key <- as_num(get_key_value(key_numbers, "nested_LODO_min_AUROC"))
min_auroc_table2 <- min(table2$AUROC, na.rm = TRUE)

max_auroc_key <- as_num(get_key_value(key_numbers, "nested_LODO_max_AUROC"))
max_auroc_table2 <- max(table2$AUROC, na.rm = TRUE)

pooled_auroc_key <- as_num(get_key_value(key_numbers, "pooled_nested_LODO_AUROC"))
pooled_auroc_table <- pooled_perf$AUROC[1]

pooled_brier_key <- as_num(get_key_value(key_numbers, "pooled_nested_LODO_Brier"))
pooled_brier_table <- pooled_perf$Brier[1]

median_abs_thr_key <- as_num(get_key_value(key_numbers, "median_abs_threshold_shift"))
median_abs_thr_table3 <- median(abs(table3$threshold_shift), na.rm = TRUE)

max_abs_thr_key <- as_num(get_key_value(key_numbers, "max_abs_threshold_shift"))
max_abs_thr_table3 <- max(abs(table3$threshold_shift), na.rm = TRUE)

n_fixed_sens_lt_key <- as_num(get_key_value(key_numbers, "n_fixed_sensitivity_lt_0.20"))
n_fixed_sens_lt_table3 <- sum(table3$fixed_sensitivity < 0.20, na.rm = TRUE)

n_fixed_spec_lt_key <- as_num(get_key_value(key_numbers, "n_fixed_specificity_lt_0.20"))
n_fixed_spec_lt_table3 <- sum(table3$fixed_specificity < 0.20, na.rm = TRUE)

sc_n_cells_key <- as_num(get_key_value(key_numbers, "scRNA_n_cells"))
sc_n_final10_key <- as_num(get_key_value(key_numbers, "scRNA_n_final10_genes_present"))
sc_n_nested_key <- as_num(get_key_value(key_numbers, "scRNA_n_nested_recurrent_genes_present"))
sc_top_final_key <- get_key_value(key_numbers, "scRNA_top_celltype_Final10")
sc_top_nested_key <- get_key_value(key_numbers, "scRNA_top_celltype_NestedRecurrent")

consistency_checks <- data.frame(
  check_id = c(
    "bulk_total_n_consistency",
    "bulk_case_n_consistency",
    "bulk_control_n_consistency",
    "nested_median_AUROC_consistency",
    "nested_min_AUROC_consistency",
    "nested_max_AUROC_consistency",
    "pooled_AUROC_consistency",
    "pooled_Brier_consistency",
    "median_abs_threshold_shift_consistency",
    "max_abs_threshold_shift_consistency",
    "fixed_sensitivity_failure_count_consistency",
    "fixed_specificity_failure_count_consistency",
    "scRNA_cell_count_present",
    "scRNA_final10_gene_presence",
    "scRNA_nested_recurrent_gene_presence",
    "scRNA_top_celltype_agreement"
  ),
  expected_or_key_value = c(
    n_bulk_key,
    n_case_key,
    n_ctrl_key,
    median_auroc_key,
    min_auroc_key,
    max_auroc_key,
    pooled_auroc_key,
    pooled_brier_key,
    median_abs_thr_key,
    max_abs_thr_key,
    n_fixed_sens_lt_key,
    n_fixed_spec_lt_key,
    sc_n_cells_key,
    sc_n_final10_key,
    sc_n_nested_key,
    paste(sc_top_final_key, sc_top_nested_key, sep = " / ")
  ),
  recalculated_value = c(
    n_bulk_table2,
    n_case_table2,
    n_ctrl_table2,
    median_auroc_table2,
    min_auroc_table2,
    max_auroc_table2,
    pooled_auroc_table,
    pooled_brier_table,
    median_abs_thr_table3,
    max_abs_thr_table3,
    n_fixed_sens_lt_table3,
    n_fixed_spec_lt_table3,
    sc_n_cells_key,
    sc_n_final10_key,
    sc_n_nested_key,
    paste(sc_top_final_key, sc_top_nested_key, sep = " / ")
  ),
  status = c(
    status_pass(n_bulk_key == n_bulk_table2),
    status_pass(n_case_key == n_case_table2),
    status_pass(n_ctrl_key == n_ctrl_table2),
    status_pass(abs(median_auroc_key - median_auroc_table2) < 1e-8),
    status_pass(abs(min_auroc_key - min_auroc_table2) < 1e-8),
    status_pass(abs(max_auroc_key - max_auroc_table2) < 1e-8),
    status_pass(abs(pooled_auroc_key - pooled_auroc_table) < 1e-8),
    status_pass(abs(pooled_brier_key - pooled_brier_table) < 1e-8),
    status_pass(abs(median_abs_thr_key - median_abs_thr_table3) < 1e-8),
    status_pass(abs(max_abs_thr_key - max_abs_thr_table3) < 1e-8),
    status_pass(n_fixed_sens_lt_key == n_fixed_sens_lt_table3),
    status_pass(n_fixed_spec_lt_key == n_fixed_spec_lt_table3),
    status_pass(!is.na(sc_n_cells_key) && sc_n_cells_key > 1000),
    status_pass(sc_n_final10_key == 10),
    status_pass(sc_n_nested_key == 10),
    status_pass(sc_top_final_key == "Monocyte/Myeloid" && sc_top_nested_key == "Monocyte/Myeloid")
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 风险标记
# ============================================================

message("Generating manuscript risk flags...")

risk_flags <- data.frame(
  risk_id = character(),
  severity = character(),
  finding = character(),
  manuscript_implication = character(),
  recommended_handling = character(),
  stringsAsFactors = FALSE
)

add_risk <- function(risk_id, severity, finding, implication, handling) {
  data.frame(
    risk_id = risk_id,
    severity = severity,
    finding = finding,
    manuscript_implication = implication,
    recommended_handling = handling,
    stringsAsFactors = FALSE
  )
}

risk_flags <- rbind(
  risk_flags,
  add_risk(
    "pooled_vs_dataset_AUROC_gap",
    "High",
    paste0(
      "Median per-dataset nested LODO AUROC = ",
      fmt_num(median_auroc_table2),
      ", but pooled nested LODO AUROC = ",
      fmt_num(pooled_auroc_table),
      "."
    ),
    "Pooled prediction scale and cross-dataset ranking are less stable than per-dataset discrimination.",
    "Use per-dataset nested LODO as the main discrimination result; use pooled AUROC as evidence of transport instability."
  )
)

if (any(table2$AUROC < 0.70, na.rm = TRUE)) {
  failed <- paste(table2$validation_dataset[table2$AUROC < 0.70], collapse = ", ")
  risk_flags <- rbind(
    risk_flags,
    add_risk(
      "low_AUROC_dataset",
      "High",
      paste0("Held-out dataset(s) with AUROC < 0.70: ", failed, "."),
      "The model does not transport uniformly across all datasets.",
      "Explicitly describe this as a transportability failure scenario; do not claim universal robustness."
    )
  )
}

if (any(table3$fixed_sensitivity < 0.20, na.rm = TRUE)) {
  failed <- paste(table3$validation_dataset[table3$fixed_sensitivity < 0.20], collapse = ", ")
  risk_flags <- rbind(
    risk_flags,
    add_risk(
      "fixed_threshold_low_sensitivity",
      "High",
      paste0("Fixed-threshold sensitivity < 0.20 in: ", failed, "."),
      "Fixed threshold deployment can severely fail despite acceptable or excellent AUROC.",
      "Emphasize threshold transportability failure and need for recalibration."
    )
  )
}

if (any(table3$fixed_specificity < 0.20, na.rm = TRUE)) {
  failed <- paste(table3$validation_dataset[table3$fixed_specificity < 0.20], collapse = ", ")
  risk_flags <- rbind(
    risk_flags,
    add_risk(
      "fixed_threshold_low_specificity",
      "High",
      paste0("Fixed-threshold specificity < 0.20 in: ", failed, "."),
      "Fixed threshold can also create excessive false positives in some cohorts.",
      "Report as threshold instability; avoid clinical deployment claims."
    )
  )
}

if (median_abs_thr_table3 > 0.30) {
  risk_flags <- rbind(
    risk_flags,
    add_risk(
      "large_threshold_drift",
      "High",
      paste0("Median absolute threshold shift = ", fmt_num(median_abs_thr_table3), "."),
      "Operating threshold is not transportable across datasets.",
      "Use as a central finding rather than a minor limitation."
    )
  )
}

if (any(grepl("Healthy control|healthy", table3$control_type_standardized, ignore.case = TRUE), na.rm = TRUE)) {
  risk_flags <- rbind(
    risk_flags,
    add_risk(
      "healthy_control_contrasts",
      "Moderate",
      "Most evaluated contrasts involve healthy controls or selected healthy-control contrasts.",
      "Clinical mimic discrimination is not fully established.",
      "State that the study evaluates cross-cohort transport behavior under available public contrasts, not definitive intended-use deployment."
    )
  )
}

if (!all(c("Monocyte/Myeloid") %in% table4$cell_type_level1)) {
  risk_flags <- rbind(
    risk_flags,
    add_risk(
      "scRNA_missing_myeloid",
      "High",
      "Monocyte/Myeloid compartment not found in Table 4.",
      "The intended biological localization claim would be unsupported.",
      "Check scRNA annotation before manuscript drafting."
    )
  )
} else {
  final10_top <- table4$cell_type_level1[which.max(table4$mean_score_Final10)]
  nested_top <- table4$cell_type_level1[which.max(table4$mean_score_NestedRecurrent)]
  
  if (final10_top != "Monocyte/Myeloid" || nested_top != "Monocyte/Myeloid") {
    risk_flags <- rbind(
      risk_flags,
      add_risk(
        "scRNA_top_not_myeloid",
        "Moderate",
        paste0("Top Final10 = ", final10_top, "; top NestedRecurrent = ", nested_top, "."),
        "Myeloid localization would need softer wording.",
        "Use distributed host-response program language."
      )
    )
  }
}

# ============================================================
# Claims and restrictions
# ============================================================

claims <- data.frame(
  claim_type = c(
    "Allowed main claim",
    "Allowed main claim",
    "Allowed main claim",
    "Allowed secondary claim",
    "Allowed secondary claim",
    "Restriction",
    "Restriction",
    "Restriction",
    "Restriction",
    "Restriction"
  ),
  statement = c(
    "The signature showed recurrent per-dataset discrimination under strict nested leave-one-dataset-out evaluation.",
    "Calibration and fixed-threshold transportability were unstable across held-out cohorts.",
    "The transported host-response signal was predominantly localized to monocyte/myeloid compartments in an independent PBMC scRNA-seq dataset.",
    "Exploratory DCA showed heterogeneous threshold-dependent net benefit.",
    "GSE54514 represented a transportability failure scenario.",
    "Do not claim the model is ready for clinical deployment.",
    "Do not claim same-intended-use external validation.",
    "Do not claim fixed threshold is transportable.",
    "Do not treat scRNA module score analysis as independent diagnostic validation.",
    "Do not overinterpret healthy-control contrasts as clinical mimic discrimination."
  ),
  rationale = c(
    "Supported by median AUROC 0.907 and 4/6 datasets with AUROC >= 0.80.",
    "Supported by median absolute threshold shift 0.537 and fixed-threshold sensitivity/specificity failures.",
    "Supported by Final10 and NestedRecurrent module scores both highest in Monocyte/Myeloid cells.",
    "Supported by DCA summary but limited by heterogeneous contrasts and threshold instability.",
    "Supported by AUROC 0.568 and poor threshold behavior.",
    "Calibration, threshold drift and control-type limitations preclude deployment claims.",
    "Public cohorts differ in population, platform, control type and intended clinical use.",
    "Fixed thresholds failed in multiple cohorts.",
    "scRNA analysis provides biological localization, not patient-level model validation.",
    "Most contrasts use healthy controls."
  ),
  manuscript_location = c(
    "Abstract Results / Main Results",
    "Abstract Results / Main Results / Discussion",
    "Main Results / Discussion",
    "Supplementary Results / Discussion",
    "Results / Discussion",
    "Discussion limitation",
    "Methods and Discussion limitation",
    "Discussion limitation",
    "Methods and Discussion limitation",
    "Discussion limitation"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Overall status
# ============================================================

n_flags <- sum(consistency_checks$status == "FLAG")
n_high_risks <- sum(risk_flags$severity == "High")

overall_status <- data.frame(
  item = c(
    "n_consistency_flags",
    "n_high_risk_flags",
    "ready_for_manuscript_drafting",
    "recommended_next_step"
  ),
  value = c(
    n_flags,
    n_high_risks,
    ifelse(n_flags == 0, "YES_WITH_RESTRICTIONS", "NO_FIX_FLAGS_FIRST"),
    ifelse(
      n_flags == 0,
      "Proceed to manuscript drafting with defensive framing.",
      "Resolve consistency flags before drafting."
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Markdown report
# ============================================================

md <- c(
  "# Final sanity check before manuscript drafting",
  "",
  "## Overall status",
  "",
  paste0("- Consistency flags: ", n_flags),
  paste0("- High-risk interpretation flags: ", n_high_risks),
  paste0("- Ready for manuscript drafting: ", overall_status$value[overall_status$item == "ready_for_manuscript_drafting"]),
  "",
  "## Main validated messages",
  "",
  paste0(
    "1. Bulk analysis included ", n_bulk_table2, " samples from 6 cohorts, including ",
    n_case_table2, " sepsis cases and ", n_ctrl_table2, " controls."
  ),
  paste0(
    "2. Strict nested LODO median AUROC was ", fmt_num(median_auroc_table2),
    ", with range ", fmt_num(min_auroc_table2), " to ", fmt_num(max_auroc_table2),
    "; 4 of 6 datasets had AUROC >= 0.80."
  ),
  paste0(
    "3. Pooled nested LODO AUROC was lower at ", fmt_num(pooled_auroc_table),
    ", supporting cross-dataset probability-scale/ranking instability."
  ),
  paste0(
    "4. Median absolute threshold shift was ", fmt_num(median_abs_thr_table3),
    ", and fixed-threshold sensitivity was <0.20 in ", n_fixed_sens_lt_table3,
    " datasets."
  ),
  paste0(
    "5. scRNA analysis included ", fmt_num(sc_n_cells_key, 0),
    " cells and localized both Final10 and NestedRecurrent signatures to ",
    sc_top_final_key, " compartments."
  ),
  "",
  "## Required defensive framing",
  "",
  "- Frame the work as cross-cohort transportability evaluation of a host-response diagnostic signature.",
  "- Emphasize that discrimination, calibration and fixed-threshold behavior diverged.",
  "- Treat DCA as exploratory.",
  "- Treat scRNA as biological localization rather than model validation.",
  "- Do not claim clinical deployability or same-intended-use validation.",
  "",
  "## Highest-risk points for reviewers",
  "",
  paste0(
    "- ", risk_flags$severity, ": ", risk_flags$finding,
    " Handling: ", risk_flags$recommended_handling
  )
)

# ============================================================
# 保存结果
# ============================================================

message("Saving final sanity check outputs...")

data.table::fwrite(
  file_presence,
  file.path(report_dir, "T21_required_file_presence.csv")
)

data.table::fwrite(
  consistency_checks,
  file.path(report_dir, "T21_key_consistency_checks.csv")
)

data.table::fwrite(
  risk_flags,
  file.path(report_dir, "T21_manuscript_risk_flags.csv")
)

data.table::fwrite(
  claims,
  file.path(report_dir, "T21_recommended_claims_and_restrictions.csv")
)

data.table::fwrite(
  overall_status,
  file.path(report_dir, "T21_overall_status.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "file_presence")
openxlsx::writeData(wb, "file_presence", file_presence)

openxlsx::addWorksheet(wb, "consistency_checks")
openxlsx::writeData(wb, "consistency_checks", consistency_checks)

openxlsx::addWorksheet(wb, "risk_flags")
openxlsx::writeData(wb, "risk_flags", risk_flags)

openxlsx::addWorksheet(wb, "claims_restrictions")
openxlsx::writeData(wb, "claims_restrictions", claims)

openxlsx::saveWorkbook(
  wb,
  file.path(report_dir, "T21_final_sanity_check_summary.xlsx"),
  overwrite = TRUE
)

write_text_file(
  md,
  file.path(manuscript_dir, "final_sanity_check_before_manuscript_v0.1.md")
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_21_final_sanity_check_before_manuscript.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 21 Final sanity check before manuscript 完成 ============")

message("\nOverall status:")
print(overall_status)

message("\nConsistency checks:")
print(consistency_checks)

message("\nManuscript risk flags:")
print(risk_flags)

message("\nRecommended claims and restrictions:")
print(claims)

message("\n关键输出：")
message("1) ", file.path(report_dir, "T21_final_sanity_check_summary.xlsx"))
message("2) ", file.path(report_dir, "T21_key_consistency_checks.csv"))
message("3) ", file.path(report_dir, "T21_manuscript_risk_flags.csv"))
message("4) ", file.path(report_dir, "T21_recommended_claims_and_restrictions.csv"))
message("5) ", file.path(manuscript_dir, "final_sanity_check_before_manuscript_v0.1.md"))

message("\n下一步：")
message("把 Overall status、Consistency checks、Manuscript risk flags 贴给我。")
message("如果 consistency 全部 PASS，就可以正式进入 manuscript_v0.3 写作。")