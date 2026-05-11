# ============================================================
# 15_cohort_control_type_eligibility_table.R
# Cohort control type and eligibility table
#
# 目的：
# 1. 整理每个 bulk 队列的 control type、sample type、timepoint 和纳入理由
# 2. 合并 nested LODO 结果，辅助解释失败队列和阈值漂移
# 3. 生成 manuscript-ready cohort eligibility table
#
# 输入：
# 04_results/reporting/T11_table1_cohort_sample_summary.csv
# 04_results/nested_LODO/T14_nested_LODO_validation_metrics.csv
# 04_results/nested_LODO/T14_nested_LODO_calibration_metrics.csv
# 04_results/nested_LODO/T14_nested_LODO_threshold_drift.csv
#
# 输出：
# 04_results/reporting/
#   T15_cohort_control_type_eligibility_table.csv
#   T15_cohort_control_type_eligibility_table.xlsx
#   T15_cohort_interpretation_notes.csv
#   T15_control_type_summary.csv
#
# 05_figures/reporting/
#   F15A_control_type_distribution.png/pdf
#   F15B_AUROC_by_control_type.png/pdf
#   F15C_Brier_by_control_type.png/pdf
#   F15D_threshold_shift_by_control_type.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

report_dir <- file.path(project_dir, "04_results", "reporting")
nested_dir <- file.path(project_dir, "04_results", "nested_LODO")
fig_dir <- file.path(project_dir, "05_figures", "reporting")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

cohort_summary_file <- file.path(report_dir, "T11_table1_cohort_sample_summary.csv")
nested_metrics_file <- file.path(nested_dir, "T14_nested_LODO_validation_metrics.csv")
nested_cal_file <- file.path(nested_dir, "T14_nested_LODO_calibration_metrics.csv")
nested_threshold_file <- file.path(nested_dir, "T14_nested_LODO_threshold_drift.csv")
nested_gene_freq_file <- file.path(nested_dir, "T14_nested_LODO_gene_selection_frequency.csv")

needed <- c(
  cohort_summary_file,
  nested_metrics_file,
  nested_cal_file,
  nested_threshold_file
)

missing <- needed[!file.exists(needed)]
if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 工具函数
# ============================================================

safe_fread <- function(file) {
  if (!file.exists(file)) return(data.frame())
  data.table::fread(file, data.table = FALSE)
}

write_csv_safe <- function(df, file) {
  data.table::fwrite(df, file)
}

fmt_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  ifelse(is.na(x), NA_character_, formatC(x, digits = digits, format = "f"))
}

# ============================================================
# 读取分析结果
# ============================================================

message("Reading cohort and nested LODO results...")

cohort_summary <- safe_fread(cohort_summary_file)
nested_metrics <- safe_fread(nested_metrics_file)
nested_cal <- safe_fread(nested_cal_file)
nested_threshold <- safe_fread(nested_threshold_file)
nested_gene_freq <- safe_fread(nested_gene_freq_file)

cohort_summary <- cohort_summary[cohort_summary$dataset != "Total", , drop = FALSE]

# ============================================================
# 手工整理队列背景表
# 后续如果发现原文细节有误，只改这一块即可
# ============================================================

message("Building manually curated cohort metadata...")

cohort_manual <- data.frame(
  dataset = c(
    "GSE137340",
    "GSE236713",
    "GSE54514",
    "GSE57065",
    "GSE65682",
    "GSE95233"
  ),
  
  GEO_title = c(
    "Blood transcriptome of human sepsis in an Indian cohort [Illumina array]",
    "Blood transcriptional profiles of sepsis and SIRS patients at ICU admission and follow-up",
    "Whole blood transcriptome of survivors and nonsurvivors of sepsis",
    "Systemic genomic response in severe ICU patients with septic shock",
    "Sepsis whole-blood transcriptome cohort with healthy controls and ICU sepsis samples",
    "Whole blood transcriptome of septic shock and healthy controls"
  ),
  
  platform = c(
    "GPL10558",
    "GPL17077",
    "GPL6947",
    "GPL570",
    "GPL13667",
    "GPL570"
  ),
  
  assay_type = c(
    "Microarray",
    "Microarray",
    "Microarray",
    "Microarray",
    "Microarray",
    "Microarray"
  ),
  
  sample_type = c(
    "Whole blood",
    "Whole blood",
    "Whole blood",
    "Whole blood",
    "Whole blood",
    "Whole blood"
  ),
  
  population = c(
    "Adult ICU severe sepsis / septic shock cohort, India",
    "Adult ICU sepsis and SIRS patients from UK hospitals plus healthy volunteers",
    "Adult ICU sepsis cohort with serial sampling and survival status",
    "Adult ICU septic shock cohort with healthy controls and early serial sampling",
    "Adult sepsis / ICU cohort with selected healthy controls for current main contrast",
    "Adult septic shock versus healthy controls"
  ),
  
  sepsis_case_definition_simplified = c(
    "Severe sepsis or septic shock",
    "Sepsis at ICU admission or early ICU course",
    "ICU sepsis patients",
    "Septic shock at onset",
    "Sepsis subset selected for current diagnostic contrast",
    "Septic shock"
  ),
  
  original_control_description = c(
    "Healthy donors / non-sepsis controls according to GEO metadata",
    "Healthy volunteers; SIRS patients available but excluded from current main Sepsis vs Healthy contrast",
    "Non-sepsis baseline samples inferred from harmonized phenotype; original study primarily serial sepsis monitoring",
    "Healthy donors used as controls in current main contrast",
    "Healthy subjects held separately; selected healthy controls used for current contrast",
    "Healthy controls"
  ),
  
  control_type_standardized = c(
    "Healthy or non-sepsis control",
    "Healthy control; SIRS available but not in main contrast",
    "Unclear or inferred control",
    "Healthy control",
    "Healthy control",
    "Healthy control"
  ),
  
  clinical_control_strength = c(
    "Moderate",
    "Potentially strong if SIRS extension is analyzed; moderate in current healthy-control contrast",
    "Weak to uncertain",
    "Weak to moderate",
    "Weak to moderate",
    "Weak"
  ),
  
  timepoint_used = c(
    "Baseline / initial blood sample according to current harmonization",
    "Day 1 / baseline ICU sample for sepsis; healthy volunteers as control",
    "Baseline / first available sample inferred; follow-up samples excluded",
    "Within 30 minutes after shock for sepsis; healthy controls",
    "Selected sepsis subset and healthy controls",
    "Baseline septic shock samples and healthy controls"
  ),
  
  followup_or_serial_samples = c(
    "No main longitudinal use in current analysis",
    "Serial day 1, day 2, day 5 and discharge samples exist; non-baseline samples excluded",
    "Daily up to 5 days; non-baseline samples excluded",
    "30 min, 24 h and 48 h samples exist; non-baseline samples excluded",
    "Complex ICU cohort; only selected subset used",
    "No main longitudinal use in current analysis"
  ),
  
  included_in_main_bulk = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  included_in_nested_LODO = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  
  main_limitation = c(
    "Control definition should be verified against source metadata and original paper.",
    "Main analysis currently excludes SIRS, so clinical mimic discrimination is not fully exploited.",
    "Control group and baseline definition require manual verification; nested LODO performance was poor.",
    "Sepsis group is septic shock, while controls are healthy; case-control contrast may be strong.",
    "Complex ICU cohort; selected subset may not represent full intended-use population.",
    "Septic shock versus healthy control contrast may overestimate discrimination."
  ),
  
  interpretation_note = c(
    "Moderate AUROC in nested LODO suggests partial transportability.",
    "Excellent AUROC but poor fixed-threshold sensitivity indicates strong ranking with severe probability-scale drift.",
    "Failed held-out AUROC suggests cohort-specific phenotype, control definition, platform, or timepoint mismatch.",
    "Perfect AUROC likely reflects a strong shock-versus-healthy contrast; threshold behavior still requires caution.",
    "Acceptable AUROC but fixed threshold failed, supporting calibration drift.",
    "Perfect AUROC likely reflects a strong septic-shock-versus-healthy contrast."
  ),
  
  GEO_URL = c(
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE137340",
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE236713",
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE54514",
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE57065",
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65682",
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE95233"
  ),
  
  curation_status = c(
    "Needs manual source verification",
    "GEO metadata reviewed",
    "GEO metadata reviewed; control definition needs verification",
    "GEO metadata reviewed",
    "Needs manual source verification",
    "Needs manual source verification"
  ),
  
  stringsAsFactors = FALSE
)

# ============================================================
# 合并样本数和 nested LODO 结果
# ============================================================

message("Merging cohort metadata with nested LODO metrics...")

cohort_table <- merge(
  cohort_manual,
  cohort_summary[, intersect(
    c("dataset", "Control", "Sepsis", "total", "n_probes_input", "n_genes_after_collapse"),
    colnames(cohort_summary)
  ), drop = FALSE],
  by = "dataset",
  all.x = TRUE,
  sort = FALSE
)

nested_keep <- intersect(
  c(
    "validation_dataset", "n", "n_case", "n_control", "prevalence",
    "AUROC", "AUROC_low", "AUROC_high", "AUPRC", "Brier",
    "threshold", "sensitivity", "specificity", "PPV", "NPV", "accuracy",
    "n_refined_candidates", "n_selected_genes", "pool_used"
  ),
  colnames(nested_metrics)
)

nested_metrics_small <- nested_metrics[, nested_keep, drop = FALSE]
colnames(nested_metrics_small)[colnames(nested_metrics_small) == "validation_dataset"] <- "dataset"

cohort_table <- merge(
  cohort_table,
  nested_metrics_small,
  by = "dataset",
  all.x = TRUE,
  sort = FALSE
)

cal_keep <- intersect(
  c(
    "validation_dataset", "observed_rate", "mean_predicted",
    "calibration_in_the_large", "calibration_intercept", "calibration_slope"
  ),
  colnames(nested_cal)
)

cal_small <- nested_cal[, cal_keep, drop = FALSE]
colnames(cal_small)[colnames(cal_small) == "validation_dataset"] <- "dataset"

cohort_table <- merge(
  cohort_table,
  cal_small,
  by = "dataset",
  all.x = TRUE,
  sort = FALSE
)

thr_keep <- intersect(
  c(
    "validation_dataset", "fixed_training_threshold", "local_youden_threshold",
    "threshold_shift", "abs_threshold_shift",
    "fixed_sensitivity", "fixed_specificity",
    "local_sensitivity", "local_specificity"
  ),
  colnames(nested_threshold)
)

thr_small <- nested_threshold[, thr_keep, drop = FALSE]
colnames(thr_small)[colnames(thr_small) == "validation_dataset"] <- "dataset"

cohort_table <- merge(
  cohort_table,
  thr_small,
  by = "dataset",
  all.x = TRUE,
  sort = FALSE
)

# 恢复原始顺序
cohort_table$dataset <- factor(cohort_table$dataset, levels = cohort_manual$dataset)
cohort_table <- cohort_table[order(cohort_table$dataset), , drop = FALSE]
cohort_table$dataset <- as.character(cohort_table$dataset)

# ============================================================
# 增加解释性分类
# ============================================================

message("Adding interpretation flags...")

cohort_table$AUROC_category <- ifelse(
  is.na(cohort_table$AUROC), NA,
  ifelse(
    cohort_table$AUROC >= 0.90, "Excellent",
    ifelse(
      cohort_table$AUROC >= 0.80, "Good",
      ifelse(
        cohort_table$AUROC >= 0.70, "Moderate",
        ifelse(cohort_table$AUROC >= 0.65, "Weak", "Poor")
      )
    )
  )
)

cohort_table$threshold_transportability <- ifelse(
  is.na(cohort_table$abs_threshold_shift), NA,
  ifelse(
    cohort_table$abs_threshold_shift < 0.10, "Stable",
    ifelse(cohort_table$abs_threshold_shift < 0.30, "Moderate drift", "Large drift")
  )
)

cohort_table$fixed_threshold_failure_flag <- ifelse(
  is.na(cohort_table$fixed_sensitivity) | is.na(cohort_table$fixed_specificity),
  NA,
  cohort_table$fixed_sensitivity < 0.20 | cohort_table$fixed_specificity < 0.20
)

cohort_table$main_manuscript_interpretation <- paste0(
  "Nested LODO AUROC category: ",
  cohort_table$AUROC_category,
  "; threshold transportability: ",
  cohort_table$threshold_transportability,
  "; main limitation: ",
  cohort_table$main_limitation
)

# ============================================================
# Control type summary
# ============================================================

control_type_summary <- aggregate(
  dataset ~ control_type_standardized + clinical_control_strength,
  data = cohort_table,
  FUN = length
)
colnames(control_type_summary)[colnames(control_type_summary) == "dataset"] <- "n_datasets"

control_type_summary <- control_type_summary[
  order(control_type_summary$clinical_control_strength, control_type_summary$control_type_standardized),
  ,
  drop = FALSE
]

# ============================================================
# Interpretation notes
# ============================================================

interpretation_notes <- cohort_table[, c(
  "dataset",
  "control_type_standardized",
  "clinical_control_strength",
  "timepoint_used",
  "AUROC",
  "AUPRC",
  "Brier",
  "fixed_sensitivity",
  "fixed_specificity",
  "threshold_shift",
  "main_limitation",
  "interpretation_note",
  "main_manuscript_interpretation"
), drop = FALSE]

# ============================================================
# Manuscript-ready compact table
# ============================================================

manuscript_table <- cohort_table[, c(
  "dataset",
  "platform",
  "sample_type",
  "population",
  "sepsis_case_definition_simplified",
  "control_type_standardized",
  "clinical_control_strength",
  "timepoint_used",
  "Sepsis",
  "Control",
  "total",
  "AUROC",
  "AUPRC",
  "Brier",
  "fixed_sensitivity",
  "fixed_specificity",
  "abs_threshold_shift",
  "AUROC_category",
  "threshold_transportability",
  "main_limitation"
), drop = FALSE]

# ============================================================
# 保存
# ============================================================

message("Writing output tables...")

write_csv_safe(
  cohort_table,
  file.path(report_dir, "T15_cohort_control_type_eligibility_table.csv")
)

write_csv_safe(
  interpretation_notes,
  file.path(report_dir, "T15_cohort_interpretation_notes.csv")
)

write_csv_safe(
  control_type_summary,
  file.path(report_dir, "T15_control_type_summary.csv")
)

write_csv_safe(
  manuscript_table,
  file.path(report_dir, "T15_manuscript_ready_cohort_table.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "full_cohort_table")
openxlsx::writeData(wb, "full_cohort_table", cohort_table)

openxlsx::addWorksheet(wb, "manuscript_table")
openxlsx::writeData(wb, "manuscript_table", manuscript_table)

openxlsx::addWorksheet(wb, "interpretation_notes")
openxlsx::writeData(wb, "interpretation_notes", interpretation_notes)

openxlsx::addWorksheet(wb, "control_type_summary")
openxlsx::writeData(wb, "control_type_summary", control_type_summary)

if (nrow(nested_gene_freq) > 0) {
  openxlsx::addWorksheet(wb, "nested_gene_frequency")
  openxlsx::writeData(wb, "nested_gene_frequency", nested_gene_freq)
}

openxlsx::saveWorkbook(
  wb,
  file.path(report_dir, "T15_cohort_control_type_eligibility_table.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 作图
# ============================================================

message("Saving figures...")

p1 <- ggplot2::ggplot(
  cohort_table,
  ggplot2::aes(x = control_type_standardized)
) +
  ggplot2::geom_bar(alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Control type distribution across included cohorts",
    x = "Control type",
    y = "Number of datasets"
  )

ggplot2::ggsave(
  file.path(fig_dir, "F15A_control_type_distribution.png"),
  p1,
  width = 7,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(fig_dir, "F15A_control_type_distribution.pdf"),
  p1,
  width = 7,
  height = 5
)

p2 <- ggplot2::ggplot(
  cohort_table,
  ggplot2::aes(x = dataset, y = AUROC, fill = clinical_control_strength)
) +
  ggplot2::geom_col(alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Nested LODO AUROC by cohort and control strength",
    x = "Held-out dataset",
    y = "AUROC",
    fill = "Control strength"
  )

ggplot2::ggsave(
  file.path(fig_dir, "F15B_AUROC_by_control_type.png"),
  p2,
  width = 7,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(fig_dir, "F15B_AUROC_by_control_type.pdf"),
  p2,
  width = 7,
  height = 5
)

p3 <- ggplot2::ggplot(
  cohort_table,
  ggplot2::aes(x = dataset, y = Brier, fill = clinical_control_strength)
) +
  ggplot2::geom_col(alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Nested LODO Brier score by cohort",
    x = "Held-out dataset",
    y = "Brier score",
    fill = "Control strength"
  )

ggplot2::ggsave(
  file.path(fig_dir, "F15C_Brier_by_control_type.png"),
  p3,
  width = 7,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(fig_dir, "F15C_Brier_by_control_type.pdf"),
  p3,
  width = 7,
  height = 5
)

p4 <- ggplot2::ggplot(
  cohort_table,
  ggplot2::aes(x = dataset, y = abs_threshold_shift, fill = clinical_control_strength)
) +
  ggplot2::geom_col(alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Absolute threshold shift by cohort",
    x = "Held-out dataset",
    y = "Absolute threshold shift",
    fill = "Control strength"
  )

ggplot2::ggsave(
  file.path(fig_dir, "F15D_threshold_shift_by_control_type.png"),
  p4,
  width = 7,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(fig_dir, "F15D_threshold_shift_by_control_type.pdf"),
  p4,
  width = 7,
  height = 5
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_15_cohort_control_type_eligibility_table.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 15 Cohort control type eligibility table 完成 ============")
message("输出目录：", report_dir)
message("图输出目录：", fig_dir)

message("\nManuscript-ready cohort table:")
print(manuscript_table)

message("\nControl type summary:")
print(control_type_summary)

message("\nInterpretation notes:")
print(interpretation_notes)

message("\n关键输出：")
message("1) ", file.path(report_dir, "T15_cohort_control_type_eligibility_table.xlsx"))
message("2) ", file.path(report_dir, "T15_cohort_control_type_eligibility_table.csv"))
message("3) ", file.path(report_dir, "T15_manuscript_ready_cohort_table.csv"))
message("4) ", file.path(report_dir, "T15_cohort_interpretation_notes.csv"))
message("5) ", file.path(report_dir, "T15_control_type_summary.csv"))
message("6) ", fig_dir)

message("\n下一步：")
message("请人工打开 T15_cohort_control_type_eligibility_table.xlsx，重点核对 control_type_standardized、timepoint_used 和 main_limitation。")
message("GSE54514、GSE65682、GSE236713 是优先核对对象。")