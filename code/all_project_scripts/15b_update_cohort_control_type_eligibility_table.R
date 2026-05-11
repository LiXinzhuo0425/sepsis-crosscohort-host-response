# ============================================================
# 15b_update_cohort_control_type_eligibility_table.R
# Curated cohort control type and eligibility table, revised version
#
# 目的：
# 1. 基于 GEO 官方信息修正 T15 队列注释
# 2. 修正 GSE54514、GSE137340、GSE65682 的 control type
# 3. 增加 primary_contrast_used、unused_clinical_mimic_groups、recommended_manuscript_role
# 4. 输出更适合 manuscript v0.2 使用的 cohort table
#
# 输入：
# 04_results/reporting/T11_table1_cohort_sample_summary.csv
# 04_results/nested_LODO/T14_nested_LODO_validation_metrics.csv
# 04_results/nested_LODO/T14_nested_LODO_calibration_metrics.csv
# 04_results/nested_LODO/T14_nested_LODO_threshold_drift.csv
#
# 输出：
# 04_results/reporting/
#   T15b_cohort_control_type_eligibility_table.csv
#   T15b_cohort_control_type_eligibility_table.xlsx
#   T15b_manuscript_ready_cohort_table.csv
#   T15b_cohort_interpretation_notes.csv
#   T15b_control_type_summary.csv
#
# 05_figures/reporting/
#   F15b_control_type_distribution.png/pdf
#   F15b_AUROC_by_control_type.png/pdf
#   F15b_Brier_by_control_type.png/pdf
#   F15b_threshold_shift_by_control_type.png/pdf
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

# ============================================================
# 读取结果
# ============================================================

message("Reading cohort and nested LODO results...")

cohort_summary <- safe_fread(cohort_summary_file)
nested_metrics <- safe_fread(nested_metrics_file)
nested_cal <- safe_fread(nested_cal_file)
nested_threshold <- safe_fread(nested_threshold_file)
nested_gene_freq <- safe_fread(nested_gene_freq_file)

cohort_summary <- cohort_summary[cohort_summary$dataset != "Total", , drop = FALSE]

# ============================================================
# 修正版手工注释表
# ============================================================

message("Building revised manually curated cohort metadata...")

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
    "Transcriptomic profiling of severe sepsis / septic shock cases and matched healthy controls",
    "Blood transcriptional profiles of sepsis and SIRS patients with healthy volunteers",
    "Whole blood transcriptome of sepsis survivors, nonsurvivors, and healthy controls",
    "Whole blood transcriptome in septic shock with healthy volunteers and serial early sampling",
    "Genome-wide blood transcriptional profiling in critically ill patients, MARS consortium",
    "Whole blood transcriptome of septic shock patients and healthy volunteers"
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
    "Adult ICU severe sepsis / septic shock cases from India and age- and gender-matched healthy controls",
    "Adult ICU sepsis and SIRS patients from UK hospitals plus healthy volunteers",
    "Adult sepsis survivors and nonsurvivors with serial whole-blood samples plus healthy controls",
    "Adult ICU septic shock patients with early serial sampling plus healthy volunteers",
    "Critically ill ICU cohort from the MARS consortium with infectious and noninfectious categories; selected subset used here",
    "Adult septic shock patients and healthy volunteers"
  ),
  
  sepsis_case_definition_simplified = c(
    "Severe sepsis or septic shock",
    "Sepsis at ICU admission or early ICU course",
    "ICU sepsis survivors and nonsurvivors",
    "Septic shock at onset",
    "Selected sepsis subset from a complex ICU cohort",
    "Septic shock"
  ),
  
  original_control_description = c(
    "Age- and gender-matched healthy controls without inflammatory disease",
    "Healthy volunteers; SIRS patients are available as clinical mimic group but excluded from current primary contrast",
    "Healthy controls",
    "Healthy volunteers",
    "Healthy controls are available; original cohort also contains infectious and noninfectious critically ill categories",
    "Healthy volunteers"
  ),
  
  control_type_standardized = c(
    "Matched healthy control",
    "Healthy control in primary contrast; SIRS available",
    "Healthy control",
    "Healthy control",
    "Selected healthy controls in current contrast; original ICU infectious/noninfectious cohort not fully used",
    "Healthy control"
  ),
  
  clinical_control_strength = c(
    "Weak to moderate",
    "Moderate currently; strong potential after SIRS extension",
    "Weak to moderate",
    "Weak",
    "Moderate but complex",
    "Weak"
  ),
  
  primary_contrast_used = c(
    "Severe sepsis / septic shock D1 or baseline samples versus matched healthy controls",
    "Sepsis day 1 / baseline ICU samples versus healthy volunteers",
    "Baseline or first available sepsis samples versus healthy controls",
    "Septic shock within 30 minutes versus healthy volunteers",
    "Selected sepsis subset versus selected healthy controls",
    "Admission septic shock samples versus healthy volunteers"
  ),
  
  unused_clinical_mimic_groups = c(
    "None identified in current main dataset",
    "SIRS day 1, day 2, day 5, and ICU discharge samples; potential future clinical mimic extension",
    "Serial follow-up samples from sepsis patients; prognosis and time-course information not used in main diagnostic contrast",
    "24 h and 48 h septic shock follow-up samples not used in main diagnostic contrast",
    "Original MARS ICU infectious and noninfectious critical illness categories not fully used in current diagnostic contrast",
    "D2/D3 septic shock follow-up samples not used in main diagnostic contrast"
  ),
  
  timepoint_used = c(
    "D1 / time of diagnosis or baseline sample according to current harmonization",
    "Day 1 / baseline ICU sample for sepsis; healthy volunteers sampled once",
    "Baseline / first available sample; follow-up samples excluded",
    "Within 30 minutes after shock onset for sepsis; healthy volunteers as controls",
    "Selected sepsis subset and selected healthy controls",
    "Admission septic shock samples and healthy volunteers"
  ),
  
  followup_or_serial_samples = c(
    "D2 samples exist for sepsis cases but are not used in current main analysis",
    "Day 1, day 2, day 5 and ICU discharge samples exist for sepsis/SIRS; non-baseline samples excluded",
    "Daily samples for up to 5 days exist; non-baseline samples excluded",
    "30 min, 24 h and 48 h samples exist; non-baseline samples excluded",
    "Complex ICU sampling across admission and ICU course; only selected subset used",
    "Admission and D2/D3 samples exist; non-baseline samples excluded"
  ),
  
  included_in_main_bulk = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  included_in_nested_LODO = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  
  recommended_manuscript_role = c(
    "Primary nested LODO cohort with matched healthy-control contrast",
    "Primary nested LODO cohort and priority candidate for later SIRS clinical mimic extension",
    "Primary nested LODO failure-cohort analysis; healthy-control contrast with serial sepsis design",
    "Primary nested LODO cohort; strong septic-shock-versus-healthy contrast",
    "Primary nested LODO cohort with complex original ICU design and selected contrast limitation",
    "Primary nested LODO cohort; strong septic-shock-versus-healthy contrast"
  ),
  
  main_limitation = c(
    "Healthy-control contrast limits direct clinical mimic interpretation despite age- and gender-matching.",
    "Current primary contrast excludes SIRS, so clinical mimic discrimination is not yet tested.",
    "Original study focused on serial sepsis immune monitoring; poor nested LODO performance may reflect cohort, platform, or sampling-design differences.",
    "Septic shock versus healthy-control contrast may overestimate discrimination and does not test clinical mimics.",
    "Current analysis uses a selected sepsis-versus-healthy contrast and does not exploit the full infectious/noninfectious ICU design.",
    "Septic shock versus healthy-control contrast may overestimate discrimination and does not test clinical mimics."
  ),
  
  interpretation_note = c(
    "Moderate AUROC in nested LODO suggests partial transportability under matched healthy-control contrast.",
    "Excellent AUROC but poor fixed-threshold sensitivity indicates strong ranking with severe probability-scale drift.",
    "Poor held-out AUROC identifies this cohort as a transportability failure scenario requiring explicit discussion.",
    "Perfect AUROC likely reflects strong shock-versus-healthy separation; threshold behavior still requires caution.",
    "Good AUROC with fixed-threshold failure supports the central finding of calibration and threshold drift.",
    "Perfect AUROC likely reflects strong septic-shock-versus-healthy separation."
  ),
  
  source_verification_summary = c(
    "GEO reports sepsis cases and age/gender matched healthy controls, with D1 and D2 case samples.",
    "GEO reports sepsis, SIRS and healthy volunteers, with ICU day 1/day 2/day 5/discharge sampling.",
    "GEO reports sepsis survivors n=26, sepsis nonsurvivors n=9, and healthy controls n=18, with daily samples up to 5 days.",
    "GEO reports 28 septic shock patients sampled at 30 min, 24 h, 48 h and 25 healthy volunteers.",
    "GEO reports genome-wide blood transcriptional profiling in critically ill patients from the MARS consortium.",
    "GEO reports septic shock patients and healthy volunteers, with admission and follow-up septic shock samples."
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
    "GEO metadata reviewed",
    "GEO metadata reviewed",
    "GEO metadata reviewed",
    "GEO metadata reviewed",
    "GEO metadata reviewed; selected-subset limitation retained",
    "GEO metadata reviewed"
  ),
  
  stringsAsFactors = FALSE
)

# ============================================================
# 合并样本数和 nested LODO 结果
# ============================================================

message("Merging revised cohort metadata with nested LODO metrics...")

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

cohort_table$dataset <- factor(cohort_table$dataset, levels = cohort_manual$dataset)
cohort_table <- cohort_table[order(cohort_table$dataset), , drop = FALSE]
cohort_table$dataset <- as.character(cohort_table$dataset)

# ============================================================
# 解释性分类
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

cohort_table$clinical_mimic_available_flag <- grepl(
  "SIRS|infectious|noninfectious|ICU",
  cohort_table$unused_clinical_mimic_groups,
  ignore.case = TRUE
)

cohort_table$main_manuscript_interpretation <- paste0(
  "Nested LODO AUROC category: ",
  cohort_table$AUROC_category,
  "; threshold transportability: ",
  cohort_table$threshold_transportability,
  "; control context: ",
  cohort_table$control_type_standardized,
  "; limitation: ",
  cohort_table$main_limitation
)

# ============================================================
# 汇总表
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

interpretation_notes <- cohort_table[, c(
  "dataset",
  "control_type_standardized",
  "clinical_control_strength",
  "primary_contrast_used",
  "unused_clinical_mimic_groups",
  "recommended_manuscript_role",
  "timepoint_used",
  "AUROC",
  "AUPRC",
  "Brier",
  "fixed_sensitivity",
  "fixed_specificity",
  "threshold_shift",
  "AUROC_category",
  "threshold_transportability",
  "main_limitation",
  "interpretation_note",
  "source_verification_summary",
  "main_manuscript_interpretation"
), drop = FALSE]

manuscript_table <- cohort_table[, c(
  "dataset",
  "platform",
  "sample_type",
  "population",
  "sepsis_case_definition_simplified",
  "control_type_standardized",
  "clinical_control_strength",
  "primary_contrast_used",
  "unused_clinical_mimic_groups",
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
  "recommended_manuscript_role",
  "main_limitation"
), drop = FALSE]

# ============================================================
# 保存
# ============================================================

message("Writing revised output tables...")

write_csv_safe(
  cohort_table,
  file.path(report_dir, "T15b_cohort_control_type_eligibility_table.csv")
)

write_csv_safe(
  interpretation_notes,
  file.path(report_dir, "T15b_cohort_interpretation_notes.csv")
)

write_csv_safe(
  control_type_summary,
  file.path(report_dir, "T15b_control_type_summary.csv")
)

write_csv_safe(
  manuscript_table,
  file.path(report_dir, "T15b_manuscript_ready_cohort_table.csv")
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
  file.path(report_dir, "T15b_cohort_control_type_eligibility_table.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 作图
# ============================================================

message("Saving revised figures...")

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

ggplot2::ggsave(file.path(fig_dir, "F15b_control_type_distribution.png"), p1, width = 8, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F15b_control_type_distribution.pdf"), p1, width = 8, height = 5)

p2 <- ggplot2::ggplot(
  cohort_table,
  ggplot2::aes(x = dataset, y = AUROC, fill = clinical_control_strength)
) +
  ggplot2::geom_col(alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Nested LODO AUROC by cohort and control context",
    x = "Held-out dataset",
    y = "AUROC",
    fill = "Control context"
  )

ggplot2::ggsave(file.path(fig_dir, "F15b_AUROC_by_control_type.png"), p2, width = 8, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F15b_AUROC_by_control_type.pdf"), p2, width = 8, height = 5)

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
    fill = "Control context"
  )

ggplot2::ggsave(file.path(fig_dir, "F15b_Brier_by_control_type.png"), p3, width = 8, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F15b_Brier_by_control_type.pdf"), p3, width = 8, height = 5)

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
    fill = "Control context"
  )

ggplot2::ggsave(file.path(fig_dir, "F15b_threshold_shift_by_control_type.png"), p4, width = 8, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F15b_threshold_shift_by_control_type.pdf"), p4, width = 8, height = 5)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_15b_update_cohort_control_type_eligibility_table.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 15b Updated cohort control type eligibility table 完成 ============")
message("输出目录：", report_dir)
message("图输出目录：", fig_dir)

message("\nRevised manuscript-ready cohort table:")
print(manuscript_table)

message("\nRevised control type summary:")
print(control_type_summary)

message("\nRevised interpretation notes:")
print(interpretation_notes)

message("\n关键输出：")
message("1) ", file.path(report_dir, "T15b_cohort_control_type_eligibility_table.xlsx"))
message("2) ", file.path(report_dir, "T15b_cohort_control_type_eligibility_table.csv"))
message("3) ", file.path(report_dir, "T15b_manuscript_ready_cohort_table.csv"))
message("4) ", file.path(report_dir, "T15b_cohort_interpretation_notes.csv"))
message("5) ", file.path(report_dir, "T15b_control_type_summary.csv"))
message("6) ", fig_dir)

message("\n下一步：")
message("核对 T15b 后，进入 16_RNAseq_external_platform_validation.R。")