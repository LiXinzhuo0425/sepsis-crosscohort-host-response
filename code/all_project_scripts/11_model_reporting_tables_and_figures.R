# ============================================================
# 11_model_reporting_tables_and_figures.R
# Manuscript-ready reporting tables and result text
#
# 目标：
# 1. 汇总队列样本构成
# 2. 汇总 DEG / GO / candidate genes
# 3. 汇总 final compact model 的基因、系数、标准化参数和 apparent performance
# 4. 汇总 LODO validation、calibration、Brier 和 threshold drift
# 5. 输出 manuscript-ready tables 和可直接改写进论文的 Results draft
#
# 输入：
# 04_results/tables/
# 04_results/differential_expression/
# 04_results/enrichment/
# 04_results/candidate_selection/
# 04_results/model_validation/
# 04_results/final_model/
#
# 输出：
# 04_results/reporting/
#   T11_manuscript_ready_tables.xlsx
#   T11_table1_cohort_sample_summary.csv
#   T11_table2_final_model_coefficients.csv
#   T11_table3_LODO_validation_performance.csv
#   T11_table4_calibration_threshold_drift.csv
#   T11_table5_key_GO_terms.csv
#   T11_reporting_checklist.csv
#
# 07_manuscript/
#   results_draft_model_reporting.txt
#
# 05_figures/reporting/
#   F13A_LODO_AUROC_Brier_summary.png/pdf
#   F13B_threshold_drift_summary.png/pdf
#   F13C_final_model_coefficients.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

table_dir <- file.path(project_dir, "04_results", "tables")
de_dir <- file.path(project_dir, "04_results", "differential_expression")
enrich_dir <- file.path(project_dir, "04_results", "enrichment")
cand_dir <- file.path(project_dir, "04_results", "candidate_selection")
val_dir <- file.path(project_dir, "04_results", "model_validation")
final_dir <- file.path(project_dir, "04_results", "final_model")

report_dir <- file.path(project_dir, "04_results", "reporting")
fig_dir <- file.path(project_dir, "05_figures", "reporting")
manuscript_dir <- file.path(project_dir, "07_manuscript")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

sample_map_file <- file.path(table_dir, "T01_core_included_sample_map.csv")
merged_pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")

deg_file <- file.path(de_dir, "limma_DEG_Sepsis_vs_Control.csv")
annotation_qc_file <- file.path(de_dir, "T02_annotation_qc_by_dataset.csv")

go_file <- file.path(enrich_dir, "T05_GO_BP_enrichment.xlsx")
enrich_summary_file <- file.path(enrich_dir, "T05_enrichment_summary.xlsx")

refined_candidate_file <- file.path(cand_dir, "T07_refined_modeling_candidate_genes.csv")

lodo_metrics_file <- file.path(val_dir, "T08_LODO_validation_metrics.csv")
lodo_selected_file <- file.path(val_dir, "T08_LODO_selected_genes_by_fold.csv")
lodo_freq_file <- file.path(val_dir, "T08_LODO_gene_selection_frequency.csv")
cal_metrics_file <- file.path(val_dir, "T09_LODO_calibration_metrics.csv")
threshold_file <- file.path(val_dir, "T09_LODO_threshold_drift_metrics.csv")

final_coef_file <- file.path(final_dir, "T10_final_model_coefficients.csv")
final_scaling_file <- file.path(final_dir, "T10_final_model_scaling_parameters.csv")
final_metrics_file <- file.path(final_dir, "T10_final_model_apparent_metrics.csv")
final_selected_file <- file.path(final_dir, "T10_final_model_selected_genes.csv")
final_alpha_file <- file.path(final_dir, "T10_final_model_alpha_cv_auc.csv")

required_files <- c(
  deg_file,
  annotation_qc_file,
  refined_candidate_file,
  lodo_metrics_file,
  cal_metrics_file,
  threshold_file,
  final_coef_file,
  final_scaling_file,
  final_metrics_file,
  final_selected_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("缺少必要输入文件：\n", paste(missing_files, collapse = "\n"))
}

# ============================================================
# 工具函数
# ============================================================

safe_fread <- function(file) {
  if (!file.exists(file)) return(data.frame())
  data.table::fread(file, data.table = FALSE)
}

safe_read_xlsx <- function(file, sheet) {
  if (!file.exists(file)) return(data.frame())
  sheets <- try(openxlsx::getSheetNames(file), silent = TRUE)
  if (inherits(sheets, "try-error")) return(data.frame())
  if (!sheet %in% sheets) return(data.frame())
  out <- try(openxlsx::read.xlsx(file, sheet = sheet), silent = TRUE)
  if (inherits(out, "try-error")) return(data.frame())
  as.data.frame(out)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(as.numeric(x), digits = digits, format = "f"))
}

fmt_p <- function(x) {
  x <- as.numeric(x)
  out <- ifelse(
    is.na(x),
    NA_character_,
    ifelse(x < 0.001, formatC(x, format = "e", digits = 2), formatC(x, format = "f", digits = 3))
  )
  out
}

make_metric_ci <- function(est, low, high, digits = 3) {
  paste0(fmt_num(est, digits), " (", fmt_num(low, digits), "-", fmt_num(high, digits), ")")
}

write_csv_safe <- function(df, file) {
  if (is.null(df)) df <- data.frame()
  data.table::fwrite(df, file)
}

save_barplot_metric <- function(df, x_col, y_col, file_prefix, title_text, y_label) {
  
  if (nrow(df) == 0) return(NULL)
  if (!all(c(x_col, y_col) %in% colnames(df))) return(NULL)
  
  plot_df <- df
  plot_df[[x_col]] <- factor(plot_df[[x_col]], levels = plot_df[[x_col]][order(plot_df[[y_col]])])
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = title_text,
      x = NULL,
      y = y_label
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".pdf")),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

# ============================================================
# 读取数据
# ============================================================

message("Reading reporting inputs...")

deg <- safe_fread(deg_file)
annotation_qc <- safe_fread(annotation_qc_file)
refined_candidates <- safe_fread(refined_candidate_file)

lodo_metrics <- safe_fread(lodo_metrics_file)
lodo_selected <- safe_fread(lodo_selected_file)
lodo_frequency <- safe_fread(lodo_freq_file)

cal_metrics <- safe_fread(cal_metrics_file)
threshold_metrics <- safe_fread(threshold_file)

final_coef <- safe_fread(final_coef_file)
final_scaling <- safe_fread(final_scaling_file)
final_metrics <- safe_fread(final_metrics_file)
final_selected <- safe_fread(final_selected_file)
final_alpha <- safe_fread(final_alpha_file)

if (file.exists(sample_map_file)) {
  sample_map <- safe_fread(sample_map_file)
} else {
  sample_map <- safe_fread(merged_pheno_file)
}

# ============================================================
# Table 1: cohort and sample summary
# ============================================================

message("Building Table 1...")

if (!all(c("dataset", "sample_id", "clinical_group_main") %in% colnames(sample_map))) {
  stop("样本表缺少 dataset / sample_id / clinical_group_main。")
}

table1_long <- aggregate(
  sample_id ~ dataset + clinical_group_main,
  data = sample_map,
  FUN = length
)
colnames(table1_long)[colnames(table1_long) == "sample_id"] <- "n"

datasets <- sort(unique(table1_long$dataset))

table1 <- data.frame(
  dataset = datasets,
  Control = 0,
  Sepsis = 0,
  total = 0,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(table1))) {
  ds <- table1$dataset[i]
  z <- table1_long[table1_long$dataset == ds, , drop = FALSE]
  table1$Control[i] <- sum(z$n[z$clinical_group_main == "Control"], na.rm = TRUE)
  table1$Sepsis[i] <- sum(z$n[z$clinical_group_main == "Sepsis"], na.rm = TRUE)
  table1$total[i] <- table1$Control[i] + table1$Sepsis[i]
}

if (all(c("dataset", "platform") %in% colnames(annotation_qc))) {
  table1 <- merge(
    table1,
    annotation_qc[, c("dataset", "platform", "n_probes_input", "n_genes_after_collapse"), drop = FALSE],
    by = "dataset",
    all.x = TRUE,
    sort = FALSE
  )
}

table1_total <- data.frame(
  dataset = "Total",
  Control = sum(table1$Control, na.rm = TRUE),
  Sepsis = sum(table1$Sepsis, na.rm = TRUE),
  total = sum(table1$total, na.rm = TRUE),
  platform = "",
  n_probes_input = NA,
  n_genes_after_collapse = NA,
  stringsAsFactors = FALSE
)

table1_out <- rbind(table1, table1_total)

# ============================================================
# DEG and enrichment summary
# ============================================================

message("Building DEG and enrichment tables...")

deg_summary <- data.frame(
  metric = c(
    "Total common genes tested",
    "Significant DEGs, adjusted P < 0.05",
    "Significant DEGs, adjusted P < 0.05 and |logFC| >= 0.5",
    "Upregulated in sepsis, adjusted P < 0.05 and |logFC| >= 0.5",
    "Downregulated in sepsis, adjusted P < 0.05 and |logFC| >= 0.5"
  ),
  value = c(
    nrow(deg),
    sum(deg$adj.P.Val < 0.05, na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5, na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC > 0, na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC < 0, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

go_up <- safe_read_xlsx(go_file, "GO_BP_up_in_sepsis")
go_down <- safe_read_xlsx(go_file, "GO_BP_down_in_sepsis")
go_all <- safe_read_xlsx(go_file, "GO_BP_all_DEG")

top_go <- data.frame()

if (nrow(go_up) > 0) {
  go_up$gene_set <- "Upregulated in sepsis"
  top_go <- rbind(top_go, head(go_up[order(go_up$p.adjust), ], 10))
}

if (nrow(go_down) > 0) {
  go_down$gene_set <- "Downregulated in sepsis"
  top_go <- rbind(top_go, head(go_down[order(go_down$p.adjust), ], 10))
}

if (nrow(go_all) > 0) {
  go_all$gene_set <- "All significant DEGs"
  top_go <- rbind(top_go, head(go_all[order(go_all$p.adjust), ], 10))
}

if (nrow(top_go) > 0) {
  keep_go_cols <- intersect(
    c("gene_set", "ID", "Description", "GeneRatio", "BgRatio", "p.adjust", "Count", "geneID"),
    colnames(top_go)
  )
  top_go <- top_go[, keep_go_cols, drop = FALSE]
}

# ============================================================
# Table 2: final model coefficients and formula
# ============================================================

message("Building final model coefficient table...")

table2_coef <- final_coef
table2_coef$coefficient_rounded <- fmt_num(table2_coef$coefficient, 6)

if (nrow(final_scaling) > 0 && all(c("gene_symbol", "center", "scale") %in% colnames(final_scaling))) {
  table2_gene_coef <- table2_coef[table2_coef$term != "(Intercept)", , drop = FALSE]
  table2_gene_coef <- merge(
    table2_gene_coef,
    final_scaling,
    by.x = "term",
    by.y = "gene_symbol",
    all.x = TRUE,
    sort = FALSE
  )
  table2_gene_coef$center_rounded <- fmt_num(table2_gene_coef$center, 6)
  table2_gene_coef$scale_rounded <- fmt_num(table2_gene_coef$scale, 6)
} else {
  table2_gene_coef <- table2_coef
}

intercept_value <- final_coef$coefficient[final_coef$term == "(Intercept)"][1]

formula_terms <- final_coef[final_coef$term != "(Intercept)", , drop = FALSE]
formula_text <- paste0(
  "logit(P[sepsis]) = ",
  fmt_num(intercept_value, 6),
  paste0(
    ifelse(formula_terms$coefficient >= 0, " + ", " - "),
    fmt_num(abs(formula_terms$coefficient), 6),
    " * z(",
    formula_terms$term,
    ")",
    collapse = ""
  )
)

formula_df <- data.frame(
  item = c("Model formula", "Standardization"),
  description = c(
    formula_text,
    "Each gene expression value should be standardized using the center and scale parameters estimated from the pooled development data before applying the coefficient."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Table 3: LODO validation performance
# ============================================================

message("Building LODO validation table...")

lodo_val <- lodo_metrics[lodo_metrics$set == "validation", , drop = FALSE]

table3_lodo <- lodo_val[, intersect(
  c(
    "validation_dataset", "n", "n_case", "n_control", "prevalence",
    "AUROC", "AUROC_low", "AUROC_high", "AUPRC", "Brier",
    "threshold", "sensitivity", "specificity", "PPV", "NPV", "accuracy",
    "n_selected_genes", "alpha", "lambda_rule_used"
  ),
  colnames(lodo_val)
), drop = FALSE]

if (nrow(table3_lodo) > 0) {
  table3_lodo$AUROC_CI <- make_metric_ci(table3_lodo$AUROC, table3_lodo$AUROC_low, table3_lodo$AUROC_high)
  table3_lodo$AUPRC <- round(table3_lodo$AUPRC, 3)
  table3_lodo$Brier <- round(table3_lodo$Brier, 3)
  table3_lodo$sensitivity <- round(table3_lodo$sensitivity, 3)
  table3_lodo$specificity <- round(table3_lodo$specificity, 3)
  table3_lodo$threshold <- round(table3_lodo$threshold, 3)
}

lodo_summary <- data.frame(
  metric = c(
    "Number of LODO validation datasets",
    "Median AUROC",
    "Minimum AUROC",
    "Maximum AUROC",
    "Median AUPRC",
    "Median Brier score",
    "Validation datasets with AUROC >= 0.80",
    "Validation datasets with AUROC < 0.80"
  ),
  value = c(
    nrow(lodo_val),
    median(lodo_val$AUROC, na.rm = TRUE),
    min(lodo_val$AUROC, na.rm = TRUE),
    max(lodo_val$AUROC, na.rm = TRUE),
    median(lodo_val$AUPRC, na.rm = TRUE),
    median(lodo_val$Brier, na.rm = TRUE),
    sum(lodo_val$AUROC >= 0.80, na.rm = TRUE),
    sum(lodo_val$AUROC < 0.80, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Table 4: calibration and threshold drift
# ============================================================

message("Building calibration and threshold drift table...")

table4_cal <- merge(
  cal_metrics,
  threshold_metrics,
  by = "validation_dataset",
  all = TRUE,
  sort = FALSE
)

keep_cal_cols <- intersect(
  c(
    "validation_dataset", "n", "observed_rate", "mean_predicted",
    "calibration_in_the_large", "calibration_intercept", "calibration_slope",
    "Brier", "AUROC",
    "fixed_training_threshold", "local_youden_threshold", "threshold_shift",
    "abs_threshold_shift",
    "fixed_sensitivity", "fixed_specificity", "local_sensitivity", "local_specificity",
    "fixed_accuracy", "local_accuracy"
  ),
  colnames(table4_cal)
)

table4_cal <- table4_cal[, keep_cal_cols, drop = FALSE]

cal_summary <- data.frame(
  metric = c(
    "Median absolute calibration-in-the-large",
    "Median absolute threshold shift",
    "Maximum absolute threshold shift",
    "Datasets with fixed-threshold sensitivity < 0.20",
    "Datasets with fixed-threshold specificity < 0.20"
  ),
  value = c(
    median(abs(cal_metrics$calibration_in_the_large), na.rm = TRUE),
    median(abs(threshold_metrics$threshold_shift), na.rm = TRUE),
    max(abs(threshold_metrics$threshold_shift), na.rm = TRUE),
    sum(threshold_metrics$fixed_sensitivity < 0.20, na.rm = TRUE),
    sum(threshold_metrics$fixed_specificity < 0.20, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Candidate and gene selection summary
# ============================================================

message("Building candidate gene summary...")

candidate_summary <- refined_candidates[, intersect(
  c(
    "selection_order", "gene_symbol", "global_logFC", "global_adjP", "global_direction",
    "direction_consistency", "mean_auc", "min_auc", "n_auc_ge_070",
    "immune_GO_keyword_hit", "priority_score"
  ),
  colnames(refined_candidates)
), drop = FALSE]

if (nrow(lodo_frequency) > 0) {
  candidate_summary <- merge(
    candidate_summary,
    lodo_frequency,
    by = "gene_symbol",
    all.x = TRUE,
    sort = FALSE
  )
}

# ============================================================
# Final model apparent metrics
# ============================================================

message("Building final model apparent performance table...")

final_metric_table <- final_metrics

if (nrow(final_metric_table) > 0) {
  final_metric_table$AUROC_CI <- make_metric_ci(
    final_metric_table$AUROC,
    final_metric_table$AUROC_low,
    final_metric_table$AUROC_high
  )
}

# ============================================================
# Reporting checklist
# ============================================================

reporting_checklist <- data.frame(
  item = c(
    "Study type clearly stated",
    "Adult blood bulk transcriptome cohorts listed",
    "Probe-to-gene mapping and aggregation documented",
    "Phenotype harmonization documented",
    "Candidate gene screening documented",
    "Final model coefficients exported",
    "Standardization parameters exported",
    "Threshold reported",
    "LODO validation reported",
    "Calibration and Brier reported",
    "Threshold drift reported",
    "Information leakage limitation stated",
    "Final model labelled as candidate model for future independent validation"
  ),
  status = c(
    "done",
    "done",
    "done",
    "done",
    "done",
    "done",
    "done",
    "done",
    "done",
    "done",
    "done",
    "must describe in manuscript",
    "must describe in manuscript"
  ),
  comment = c(
    "Cross-cohort adult blood transcriptomic diagnostic model with transportability evaluation.",
    "Table 1 generated.",
    "Annotation QC generated in previous scripts.",
    "Harmonized sample map and phenotype files generated.",
    "DEG, GO, direction consistency and AUROC-based refinement completed.",
    "T10_final_model_coefficients.csv generated.",
    "T10_final_model_scaling_parameters.csv generated.",
    "Apparent Youden threshold generated.",
    "T08_LODO_validation_metrics.csv generated.",
    "T09 calibration metrics generated.",
    "T09 threshold drift metrics generated.",
    "Feature screening used multiple cohorts; LODO should be described as dataset-level transportability analysis, not fully independent external validation.",
    "Prospective or independent validation remains required before clinical deployment."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存 CSV
# ============================================================

message("Writing CSV tables...")

write_csv_safe(table1_out, file.path(report_dir, "T11_table1_cohort_sample_summary.csv"))
write_csv_safe(table2_coef, file.path(report_dir, "T11_table2_final_model_coefficients.csv"))
write_csv_safe(table3_lodo, file.path(report_dir, "T11_table3_LODO_validation_performance.csv"))
write_csv_safe(table4_cal, file.path(report_dir, "T11_table4_calibration_threshold_drift.csv"))
write_csv_safe(top_go, file.path(report_dir, "T11_table5_key_GO_terms.csv"))
write_csv_safe(reporting_checklist, file.path(report_dir, "T11_reporting_checklist.csv"))

# ============================================================
# 保存 Excel
# ============================================================

message("Writing manuscript-ready Excel workbook...")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Table1_cohorts")
openxlsx::writeData(wb, "Table1_cohorts", table1_out)

openxlsx::addWorksheet(wb, "DEG_summary")
openxlsx::writeData(wb, "DEG_summary", deg_summary)

openxlsx::addWorksheet(wb, "Key_GO_terms")
openxlsx::writeData(wb, "Key_GO_terms", top_go)

openxlsx::addWorksheet(wb, "Candidate_genes")
openxlsx::writeData(wb, "Candidate_genes", candidate_summary)

openxlsx::addWorksheet(wb, "Final_model_formula")
openxlsx::writeData(wb, "Final_model_formula", formula_df)

openxlsx::addWorksheet(wb, "Final_model_coefficients")
openxlsx::writeData(wb, "Final_model_coefficients", table2_coef)

openxlsx::addWorksheet(wb, "Final_model_scaling")
openxlsx::writeData(wb, "Final_model_scaling", final_scaling)

openxlsx::addWorksheet(wb, "Final_model_apparent")
openxlsx::writeData(wb, "Final_model_apparent", final_metric_table)

openxlsx::addWorksheet(wb, "LODO_validation")
openxlsx::writeData(wb, "LODO_validation", table3_lodo)

openxlsx::addWorksheet(wb, "LODO_summary")
openxlsx::writeData(wb, "LODO_summary", lodo_summary)

openxlsx::addWorksheet(wb, "Calibration_threshold")
openxlsx::writeData(wb, "Calibration_threshold", table4_cal)

openxlsx::addWorksheet(wb, "Calibration_summary")
openxlsx::writeData(wb, "Calibration_summary", cal_summary)

openxlsx::addWorksheet(wb, "Gene_selection_frequency")
openxlsx::writeData(wb, "Gene_selection_frequency", lodo_frequency)

openxlsx::addWorksheet(wb, "Reporting_checklist")
openxlsx::writeData(wb, "Reporting_checklist", reporting_checklist)

openxlsx::saveWorkbook(
  wb,
  file.path(report_dir, "T11_manuscript_ready_tables.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 论文结果段落草稿
# ============================================================

message("Writing draft results text...")

final_genes_text <- paste(final_selected$gene_symbol, collapse = ", ")

median_auc <- median(lodo_val$AUROC, na.rm = TRUE)
min_auc <- min(lodo_val$AUROC, na.rm = TRUE)
max_auc <- max(lodo_val$AUROC, na.rm = TRUE)
median_auprc <- median(lodo_val$AUPRC, na.rm = TRUE)
median_brier <- median(lodo_val$Brier, na.rm = TRUE)

n_lodo_good <- sum(lodo_val$AUROC >= 0.80, na.rm = TRUE)
n_lodo_total <- nrow(lodo_val)

median_abs_cal <- median(abs(cal_metrics$calibration_in_the_large), na.rm = TRUE)
median_abs_thr <- median(abs(threshold_metrics$threshold_shift), na.rm = TRUE)
max_abs_thr <- max(abs(threshold_metrics$threshold_shift), na.rm = TRUE)

draft_lines <- c(
  "Results draft for manuscript reporting",
  "",
  "Cohort composition",
  paste0(
    "A total of ", sum(table1$Control, na.rm = TRUE) + sum(table1$Sepsis, na.rm = TRUE),
    " adult blood transcriptome samples were included across ", nrow(table1),
    " public cohorts, including ", sum(table1$Sepsis, na.rm = TRUE),
    " sepsis samples and ", sum(table1$Control, na.rm = TRUE),
    " controls."
  ),
  "",
  "Differential expression and functional enrichment",
  paste0(
    "Differential expression analysis identified ",
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5, na.rm = TRUE),
    " significant genes at adjusted P < 0.05 and |logFC| >= 0.5, including ",
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC > 0, na.rm = TRUE),
    " genes upregulated in sepsis and ",
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC < 0, na.rm = TRUE),
    " genes downregulated in sepsis."
  ),
  "GO Biological Process enrichment indicated upregulation of myeloid leukocyte activation, response to bacterium, humoral immune response, inflammatory response and coagulation-related processes, while downregulated genes were enriched for T cell activation, adaptive immune response and antigen receptor-mediated signaling.",
  "",
  "Final compact model",
  paste0(
    "The final compact model included ten genes: ",
    final_genes_text,
    "."
  ),
  paste0(
    "In the pooled development dataset, the final model showed apparent AUROC ",
    fmt_num(final_metrics$AUROC[1], 3),
    " with 95% CI ",
    fmt_num(final_metrics$AUROC_low[1], 3),
    " to ",
    fmt_num(final_metrics$AUROC_high[1], 3),
    ", AUPRC ",
    fmt_num(final_metrics$AUPRC[1], 3),
    ", Brier score ",
    fmt_num(final_metrics$Brier[1], 3),
    ", sensitivity ",
    fmt_num(final_metrics$sensitivity[1], 3),
    " and specificity ",
    fmt_num(final_metrics$specificity[1], 3),
    " at the apparent Youden threshold."
  ),
  "",
  "Leave-one-dataset-out transportability evaluation",
  paste0(
    "In leave-one-dataset-out validation, AUROC ranged from ",
    fmt_num(min_auc, 3),
    " to ",
    fmt_num(max_auc, 3),
    " with a median AUROC of ",
    fmt_num(median_auc, 3),
    ". ",
    n_lodo_good,
    " of ",
    n_lodo_total,
    " validation datasets achieved AUROC >= 0.80."
  ),
  paste0(
    "The median AUPRC was ",
    fmt_num(median_auprc, 3),
    ", and the median Brier score was ",
    fmt_num(median_brier, 3),
    "."
  ),
  "",
  "Calibration and threshold transportability",
  paste0(
    "Calibration analysis revealed cohort-dependent probability-scale drift, with a median absolute calibration-in-the-large of ",
    fmt_num(median_abs_cal, 3),
    ". The absolute shift between the fixed training threshold and validation-set local Youden threshold had a median of ",
    fmt_num(median_abs_thr, 3),
    " and a maximum of ",
    fmt_num(max_abs_thr, 3),
    "."
  ),
  "These findings indicate that the host-response model had transportable ranking performance across several cohorts, whereas the absolute probability scale and fixed decision threshold were not consistently transportable. Local recalibration should be considered before clinical translation.",
  "",
  "Important wording boundary",
  "The final compact model should be described as a candidate model trained on pooled public adult bulk transcriptome cohorts. The LODO analysis should be described as dataset-level transportability evaluation. The present data should not be described as fully independent prospective validation."
)

writeLines(
  draft_lines,
  con = file.path(manuscript_dir, "results_draft_model_reporting.txt")
)

# ============================================================
# 图
# ============================================================

message("Saving reporting figures...")

if (nrow(lodo_val) > 0) {
  
  plot_lodo <- merge(
    lodo_val[, c("validation_dataset", "AUROC", "Brier"), drop = FALSE],
    cal_metrics[, c("validation_dataset", "mean_predicted", "observed_rate"), drop = FALSE],
    by = "validation_dataset",
    all.x = TRUE,
    sort = FALSE
  )
  
  p1 <- ggplot2::ggplot(
    plot_lodo,
    ggplot2::aes(x = validation_dataset, y = AUROC)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "LODO validation AUROC by dataset",
      x = "Validation dataset",
      y = "AUROC"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F13A_LODO_AUROC_Brier_summary.png"),
    plot = p1,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F13A_LODO_AUROC_Brier_summary.pdf"),
    plot = p1,
    width = 7,
    height = 5
  )
}

if (nrow(threshold_metrics) > 0) {
  
  p2 <- ggplot2::ggplot(
    threshold_metrics,
    ggplot2::aes(x = validation_dataset)
  ) +
    ggplot2::geom_point(ggplot2::aes(y = fixed_training_threshold), size = 3, alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(y = local_youden_threshold), size = 3, alpha = 0.85, shape = 17) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = validation_dataset,
        y = fixed_training_threshold,
        yend = local_youden_threshold
      ),
      linewidth = 0.6,
      alpha = 0.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Fixed threshold and local Youden threshold by dataset",
      x = "Validation dataset",
      y = "Probability threshold"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F13B_threshold_drift_summary.png"),
    plot = p2,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F13B_threshold_drift_summary.pdf"),
    plot = p2,
    width = 7,
    height = 5
  )
}

coef_plot_df <- final_coef[final_coef$term != "(Intercept)", , drop = FALSE]

if (nrow(coef_plot_df) > 0) {
  
  coef_plot_df$term <- factor(coef_plot_df$term, levels = coef_plot_df$term[order(coef_plot_df$coefficient)])
  
  p3 <- ggplot2::ggplot(
    coef_plot_df,
    ggplot2::aes(x = term, y = coefficient)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Final compact model coefficients",
      x = "Gene",
      y = "Coefficient"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F13C_final_model_coefficients.png"),
    plot = p3,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F13C_final_model_coefficients.pdf"),
    plot = p3,
    width = 7,
    height = 5
  )
}

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_11_model_reporting_tables_and_figures.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 11 Model reporting tables and figures 完成 ============")
message("Reporting 输出目录：", report_dir)
message("图输出目录：", fig_dir)
message("论文草稿目录：", manuscript_dir)

message("\nTable 1 cohort summary:")
print(table1_out)

message("\nFinal model formula:")
print(formula_df)

message("\nLODO summary:")
print(lodo_summary)

message("\nCalibration summary:")
print(cal_summary)

message("\n关键输出：")
message("1) ", file.path(report_dir, "T11_manuscript_ready_tables.xlsx"))
message("2) ", file.path(report_dir, "T11_table1_cohort_sample_summary.csv"))
message("3) ", file.path(report_dir, "T11_table2_final_model_coefficients.csv"))
message("4) ", file.path(report_dir, "T11_table3_LODO_validation_performance.csv"))
message("5) ", file.path(report_dir, "T11_table4_calibration_threshold_drift.csv"))
message("6) ", file.path(report_dir, "T11_table5_key_GO_terms.csv"))
message("7) ", file.path(manuscript_dir, "results_draft_model_reporting.txt"))
message("8) ", fig_dir)