# ============================================================
# 20_update_manuscript_tables_and_results_after_nested_LODO_scRNA.R
# Manuscript-ready summary after nested LODO, calibration/DCA and scRNA localization
#
# 目的：
# 1. 整合 14 nested LODO、17 calibration/DCA、19 scRNA localization 的最终结果
# 2. 生成 manuscript-ready tables
# 3. 生成主图/补图计划
# 4. 生成英文 Results 段落素材
# 5. 生成中文解读备忘
#
# 输入：
# 04_results/nested_LODO/
#   T14_nested_LODO_validation_metrics.csv
#   T14_nested_LODO_gene_selection_frequency.csv
#   T14_nested_LODO_selected_genes_by_fold.csv
#
# 04_results/dca/
#   T17_pooled_nested_LODO_performance.csv
#   T17_nested_LODO_calibration_metrics_final.csv
#   T17_nested_LODO_threshold_drift_final.csv
#   T17_nested_LODO_DCA_summary.csv
#   T17_calibration_threshold_DCA_summary_metrics.csv
#
# 04_results/reporting/
#   T15b_manuscript_ready_cohort_table.csv
#
# 04_results/single_cell/
#   T18_GSE167363_prepare_summary.csv
#   T19_scRNA_signature_localization_summary.csv
#   T19_scRNA_cluster_annotation.csv
#   T19_scRNA_signature_score_by_celltype.csv
#   T19_scRNA_signature_score_by_group.csv
#   T19_scRNA_signature_score_by_outcome_timepoint.csv
#
# 输出：
# 04_results/reporting/
#   T20_manuscript_ready_integrated_tables.xlsx
#   T20_table1_cohort_context.csv
#   T20_table2_nested_LODO_validation.csv
#   T20_table3_calibration_threshold_transport.csv
#   T20_table4_scRNA_localization.csv
#   T20_figure_plan_final.csv
#   T20_key_results_numbers.csv
#
# 07_manuscript/
#   results_draft_v0.2_after_nested_LODO_scRNA.md
#   figure_table_plan_v0.2_after_nested_LODO_scRNA.md
#   interpretation_notes_v0.2_after_nested_LODO_scRNA.md
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

nested_dir <- file.path(project_dir, "04_results", "nested_LODO")
dca_dir <- file.path(project_dir, "04_results", "dca")
report_dir <- file.path(project_dir, "04_results", "reporting")
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
  cohort_table = file.path(report_dir, "T15b_manuscript_ready_cohort_table.csv"),
  nested_metrics = file.path(nested_dir, "T14_nested_LODO_validation_metrics.csv"),
  nested_gene_frequency = file.path(nested_dir, "T14_nested_LODO_gene_selection_frequency.csv"),
  nested_selected_genes = file.path(nested_dir, "T14_nested_LODO_selected_genes_by_fold.csv"),
  pooled_perf = file.path(dca_dir, "T17_pooled_nested_LODO_performance.csv"),
  cal_final = file.path(dca_dir, "T17_nested_LODO_calibration_metrics_final.csv"),
  thr_final = file.path(dca_dir, "T17_nested_LODO_threshold_drift_final.csv"),
  dca_summary = file.path(dca_dir, "T17_nested_LODO_DCA_summary.csv"),
  dca_summary_metrics = file.path(dca_dir, "T17_calibration_threshold_DCA_summary_metrics.csv"),
  sc_prepare_summary = file.path(sc_dir, "T18_GSE167363_prepare_summary.csv"),
  sc_localization = file.path(sc_dir, "T19_scRNA_signature_localization_summary.csv"),
  sc_cluster_annotation = file.path(sc_dir, "T19_scRNA_cluster_annotation.csv"),
  sc_score_celltype = file.path(sc_dir, "T19_scRNA_signature_score_by_celltype.csv"),
  sc_score_group = file.path(sc_dir, "T19_scRNA_signature_score_by_group.csv"),
  sc_score_outcome_timepoint = file.path(sc_dir, "T19_scRNA_signature_score_by_outcome_timepoint.csv")
)

missing <- names(files)[!file.exists(unlist(files))]

if (length(missing) > 0) {
  stop(
    "缺少必要输入文件：\n",
    paste(missing, unlist(files)[missing], sep = ": ", collapse = "\n")
  )
}

# ============================================================
# 工具函数
# ============================================================

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
}

get_metric_value <- function(df, metric_name) {
  if (!all(c("metric", "value") %in% colnames(df))) return(NA)
  x <- df$value[df$metric == metric_name]
  if (length(x) == 0) return(NA)
  x[1]
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return("NA")
  formatC(as.numeric(x), format = "f", digits = digits)
}

fmt_int <- function(x) {
  if (is.na(x)) return("NA")
  as.character(as.integer(round(as.numeric(x))))
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

# ============================================================
# 读取数据
# ============================================================

message("Reading final result tables...")

cohort_table <- read_csv_df(files$cohort_table)
nested_metrics <- read_csv_df(files$nested_metrics)
nested_gene_frequency <- read_csv_df(files$nested_gene_frequency)
nested_selected_genes <- read_csv_df(files$nested_selected_genes)
pooled_perf <- read_csv_df(files$pooled_perf)
cal_final <- read_csv_df(files$cal_final)
thr_final <- read_csv_df(files$thr_final)
dca_summary <- read_csv_df(files$dca_summary)
dca_summary_metrics <- read_csv_df(files$dca_summary_metrics)
sc_prepare_summary <- read_csv_df(files$sc_prepare_summary)
sc_localization <- read_csv_df(files$sc_localization)
sc_cluster_annotation <- read_csv_df(files$sc_cluster_annotation)
sc_score_celltype <- read_csv_df(files$sc_score_celltype)
sc_score_group <- read_csv_df(files$sc_score_group)
sc_score_outcome_timepoint <- read_csv_df(files$sc_score_outcome_timepoint)

# ============================================================
# 关键数字
# ============================================================

message("Extracting key numbers...")

n_dataset <- length(unique(nested_metrics$validation_dataset))
n_total <- sum(nested_metrics$n, na.rm = TRUE)
n_case <- sum(nested_metrics$n_case, na.rm = TRUE)
n_control <- sum(nested_metrics$n_control, na.rm = TRUE)

median_auroc <- median(nested_metrics$AUROC, na.rm = TRUE)
min_auroc <- min(nested_metrics$AUROC, na.rm = TRUE)
max_auroc <- max(nested_metrics$AUROC, na.rm = TRUE)
n_auroc_ge_080 <- sum(nested_metrics$AUROC >= 0.80, na.rm = TRUE)
n_auroc_lt_080 <- sum(nested_metrics$AUROC < 0.80, na.rm = TRUE)

pooled_auroc <- pooled_perf$AUROC[1]
pooled_auroc_low <- pooled_perf$AUROC_low[1]
pooled_auroc_high <- pooled_perf$AUROC_high[1]
pooled_auprc <- pooled_perf$AUPRC[1]
pooled_brier <- pooled_perf$Brier[1]
pooled_mean_pred <- pooled_perf$mean_predicted[1]
pooled_obs_rate <- pooled_perf$observed_rate[1]
pooled_cal_in_large <- pooled_perf$calibration_in_the_large[1]
pooled_fixed_sens <- pooled_perf$fold_fixed_sensitivity[1]
pooled_fixed_spec <- pooled_perf$fold_fixed_specificity[1]

median_abs_cal <- median(abs(cal_final$calibration_in_the_large), na.rm = TRUE)
median_abs_thr_shift <- median(abs(thr_final$threshold_shift), na.rm = TRUE)
max_abs_thr_shift <- max(abs(thr_final$threshold_shift), na.rm = TRUE)
n_fixed_sens_lt_020 <- sum(thr_final$fixed_sensitivity < 0.20, na.rm = TRUE)
n_fixed_spec_lt_020 <- sum(thr_final$fixed_specificity < 0.20, na.rm = TRUE)

recurrent_all <- nested_gene_frequency[
  nested_gene_frequency$selection_frequency >= 1,
  ,
  drop = FALSE
]
n_genes_selected_all_folds <- nrow(recurrent_all)
genes_selected_all_folds <- paste(recurrent_all$gene_symbol, collapse = ", ")

top_genes <- nested_gene_frequency[
  order(-nested_gene_frequency$selection_frequency, nested_gene_frequency$gene_symbol),
  ,
  drop = FALSE
]
top_gene_string <- paste(head(top_genes$gene_symbol, 10), collapse = ", ")

sc_n_cells <- get_metric_value(sc_localization, "n_cells")
sc_n_clusters <- get_metric_value(sc_localization, "n_clusters")
sc_n_celltypes <- get_metric_value(sc_localization, "n_cell_type_level1")
sc_n_final10 <- get_metric_value(sc_localization, "n_final10_genes_present")
sc_n_nested <- get_metric_value(sc_localization, "n_nested_recurrent_genes_present")
sc_top_final <- get_metric_value(sc_localization, "top_celltype_Final10_score")
sc_top_nested <- get_metric_value(sc_localization, "top_celltype_NestedRecurrent_score")
sc_top_final_score <- get_metric_value(sc_localization, "top_celltype_Final10_mean_score")
sc_top_nested_score <- get_metric_value(sc_localization, "top_celltype_NestedRecurrent_mean_score")

dca_pooled <- dca_summary[dca_summary$validation_dataset == "Pooled_nested_LODO", , drop = FALSE]
dca_pooled_fraction <- ifelse(
  nrow(dca_pooled) > 0,
  dca_pooled$fraction_thresholds_clinically_preferable[1],
  NA
)

key_numbers <- data.frame(
  item = c(
    "n_datasets",
    "n_total_samples_bulk",
    "n_sepsis_bulk",
    "n_control_bulk",
    "nested_LODO_median_AUROC",
    "nested_LODO_min_AUROC",
    "nested_LODO_max_AUROC",
    "nested_LODO_n_AUROC_ge_0.80",
    "nested_LODO_n_AUROC_lt_0.80",
    "pooled_nested_LODO_AUROC",
    "pooled_nested_LODO_AUROC_low",
    "pooled_nested_LODO_AUROC_high",
    "pooled_nested_LODO_AUPRC",
    "pooled_nested_LODO_Brier",
    "pooled_mean_predicted",
    "pooled_observed_rate",
    "pooled_calibration_in_the_large",
    "pooled_fold_fixed_sensitivity",
    "pooled_fold_fixed_specificity",
    "median_abs_calibration_in_the_large",
    "median_abs_threshold_shift",
    "max_abs_threshold_shift",
    "n_fixed_sensitivity_lt_0.20",
    "n_fixed_specificity_lt_0.20",
    "n_genes_selected_in_all_nested_LODO_folds",
    "genes_selected_in_all_nested_LODO_folds",
    "top_selected_genes",
    "scRNA_n_cells",
    "scRNA_n_clusters",
    "scRNA_n_celltype_level1",
    "scRNA_n_final10_genes_present",
    "scRNA_n_nested_recurrent_genes_present",
    "scRNA_top_celltype_Final10",
    "scRNA_top_celltype_NestedRecurrent",
    "scRNA_top_celltype_Final10_mean_score",
    "scRNA_top_celltype_NestedRecurrent_mean_score",
    "pooled_DCA_fraction_thresholds_clinically_preferable"
  ),
  value = c(
    n_dataset,
    n_total,
    n_case,
    n_control,
    median_auroc,
    min_auroc,
    max_auroc,
    n_auroc_ge_080,
    n_auroc_lt_080,
    pooled_auroc,
    pooled_auroc_low,
    pooled_auroc_high,
    pooled_auprc,
    pooled_brier,
    pooled_mean_pred,
    pooled_obs_rate,
    pooled_cal_in_large,
    pooled_fixed_sens,
    pooled_fixed_spec,
    median_abs_cal,
    median_abs_thr_shift,
    max_abs_thr_shift,
    n_fixed_sens_lt_020,
    n_fixed_spec_lt_020,
    n_genes_selected_all_folds,
    genes_selected_all_folds,
    top_gene_string,
    sc_n_cells,
    sc_n_clusters,
    sc_n_celltypes,
    sc_n_final10,
    sc_n_nested,
    sc_top_final,
    sc_top_nested,
    sc_top_final_score,
    sc_top_nested_score,
    dca_pooled_fraction
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Manuscript-ready tables
# ============================================================

message("Building manuscript-ready tables...")

table1 <- cohort_table

table2 <- nested_metrics[, intersect(
  c(
    "validation_dataset", "n", "n_case", "n_control", "prevalence",
    "AUROC", "AUROC_low", "AUROC_high", "AUPRC", "Brier",
    "threshold", "sensitivity", "specificity", "PPV", "NPV", "accuracy",
    "best_alpha", "lambda_rule_used", "n_refined_candidates",
    "n_selected_genes", "pool_used"
  ),
  colnames(nested_metrics)
), drop = FALSE]

table2 <- table2[order(table2$validation_dataset), , drop = FALSE]

table3 <- merge(
  cal_final[, c(
    "validation_dataset", "observed_rate", "mean_predicted",
    "calibration_in_the_large", "Brier", "AUROC", "AUPRC"
  ), drop = FALSE],
  thr_final[, c(
    "validation_dataset", "fixed_training_threshold",
    "local_youden_threshold", "threshold_shift", "abs_threshold_shift",
    "fixed_sensitivity", "fixed_specificity", "local_sensitivity", "local_specificity"
  ), drop = FALSE],
  by = "validation_dataset",
  all = TRUE,
  sort = FALSE
)

if ("validation_dataset" %in% colnames(cohort_table)) {
  context <- cohort_table
} else if ("dataset" %in% colnames(cohort_table)) {
  context <- cohort_table
  names(context)[names(context) == "dataset"] <- "validation_dataset"
} else {
  context <- data.frame()
}

if (nrow(context) > 0) {
  keep_context <- intersect(
    c("validation_dataset", "control_type_standardized", "clinical_control_strength", "main_limitation"),
    colnames(context)
  )
  table3 <- merge(
    table3,
    context[, keep_context, drop = FALSE],
    by = "validation_dataset",
    all.x = TRUE,
    sort = FALSE
  )
}

sc_final10_celltype <- sc_score_celltype[
  sc_score_celltype$score_name == "Final10_score",
  ,
  drop = FALSE
]
sc_nested_celltype <- sc_score_celltype[
  sc_score_celltype$score_name == "NestedRecurrent_score",
  ,
  drop = FALSE
]
sc_myeloid_celltype <- sc_score_celltype[
  sc_score_celltype$score_name == "MyeloidInnateUp_score",
  ,
  drop = FALSE
]

table4 <- merge(
  sc_final10_celltype[, c("cell_type_level1", "n_cells", "mean_score", "median_score"), drop = FALSE],
  sc_nested_celltype[, c("cell_type_level1", "mean_score", "median_score"), drop = FALSE],
  by = "cell_type_level1",
  all = TRUE,
  suffixes = c("_Final10", "_NestedRecurrent")
)

names(table4)[names(table4) == "mean_score"] <- "mean_score_NestedRecurrent"
names(table4)[names(table4) == "median_score"] <- "median_score_NestedRecurrent"

if (nrow(sc_myeloid_celltype) > 0) {
  table4 <- merge(
    table4,
    sc_myeloid_celltype[, c("cell_type_level1", "mean_score", "median_score"), drop = FALSE],
    by = "cell_type_level1",
    all.x = TRUE,
    sort = FALSE
  )
  names(table4)[names(table4) == "mean_score"] <- "mean_score_MyeloidInnateUp"
  names(table4)[names(table4) == "median_score"] <- "median_score_MyeloidInnateUp"
}

table4 <- table4[order(-table4$mean_score_Final10), , drop = FALSE]

# ============================================================
# Figure plan
# ============================================================

figure_plan <- data.frame(
  figure_id = c(
    "Figure 1",
    "Figure 2",
    "Figure 3",
    "Figure 4",
    "Figure 5",
    "Supplementary Figure 1",
    "Supplementary Figure 2",
    "Supplementary Figure 3",
    "Supplementary Figure 4"
  ),
  proposed_title = c(
    "Study design, cohort inclusion and analysis workflow",
    "Nested leave-one-dataset-out diagnostic performance",
    "Calibration and threshold transportability across held-out datasets",
    "Functional enrichment of sepsis-associated differential expression",
    "Single-cell localization of the transported host-response signature",
    "Quality control and PCA/UMAP diagnostics for bulk and scRNA-seq data",
    "Candidate gene selection and nested-fold gene recurrence",
    "Exploratory decision-curve analysis",
    "Canonical marker-based coarse cell-type annotation"
  ),
  main_content = c(
    "Cohort flow, data harmonization, nested LODO design, scRNA localization module",
    "Per-dataset ROC/AUPRC summaries and selected-gene recurrence",
    "Observed versus predicted probability, calibration bins, fixed versus local thresholds",
    "GO biological process terms showing myeloid activation, bacterial response and adaptive immune suppression patterns",
    "UMAP by broad cell type, module score UMAP, violin by cell type, signature gene dotplot",
    "QC summaries, PCA/UMAP by sample and group",
    "Priority gene selection, selected genes by fold, recurrent genes",
    "Net-benefit curves by held-out dataset and pooled nested LODO predictions",
    "Canonical marker dotplot and cluster annotation table"
  ),
  manuscript_role = c(
    "Main",
    "Main",
    "Main",
    "Main or Supplement depending on journal figure limit",
    "Main",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement"
  ),
  source_outputs = c(
    "T15b, T20",
    "T14, T20",
    "T17, T20",
    "T05 enrichment outputs",
    "T19, F19 outputs",
    "T03, T18",
    "T07, T14",
    "T17",
    "T19"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Results draft text
# ============================================================

message("Writing manuscript draft materials...")

results_lines <- c(
  "# Results draft v0.2 after nested LODO and scRNA localization",
  "",
  "## Cohort composition",
  "",
  paste0(
    "The final bulk transcriptomic analysis included ",
    fmt_int(n_total), " samples from ", fmt_int(n_dataset),
    " independent whole-blood cohorts, comprising ",
    fmt_int(n_case), " sepsis cases and ", fmt_int(n_control),
    " controls. Cohorts differed in platform, clinical context and control definition, including healthy-control contrasts, matched healthy controls and selected contrasts from more complex ICU cohorts."
  ),
  "",
  "## Strict nested leave-one-dataset-out model evaluation",
  "",
  paste0(
    "To avoid information leakage from feature selection, the diagnostic modeling procedure was re-run under a strict nested leave-one-dataset-out design. In each outer fold, the held-out dataset was excluded from candidate screening, correlation-based redundancy reduction, standardization, regularized model fitting and threshold selection. Across the six held-out datasets, the median AUROC was ",
    fmt_num(median_auroc), " with a range of ",
    fmt_num(min_auroc), " to ", fmt_num(max_auroc),
    ". Four of six held-out datasets achieved AUROC >= 0.80, whereas two datasets fell below this threshold."
  ),
  "",
  paste0(
    "Gene selection was not completely fixed across folds. ",
    fmt_int(n_genes_selected_all_folds),
    " genes were selected in all nested folds: ",
    genes_selected_all_folds,
    ". The most frequently selected genes across folds included ",
    top_gene_string,
    "."
  ),
  "",
  "## Probability-scale instability, calibration drift and threshold transportability",
  "",
  paste0(
    "Although per-dataset discrimination was often preserved, pooled nested-LODO predictions showed weaker transportability when predictions from all held-out folds were combined. The pooled AUROC was ",
    fmt_num(pooled_auroc), " (95% CI ",
    fmt_num(pooled_auroc_low), " to ", fmt_num(pooled_auroc_high),
    "), with AUPRC ", fmt_num(pooled_auprc),
    " and Brier score ", fmt_num(pooled_brier),
    ". The mean predicted probability was ",
    fmt_num(pooled_mean_pred),
    " compared with an observed sepsis prevalence of ",
    fmt_num(pooled_obs_rate),
    ", corresponding to calibration-in-the-large of ",
    fmt_num(pooled_cal_in_large),
    "."
  ),
  "",
  paste0(
    "Threshold behavior was highly dataset-dependent. The median absolute shift between the fold-specific training threshold and the local Youden threshold was ",
    fmt_num(median_abs_thr_shift),
    ", with a maximum absolute shift of ",
    fmt_num(max_abs_thr_shift),
    ". At the fixed training thresholds, ",
    fmt_int(n_fixed_sens_lt_020),
    " held-out datasets had sensitivity below 0.20 and ",
    fmt_int(n_fixed_spec_lt_020),
    " held-out dataset had specificity below 0.20. These findings indicate that discrimination and fixed-threshold transportability diverged under cross-cohort deployment."
  ),
  "",
  paste0(
    "Exploratory decision-curve analysis showed heterogeneous threshold-dependent net benefit. In pooled nested-LODO predictions, the model was clinically preferable over both treat-all and treat-none strategies across ",
    fmt_num(as.numeric(dca_pooled_fraction) * 100, 1),
    "% of evaluated threshold probabilities. This analysis was treated as exploratory because the available contrasts were heterogeneous and often involved healthy controls rather than clinical mimics."
  ),
  "",
  "## Single-cell localization of the transported host-response signature",
  "",
  paste0(
    "To provide biological context for the bulk-derived signature, an independent PBMC single-cell RNA-seq dataset was analyzed. After quality control, the single-cell object contained ",
    fmt_int(sc_n_cells), " cells, ",
    fmt_int(sc_n_clusters), " Seurat clusters and ",
    fmt_int(sc_n_celltypes),
    " broad canonical-marker-defined cell-type compartments. All ",
    fmt_int(sc_n_final10), " genes in the final compact signature and all ",
    fmt_int(sc_n_nested), " recurrent nested-LODO genes were detected in the single-cell object."
  ),
  "",
  paste0(
    "Both the final 10-gene module score and the recurrent nested-LODO module score were highest in ",
    sc_top_final,
    " compartments. The mean module scores in this compartment were ",
    fmt_num(as.numeric(sc_top_final_score)),
    " for the final 10-gene signature and ",
    fmt_num(as.numeric(sc_top_nested_score)),
    " for the recurrent nested-LODO signature. These results support a predominantly monocyte/myeloid origin of the transported host-response signal."
  ),
  "",
  paste0(
    "At the group level, signature scores were descriptively higher in sepsis cells, particularly nonsurvivor samples, than in healthy-control cells. Because the single-cell dataset contained a limited number of independent donors, these group-level score differences were interpreted as biological localization and contextual evidence rather than independent diagnostic validation."
  )
)

figure_plan_lines <- c(
  "# Figure and table plan v0.2",
  "",
  "## Main tables",
  "",
  "Table 1. Cohort context, control definitions and sample composition.",
  "",
  "Table 2. Nested leave-one-dataset-out validation performance.",
  "",
  "Table 3. Calibration and threshold transportability across held-out datasets.",
  "",
  "Table 4. Single-cell localization of signature module scores by broad cell type.",
  "",
  "## Figures",
  "",
  apply(
    figure_plan,
    1,
    function(x) {
      paste0(
        "### ", x[["figure_id"]], ": ", x[["proposed_title"]], "\n\n",
        x[["main_content"]], "\n\n",
        "Role: ", x[["manuscript_role"]], "\n\n",
        "Source: ", x[["source_outputs"]], "\n"
      )
    }
  )
)

interpretation_lines <- c(
  "# Interpretation notes v0.2",
  "",
  "## Current scientific positioning",
  "",
  "The strongest framing is not direct clinical deployment of a locked diagnostic model. The stronger and more defensible framing is cross-cohort transportability of a host-response diagnostic signature under strict nested leave-one-dataset-out evaluation.",
  "",
  "## Main positive findings",
  "",
  "1. Per-dataset discrimination is frequently preserved under strict nested LODO.",
  "2. The signature is biologically coherent, with enrichment and scRNA localization pointing to monocyte/myeloid and innate immune compartments.",
  "3. The study explicitly exposes calibration and fixed-threshold instability, which is methodologically important for transcriptomic diagnostic signatures.",
  "",
  "## Main limitations to state clearly",
  "",
  "1. Most validation contrasts are sepsis or septic shock versus healthy controls, limiting direct clinical mimic interpretation.",
  "2. GSE54514 is a clear transportability failure and should be discussed as such.",
  "3. Pooled probability-scale behavior is unstable, so fixed-threshold deployment is not supported.",
  "4. scRNA analysis supports biological localization, not independent diagnostic validation.",
  "",
  "## Recommended conclusion style",
  "",
  "Use: The signature showed recurrent cross-cohort discrimination and predominantly myeloid single-cell localization, but calibration and fixed-threshold transportability were unstable.",
  "",
  "Avoid: The model is ready for clinical diagnosis or shows robust clinical utility."
)

# ============================================================
# 保存表格
# ============================================================

message("Saving integrated reporting outputs...")

data.table::fwrite(
  key_numbers,
  file.path(report_dir, "T20_key_results_numbers.csv")
)

data.table::fwrite(
  table1,
  file.path(report_dir, "T20_table1_cohort_context.csv")
)

data.table::fwrite(
  table2,
  file.path(report_dir, "T20_table2_nested_LODO_validation.csv")
)

data.table::fwrite(
  table3,
  file.path(report_dir, "T20_table3_calibration_threshold_transport.csv")
)

data.table::fwrite(
  table4,
  file.path(report_dir, "T20_table4_scRNA_localization.csv")
)

data.table::fwrite(
  figure_plan,
  file.path(report_dir, "T20_figure_plan_final.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "key_numbers")
openxlsx::writeData(wb, "key_numbers", key_numbers)

openxlsx::addWorksheet(wb, "Table1_cohort_context")
openxlsx::writeData(wb, "Table1_cohort_context", table1)

openxlsx::addWorksheet(wb, "Table2_nested_LODO")
openxlsx::writeData(wb, "Table2_nested_LODO", table2)

openxlsx::addWorksheet(wb, "Table3_calibration_threshold")
openxlsx::writeData(wb, "Table3_calibration_threshold", table3)

openxlsx::addWorksheet(wb, "Table4_scRNA_localization")
openxlsx::writeData(wb, "Table4_scRNA_localization", table4)

openxlsx::addWorksheet(wb, "gene_selection_frequency")
openxlsx::writeData(wb, "gene_selection_frequency", nested_gene_frequency)

openxlsx::addWorksheet(wb, "selected_genes_by_fold")
openxlsx::writeData(wb, "selected_genes_by_fold", nested_selected_genes)

openxlsx::addWorksheet(wb, "DCA_summary")
openxlsx::writeData(wb, "DCA_summary", dca_summary)

openxlsx::addWorksheet(wb, "scRNA_cluster_annotation")
openxlsx::writeData(wb, "scRNA_cluster_annotation", sc_cluster_annotation)

openxlsx::addWorksheet(wb, "scRNA_score_by_group")
openxlsx::writeData(wb, "scRNA_score_by_group", sc_score_group)

openxlsx::addWorksheet(wb, "scRNA_score_by_outcome")
openxlsx::writeData(wb, "scRNA_score_by_outcome", sc_score_outcome_timepoint)

openxlsx::addWorksheet(wb, "figure_plan")
openxlsx::writeData(wb, "figure_plan", figure_plan)

openxlsx::saveWorkbook(
  wb,
  file.path(report_dir, "T20_manuscript_ready_integrated_tables.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 保存文稿素材
# ============================================================

write_text_file(
  results_lines,
  file.path(manuscript_dir, "results_draft_v0.2_after_nested_LODO_scRNA.md")
)

write_text_file(
  figure_plan_lines,
  file.path(manuscript_dir, "figure_table_plan_v0.2_after_nested_LODO_scRNA.md")
)

write_text_file(
  interpretation_lines,
  file.path(manuscript_dir, "interpretation_notes_v0.2_after_nested_LODO_scRNA.md")
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_20_update_manuscript_tables_and_results.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 20 Manuscript tables and results update 完成 ============")
message("Reporting 输出目录：", report_dir)
message("Manuscript 输出目录：", manuscript_dir)

message("\nKey numbers:")
print(key_numbers)

message("\nTable 2 nested LODO validation:")
print(table2)

message("\nTable 3 calibration and threshold transport:")
print(table3)

message("\nTable 4 scRNA localization:")
print(table4)

message("\nFigure plan:")
print(figure_plan)

message("\n关键输出：")
message("1) ", file.path(report_dir, "T20_manuscript_ready_integrated_tables.xlsx"))
message("2) ", file.path(report_dir, "T20_key_results_numbers.csv"))
message("3) ", file.path(report_dir, "T20_table1_cohort_context.csv"))
message("4) ", file.path(report_dir, "T20_table2_nested_LODO_validation.csv"))
message("5) ", file.path(report_dir, "T20_table3_calibration_threshold_transport.csv"))
message("6) ", file.path(report_dir, "T20_table4_scRNA_localization.csv"))
message("7) ", file.path(report_dir, "T20_figure_plan_final.csv"))
message("8) ", file.path(manuscript_dir, "results_draft_v0.2_after_nested_LODO_scRNA.md"))
message("9) ", file.path(manuscript_dir, "figure_table_plan_v0.2_after_nested_LODO_scRNA.md"))
message("10) ", file.path(manuscript_dir, "interpretation_notes_v0.2_after_nested_LODO_scRNA.md"))

message("\n下一步：")
message("把 Key numbers、Table 2、Table 3、Table 4 贴给我。")
message("我会据此判断是否需要再补一个 21_final_sanity_check_before_manuscript.R。")