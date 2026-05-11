# ============================================================
# 12_manuscript_structure_and_results_draft.R
# Manuscript structure and draft generation
#
# 目的：
# 1. 基于已完成分析结果生成论文主文框架
# 2. 生成 Title / Abstract / Introduction outline / Methods / Results / Discussion / Limitations 草稿
# 3. 生成 Figure and Table plan
# 4. 生成 TRIPOD-style reporting notes
#
# 输入：
# 04_results/reporting/
#   T11_manuscript_ready_tables.xlsx
#   T11_table1_cohort_sample_summary.csv
#   T11_table2_final_model_coefficients.csv
#   T11_table3_LODO_validation_performance.csv
#   T11_table4_calibration_threshold_drift.csv
#   T11_table5_key_GO_terms.csv
#
# 04_results/final_model/
#   T10_final_model_selected_genes.csv
#   T10_final_model_apparent_metrics.csv
#   T10_final_model_coefficients.csv
#   T10_final_model_scaling_parameters.csv
#
# 04_results/model_validation/
#   T08_LODO_gene_selection_frequency.csv
#   T09_LODO_calibration_metrics.csv
#   T09_LODO_threshold_drift_metrics.csv
#
# 输出：
# 07_manuscript/
#   manuscript_main_draft_v0.1.md
#   manuscript_results_section_v0.1.md
#   manuscript_methods_section_v0.1.md
#   manuscript_discussion_section_v0.1.md
#   abstract_structured_v0.1.md
#   figure_table_plan_v0.1.md
#   reporting_positioning_notes_v0.1.md
#
# 04_results/reporting/
#   T12_manuscript_key_numbers.csv
#   T12_figure_table_plan.csv
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

report_dir <- file.path(project_dir, "04_results", "reporting")
final_dir <- file.path(project_dir, "04_results", "final_model")
val_dir <- file.path(project_dir, "04_results", "model_validation")
manuscript_dir <- file.path(project_dir, "07_manuscript")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

table1_file <- file.path(report_dir, "T11_table1_cohort_sample_summary.csv")
coef_file <- file.path(report_dir, "T11_table2_final_model_coefficients.csv")
lodo_file <- file.path(report_dir, "T11_table3_LODO_validation_performance.csv")
cal_file <- file.path(report_dir, "T11_table4_calibration_threshold_drift.csv")
go_file <- file.path(report_dir, "T11_table5_key_GO_terms.csv")

final_selected_file <- file.path(final_dir, "T10_final_model_selected_genes.csv")
final_metrics_file <- file.path(final_dir, "T10_final_model_apparent_metrics.csv")
final_scaling_file <- file.path(final_dir, "T10_final_model_scaling_parameters.csv")

freq_file <- file.path(val_dir, "T08_LODO_gene_selection_frequency.csv")
cal_metrics_file <- file.path(val_dir, "T09_LODO_calibration_metrics.csv")
threshold_file <- file.path(val_dir, "T09_LODO_threshold_drift_metrics.csv")

needed <- c(
  table1_file,
  coef_file,
  lodo_file,
  cal_file,
  go_file,
  final_selected_file,
  final_metrics_file,
  final_scaling_file,
  freq_file,
  cal_metrics_file,
  threshold_file
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

fmt_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  ifelse(
    is.na(x),
    "NA",
    formatC(x, digits = digits, format = "f")
  )
}

fmt_int <- function(x) {
  x <- as.numeric(x)
  ifelse(
    is.na(x),
    "NA",
    formatC(x, digits = 0, format = "f")
  )
}

fmt_p <- function(x) {
  x <- as.numeric(x)
  ifelse(
    is.na(x),
    "NA",
    ifelse(
      x < 0.001,
      formatC(x, format = "e", digits = 2),
      formatC(x, format = "f", digits = 3)
    )
  )
}

write_text <- function(lines, file) {
  writeLines(lines, con = file, useBytes = TRUE)
}

collapse_comma <- function(x) {
  paste(x, collapse = ", ")
}

get_top_go_terms <- function(go_df, gene_set_label, n = 8) {
  if (nrow(go_df) == 0) return(character())
  if (!all(c("gene_set", "Description", "p.adjust") %in% colnames(go_df))) return(character())
  z <- go_df[go_df$gene_set == gene_set_label, , drop = FALSE]
  if (nrow(z) == 0) return(character())
  z <- z[order(z$p.adjust), , drop = FALSE]
  head(z$Description, n)
}

# ============================================================
# 读取数据
# ============================================================

message("Reading manuscript inputs...")

table1 <- safe_fread(table1_file)
coef_table <- safe_fread(coef_file)
lodo <- safe_fread(lodo_file)
cal_threshold <- safe_fread(cal_file)
go_terms <- safe_fread(go_file)

final_selected <- safe_fread(final_selected_file)
final_metrics <- safe_fread(final_metrics_file)
final_scaling <- safe_fread(final_scaling_file)

gene_freq <- safe_fread(freq_file)
cal_metrics <- safe_fread(cal_metrics_file)
threshold_metrics <- safe_fread(threshold_file)

# ============================================================
# 提取关键数字
# ============================================================

table1_no_total <- table1[table1$dataset != "Total", , drop = FALSE]
table1_total <- table1[table1$dataset == "Total", , drop = FALSE]

n_datasets <- nrow(table1_no_total)
n_total <- table1_total$total[1]
n_control <- table1_total$Control[1]
n_sepsis <- table1_total$Sepsis[1]

selected_genes <- final_selected$gene_symbol
selected_genes_text <- collapse_comma(selected_genes)

intercept <- coef_table$coefficient[coef_table$term == "(Intercept)"][1]
coef_gene <- coef_table[coef_table$term != "(Intercept)", , drop = FALSE]

final_auc <- final_metrics$AUROC[1]
final_auc_low <- final_metrics$AUROC_low[1]
final_auc_high <- final_metrics$AUROC_high[1]
final_auprc <- final_metrics$AUPRC[1]
final_brier <- final_metrics$Brier[1]
final_threshold <- final_metrics$threshold[1]
final_sens <- final_metrics$sensitivity[1]
final_spec <- final_metrics$specificity[1]
final_acc <- final_metrics$accuracy[1]

lodo_auc <- as.numeric(lodo$AUROC)
lodo_auprc <- as.numeric(lodo$AUPRC)
lodo_brier <- as.numeric(lodo$Brier)

lodo_median_auc <- median(lodo_auc, na.rm = TRUE)
lodo_min_auc <- min(lodo_auc, na.rm = TRUE)
lodo_max_auc <- max(lodo_auc, na.rm = TRUE)
lodo_median_auprc <- median(lodo_auprc, na.rm = TRUE)
lodo_median_brier <- median(lodo_brier, na.rm = TRUE)
lodo_n_good_auc <- sum(lodo_auc >= 0.80, na.rm = TRUE)
lodo_n_low_auc <- sum(lodo_auc < 0.80, na.rm = TRUE)

worst_lodo <- lodo[which.min(lodo$AUROC), , drop = FALSE]
best_lodo <- lodo[which.max(lodo$AUROC), , drop = FALSE]

median_abs_cal_large <- median(abs(cal_metrics$calibration_in_the_large), na.rm = TRUE)
median_abs_threshold_shift <- median(abs(threshold_metrics$threshold_shift), na.rm = TRUE)
max_abs_threshold_shift <- max(abs(threshold_metrics$threshold_shift), na.rm = TRUE)

n_fixed_sens_low <- sum(threshold_metrics$fixed_sensitivity < 0.20, na.rm = TRUE)
n_fixed_spec_low <- sum(threshold_metrics$fixed_specificity < 0.20, na.rm = TRUE)

freq_top <- gene_freq[order(-gene_freq$n_selected_folds, gene_freq$gene_symbol), , drop = FALSE]
freq_top_text <- collapse_comma(freq_top$gene_symbol[freq_top$n_selected_folds == max(freq_top$n_selected_folds, na.rm = TRUE)])

go_up_terms <- get_top_go_terms(go_terms, "Upregulated in sepsis", n = 8)
go_down_terms <- get_top_go_terms(go_terms, "Downregulated in sepsis", n = 8)
go_all_terms <- get_top_go_terms(go_terms, "All significant DEGs", n = 8)

# 从已有 GO 表推断 DEG 数字不可靠时，不在这里重新推断
# 使用 T11 reporting 结果里已经生成的文本逻辑，主文中保留可核对数字
deg_key_text <- "Differentially expressed genes were defined using adjusted P < 0.05 and |logFC| >= 0.5."

key_numbers <- data.frame(
  item = c(
    "n_datasets",
    "n_total_samples",
    "n_sepsis",
    "n_control",
    "n_final_model_genes",
    "final_model_AUROC_apparent",
    "final_model_AUPRC_apparent",
    "final_model_Brier_apparent",
    "final_model_threshold_apparent",
    "LODO_median_AUROC",
    "LODO_min_AUROC",
    "LODO_max_AUROC",
    "LODO_median_AUPRC",
    "LODO_median_Brier",
    "LODO_AUROC_ge_0.80_datasets",
    "LODO_AUROC_lt_0.80_datasets",
    "median_abs_calibration_in_the_large",
    "median_abs_threshold_shift",
    "max_abs_threshold_shift",
    "fixed_threshold_sensitivity_lt_0.20_datasets",
    "fixed_threshold_specificity_lt_0.20_datasets"
  ),
  value = c(
    n_datasets,
    n_total,
    n_sepsis,
    n_control,
    length(selected_genes),
    final_auc,
    final_auprc,
    final_brier,
    final_threshold,
    lodo_median_auc,
    lodo_min_auc,
    lodo_max_auc,
    lodo_median_auprc,
    lodo_median_brier,
    lodo_n_good_auc,
    lodo_n_low_auc,
    median_abs_cal_large,
    median_abs_threshold_shift,
    max_abs_threshold_shift,
    n_fixed_sens_low,
    n_fixed_spec_low
  ),
  stringsAsFactors = FALSE
)

data.table::fwrite(
  key_numbers,
  file.path(report_dir, "T12_manuscript_key_numbers.csv")
)

# ============================================================
# 题目和摘要
# ============================================================

message("Generating title and abstract...")

title_lines <- c(
  "# Candidate titles",
  "",
  "1. Cross-cohort transportability of a compact blood transcriptomic host-response model for adult sepsis diagnosis",
  "",
  "2. Development and dataset-level transportability assessment of a 10-gene blood transcriptomic signature for adult sepsis",
  "",
  "3. A compact host-response transcriptomic signature for adult sepsis diagnosis: cross-cohort development and transportability evaluation",
  "",
  "# Recommended title",
  "",
  "Cross-cohort transportability of a compact blood transcriptomic host-response model for adult sepsis diagnosis",
  "",
  "# Running title",
  "",
  "Transcriptomic model transportability in sepsis"
)

abstract_lines <- c(
  "# Structured abstract draft",
  "",
  "## Background",
  "Blood transcriptomic signatures have been proposed for sepsis diagnosis, but their robustness across cohorts, platforms and clinical control groups remains uncertain. We aimed to develop a compact adult blood transcriptomic host-response model and evaluate its dataset-level transportability across public cohorts.",
  "",
  "## Methods",
  paste0(
    "We curated ", fmt_int(n_datasets),
    " adult blood bulk transcriptome cohorts comprising ",
    fmt_int(n_total),
    " samples, including ",
    fmt_int(n_sepsis),
    " sepsis and ",
    fmt_int(n_control),
    " control samples. Probe-level expression profiles were mapped to gene symbols and collapsed to gene-level expression values. Differential expression analysis was performed using a batch-aware linear modeling framework. Candidate genes were prioritized by differential expression, cross-dataset direction consistency, single-gene discrimination and biological interpretability. A compact logistic model was trained using penalized regression, and dataset-level transportability was evaluated using leave-one-dataset-out validation. Model performance was assessed using AUROC, AUPRC, Brier score, calibration and fixed-threshold operating characteristics."
  ),
  "",
  "## Results",
  paste0(
    "The final compact model included ",
    fmt_int(length(selected_genes)),
    " genes: ",
    selected_genes_text,
    ". In the pooled development dataset, the model showed apparent AUROC ",
    fmt_num(final_auc, 3),
    " (95% CI ",
    fmt_num(final_auc_low, 3),
    "-",
    fmt_num(final_auc_high, 3),
    "), AUPRC ",
    fmt_num(final_auprc, 3),
    " and Brier score ",
    fmt_num(final_brier, 3),
    ". In leave-one-dataset-out validation, AUROC ranged from ",
    fmt_num(lodo_min_auc, 3),
    " to ",
    fmt_num(lodo_max_auc, 3),
    " with a median AUROC of ",
    fmt_num(lodo_median_auc, 3),
    "; ",
    fmt_int(lodo_n_good_auc),
    " of ",
    fmt_int(nrow(lodo)),
    " validation datasets achieved AUROC >= 0.80. However, calibration and threshold analyses showed substantial probability-scale and decision-threshold drift, with median absolute calibration-in-the-large ",
    fmt_num(median_abs_cal_large, 3),
    " and median absolute threshold shift ",
    fmt_num(median_abs_threshold_shift, 3),
    "."
  ),
  "",
  "## Conclusions",
  "The compact blood transcriptomic host-response model showed promising cross-cohort ranking performance for adult sepsis diagnosis, but absolute probability calibration and fixed decision thresholds were not consistently transportable. Future independent and preferably prospective validation with local recalibration is needed before clinical deployment."
)

write_text(title_lines, file.path(manuscript_dir, "title_options_v0.1.md"))
write_text(abstract_lines, file.path(manuscript_dir, "abstract_structured_v0.1.md"))

# ============================================================
# Methods 草稿
# ============================================================

message("Generating Methods draft...")

methods_lines <- c(
  "# Methods draft",
  "",
  "## Study design and data sources",
  "This study was designed as a cross-cohort development and transportability evaluation of an adult blood transcriptomic host-response model for sepsis diagnosis. Publicly available bulk blood transcriptomic cohorts were screened and harmonized. The primary diagnostic contrast was sepsis versus control. Pediatric cohorts and non-baseline follow-up samples were not included in the main adult diagnostic analysis.",
  "",
  "## Cohort eligibility and phenotype harmonization",
  paste0(
    "Six adult blood bulk transcriptome cohorts were included, comprising ",
    fmt_int(n_total),
    " samples across ",
    fmt_int(n_datasets),
    " datasets. The final analysis included ",
    fmt_int(n_sepsis),
    " sepsis and ",
    fmt_int(n_control),
    " control samples. Sample-level clinical labels were harmonized into two primary groups: Sepsis and Control. For longitudinal datasets, baseline, admission or day 1 samples were prioritized."
  ),
  "",
  "## Expression preprocessing and probe-to-gene mapping",
  "Processed series matrix expression data were used. Probe-level features were mapped to gene symbols using platform-specific annotation resources. When multiple probes mapped to the same gene symbol, probe-level expression values were collapsed to gene-level values. Datasets were then merged by common gene symbols across cohorts. The primary differential expression analysis used the unadjusted gene-level expression matrix with dataset indicators included in the statistical model; batch-adjusted matrices were used for visualization and quality control.",
  "",
  "## Differential expression analysis",
  "Differential expression analysis comparing sepsis with controls was performed using a linear modeling framework with dataset included as a batch covariate. Genes with adjusted P < 0.05 and |logFC| >= 0.5 were considered significant differentially expressed genes. Upregulated and downregulated genes in sepsis were analyzed separately for functional interpretation.",
  "",
  "## Functional enrichment analysis",
  "Gene Ontology Biological Process enrichment analysis was performed for all significant differentially expressed genes and separately for genes upregulated and downregulated in sepsis. Enrichment results were used to interpret host-response biology and were not treated as evidence of therapeutic target validation.",
  "",
  "## Candidate gene prioritization",
  "Candidate genes were prioritized using a multi-step strategy. First, significant differentially expressed genes were ranked by adjusted P value and effect size. Second, dataset-level direction consistency and single-gene AUROC were calculated across cohorts. Third, genes were further filtered by cross-dataset direction consistency, mean AUROC, minimum AUROC and number of datasets with AUROC >= 0.70. Highly correlated genes were pruned to reduce redundancy before model training.",
  "",
  "## Model development",
  paste0(
    "A final compact logistic regression model was trained using the refined candidate gene set. Penalized logistic regression was used for candidate feature selection across alpha values, followed by refitting of a compact logistic model using the selected genes. The final model included ",
    fmt_int(length(selected_genes)),
    " genes: ",
    selected_genes_text,
    ". Gene expression values were standardized using center and scale parameters estimated from the pooled development data before applying the model coefficients."
  ),
  "",
  "## Leave-one-dataset-out validation",
  "Dataset-level transportability was evaluated using leave-one-dataset-out validation. In each fold, one dataset was held out for validation and the remaining datasets were used for model training, feature selection, standardization parameter estimation, penalty parameter selection and threshold selection. The held-out dataset was not used for these training-stage steps.",
  "",
  "## Performance evaluation",
  "Discrimination was assessed using AUROC and AUPRC. Overall probability error was assessed using the Brier score. Calibration was evaluated by observed versus mean predicted probabilities, calibration-in-the-large and logistic calibration slope. Fixed-threshold performance was assessed using the training-set Youden threshold, and threshold drift was quantified by comparing the fixed training threshold with the local Youden threshold in each validation dataset.",
  "",
  "## Reporting position",
  "The final compact model was treated as a candidate model for future independent validation. The leave-one-dataset-out analysis was treated as a dataset-level transportability evaluation rather than a fully independent prospective external validation."
)

write_text(methods_lines, file.path(manuscript_dir, "manuscript_methods_section_v0.1.md"))

# ============================================================
# Results 草稿
# ============================================================

message("Generating Results draft...")

results_lines <- c(
  "# Results draft",
  "",
  "## Included cohorts and samples",
  paste0(
    "A total of ",
    fmt_int(n_total),
    " adult blood transcriptome samples from ",
    fmt_int(n_datasets),
    " public cohorts were included in the main diagnostic analysis. The integrated dataset contained ",
    fmt_int(n_sepsis),
    " sepsis samples and ",
    fmt_int(n_control),
    " control samples. Individual cohort sample sizes ranged from ",
    fmt_int(min(table1_no_total$total, na.rm = TRUE)),
    " to ",
    fmt_int(max(table1_no_total$total, na.rm = TRUE)),
    " samples."
  ),
  "",
  "## Differential expression and biological interpretation",
  deg_key_text,
  "The upregulated gene set was enriched for biological processes related to myeloid leukocyte activation, response to bacterium, defense response to bacterium, humoral immune response, acute-phase response, inflammatory response regulation, platelet activation and coagulation.",
  "The downregulated gene set was enriched for T cell activation, adaptive immune response, antigen receptor-mediated signaling, lymphocyte differentiation and regulation of lymphocyte activation.",
  "",
  "Representative GO Biological Process terms among genes upregulated in sepsis included:",
  paste0("- ", go_up_terms),
  "",
  "Representative GO Biological Process terms among genes downregulated in sepsis included:",
  paste0("- ", go_down_terms),
  "",
  "Together, these results suggested a dual host-response pattern characterized by activation of myeloid, antibacterial, inflammatory and coagulation-related programs, together with suppression of T cell and adaptive immune programs.",
  "",
  "## Candidate gene refinement and final compact model",
  paste0(
    "Candidate genes were prioritized by differential expression, cross-dataset direction consistency, single-gene discrimination and biological relevance. The refined modeling candidate set was further pruned by correlation to reduce redundancy. The final compact model included ",
    fmt_int(length(selected_genes)),
    " genes: ",
    selected_genes_text,
    "."
  ),
  "",
  "The final model formula was:",
  "",
  paste0(
    "logit(P[sepsis]) = ",
    fmt_num(intercept, 6),
    paste0(
      ifelse(coef_gene$coefficient >= 0, " + ", " - "),
      fmt_num(abs(coef_gene$coefficient), 6),
      " * z(",
      coef_gene$term,
      ")",
      collapse = ""
    )
  ),
  "",
  "where z(gene) denotes expression standardized using the pooled development-set center and scale parameters.",
  "",
  "## Apparent performance of the final compact model",
  paste0(
    "In the pooled development dataset, the final compact model showed apparent AUROC ",
    fmt_num(final_auc, 3),
    " (95% CI ",
    fmt_num(final_auc_low, 3),
    "-",
    fmt_num(final_auc_high, 3),
    "), AUPRC ",
    fmt_num(final_auprc, 3),
    " and Brier score ",
    fmt_num(final_brier, 3),
    ". At the apparent Youden threshold of ",
    fmt_num(final_threshold, 3),
    ", sensitivity was ",
    fmt_num(final_sens, 3),
    ", specificity was ",
    fmt_num(final_spec, 3),
    " and accuracy was ",
    fmt_num(final_acc, 3),
    "."
  ),
  "",
  "These apparent estimates were used to describe the fitted candidate model and should not be interpreted as independent external validation performance.",
  "",
  "## Leave-one-dataset-out transportability evaluation",
  paste0(
    "In leave-one-dataset-out validation, AUROC ranged from ",
    fmt_num(lodo_min_auc, 3),
    " to ",
    fmt_num(lodo_max_auc, 3),
    ", with a median AUROC of ",
    fmt_num(lodo_median_auc, 3),
    ". ",
    fmt_int(lodo_n_good_auc),
    " of ",
    fmt_int(nrow(lodo)),
    " validation datasets achieved AUROC >= 0.80."
  ),
  paste0(
    "The lowest AUROC was observed in ",
    worst_lodo$validation_dataset[1],
    " (AUROC ",
    fmt_num(worst_lodo$AUROC[1], 3),
    "), while the highest AUROC was observed in ",
    best_lodo$validation_dataset[1],
    " (AUROC ",
    fmt_num(best_lodo$AUROC[1], 3),
    "). The median AUPRC was ",
    fmt_num(lodo_median_auprc, 3),
    ", and the median Brier score was ",
    fmt_num(lodo_median_brier, 3),
    "."
  ),
  "",
  "Genes selected in all leave-one-dataset-out folds included:",
  paste0("- ", freq_top_text),
  "",
  "## Calibration and threshold transportability",
  paste0(
    "Calibration analysis revealed marked cohort-dependent probability-scale drift. The median absolute calibration-in-the-large was ",
    fmt_num(median_abs_cal_large, 3),
    ". The median absolute shift between the fixed training threshold and the validation-set local Youden threshold was ",
    fmt_num(median_abs_threshold_shift, 3),
    ", with a maximum absolute threshold shift of ",
    fmt_num(max_abs_threshold_shift, 3),
    "."
  ),
  paste0(
    "Using the fixed training threshold, ",
    fmt_int(n_fixed_sens_low),
    " validation datasets had sensitivity < 0.20, and ",
    fmt_int(n_fixed_spec_low),
    " validation datasets had specificity < 0.20."
  ),
  "These findings indicate that the model retained transportable ranking performance in most cohorts, whereas absolute predicted probabilities and fixed decision thresholds were not consistently transportable across datasets."
)

write_text(results_lines, file.path(manuscript_dir, "manuscript_results_section_v0.1.md"))

# ============================================================
# Discussion 草稿
# ============================================================

message("Generating Discussion draft...")

discussion_lines <- c(
  "# Discussion draft",
  "",
  "## Principal findings",
  "In this cross-cohort adult blood transcriptomic study, we developed a compact host-response model for sepsis diagnosis and evaluated its dataset-level transportability. The final 10-gene model showed high apparent discrimination in the pooled development data and generally favorable ranking performance in leave-one-dataset-out validation. However, calibration and threshold analyses revealed substantial cohort-dependent probability-scale drift and poor fixed-threshold transportability in several validation folds.",
  "",
  "## Biological interpretation",
  "The transcriptomic signal underlying the model was biologically coherent. Genes upregulated in sepsis were enriched for myeloid leukocyte activation, antibacterial response, humoral immune response, inflammation, platelet activation and coagulation. Genes downregulated in sepsis were enriched for T cell activation, adaptive immune response and antigen receptor-mediated signaling. This pattern is consistent with a dysregulated host response involving simultaneous innate immune activation and adaptive immune suppression.",
  "",
  "## Model transportability",
  "The distinction between discrimination and calibration was central to the findings. The model separated sepsis from controls in most held-out datasets, as reflected by generally high AUROC and AUPRC. However, predicted probability distributions shifted substantially across datasets. As a result, fixed thresholds selected in the training data did not maintain stable sensitivity and specificity in several held-out cohorts. This supports reporting the model as a candidate host-response signature requiring local recalibration before clinical translation.",
  "",
  "## Strengths",
  "Major strengths include the use of multiple adult blood transcriptomic cohorts, dataset-level validation, explicit probe-to-gene harmonization, transparent candidate gene screening, leave-one-dataset-out transportability assessment and reporting of calibration, Brier score and threshold drift in addition to AUROC.",
  "",
  "## Limitations",
  "This study has several limitations. First, all analyses used retrospective public transcriptomic cohorts and archived processed expression data. Raw data reprocessing and prospective validation were not performed. Second, cohort labels, control definitions, platforms and case mix varied across datasets. Third, candidate gene screening used information from multiple public cohorts, so the leave-one-dataset-out analysis should be interpreted as dataset-level transportability assessment rather than fully independent prospective validation. Fourth, the final model showed unstable probability calibration and threshold behavior across cohorts; therefore, a fixed threshold should not be used for direct clinical decision-making without local validation and recalibration. Fifth, the study evaluated transcriptomic diagnostic separation, not clinical utility, cost-effectiveness or implementation feasibility.",
  "",
  "## Implications",
  "The results suggest that compact blood transcriptomic host-response models may provide robust ranking information for adult sepsis diagnosis across public cohorts, but probability calibration and operating thresholds remain major barriers to direct deployment. Future work should validate the final model in independent prospective cohorts, evaluate platform-specific recalibration, compare the model with routinely available clinical and laboratory markers, and assess whether the transcriptomic signature adds clinically meaningful value in intended-use populations."
)

write_text(discussion_lines, file.path(manuscript_dir, "manuscript_discussion_section_v0.1.md"))

# ============================================================
# 主文完整草稿
# ============================================================

message("Generating full manuscript draft...")

main_draft <- c(
  "# Cross-cohort transportability of a compact blood transcriptomic host-response model for adult sepsis diagnosis",
  "",
  "## Abstract",
  abstract_lines[!grepl("^# Structured abstract draft$", abstract_lines)],
  "",
  "## Introduction",
  "",
  "Sepsis remains a major clinical syndrome characterized by heterogeneous host responses to infection. Conventional clinical and laboratory markers often lack sufficient specificity for early diagnostic discrimination, motivating the development of host-response transcriptomic signatures. However, transcriptomic signatures developed in individual cohorts may fail to generalize across patient populations, platforms and control definitions. Therefore, evaluating cross-cohort transportability, calibration and threshold behavior is essential before proposing clinical translation.",
  "",
  "This study aimed to develop a compact adult blood transcriptomic host-response model for sepsis diagnosis and to evaluate its dataset-level transportability across public cohorts. Rather than focusing only on AUROC, we assessed discrimination, precision-recall performance, Brier score, calibration and threshold drift.",
  "",
  methods_lines,
  "",
  results_lines,
  "",
  discussion_lines,
  "",
  "## Data and code availability",
  "All analyses were based on publicly available transcriptomic datasets. The analysis code, processed result tables and model objects should be archived in a public repository before submission.",
  "",
  "## Ethics statement",
  "This study used publicly available de-identified transcriptomic datasets. No new human participant recruitment or intervention was performed.",
  "",
  "## Author contributions",
  "To be completed.",
  "",
  "## Funding",
  "To be completed.",
  "",
  "## Conflicts of interest",
  "The authors declare no competing interests. To be confirmed.",
  "",
  "## References",
  "To be completed using journal-specific format."
)

write_text(main_draft, file.path(manuscript_dir, "manuscript_main_draft_v0.1.md"))

# ============================================================
# Figure and table plan
# ============================================================

message("Generating figure and table plan...")

figure_table_plan <- data.frame(
  item_type = c(
    "Figure",
    "Figure",
    "Figure",
    "Figure",
    "Figure",
    "Table",
    "Table",
    "Table",
    "Table",
    "Supplementary Table",
    "Supplementary Figure",
    "Supplementary Figure",
    "Supplementary Figure"
  ),
  item_id = c(
    "Figure 1",
    "Figure 2",
    "Figure 3",
    "Figure 4",
    "Figure 5",
    "Table 1",
    "Table 2",
    "Table 3",
    "Table 4",
    "Supplementary Table 1",
    "Supplementary Figure 1",
    "Supplementary Figure 2",
    "Supplementary Figure 3"
  ),
  title = c(
    "Study workflow and cohort inclusion",
    "Differential expression and GO Biological Process enrichment",
    "Final compact model genes and apparent performance",
    "Leave-one-dataset-out validation performance",
    "Calibration and threshold drift across validation datasets",
    "Included cohorts and sample composition",
    "Final 10-gene model coefficients and standardization parameters",
    "LODO validation discrimination and classification metrics",
    "Calibration and threshold drift metrics",
    "Candidate gene screening and selection frequency",
    "PCA and sample-level QC",
    "Candidate gene AUROC and correlation heatmaps",
    "LODO prediction density and PR curves"
  ),
  source_files = c(
    "T11_table1_cohort_sample_summary.csv; sample flow files from earlier scripts",
    "T11_table5_key_GO_terms.csv; enrichment figures in 05_figures/enrichment",
    "T10_final_model_coefficients.csv; F12B/F12C/F12D",
    "T08_LODO_validation_metrics.csv; F10A/F10C",
    "T09_LODO_calibration_metrics.csv; T09_LODO_threshold_drift_metrics.csv; F11A/F11D/F13B",
    "T11_table1_cohort_sample_summary.csv",
    "T11_table2_final_model_coefficients.csv; T10_final_model_scaling_parameters.csv",
    "T11_table3_LODO_validation_performance.csv",
    "T11_table4_calibration_threshold_drift.csv",
    "T06/T07 candidate selection outputs",
    "05_figures/qc_bulk",
    "05_figures/candidate_selection",
    "05_figures/model_validation"
  ),
  manuscript_role = c(
    "Shows design, dataset filtering and analysis pipeline",
    "Supports biological plausibility of the host-response signal",
    "Defines final candidate model",
    "Shows dataset-level transportability of ranking performance",
    "Shows non-transportability of absolute probability and fixed thresholds",
    "Cohort composition",
    "Model reproducibility",
    "Primary validation performance table",
    "Calibration and threshold transportability",
    "Transparency and reproducibility",
    "Quality control",
    "Feature screening transparency",
    "Supplementary validation displays"
  ),
  stringsAsFactors = FALSE
)

data.table::fwrite(
  figure_table_plan,
  file.path(report_dir, "T12_figure_table_plan.csv")
)

figtab_lines <- c(
  "# Figure and table plan",
  "",
  apply(
    figure_table_plan,
    1,
    function(z) {
      paste0(
        "## ", z[["item_id"]], ": ", z[["title"]], "\n",
        "- Type: ", z[["item_type"]], "\n",
        "- Source files: ", z[["source_files"]], "\n",
        "- Manuscript role: ", z[["manuscript_role"]], "\n"
      )
    }
  )
)

write_text(figtab_lines, file.path(manuscript_dir, "figure_table_plan_v0.1.md"))

# ============================================================
# Reporting positioning notes
# ============================================================

message("Generating reporting positioning notes...")

positioning_lines <- c(
  "# Reporting and positioning notes",
  "",
  "## Recommended manuscript identity",
  "Cross-cohort development and dataset-level transportability evaluation of a compact adult blood transcriptomic host-response model for sepsis diagnosis.",
  "",
  "## Claims that are supported",
  "- A compact 10-gene model was trained using public adult blood transcriptomic cohorts.",
  "- The model showed high apparent discrimination in pooled development data.",
  "- Dataset-level leave-one-dataset-out validation suggested generally transportable ranking performance.",
  "- Calibration and fixed-threshold behavior were not consistently transportable.",
  "- Local recalibration and independent prospective validation are needed before clinical deployment.",
  "",
  "## Claims to avoid",
  "- Do not claim that the model is ready for direct clinical use.",
  "- Do not describe the apparent pooled performance as external validation.",
  "- Do not claim that the fixed threshold is clinically deployable across platforms.",
  "- Do not claim therapeutic target discovery from GO enrichment.",
  "- Do not overstate KEGG if no significant KEGG terms were found.",
  "",
  "## Suggested key sentence",
  "The model demonstrated cross-cohort transportability in ranking performance, whereas predicted probability calibration and fixed decision thresholds showed substantial cohort dependence.",
  "",
  "## Suggested limitation sentence",
  "Because candidate gene screening and final model training used retrospective public cohorts, the leave-one-dataset-out analysis should be interpreted as dataset-level transportability assessment rather than fully independent prospective validation."
)

write_text(positioning_lines, file.path(manuscript_dir, "reporting_positioning_notes_v0.1.md"))

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_12_manuscript_structure_and_results_draft.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 12 Manuscript structure and results draft 完成 ============")
message("论文草稿目录：", manuscript_dir)
message("Reporting 输出目录：", report_dir)

message("\nKey numbers:")
print(key_numbers)

message("\nGenerated manuscript files:")
generated_files <- c(
  file.path(manuscript_dir, "title_options_v0.1.md"),
  file.path(manuscript_dir, "abstract_structured_v0.1.md"),
  file.path(manuscript_dir, "manuscript_methods_section_v0.1.md"),
  file.path(manuscript_dir, "manuscript_results_section_v0.1.md"),
  file.path(manuscript_dir, "manuscript_discussion_section_v0.1.md"),
  file.path(manuscript_dir, "manuscript_main_draft_v0.1.md"),
  file.path(manuscript_dir, "figure_table_plan_v0.1.md"),
  file.path(manuscript_dir, "reporting_positioning_notes_v0.1.md")
)
print(generated_files)

message("\nGenerated reporting files:")
print(c(
  file.path(report_dir, "T12_manuscript_key_numbers.csv"),
  file.path(report_dir, "T12_figure_table_plan.csv")
))

message("\n下一步建议：")
message("1) 先打开 manuscript_main_draft_v0.1.md 粗读整体逻辑。")
message("2) 再重点修改 manuscript_results_section_v0.1.md 和 manuscript_discussion_section_v0.1.md。")
message("3) 后续可继续生成 Word 版初稿。")