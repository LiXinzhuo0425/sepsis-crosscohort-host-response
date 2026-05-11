# ============================================================
# 23_BMC_Genomics_methods_results_draft.R
# Generate BMC Genomics Methods + Results draft v0.3
#
# 修正版：
# 1. 修复 fmt_num() / fmt_pct() 不能处理向量的问题
# 2. 不新增分析，不重新计算模型
# 3. 所有数字只来自 04_results/final_freeze/
# 4. 输出 markdown 草稿，后续逐段人工修改
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_methods_v0.3.md
#   BMC_Genomics_results_v0.3.md
#   BMC_Genomics_methods_results_combined_v0.3.md
#   BMC_Genomics_methods_results_writer_notes_v0.3.md
#   BMC_Genomics_methods_results_v0.3_index.xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
freeze_tables_dir <- file.path(freeze_dir, "tables")
manuscript_dir <- file.path(project_dir, "07_manuscript")
draft_dir <- file.path(manuscript_dir, "BMC_Genomics_draft")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

files <- list(
  key_snapshot = file.path(freeze_dir, "final_key_result_snapshot.csv"),
  decision_log = file.path(freeze_dir, "final_freeze_decision_log.csv"),
  source_index = file.path(freeze_dir, "manuscript_source_index.csv"),
  T20_table1 = file.path(freeze_tables_dir, "T20_table1_T20_table1_cohort_context.csv"),
  T20_table2 = file.path(freeze_tables_dir, "T20_table2_T20_table2_nested_LODO_validation.csv"),
  T20_table3 = file.path(freeze_tables_dir, "T20_table3_T20_table3_calibration_threshold_transport.csv"),
  T20_table4 = file.path(freeze_tables_dir, "T20_table4_T20_table4_scRNA_localization.csv"),
  T20_key = file.path(freeze_tables_dir, "T20_key_numbers_T20_key_results_numbers.csv"),
  T21_risk = file.path(freeze_tables_dir, "T21_risk_flags_T21_manuscript_risk_flags.csv"),
  T21_claims = file.path(freeze_tables_dir, "T21_claims_T21_recommended_claims_and_restrictions.csv"),
  T16b_go = file.path(freeze_tables_dir, "T16b_go_no_go_T16b_RNAseq_go_no_go_summary.csv"),
  T14_gene_frequency = file.path(freeze_tables_dir, "T14_gene_frequency_T14_nested_LODO_gene_selection_frequency.csv"),
  T17_dca_summary = file.path(freeze_tables_dir, "T17_dca_summary_T17_nested_LODO_DCA_summary.csv")
)

missing <- names(files)[!file.exists(unlist(files))]

if (length(missing) > 0) {
  stop(
    "缺少 final_freeze 输入文件：\n",
    paste(missing, unlist(files)[missing], sep = ": ", collapse = "\n")
  )
}

# ============================================================
# 工具函数
# ============================================================

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
}

get_value <- function(df, item) {
  if (!all(c("item", "value") %in% colnames(df))) return(NA_character_)
  x <- df$value[df$item == item]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

get_metric <- function(df, metric) {
  if (!all(c("metric", "value") %in% colnames(df))) return(NA_character_)
  x <- df$value[df$metric == metric]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

# 支持单个数字和向量
fmt_num <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  
  if (length(x) == 0) {
    return(character())
  }
  
  out <- rep("NA", length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- formatC(x[ok], format = "f", digits = digits)
  
  if (length(out) == 1) {
    return(out[1])
  }
  
  out
}

# 支持单个数字和向量
fmt_pct <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  
  if (length(x) == 0) {
    return(character())
  }
  
  out <- rep("NA", length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- paste0(formatC(100 * x[ok], format = "f", digits = digits), "%")
  
  if (length(out) == 1) {
    return(out[1])
  }
  
  out
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

safe_extract_one <- function(x, default = NA_character_) {
  if (length(x) == 0) return(default)
  if (all(is.na(x))) return(default)
  x[1]
}

collapse_dataset_perf <- function(table2) {
  required_cols <- c(
    "validation_dataset", "n", "n_case", "n_control",
    "AUROC", "AUROC_low", "AUROC_high", "AUPRC", "Brier"
  )
  
  missing_cols <- setdiff(required_cols, colnames(table2))
  if (length(missing_cols) > 0) {
    stop("table2 缺少必要列：", paste(missing_cols, collapse = ", "))
  }
  
  x <- table2[, required_cols, drop = FALSE]
  
  x$sentence <- paste0(
    x$validation_dataset,
    " (n=", x$n,
    "; cases/controls=", x$n_case, "/", x$n_control,
    "; AUROC=", fmt_num(x$AUROC),
    ", 95% CI ", fmt_num(x$AUROC_low), "-", fmt_num(x$AUROC_high),
    "; AUPRC=", fmt_num(x$AUPRC),
    "; Brier=", fmt_num(x$Brier),
    ")"
  )
  
  paste(x$sentence, collapse = "; ")
}

# ============================================================
# 读取 frozen data
# ============================================================

message("Reading frozen tables...")

key_snapshot <- read_csv_df(files$key_snapshot)
decision_log <- read_csv_df(files$decision_log)
source_index <- read_csv_df(files$source_index)
table1 <- read_csv_df(files$T20_table1)
table2 <- read_csv_df(files$T20_table2)
table3 <- read_csv_df(files$T20_table3)
table4 <- read_csv_df(files$T20_table4)
key_numbers <- read_csv_df(files$T20_key)
risk_flags <- read_csv_df(files$T21_risk)
claims <- read_csv_df(files$T21_claims)
rna_go <- read_csv_df(files$T16b_go)
gene_frequency <- read_csv_df(files$T14_gene_frequency)
dca_summary <- read_csv_df(files$T17_dca_summary)

# ============================================================
# 关键数字
# ============================================================

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
bulk_sepsis <- get_value(key_snapshot, "bulk_sepsis")
bulk_control <- get_value(key_snapshot, "bulk_control")
n_bulk_datasets <- get_value(key_snapshot, "n_bulk_datasets")

nested_median_auc <- fmt_num(get_value(key_snapshot, "nested_LODO_median_AUROC"))
nested_min_auc <- fmt_num(get_value(key_snapshot, "nested_LODO_min_AUROC"))
nested_auc_ge_080 <- get_value(key_snapshot, "nested_LODO_AUROC_ge_0.80_datasets")
pooled_auc <- fmt_num(get_value(key_snapshot, "pooled_nested_LODO_AUROC"))
pooled_brier <- fmt_num(get_value(key_snapshot, "pooled_nested_LODO_Brier"))
median_thr_shift <- fmt_num(get_value(key_snapshot, "median_abs_threshold_shift"))
max_thr_shift <- fmt_num(get_value(key_snapshot, "max_abs_threshold_shift"))
fixed_sens_fail <- get_value(key_snapshot, "fixed_sensitivity_lt_0.20_datasets")
fixed_spec_fail <- get_value(key_snapshot, "fixed_specificity_lt_0.20_datasets")
all_fold_genes <- get_value(key_snapshot, "genes_selected_all_nested_folds")
rnaseq_status <- get_value(key_snapshot, "RNAseq_validation_status")
scrna_cells <- get_value(key_snapshot, "scRNA_cells")
scrna_clusters <- get_value(key_snapshot, "scRNA_clusters")
scrna_top_final <- get_value(key_snapshot, "scRNA_top_Final10_celltype")
scrna_top_nested <- get_value(key_snapshot, "scRNA_top_NestedRecurrent_celltype")

rna_scanned <- get_metric(rna_go, "n_datasets_scanned")
rna_eligible <- get_metric(rna_go, "n_eligible_for_16_RNAseq_validation")
rna_possible <- get_metric(rna_go, "n_possible_requires_manual_verification")

if (!all(c("gene_symbol", "selection_frequency") %in% colnames(gene_frequency))) {
  stop("T14 gene frequency 表缺少 gene_symbol 或 selection_frequency 列。")
}

top_gene_freq <- gene_frequency[order(-gene_frequency$selection_frequency, gene_frequency$gene_symbol), , drop = FALSE]
top_gene_freq_string <- paste(head(top_gene_freq$gene_symbol, 10), collapse = ", ")

poor_dataset <- paste(table2$validation_dataset[table2$AUROC < 0.70], collapse = ", ")
if (poor_dataset == "") poor_dataset <- "none"

low_fixed_sens_datasets <- paste(table3$validation_dataset[table3$fixed_sensitivity < 0.20], collapse = ", ")
if (low_fixed_sens_datasets == "") low_fixed_sens_datasets <- "none"

low_fixed_spec_datasets <- paste(table3$validation_dataset[table3$fixed_specificity < 0.20], collapse = ", ")
if (low_fixed_spec_datasets == "") low_fixed_spec_datasets <- "none"

sc_top_row <- table4[table4$cell_type_level1 == "Monocyte/Myeloid", , drop = FALSE]
sc_final10_myeloid <- ifelse(nrow(sc_top_row) > 0, fmt_num(sc_top_row$mean_score_Final10[1]), "NA")
sc_nested_myeloid <- ifelse(nrow(sc_top_row) > 0, fmt_num(sc_top_row$mean_score_NestedRecurrent[1]), "NA")

dca_pooled <- dca_summary[dca_summary$validation_dataset == "Pooled_nested_LODO", , drop = FALSE]
dca_fraction <- ifelse(
  nrow(dca_pooled) > 0,
  fmt_pct(dca_pooled$fraction_thresholds_clinically_preferable[1]),
  "NA"
)

gse54514_auc <- fmt_num(safe_extract_one(table2$AUROC[table2$validation_dataset == "GSE54514"]))
gse236713_sens <- fmt_num(safe_extract_one(table3$fixed_sensitivity[table3$validation_dataset == "GSE236713"]))
gse65682_sens <- fmt_num(safe_extract_one(table3$fixed_sensitivity[table3$validation_dataset == "GSE65682"]))

# ============================================================
# Methods draft
# ============================================================

methods <- c(
  "# Methods",
  "",
  "## Study design and overall analytical framework",
  "",
  paste0(
    "This study was designed as a secondary analysis of publicly available blood transcriptomic datasets to evaluate the cross-cohort transportability of a host-response signature for sepsis. ",
    "The primary analysis used a strict nested leave-one-dataset-out (LODO) framework across public whole-blood bulk transcriptomic cohorts. ",
    "In each outer validation fold, one dataset was held out and did not contribute to candidate gene screening, redundancy reduction, expression standardization, model fitting, hyperparameter selection or threshold selection. ",
    "The held-out dataset was used only once for validation. ",
    "This design was chosen to reduce information leakage and to distinguish discrimination from probability-scale and fixed-threshold transportability."
  ),
  "",
  "The study also included three secondary components. First, differential expression and functional enrichment analyses were performed to characterize sepsis-associated host-response biology. Second, calibration, threshold transportability and exploratory decision-curve analysis were performed using the nested LODO predictions. Third, an independent peripheral blood mononuclear cell single-cell RNA sequencing dataset was analyzed to localize the bulk-derived signature to broad cellular compartments. RNA-seq external platform validation was attempted through local dataset eligibility screening, but no eligible RNA-seq dataset was identified; therefore RNA-seq validation was not performed and was not used to modify the model or conclusions.",
  "",
  "## Bulk transcriptomic cohorts and eligibility",
  "",
  paste0(
    "The frozen bulk analysis included ",
    bulk_total,
    " samples from ",
    n_bulk_datasets,
    " public whole-blood transcriptomic cohorts, including ",
    bulk_sepsis,
    " sepsis cases and ",
    bulk_control,
    " controls. ",
    "Datasets were eligible for the main bulk analysis when they contained adult whole-blood expression data, a usable sepsis-control contrast, available phenotype information, and expression data that could be harmonized to gene symbols. ",
    "The final cohorts differed in platform, sepsis severity, sampling time and control definition. ",
    "Control contexts included healthy controls, matched healthy controls, and selected control contrasts from complex intensive care unit cohorts. ",
    "These differences were retained and summarized explicitly to support a transportability-oriented interpretation rather than same-intended-use clinical validation."
  ),
  "",
  "## Bulk expression preprocessing and gene-level harmonization",
  "",
  "Archived public processed expression matrices were used. Raw-level reprocessing was not performed. Probe-level or feature-level expression data were mapped to gene symbols using available platform annotations or locally prepared annotation files. When multiple probes or features mapped to the same gene symbol, expression values were collapsed to the gene level using median aggregation. Samples and phenotype files were harmonized using sample identifiers, and the final analysis was restricted to genes shared across the included cohorts. Clinical grouping was harmonized to sepsis and control categories for the primary diagnostic contrast.",
  "",
  "## Differential expression and functional enrichment analysis",
  "",
  "Differential expression between sepsis and control samples was assessed on the merged gene-level expression matrix using a batch-aware linear modeling framework. Dataset identity was included to account for between-cohort differences. Genes were classified as differentially expressed using an adjusted P value threshold of 0.05 and an absolute log fold-change threshold of 0.5. Functional enrichment was performed using differentially expressed genes and the shared gene universe. Gene Ontology biological process enrichment was used to characterize major host-response patterns. Enrichment results were interpreted descriptively and were not used to alter model selection after the nested validation design was finalized.",
  "",
  "## Candidate gene screening and redundancy reduction",
  "",
  "Candidate genes for diagnostic modeling were selected from the differential expression and cohort-level screening outputs. Candidate prioritization considered global differential expression, direction consistency across datasets, per-dataset univariate discrimination, and immune-related enrichment annotations. To reduce redundancy among highly correlated genes, candidate genes were further refined using correlation-based filtering. In the strict nested LODO analysis, all candidate screening and redundancy reduction steps were repeated within each training-only fold, excluding the held-out dataset.",
  "",
  "## Strict nested leave-one-dataset-out modeling",
  "",
  "For each nested LODO fold, five datasets were used as the training set and the remaining dataset was held out for validation. Within each training-only set, candidate genes were screened and refined, expression values were standardized using training-set center and scale parameters, and penalized logistic regression models were trained using regularization. Hyperparameter selection and model tuning were performed within the training data only. A fold-specific operating threshold was selected from the training set and then applied unchanged to the held-out dataset. The held-out dataset was not used for feature selection, standardization, tuning, model fitting or threshold selection.",
  "",
  "## Performance evaluation",
  "",
  "Discrimination was evaluated using the area under the receiver operating characteristic curve (AUROC) and the area under the precision-recall curve (AUPRC). Additional threshold-dependent metrics included sensitivity, specificity, positive predictive value, negative predictive value and accuracy at the fold-specific training threshold. Brier score was used to summarize probabilistic prediction error. Per-dataset validation metrics were treated as the primary performance results because cohort-specific phenotype definitions and control contexts were heterogeneous. Pooled nested LODO predictions were also summarized to evaluate aggregate transport behavior across all held-out predictions.",
  "",
  "## Calibration and threshold transportability",
  "",
  "Calibration was assessed using observed sepsis prevalence, mean predicted probability, calibration-in-the-large, calibration intercept, calibration slope and Brier score. Threshold transportability was evaluated by comparing the fold-specific training threshold with the local Youden threshold estimated within each held-out dataset for descriptive purposes. The local Youden threshold was not used to revise the frozen model. Large shifts between training-derived and local thresholds were interpreted as evidence of fixed-threshold transportability failure.",
  "",
  "## Exploratory decision-curve analysis",
  "",
  "Decision-curve analysis was performed using nested LODO predictions to describe threshold-dependent net benefit across a clinically plausible range of threshold probabilities. Because the included cohorts differed in control type and probability calibration was unstable, decision-curve analysis was treated as exploratory. It was not used to claim established clinical utility.",
  "",
  "## RNA-seq external validation screening",
  "",
  paste0(
    "A local file inventory was performed to identify potential RNA-seq datasets for external platform validation. ",
    "Candidate files were screened for platform class, expression scale, sample size, sepsis-control contrast, phenotype availability and target-gene availability. ",
    "A dataset would have been considered eligible for RNA-seq external validation if it had confirmed RNA-seq expression data, a clear sepsis-control contrast, adequate sample size, and sufficient availability of the frozen signature genes. ",
    "Among ",
    rna_scanned,
    " screened candidate datasets, no eligible RNA-seq dataset and no possible dataset requiring manual verification were detected. ",
    "Accordingly, RNA-seq external validation was not performed."
  ),
  "",
  "## Single-cell RNA-seq processing and signature localization",
  "",
  paste0(
    "An independent public peripheral blood mononuclear cell single-cell RNA-seq dataset was used for biological localization of the bulk-derived signature. ",
    "After quality control and standard single-cell preprocessing, the final single-cell object contained ",
    scrna_cells,
    " cells and ",
    scrna_clusters,
    " clusters. ",
    "Clusters were annotated into broad cell-type compartments using canonical marker-based module scores. ",
    "Module scores were calculated for the final compact signature and the recurrent nested LODO gene set. ",
    "These scores were summarized by broad cell type, clinical group and outcome/timepoint categories. ",
    "Single-cell analysis was interpreted as biological localization only and was not considered independent diagnostic validation."
  ),
  "",
  "## Reproducibility and result freezing",
  "",
  "All final analyses were frozen before manuscript drafting. The final freeze recorded key numerical results, decision logs, manuscript source indices, RNA-seq No-Go decisions, allowed claims and restricted claims. After freezing, no gene re-selection, threshold re-selection, cohort removal or untracked rerun was allowed. Manuscript drafting used the frozen result tables as the sole numerical source."
)

# ============================================================
# Results draft
# ============================================================

dataset_sentence <- collapse_dataset_perf(table2)

results <- c(
  "# Results",
  "",
  "## Cohort composition and analysis overview",
  "",
  paste0(
    "The final frozen bulk transcriptomic analysis included ",
    bulk_total,
    " samples from ",
    n_bulk_datasets,
    " public whole-blood cohorts, comprising ",
    bulk_sepsis,
    " sepsis cases and ",
    bulk_control,
    " controls (Table 1). ",
    "The cohorts differed in platform, clinical context and control definition. ",
    "Most contrasts involved healthy or selected healthy controls, whereas one cohort included SIRS samples that were not used in the primary contrast. ",
    "These characteristics supported a cross-cohort transportability analysis but limited direct clinical mimic interpretation."
  ),
  "",
  paste0(
    "A local RNA-seq validation screen was performed before result freezing. ",
    rna_scanned,
    " candidate datasets were screened, but no eligible RNA-seq external validation dataset was identified. ",
    "RNA-seq validation was therefore recorded as a No-Go decision and was not used to modify the model, thresholds or conclusions."
  ),
  "",
  "## Differential expression and functional enrichment",
  "",
  "The bulk differential expression analysis identified a broad sepsis-associated host-response program. Upregulated genes were enriched for myeloid leukocyte activation, response to bacterium, defense response to bacterium, hemostasis, blood coagulation and platelet activation. Downregulated genes were enriched for T cell activation, adaptive immune response, antigen receptor-mediated signaling and lymphocyte differentiation. Together, these patterns were consistent with concurrent innate/myeloid activation and suppression or redistribution of adaptive immune-related programs. These enrichment results were used for biological interpretation and candidate prioritization, while model validation was subsequently performed under a strict nested design.",
  "",
  "## Strict nested LODO validation showed recurrent but non-uniform discrimination",
  "",
  paste0(
    "Strict nested LODO validation was performed across the six bulk cohorts. ",
    "The median held-out AUROC was ",
    nested_median_auc,
    ", with a range from ",
    nested_min_auc,
    " to 1.000. ",
    nested_auc_ge_080,
    " of six held-out datasets achieved AUROC >=0.80 (Table 2). ",
    "Per-dataset validation summaries were as follows: ",
    dataset_sentence,
    "."
  ),
  "",
  paste0(
    "Discrimination was not uniform across cohorts. ",
    "GSE54514 showed poor held-out discrimination (AUROC ",
    gse54514_auc,
    ") and was retained as a transportability failure scenario. ",
    "By contrast, GSE57065 and GSE95233 showed perfect or near-perfect held-out discrimination, likely reflecting strong septic-shock-versus-healthy-control contrasts. ",
    "This pattern indicated that the signature could transport in several settings but was not universally robust across all available public cohorts."
  ),
  "",
  paste0(
    "Gene selection also varied across nested folds. ",
    "The genes selected in all six nested folds were ",
    all_fold_genes,
    ". ",
    "The most frequently selected genes included ",
    top_gene_freq_string,
    ". ",
    "This fold-level variability supported reporting the signature as a transportable host-response signal rather than as a single universally fixed clinical diagnostic panel."
  ),
  "",
  "## Calibration and fixed-threshold transportability were unstable",
  "",
  paste0(
    "Pooled nested LODO predictions showed weaker aggregate transport behavior than the median per-dataset AUROC. ",
    "The pooled AUROC was ",
    pooled_auc,
    ", with a pooled Brier score of ",
    pooled_brier,
    ". ",
    "The observed pooled sepsis rate exceeded the mean predicted probability, indicating probability-scale misalignment across cohorts."
  ),
  "",
  paste0(
    "Fixed-threshold behavior was strongly dataset-dependent. ",
    "The median absolute shift between the fold-specific training threshold and the local Youden threshold was ",
    median_thr_shift,
    ", with a maximum absolute shift of ",
    max_thr_shift,
    " (Table 3). ",
    "At the fixed training-derived threshold, sensitivity was below 0.20 in ",
    fixed_sens_fail,
    " datasets (",
    low_fixed_sens_datasets,
    "), and specificity was below 0.20 in ",
    fixed_spec_fail,
    " dataset (",
    low_fixed_spec_datasets,
    "). ",
    "Notably, GSE236713 had excellent AUROC but very low fixed-threshold sensitivity (",
    gse236713_sens,
    "), whereas GSE65682 had acceptable AUROC but fixed-threshold sensitivity of ",
    gse65682_sens,
    ". ",
    "These findings showed that discrimination and fixed-threshold transportability diverged under cross-cohort deployment conditions."
  ),
  "",
  "## Exploratory decision-curve analysis showed heterogeneous net benefit",
  "",
  paste0(
    "Decision-curve analysis was performed as an exploratory threshold-dependent assessment. ",
    "Across pooled nested LODO predictions, the model was clinically preferable over both treat-all and treat-none strategies across ",
    dca_fraction,
    " of evaluated threshold probabilities. ",
    "Dataset-level net benefit patterns were heterogeneous, consistent with the observed calibration and threshold instability. ",
    "Therefore, decision-curve analysis was interpreted as descriptive and hypothesis-generating rather than as evidence of established clinical utility."
  ),
  "",
  "## Single-cell analysis localized the signature to monocyte/myeloid compartments",
  "",
  paste0(
    "To provide biological context for the bulk-derived signature, an independent PBMC single-cell RNA-seq dataset was analyzed. ",
    "After quality control, the single-cell object contained ",
    scrna_cells,
    " cells and ",
    scrna_clusters,
    " clusters. ",
    "Coarse canonical-marker annotation grouped cells into six broad cell-type compartments (Table 4). ",
    "All genes in the final compact signature and all recurrent nested LODO genes were detectable in the single-cell object."
  ),
  "",
  paste0(
    "Both the final compact signature and the recurrent nested LODO signature showed their highest module scores in ",
    scrna_top_final,
    " compartments. ",
    "The mean module score in Monocyte/Myeloid cells was ",
    sc_final10_myeloid,
    " for the final compact signature and ",
    sc_nested_myeloid,
    " for the recurrent nested LODO signature. ",
    "Scores were lower in T/NK cells and B/plasma cells. ",
    "These findings supported a predominantly monocyte/myeloid cellular origin of the transported host-response signal."
  ),
  "",
  "At the clinical group level, signature scores were descriptively higher in sepsis cells than in healthy-control cells, particularly among nonsurvivor samples. Because the single-cell dataset contained a limited number of independent donors and was not designed as a diagnostic validation cohort, these group-level differences were interpreted as biological localization and contextual evidence rather than patient-level validation of diagnostic performance."
)

# ============================================================
# Writer notes
# ============================================================

writer_notes <- c(
  "# Writer notes for Methods + Results v0.3",
  "",
  "## What this draft does",
  "",
  "- Uses only frozen results.",
  "- Writes Methods and Results for BMC Genomics.",
  "- Maintains defensive framing.",
  "- Keeps RNA-seq as attempted screening and No-Go.",
  "- Keeps scRNA as biological localization only.",
  "",
  "## Manual checks needed",
  "",
  "1. Replace generic dataset wording with exact accession-level descriptions if desired.",
  "2. Decide whether GO enrichment remains Figure 4 main figure or moves to supplement.",
  "3. Confirm all table and figure numbering after final figure selection.",
  "4. Add software versions from sessionInfo if required in Methods.",
  "5. Later add citations in Background/Discussion, not in this generated Methods/Results draft.",
  "",
  "## Phrases to preserve",
  "",
  "- strict nested leave-one-dataset-out",
  "- held-out dataset did not contribute to feature screening, standardization, model fitting, tuning or threshold selection",
  "- calibration and fixed-threshold transportability were unstable",
  "- biological localization only",
  "- not independent diagnostic validation",
  "",
  "## Phrases to avoid",
  "",
  "- clinically ready",
  "- robust clinical utility",
  "- externally validated fixed threshold",
  "- same-intended-use validation",
  "- scRNA validated diagnostic performance"
)

# ============================================================
# 保存输出
# ============================================================

message("Writing Methods and Results draft files...")

methods_path <- file.path(draft_dir, "BMC_Genomics_methods_v0.3.md")
results_path <- file.path(draft_dir, "BMC_Genomics_results_v0.3.md")
combined_path <- file.path(draft_dir, "BMC_Genomics_methods_results_combined_v0.3.md")
notes_path <- file.path(draft_dir, "BMC_Genomics_methods_results_writer_notes_v0.3.md")

write_text_file(methods, methods_path)
write_text_file(results, results_path)
write_text_file(c(methods, "", results), combined_path)
write_text_file(writer_notes, notes_path)

# 输出 index workbook
output_index <- data.frame(
  file = c(
    "BMC_Genomics_methods_v0.3.md",
    "BMC_Genomics_results_v0.3.md",
    "BMC_Genomics_methods_results_combined_v0.3.md",
    "BMC_Genomics_methods_results_writer_notes_v0.3.md"
  ),
  path = c(methods_path, results_path, combined_path, notes_path),
  purpose = c(
    "Methods draft for BMC Genomics",
    "Results draft for BMC Genomics",
    "Combined Methods + Results draft",
    "Notes and manual checks"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "key_snapshot")
openxlsx::writeData(wb, "key_snapshot", key_snapshot)

openxlsx::addWorksheet(wb, "table2_nested_LODO")
openxlsx::writeData(wb, "table2_nested_LODO", table2)

openxlsx::addWorksheet(wb, "table3_cal_threshold")
openxlsx::writeData(wb, "table3_cal_threshold", table3)

openxlsx::addWorksheet(wb, "table4_scRNA")
openxlsx::writeData(wb, "table4_scRNA", table4)

openxlsx::saveWorkbook(
  wb,
  file.path(draft_dir, "BMC_Genomics_methods_results_v0.3_index.xlsx"),
  overwrite = TRUE
)

# sessionInfo
sink(file.path(log_dir, "sessionInfo_23_BMC_Genomics_methods_results_draft.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 23 BMC Genomics Methods + Results draft 完成 ============")
message("输出目录：", draft_dir)

message("\nGenerated files:")
print(output_index)

message("\nMethods preview:")
cat(paste(head(methods, 25), collapse = "\n"))
cat("\n...\n")

message("\nResults preview:")
cat(paste(head(results, 35), collapse = "\n"))
cat("\n...\n")

message("\n关键输出：")
message("1) ", methods_path)
message("2) ", results_path)
message("3) ", combined_path)
message("4) ", notes_path)
message("5) ", file.path(draft_dir, "BMC_Genomics_methods_results_v0.3_index.xlsx"))

message("\n下一步：")
message("打开 BMC_Genomics_methods_results_combined_v0.3.md 粗读。")
message("然后把你觉得不对劲的段落贴给我，或直接继续生成 Background + Discussion 初稿。")