# ============================================================
# 29_BMC_Genomics_table_figure_caption_draft.R
# Draft tables, figure legends and supplementary material plan
# for BMC Genomics manuscript v0.5.1
#
# 目的：
# 1. 基于 final_freeze 和 v0.5.1 稿件生成 BMC Genomics 图表标题和图注
# 2. 不新增分析，不重算任何结果
# 3. 输出 main figures/tables + supplementary plan
# 4. 输出图表 claim 风险自查
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.1_polished.md
# 04_results/final_freeze/
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_table_figure_captions_v0.6.md
#   BMC_Genomics_table_figure_caption_index_v0.6.xlsx
#   BMC_Genomics_main_table_plan_v0.6.csv
#   BMC_Genomics_main_figure_plan_v0.6.csv
#   BMC_Genomics_supplementary_material_plan_v0.6.csv
#   BMC_Genomics_table_figure_audit_v0.6.csv
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
freeze_tables_dir <- file.path(freeze_dir, "tables")
freeze_figures_dir <- file.path(freeze_dir, "figures")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

manuscript_file <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.1_polished.md")
key_snapshot_file <- file.path(freeze_dir, "final_key_result_snapshot.csv")
figure_inventory_file <- file.path(freeze_dir, "final_freeze_figure_inventory.csv")
copy_manifest_file <- file.path(freeze_dir, "final_freeze_file_copy_manifest.csv")
source_index_file <- file.path(freeze_dir, "manuscript_source_index.csv")
decision_log_file <- file.path(freeze_dir, "final_freeze_decision_log.csv")

required_inputs <- c(
  manuscript_file,
  key_snapshot_file,
  figure_inventory_file,
  copy_manifest_file,
  source_index_file,
  decision_log_file
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop("缺少输入文件：\n", paste(missing_inputs, collapse = "\n"))
}

# ============================================================
# 工具函数
# ============================================================

read_lines_utf8 <- function(path) {
  readLines(path, encoding = "UTF-8", warn = FALSE)
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
}

get_value <- function(df, item) {
  if (!all(c("item", "value") %in% colnames(df))) return(NA_character_)
  x <- df$value[df$item == item]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

fmt_num <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x[1])) return("NA")
  formatC(x[1], format = "f", digits = digits)
}

contains_forbidden <- function(text, patterns) {
  vapply(
    patterns,
    function(p) grepl(p, text, ignore.case = TRUE, perl = TRUE),
    logical(1)
  )
}

# ============================================================
# 读取输入
# ============================================================

message("Reading final freeze and manuscript materials...")

ms <- read_lines_utf8(manuscript_file)
key_snapshot <- read_csv_df(key_snapshot_file)
figure_inventory <- read_csv_df(figure_inventory_file)
copy_manifest <- read_csv_df(copy_manifest_file)
source_index <- read_csv_df(source_index_file)
decision_log <- read_csv_df(decision_log_file)

# ============================================================
# 关键数字
# ============================================================

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
bulk_sepsis <- get_value(key_snapshot, "bulk_sepsis")
bulk_control <- get_value(key_snapshot, "bulk_control")
n_bulk_datasets <- get_value(key_snapshot, "n_bulk_datasets")

nested_median_auc <- fmt_num(get_value(key_snapshot, "nested_LODO_median_AUROC"))
nested_min_auc <- fmt_num(get_value(key_snapshot, "nested_LODO_min_AUROC"))
nested_ge080 <- get_value(key_snapshot, "nested_LODO_AUROC_ge_0.80_datasets")
pooled_auc <- fmt_num(get_value(key_snapshot, "pooled_nested_LODO_AUROC"))
pooled_brier <- fmt_num(get_value(key_snapshot, "pooled_nested_LODO_Brier"))
threshold_shift <- fmt_num(get_value(key_snapshot, "median_abs_threshold_shift"))
threshold_shift_max <- fmt_num(get_value(key_snapshot, "max_abs_threshold_shift"))
fixed_sens_fail <- get_value(key_snapshot, "fixed_sensitivity_lt_0.20_datasets")
fixed_spec_fail <- get_value(key_snapshot, "fixed_specificity_lt_0.20_datasets")

all_fold_genes <- get_value(key_snapshot, "genes_selected_all_nested_folds")
rnaseq_status <- get_value(key_snapshot, "RNAseq_validation_status")

scrna_cells <- get_value(key_snapshot, "scRNA_cells")
scrna_clusters <- get_value(key_snapshot, "scRNA_clusters")
scrna_top_final <- get_value(key_snapshot, "scRNA_top_Final10_celltype")
scrna_top_nested <- get_value(key_snapshot, "scRNA_top_NestedRecurrent_celltype")

# ============================================================
# Main table plan
# ============================================================

main_table_plan <- data.frame(
  table_id = c("Table 1", "Table 2", "Table 3", "Table 4"),
  title = c(
    "Cohort characteristics and control definitions",
    "Strict nested leave-one-dataset-out validation performance",
    "Calibration and fixed-threshold transportability across held-out cohorts",
    "Single-cell localization of host-response signature module scores"
  ),
  source_file = c(
    "T20_table1_cohort_context.csv",
    "T20_table2_nested_LODO_validation.csv",
    "T20_table3_calibration_threshold_transport.csv",
    "T20_table4_scRNA_localization.csv"
  ),
  frozen_source = c(
    "04_results/final_freeze/tables/T20_table1_T20_table1_cohort_context.csv",
    "04_results/final_freeze/tables/T20_table2_T20_table2_nested_LODO_validation.csv",
    "04_results/final_freeze/tables/T20_table3_T20_table3_calibration_threshold_transport.csv",
    "04_results/final_freeze/tables/T20_table4_T20_table4_scRNA_localization.csv"
  ),
  manuscript_role = c(
    "Main cohort context table. Must show control type and limitation.",
    "Primary validation performance table.",
    "Central table for calibration and threshold drift.",
    "Biological localization table. Not diagnostic validation."
  ),
  required_claim_boundary = c(
    "Do not overstate clinical mimic validation.",
    "Report both strong and weak held-out performance.",
    "State threshold instability directly.",
    "State scRNA as localization only."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Main figure plan
# ============================================================

main_figure_plan <- data.frame(
  figure_id = c("Figure 1", "Figure 2", "Figure 3", "Figure 4", "Figure 5"),
  short_title = c(
    "Study design and analysis workflow",
    "Nested LODO diagnostic performance",
    "Calibration and threshold transportability",
    "Functional enrichment of sepsis-associated transcriptomic alterations",
    "Single-cell localization of the host-response signature"
  ),
  recommended_panels = c(
    "A: cohort inclusion; B: nested LODO design; C: RNA-seq No-Go screening; D: scRNA localization workflow",
    "A: per-dataset ROC or AUROC forest plot; B: AUPRC summary; C: selected-gene recurrence",
    "A: observed versus mean predicted probability; B: calibration-in-the-large; C: fixed versus local thresholds; D: fixed-threshold failures",
    "A: upregulated GO biological processes; B: downregulated GO biological processes; C: schematic host-response interpretation",
    "A: UMAP by broad cell type; B: module score by cell type; C: gene-expression dot plot; D: clinical group score summary"
  ),
  frozen_source = c(
    "final_freeze decision log; T15b; T16b; T18/T19",
    "T14 nested LODO outputs; T20 Table 2",
    "T17 calibration and threshold outputs; T20 Table 3",
    "T05 enrichment outputs",
    "T19 scRNA outputs; T20 Table 4"
  ),
  manuscript_message = c(
    "The study evaluates transportability using public bulk transcriptomic cohorts, records RNA-seq No-Go screening, and uses scRNA for localization.",
    "The signature shows recurrent but non-uniform held-out discrimination under strict nested LODO.",
    "Calibration and fixed-threshold transportability are unstable across cohorts.",
    "The bulk signal is biologically consistent with innate/myeloid activation and adaptive immune-related suppression.",
    "The signature localizes predominantly to monocyte/myeloid compartments."
  ),
  claim_boundary = c(
    "Do not imply prospective clinical validation.",
    "Do not hide GSE54514 failure.",
    "Do not claim fixed threshold is validated.",
    "Do not overstate mechanistic proof.",
    "Do not call scRNA diagnostic validation."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Supplementary material plan
# ============================================================

supplementary_plan <- data.frame(
  supplement_id = c(
    "Supplementary Table S1",
    "Supplementary Table S2",
    "Supplementary Table S3",
    "Supplementary Table S4",
    "Supplementary Table S5",
    "Supplementary Table S6",
    "Supplementary Figure S1",
    "Supplementary Figure S2",
    "Supplementary Figure S3",
    "Supplementary Figure S4"
  ),
  title = c(
    "Dataset eligibility, cohort-control context and harmonized phenotype definitions",
    "Differential expression and Gene Ontology enrichment results",
    "Candidate gene screening, redundancy filtering and recurrence",
    "Nested LODO selected genes and model parameters by fold",
    "RNA-seq eligibility screening and No-Go decision",
    "Final freeze decision log and allowed claims",
    "Bulk transcriptomic quality control and PCA summaries",
    "Candidate gene selection and correlation filtering",
    "Exploratory decision-curve analysis",
    "Canonical marker-based scRNA cluster annotation"
  ),
  source = c(
    "T15b and T20 Table 1",
    "T05 enrichment and DEG outputs",
    "T06/T07/T14 gene outputs",
    "T14 nested LODO selected genes and model objects",
    "T16b RNA-seq screening outputs",
    "final_freeze decision log; T21 claims/restrictions",
    "T03 QC outputs",
    "T06/T07 candidate-selection figures",
    "T17 DCA outputs",
    "T19 scRNA cluster annotation"
  ),
  purpose = c(
    "Defend cohort-control heterogeneity and contrast definitions.",
    "Support biological interpretation.",
    "Show transparent candidate selection.",
    "Show no hidden fold-level model selection.",
    "Document transparent attempted RNA-seq validation screening.",
    "Document frozen claim boundaries.",
    "Support preprocessing transparency.",
    "Support candidate refinement transparency.",
    "Keep DCA exploratory and transparent.",
    "Support scRNA coarse annotation."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Captions
# ============================================================

caption_lines <- c(
  "# BMC Genomics table and figure caption draft v0.6",
  "",
  "## Main tables",
  "",
  "### Table 1. Cohort characteristics and control definitions",
  "",
  paste0(
    "Summary of the six public whole-blood transcriptomic cohorts included in the frozen bulk analysis. ",
    "The final analysis included ", bulk_total, " samples, comprising ", bulk_sepsis, " sepsis cases and ", bulk_control, " controls. ",
    "The table reports dataset accession, platform, sample type, population, sepsis case definition, control type, clinical control strength, primary contrast, unused clinical mimic groups, sample counts and major interpretive limitations. ",
    "Control-type heterogeneity was retained to support a transportability-oriented interpretation and should not be interpreted as definitive clinical mimic validation."
  ),
  "",
  "### Table 2. Strict nested leave-one-dataset-out validation performance",
  "",
  paste0(
    "Held-out validation performance from the strict nested leave-one-dataset-out analysis. ",
    "In each fold, the held-out dataset did not participate in candidate gene screening, correlation-based redundancy reduction, standardization, model fitting, hyperparameter tuning or threshold selection. ",
    "The table reports sample counts, prevalence, AUROC, AUPRC, Brier score, training-derived threshold and threshold-dependent metrics. ",
    "The median held-out AUROC was ", nested_median_auc, ", with ", nested_ge080, " of six datasets achieving AUROC >=0.80 and a minimum AUROC of ", nested_min_auc, "."
  ),
  "",
  "### Table 3. Calibration and fixed-threshold transportability across held-out cohorts",
  "",
  paste0(
    "Calibration and threshold-transportability metrics calculated from nested LODO predictions. ",
    "The table reports observed sepsis rate, mean predicted probability, calibration-in-the-large, Brier score, fixed training-derived threshold, local Youden threshold and threshold-dependent sensitivity and specificity. ",
    "The median absolute threshold shift was ", threshold_shift, ", with a maximum absolute shift of ", threshold_shift_max, ". ",
    "Fixed-threshold sensitivity was below 0.20 in ", fixed_sens_fail, " datasets, and fixed-threshold specificity was below 0.20 in ", fixed_spec_fail, " dataset. ",
    "Local thresholds are reported descriptively and were not used to revise the frozen model."
  ),
  "",
  "### Table 4. Single-cell localization of host-response signature module scores",
  "",
  paste0(
    "Module-score summaries for the final compact signature and recurrent nested LODO signature across broad cell-type compartments in the independent PBMC scRNA-seq dataset. ",
    "After quality control, the scRNA-seq analysis included ", scrna_cells, " cells and ", scrna_clusters, " clusters. ",
    "Both the final compact signature and the recurrent nested LODO signature showed the highest mean module scores in ", scrna_top_final, " compartments. ",
    "This table supports biological localization of the host-response signal and should not be interpreted as patient-level diagnostic validation."
  ),
  "",
  "## Main figures",
  "",
  "### Figure 1. Study design and analysis workflow",
  "",
  "Overview of the study design. Public whole-blood bulk transcriptomic cohorts were harmonized for a sepsis-control contrast and evaluated using a strict nested leave-one-dataset-out framework. In each outer fold, candidate gene screening, redundancy filtering, standardization, model fitting, hyperparameter selection and threshold selection were performed using training datasets only. The held-out dataset was used only for validation. RNA-seq external validation was attempted through local eligibility screening, but no eligible RNA-seq dataset was detected. An independent PBMC scRNA-seq dataset was used for biological localization of the bulk-derived host-response signature.",
  "",
  "### Figure 2. Strict nested leave-one-dataset-out diagnostic performance",
  "",
  paste0(
    "Discrimination performance and gene-selection recurrence under strict nested LODO validation. ",
    "Panels show held-out AUROC/AUPRC summaries across datasets and recurrence of selected genes across nested folds. ",
    "The median held-out AUROC was ", nested_median_auc, ", but performance was not uniform across cohorts. ",
    "GSE54514 was retained as a transportability failure scenario rather than excluded post hoc. ",
    "Genes selected in all nested folds included ", all_fold_genes, "."
  ),
  "",
  "### Figure 3. Calibration and fixed-threshold transportability across held-out cohorts",
  "",
  paste0(
    "Calibration and threshold behavior of nested LODO predictions. ",
    "Panels summarize observed versus predicted probability, calibration-in-the-large and the shift between fold-specific training thresholds and local held-out Youden thresholds. ",
    "Although several datasets showed acceptable or excellent discrimination, fixed-threshold behavior varied substantially across cohorts. ",
    "The median absolute threshold shift was ", threshold_shift, ", supporting the conclusion that a single fixed operating threshold was not transportable across datasets."
  ),
  "",
  "### Figure 4. Functional enrichment of sepsis-associated transcriptomic alterations",
  "",
  "Gene Ontology biological process enrichment for sepsis-associated differential expression. Enriched upregulated processes were dominated by innate/myeloid activation, response to bacterium, hemostasis and coagulation-related programs, whereas downregulated processes involved T cell activation, adaptive immune response and lymphocyte-related terms. The figure provides biological context for the host-response signature and should be interpreted descriptively rather than as mechanistic proof.",
  "",
  "### Figure 5. Single-cell localization of the host-response signature",
  "",
  paste0(
    "Single-cell localization of the final compact and recurrent nested LODO signatures in the independent PBMC scRNA-seq dataset. ",
    "Panels show broad cell-type annotation, signature module scores and signature gene expression across cell-type compartments. ",
    "Both signatures showed their highest module scores in ", scrna_top_final, " cells, supporting predominant monocyte/myeloid localization of the bulk host-response signal. ",
    "The scRNA-seq analysis was used for biological localization only and was not considered independent diagnostic validation."
  ),
  "",
  "## Supplementary materials",
  "",
  paste0(
    "Supplementary materials should provide transparent reporting of dataset eligibility, preprocessing, candidate gene selection, nested-fold selected genes, RNA-seq eligibility screening, decision-curve analysis and scRNA annotation. ",
    "The RNA-seq screening supplement should explicitly report the final decision: ", rnaseq_status, "."
  )
)

# ============================================================
# Audit
# ============================================================

forbidden_patterns <- c(
  "ready for clinical deployment",
  "clinically ready",
  "proven clinical utility",
  "validated fixed threshold",
  "externally validated fixed threshold",
  "same-intended-use clinical validation",
  "single-cell.*validated diagnostic",
  "scRNA.*validated diagnostic",
  "robust diagnostic tool",
  "clinical utility is established",
  "definitive diagnostic model"
)

all_caption_text <- paste(caption_lines, collapse = "\n")

audit_items <- data.frame(
  audit_id = c(
    "main_tables_defined",
    "main_figures_defined",
    "supplementary_materials_defined",
    "table2_nested_LODO_boundary",
    "table3_threshold_boundary",
    "table4_scRNA_boundary",
    "RNAseq_NoGo_reported",
    "forbidden_claim_absent"
  ),
  expected = c(
    "4 main tables",
    "5 main figures",
    ">=8 supplementary items",
    "Held-out did not participate in feature selection/modeling/threshold selection",
    "Local thresholds descriptive, fixed threshold not transportable",
    "scRNA localization only, not diagnostic validation",
    "RNA-seq No-Go status reported",
    "No forbidden high-risk claim phrases"
  ),
  observed = c(
    nrow(main_table_plan),
    nrow(main_figure_plan),
    nrow(supplementary_plan),
    grepl("held-out dataset did not participate", all_caption_text, ignore.case = TRUE),
    grepl("not transportable", all_caption_text, ignore.case = TRUE),
    grepl("not.*diagnostic validation", all_caption_text, ignore.case = TRUE),
    grepl("NO_GO_no_eligible_RNAseq_dataset_detected", all_caption_text, fixed = TRUE),
    !any(contains_forbidden(all_caption_text, forbidden_patterns))
  ),
  status = c(
    ifelse(nrow(main_table_plan) == 4, "PASS", "CHECK"),
    ifelse(nrow(main_figure_plan) == 5, "PASS", "CHECK"),
    ifelse(nrow(supplementary_plan) >= 8, "PASS", "CHECK"),
    ifelse(grepl("held-out dataset did not participate", all_caption_text, ignore.case = TRUE), "PASS", "CHECK"),
    ifelse(grepl("not transportable", all_caption_text, ignore.case = TRUE), "PASS", "CHECK"),
    ifelse(grepl("not.*diagnostic validation", all_caption_text, ignore.case = TRUE), "PASS", "CHECK"),
    ifelse(grepl("NO_GO_no_eligible_RNAseq_dataset_detected", all_caption_text, fixed = TRUE), "PASS", "CHECK"),
    ifelse(!any(contains_forbidden(all_caption_text, forbidden_patterns)), "PASS", "CHECK")
  ),
  stringsAsFactors = FALSE
)

overall_status <- data.frame(
  metric = c(
    "n_main_tables",
    "n_main_figures",
    "n_supplementary_items",
    "n_audit_checks_failed",
    "ready_for_next_step"
  ),
  value = c(
    nrow(main_table_plan),
    nrow(main_figure_plan),
    nrow(supplementary_plan),
    sum(audit_items$status != "PASS"),
    ifelse(sum(audit_items$status != "PASS") == 0, "YES", "CHECK_BEFORE_NEXT_STEP")
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 输出
# ============================================================

message("Writing table and figure caption outputs...")

caption_path <- file.path(draft_dir, "BMC_Genomics_table_figure_captions_v0.6.md")
main_table_path <- file.path(draft_dir, "BMC_Genomics_main_table_plan_v0.6.csv")
main_figure_path <- file.path(draft_dir, "BMC_Genomics_main_figure_plan_v0.6.csv")
supplement_path <- file.path(draft_dir, "BMC_Genomics_supplementary_material_plan_v0.6.csv")
audit_path <- file.path(draft_dir, "BMC_Genomics_table_figure_audit_v0.6.csv")
index_path <- file.path(draft_dir, "BMC_Genomics_table_figure_caption_index_v0.6.xlsx")

write_text_file(caption_lines, caption_path)
data.table::fwrite(main_table_plan, main_table_path)
data.table::fwrite(main_figure_plan, main_figure_path)
data.table::fwrite(supplementary_plan, supplement_path)
data.table::fwrite(audit_items, audit_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_table_figure_captions_v0.6.md",
    "BMC_Genomics_main_table_plan_v0.6.csv",
    "BMC_Genomics_main_figure_plan_v0.6.csv",
    "BMC_Genomics_supplementary_material_plan_v0.6.csv",
    "BMC_Genomics_table_figure_audit_v0.6.csv",
    "BMC_Genomics_table_figure_caption_index_v0.6.xlsx"
  ),
  path = c(
    caption_path,
    main_table_path,
    main_figure_path,
    supplement_path,
    audit_path,
    index_path
  ),
  purpose = c(
    "Main table and figure captions",
    "Main table plan",
    "Main figure plan",
    "Supplementary table and figure plan",
    "Figure/table claim audit",
    "Workbook index"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "main_tables")
openxlsx::writeData(wb, "main_tables", main_table_plan)

openxlsx::addWorksheet(wb, "main_figures")
openxlsx::writeData(wb, "main_figures", main_figure_plan)

openxlsx::addWorksheet(wb, "supplementary")
openxlsx::writeData(wb, "supplementary", supplementary_plan)

openxlsx::addWorksheet(wb, "audit")
openxlsx::writeData(wb, "audit", audit_items)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

openxlsx::addWorksheet(wb, "copy_manifest")
openxlsx::writeData(wb, "copy_manifest", copy_manifest)

openxlsx::addWorksheet(wb, "source_index")
openxlsx::writeData(wb, "source_index", source_index)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_29_BMC_Genomics_table_figure_caption_draft.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 29 BMC Genomics table and figure caption draft 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nMain table plan:")
print(main_table_plan)

message("\nMain figure plan:")
print(main_figure_plan)

message("\nSupplementary material plan:")
print(supplementary_plan)

message("\nAudit:")
print(audit_items)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", caption_path)
message("2) ", main_table_path)
message("3) ", main_figure_path)
message("4) ", supplement_path)
message("5) ", audit_path)
message("6) ", index_path)

message("\n下一步：")
message("把 Overall status、Main figure plan、Audit 贴给我。")
message("如果 ready_for_next_step = YES，就继续 30_BMC_Genomics_submission_package_check.R。")