# ============================================================
# 24_BMC_Genomics_background_discussion_draft.R
# Generate BMC Genomics Background + Discussion draft v0.3
#
# 目的：
# 1. 基于 final_freeze 结果生成 Background 和 Discussion 英文初稿
# 2. 不新增分析，不重新计算模型
# 3. 所有结果数字只来自 04_results/final_freeze/
# 4. 生成 citation placeholders，后续人工替换为正式参考文献
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_background_v0.3.md
#   BMC_Genomics_discussion_v0.3.md
#   BMC_Genomics_background_discussion_combined_v0.3.md
#   BMC_Genomics_citation_placeholder_list_v0.3.csv
#   BMC_Genomics_background_discussion_writer_notes_v0.3.md
#   BMC_Genomics_background_discussion_v0.3_index.xlsx
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
framework_dir <- file.path(manuscript_dir, "BMC_Genomics_framework")
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
  T20_table2 = file.path(freeze_tables_dir, "T20_table2_T20_table2_nested_LODO_validation.csv"),
  T20_table3 = file.path(freeze_tables_dir, "T20_table3_T20_table3_calibration_threshold_transport.csv"),
  T20_table4 = file.path(freeze_tables_dir, "T20_table4_T20_table4_scRNA_localization.csv"),
  T21_risk = file.path(freeze_tables_dir, "T21_risk_flags_T21_manuscript_risk_flags.csv"),
  T21_claims = file.path(freeze_tables_dir, "T21_claims_T21_recommended_claims_and_restrictions.csv"),
  T16b_go = file.path(freeze_tables_dir, "T16b_go_no_go_T16b_RNAseq_go_no_go_summary.csv"),
  claim_control = file.path(framework_dir, "BMC_Genomics_claim_control_sheet_v0.1.md"),
  writer_brief = file.path(framework_dir, "BMC_Genomics_writer_brief_v0.1.md")
)

missing <- names(files)[!file.exists(unlist(files))]

if (length(missing) > 0) {
  stop(
    "缺少输入文件：\n",
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

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

# ============================================================
# 读取 frozen data
# ============================================================

message("Reading frozen materials...")

key_snapshot <- read_csv_df(files$key_snapshot)
decision_log <- read_csv_df(files$decision_log)
source_index <- read_csv_df(files$source_index)
table2 <- read_csv_df(files$T20_table2)
table3 <- read_csv_df(files$T20_table3)
table4 <- read_csv_df(files$T20_table4)
risk_flags <- read_csv_df(files$T21_risk)
claims <- read_csv_df(files$T21_claims)
rna_go <- read_csv_df(files$T16b_go)

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

poor_dataset <- paste(table2$validation_dataset[table2$AUROC < 0.70], collapse = ", ")
if (poor_dataset == "") poor_dataset <- "none"

low_fixed_sens_datasets <- paste(table3$validation_dataset[table3$fixed_sensitivity < 0.20], collapse = ", ")
if (low_fixed_sens_datasets == "") low_fixed_sens_datasets <- "none"

low_fixed_spec_datasets <- paste(table3$validation_dataset[table3$fixed_specificity < 0.20], collapse = ", ")
if (low_fixed_spec_datasets == "") low_fixed_spec_datasets <- "none"

# ============================================================
# Citation placeholder list
# ============================================================

citation_placeholders <- data.frame(
  placeholder = c(
    "[REF_SEPSIS_DEFINITION]",
    "[REF_SEPSIS_HOST_RESPONSE]",
    "[REF_SEPSIS_TRANSCRIPTOMIC_SIGNATURES]",
    "[REF_SEPSIS_SINGLE_CELL]",
    "[REF_PREDICTION_MODEL_REPORTING]",
    "[REF_TRIPOD_AI]",
    "[REF_CALIBRATION_PREDICTION_MODELS]",
    "[REF_DECISION_CURVE_ANALYSIS]",
    "[REF_SINGLE_CELL_MODULE_SCORE]",
    "[REF_BMC_GENOMICS_SCOPE]",
    "[REF_GEO_DATASETS]"
  ),
  needed_for = c(
    "Definition and clinical importance of sepsis.",
    "Host immune dysregulation and blood transcriptomic host response.",
    "Existing sepsis transcriptomic diagnostic signatures and biomarker studies.",
    "Existing sepsis single-cell immune landscape studies.",
    "General prediction model reporting and validation principles.",
    "TRIPOD+AI or related reporting guidance.",
    "Calibration, Brier score and threshold transportability discussion.",
    "Decision curve analysis interpretation.",
    "Seurat or module score method reference.",
    "Journal fit and manuscript framing if needed, normally not cited in manuscript.",
    "Original GEO dataset source papers."
  ),
  suggested_source_type = c(
    "Sepsis-3 / WHO / major review",
    "Review or high-quality sepsis immunology paper",
    "Recent systematic or representative studies",
    "Representative scRNA-seq sepsis studies",
    "TRIPOD / PROBAST / methodological article",
    "TRIPOD+AI statement",
    "Prediction model calibration methodological article",
    "Vickers decision curve analysis article",
    "Seurat / AddModuleScore reference",
    "Journal information only, not manuscript reference",
    "Original dataset publications and GEO records"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Background draft
# ============================================================

background <- c(
  "# Background",
  "",
  "Sepsis is a life-threatening syndrome caused by a dysregulated host response to infection and remains a major cause of morbidity and mortality worldwide [REF_SEPSIS_DEFINITION]. Early recognition is clinically important, yet the biological and clinical heterogeneity of sepsis complicates diagnosis, risk stratification and translation of molecular biomarkers into practice. Blood-based host-response profiling has therefore been widely explored as a strategy to capture systemic immune activation, immune suppression and other transcriptomic programs associated with sepsis [REF_SEPSIS_HOST_RESPONSE].",
  "",
  "Whole-blood transcriptomic studies have reported numerous sepsis-associated genes, pathways and diagnostic signatures [REF_SEPSIS_TRANSCRIPTOMIC_SIGNATURES]. These studies have contributed to the understanding of host-response biology and have generated candidate biomarkers for distinguishing sepsis from control states. However, many transcriptomic signatures are developed or optimized within a limited set of cohorts, and reported performance often emphasizes discrimination metrics such as the area under the receiver operating characteristic curve. Discrimination alone does not establish whether predicted probabilities, operating thresholds or decision behavior remain stable when a signature is transported to independent cohorts with different platforms, sampling times, disease spectra and control definitions.",
  "",
  "Transportability is particularly important for sepsis transcriptomic signatures because public cohorts frequently differ in patient composition, sepsis severity, control type and microarray or sequencing platform. A signature may rank cases and controls reasonably well in a held-out cohort while still showing poor calibration or failure of a fixed operating threshold. Such behavior is directly relevant to diagnostic translation, because clinical use requires not only a ranking score but also a probability scale or operating point that remains interpretable in the target setting [REF_CALIBRATION_PREDICTION_MODELS]. Transparent reporting of model development, validation design and performance heterogeneity is also central to current prediction model reporting guidance [REF_PREDICTION_MODEL_REPORTING; REF_TRIPOD_AI].",
  "",
  "Another limitation of bulk blood transcriptomic signatures is that they aggregate signals from multiple immune and blood cell populations. Sepsis is characterized by coordinated changes across innate and adaptive immune compartments, and single-cell RNA sequencing can help localize bulk-derived host-response signals to broad cellular sources [REF_SEPSIS_SINGLE_CELL]. Such localization can support biological interpretation, but it should be distinguished from independent diagnostic validation because single-cell datasets often differ in sample type, cell capture, donor structure and clinical design.",
  "",
  "In this study, we evaluated the cross-cohort transportability of a blood transcriptomic host-response signature for sepsis using a strict nested leave-one-dataset-out framework across public whole-blood cohorts. In each fold, the held-out dataset was excluded from feature screening, redundancy reduction, standardization, model fitting, tuning and threshold selection. We assessed discrimination, calibration, fixed-threshold transportability and exploratory decision-curve behavior, and we used an independent PBMC single-cell RNA-seq dataset to localize the resulting host-response signal. We framed the analysis as a transportability evaluation rather than same-intended-use clinical validation."
)

# ============================================================
# Discussion draft
# ============================================================

discussion <- c(
  "# Discussion",
  "",
  "## Principal findings",
  "",
  paste0(
    "This study evaluated a blood transcriptomic host-response signature for sepsis under a strict nested cross-cohort design and complemented the bulk analysis with single-cell biological localization. ",
    "Across ",
    n_bulk_datasets,
    " public whole-blood cohorts comprising ",
    bulk_total,
    " samples, the signature showed recurrent but non-uniform discrimination. ",
    "The median held-out AUROC was ",
    nested_median_auc,
    ", and ",
    nested_auc_ge_080,
    " of six datasets achieved AUROC >=0.80. ",
    "However, pooled held-out performance was weaker, with a pooled AUROC of ",
    pooled_auc,
    " and Brier score of ",
    pooled_brier,
    ". ",
    "Calibration and fixed-threshold behavior were unstable, with a median absolute threshold shift of ",
    median_thr_shift,
    " and a maximum shift of ",
    max_thr_shift,
    ". ",
    "At the training-derived fixed thresholds, sensitivity was below 0.20 in ",
    fixed_sens_fail,
    " datasets and specificity was below 0.20 in ",
    fixed_spec_fail,
    " dataset. ",
    "Single-cell analysis localized both final and recurrent signatures predominantly to ",
    scrna_top_final,
    " compartments."
  ),
  "",
  "These findings support two linked conclusions. First, a blood transcriptomic host-response signal for sepsis can show recurrent discrimination across multiple independent cohorts when evaluated under a leakage-resistant nested design. Second, discrimination does not guarantee transportability of the probability scale or a fixed operating threshold. The latter point is central for diagnostic translation, because a biomarker or model that ranks patients well may still fail when a fixed decision threshold is applied in a new cohort.",
  "",
  "## Relation to previous transcriptomic sepsis studies",
  "",
  "Numerous prior studies have proposed sepsis transcriptomic signatures using differential expression, pathway analysis and machine learning [REF_SEPSIS_TRANSCRIPTOMIC_SIGNATURES]. Many such studies report high AUROC values in internal or external validation sets. The present study differs in its primary emphasis. We did not frame the work as discovery of a definitive new clinical diagnostic model. Instead, we focused on whether a host-response signature remains transportable when all feature selection, redundancy reduction, scaling, tuning and threshold selection are repeated within training-only folds. This distinction is important because feature selection outside the validation loop can inflate apparent performance and obscure the extent to which a signature generalizes to fully held-out cohorts.",
  "",
  paste0(
    "The results also show why failure scenarios should be retained rather than removed. ",
    poor_dataset,
    " showed poor held-out discrimination and was interpreted as a transportability failure scenario. ",
    "In contrast, some datasets showed excellent or perfect discrimination, likely reflecting strong sepsis or septic-shock versus healthy-control contrasts. ",
    "The coexistence of strong and weak transport behavior across cohorts suggests that public transcriptomic performance estimates should be interpreted in relation to cohort context, platform, sampling design and control definition."
  ),
  "",
  "## Methodological implications",
  "",
  "A major methodological feature of this study is the strict nested LODO design. In each outer fold, the held-out dataset did not contribute to gene screening, correlation-based redundancy reduction, training-set standardization, model fitting, hyperparameter selection or threshold selection. This design is more conservative than workflows in which genes are selected using all datasets before external validation. It better reflects the question of whether a complete modeling procedure can be transported to a new dataset.",
  "",
  "The divergence between per-dataset discrimination and pooled transport behavior also has implications for reporting. The median per-dataset AUROC was high, but the pooled AUROC was substantially lower. This gap suggests that cohort-specific score scales and between-cohort shifts affected the aggregate ranking of all held-out predictions. Reporting only a median or best-case AUROC would therefore provide an incomplete picture of model behavior. For molecular diagnostic signatures, AUROC should be accompanied by calibration, Brier score, threshold-dependent metrics and a clear description of cohort context.",
  "",
  paste0(
    "Fixed-threshold instability was one of the most important findings. ",
    "The fixed training-derived threshold led to very low sensitivity in ",
    low_fixed_sens_datasets,
    " and very low specificity in ",
    low_fixed_spec_datasets,
    ". ",
    "This indicates that a single operating threshold selected in one training context may not be directly transferable to a new cohort, even when discrimination remains acceptable or excellent. ",
    "For deployment-oriented studies, this finding would argue for prospective threshold calibration or setting-specific recalibration before clinical use."
  ),
  "",
  "Exploratory decision-curve analysis showed heterogeneous net-benefit patterns. Because decision-curve results depend on both calibration and threshold probability, their interpretation is limited when probability scales are unstable and cohort contrasts differ. We therefore treated decision-curve analysis as descriptive rather than as evidence that the signature has established clinical utility [REF_DECISION_CURVE_ANALYSIS].",
  "",
  "## Biological interpretation",
  "",
  "The differential expression and enrichment results supported a host-response pattern characterized by myeloid leukocyte activation, response to bacterium, hemostasis or coagulation-related processes, and relative downregulation of adaptive immune and T cell-related processes. This pattern is compatible with known features of sepsis immunobiology, including innate immune activation and adaptive immune dysfunction [REF_SEPSIS_HOST_RESPONSE].",
  "",
  paste0(
    "The single-cell analysis provided an additional level of biological interpretation. ",
    "Across ",
    scrna_cells,
    " cells and ",
    scrna_clusters,
    " clusters, all final and recurrent signature genes were detectable, and both module scores were highest in ",
    scrna_top_final,
    " compartments. ",
    "This supports the interpretation that the transported bulk signal is predominantly driven by monocyte/myeloid transcriptional programs. ",
    "However, this analysis should be interpreted as cellular localization rather than diagnostic validation. ",
    "The single-cell dataset differed from the bulk cohorts in sample type and study design, and module scores summarize gene-program activity at the cell level rather than patient-level diagnostic accuracy [REF_SINGLE_CELL_MODULE_SCORE]."
  ),
  "",
  "## Clinical and translational implications",
  "",
  "The study does not support immediate clinical deployment of the signature. The main translational implication is more specific: transcriptomic host-response signatures may show recurrent cross-cohort discrimination, but their probability scale and operating threshold require explicit external evaluation. This is especially relevant for sepsis, where intended-use populations often include critically ill noninfectious controls, SIRS, postoperative inflammation, trauma, shock or other clinical mimics. Most evaluated contrasts in this study involved healthy or selected healthy controls, so clinical mimic discrimination remains insufficiently established.",
  "",
  "The RNA-seq screening result also has a practical implication. We attempted to identify an eligible local RNA-seq dataset for external platform validation, but no eligible dataset was detected after screening candidate files for platform type, sample size, sepsis-control contrast and signature-gene availability. This No-Go decision was frozen and recorded rather than replaced by a weaker or ambiguous validation. Future studies should evaluate the signature in prospectively defined RNA-seq or targeted transcriptomic datasets with clinically matched controls.",
  "",
  "## Strengths",
  "",
  "This study has several strengths. First, it used a strict nested LODO design that excluded the held-out dataset from all feature selection and model-construction steps. Second, it reported not only discrimination but also calibration, Brier score, threshold transportability and exploratory net benefit. Third, it retained and discussed a failure cohort rather than removing it post hoc. Fourth, it separated biological localization from diagnostic validation in the single-cell analysis. Fifth, it froze final results before manuscript drafting, including the RNA-seq No-Go decision and the allowed versus restricted claims.",
  "",
  "## Limitations",
  "",
  "This study also has important limitations. First, it was a secondary analysis of archived public datasets, and raw-level reprocessing was not performed. Differences in platform, preprocessing, annotation and cohort design may have contributed to probability-scale instability. Second, most contrasts involved healthy or selected healthy controls. This limits inference about clinically difficult sepsis diagnosis against noninfectious inflammatory mimics. Third, the number of independent bulk cohorts was limited, and one held-out cohort showed poor transportability. Fourth, RNA-seq external platform validation could not be performed because no eligible local RNA-seq dataset was identified. Fifth, the single-cell analysis used an independent PBMC dataset for localization, but it was not designed to validate patient-level diagnostic performance. Sixth, the signature should not be interpreted as a fixed-threshold clinical test without further setting-specific validation and recalibration.",
  "",
  "## Conclusions",
  "",
  "A blood transcriptomic host-response signature showed recurrent discrimination across several held-out sepsis cohorts under strict nested cross-cohort evaluation. However, calibration and fixed-threshold transportability were unstable, and one cohort represented a clear failure scenario. Independent single-cell analysis localized the signal predominantly to monocyte/myeloid compartments. These findings support further evaluation of transcriptomic host-response signatures in clinically matched, prospectively designed validation studies before deployment-oriented use."
)

# ============================================================
# Writer notes
# ============================================================

writer_notes <- c(
  "# Writer notes for Background + Discussion v0.3",
  "",
  "## What this draft does",
  "",
  "- Provides BMC Genomics-oriented Background and Discussion.",
  "- Frames the study as transportability evaluation rather than ordinary biomarker discovery.",
  "- Explicitly separates discrimination from calibration and threshold transportability.",
  "- Keeps scRNA as biological localization.",
  "- Keeps RNA-seq as screened but No-Go.",
  "",
  "## Citation placeholders to replace",
  "",
  paste0("- ", citation_placeholders$placeholder, ": ", citation_placeholders$needed_for),
  "",
  "## Must preserve",
  "",
  "- Strict nested LODO design.",
  "- Held-out dataset excluded from feature selection, scaling, tuning and threshold selection.",
  "- GSE54514 retained as failure scenario.",
  "- Fixed-threshold transportability not supported.",
  "- scRNA is not diagnostic validation.",
  "- RNA-seq validation not performed.",
  "",
  "## Avoid",
  "",
  "- 'Clinically ready'",
  "- 'Robust diagnostic tool'",
  "- 'Validated fixed threshold'",
  "- 'Same-intended-use validation'",
  "- 'Single-cell validation of diagnostic model'",
  "- 'Proven clinical utility'",
  "",
  "## Next manual editing tasks",
  "",
  "1. Add formal citations for all placeholders.",
  "2. Decide whether Discussion should be shortened after combining full manuscript.",
  "3. Align terminology: signature, host-response signature, nested LODO, transportability.",
  "4. Remove repeated phrases after combining with Methods and Results.",
  "5. Check journal word count and figure/table count after full manuscript assembly."
)

# ============================================================
# 保存输出
# ============================================================

message("Writing Background and Discussion draft files...")

background_path <- file.path(draft_dir, "BMC_Genomics_background_v0.3.md")
discussion_path <- file.path(draft_dir, "BMC_Genomics_discussion_v0.3.md")
combined_path <- file.path(draft_dir, "BMC_Genomics_background_discussion_combined_v0.3.md")
citation_path <- file.path(draft_dir, "BMC_Genomics_citation_placeholder_list_v0.3.csv")
notes_path <- file.path(draft_dir, "BMC_Genomics_background_discussion_writer_notes_v0.3.md")

write_text_file(background, background_path)
write_text_file(discussion, discussion_path)
write_text_file(c(background, "", discussion), combined_path)
write_text_file(writer_notes, notes_path)

data.table::fwrite(citation_placeholders, citation_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_background_v0.3.md",
    "BMC_Genomics_discussion_v0.3.md",
    "BMC_Genomics_background_discussion_combined_v0.3.md",
    "BMC_Genomics_citation_placeholder_list_v0.3.csv",
    "BMC_Genomics_background_discussion_writer_notes_v0.3.md"
  ),
  path = c(
    background_path,
    discussion_path,
    combined_path,
    citation_path,
    notes_path
  ),
  purpose = c(
    "Background draft for BMC Genomics",
    "Discussion draft for BMC Genomics",
    "Combined Background + Discussion draft",
    "Citation placeholder list for literature insertion",
    "Manual editing notes"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "citation_placeholders")
openxlsx::writeData(wb, "citation_placeholders", citation_placeholders)

openxlsx::addWorksheet(wb, "key_snapshot")
openxlsx::writeData(wb, "key_snapshot", key_snapshot)

openxlsx::addWorksheet(wb, "risk_flags")
openxlsx::writeData(wb, "risk_flags", risk_flags)

openxlsx::addWorksheet(wb, "claims")
openxlsx::writeData(wb, "claims", claims)

openxlsx::saveWorkbook(
  wb,
  file.path(draft_dir, "BMC_Genomics_background_discussion_v0.3_index.xlsx"),
  overwrite = TRUE
)

# sessionInfo
sink(file.path(log_dir, "sessionInfo_24_BMC_Genomics_background_discussion_draft.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 24 BMC Genomics Background + Discussion draft 完成 ============")
message("输出目录：", draft_dir)

message("\nGenerated files:")
print(output_index)

message("\nBackground preview:")
cat(paste(head(background, 25), collapse = "\n"))
cat("\n...\n")

message("\nDiscussion preview:")
cat(paste(head(discussion, 35), collapse = "\n"))
cat("\n...\n")

message("\nCitation placeholders:")
print(citation_placeholders)

message("\n关键输出：")
message("1) ", background_path)
message("2) ", discussion_path)
message("3) ", combined_path)
message("4) ", citation_path)
message("5) ", notes_path)
message("6) ", file.path(draft_dir, "BMC_Genomics_background_discussion_v0.3_index.xlsx"))

message("\n下一步：")
message("打开 BMC_Genomics_background_discussion_combined_v0.3.md 粗读。")
message("如果顺利，下一步生成 25_BMC_Genomics_full_manuscript_assembly.R，把 Title/Abstract/Methods/Results/Discussion/Declarations 组装成完整 markdown 初稿。")