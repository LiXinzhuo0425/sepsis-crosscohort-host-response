# ============================================================
# 22_prepare_BMC_Genomics_manuscript_framework.R
# BMC Genomics manuscript framework preparation
#
# 目的：
# 1. 基于 final_freeze 结果生成 BMC Genomics 写作框架
# 2. 生成题目页、摘要骨架、正文结构、图表映射、claim control sheet
# 3. 不写完整正文，不新增分析
# 4. 后续 manuscript_v0.3 只从 final_freeze 读取数字
#
# 输出目录：
# 07_manuscript/BMC_Genomics_framework/
#
# 输出文件：
#   BMC_Genomics_title_page_v0.1.md
#   BMC_Genomics_abstract_skeleton_v0.1.md
#   BMC_Genomics_section_outline_v0.1.md
#   BMC_Genomics_figure_table_mapping_v0.1.md
#   BMC_Genomics_claim_control_sheet_v0.1.md
#   BMC_Genomics_declarations_skeleton_v0.1.md
#   BMC_Genomics_writer_brief_v0.1.md
#   BMC_Genomics_framework_index.xlsx
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
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(framework_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

files <- list(
  key_snapshot = file.path(freeze_dir, "final_key_result_snapshot.csv"),
  decision_log = file.path(freeze_dir, "final_freeze_decision_log.csv"),
  source_index = file.path(freeze_dir, "manuscript_source_index.csv"),
  freeze_summary = file.path(freeze_dir, "final_freeze_summary.md"),
  figure_inventory = file.path(freeze_dir, "final_freeze_figure_inventory.csv"),
  copy_manifest = file.path(freeze_dir, "final_freeze_file_copy_manifest.csv")
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

fmt_num <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("NA")
  formatC(x, format = "f", digits = digits)
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

# ============================================================
# 读取 frozen data
# ============================================================

message("Reading final freeze materials...")

key_snapshot <- read_csv_df(files$key_snapshot)
decision_log <- read_csv_df(files$decision_log)
source_index <- read_csv_df(files$source_index)
figure_inventory <- read_csv_df(files$figure_inventory)
copy_manifest <- read_csv_df(files$copy_manifest)

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
bulk_sepsis <- get_value(key_snapshot, "bulk_sepsis")
bulk_control <- get_value(key_snapshot, "bulk_control")
n_bulk_datasets <- get_value(key_snapshot, "n_bulk_datasets")
nested_median_auc <- fmt_num(get_value(key_snapshot, "nested_LODO_median_AUROC"), 3)
nested_min_auc <- fmt_num(get_value(key_snapshot, "nested_LODO_min_AUROC"), 3)
nested_auc_ge_080 <- get_value(key_snapshot, "nested_LODO_AUROC_ge_0.80_datasets")
pooled_auc <- fmt_num(get_value(key_snapshot, "pooled_nested_LODO_AUROC"), 3)
pooled_brier <- fmt_num(get_value(key_snapshot, "pooled_nested_LODO_Brier"), 3)
median_thr_shift <- fmt_num(get_value(key_snapshot, "median_abs_threshold_shift"), 3)
max_thr_shift <- fmt_num(get_value(key_snapshot, "max_abs_threshold_shift"), 3)
fixed_sens_fail <- get_value(key_snapshot, "fixed_sensitivity_lt_0.20_datasets")
fixed_spec_fail <- get_value(key_snapshot, "fixed_specificity_lt_0.20_datasets")
all_fold_genes <- get_value(key_snapshot, "genes_selected_all_nested_folds")
rnaseq_status <- get_value(key_snapshot, "RNAseq_validation_status")
scrna_cells <- get_value(key_snapshot, "scRNA_cells")
scrna_clusters <- get_value(key_snapshot, "scRNA_clusters")
scrna_top_final <- get_value(key_snapshot, "scRNA_top_Final10_celltype")
scrna_top_nested <- get_value(key_snapshot, "scRNA_top_NestedRecurrent_celltype")

# ============================================================
# Title page
# ============================================================

title_options <- data.frame(
  option = c("Recommended", "Defensive", "Biological emphasis", "Methodological emphasis"),
  title = c(
    "Nested cross-cohort evaluation and single-cell localization of a blood transcriptomic host-response signature for sepsis",
    "Transportability and threshold instability of a blood transcriptomic host-response signature for sepsis across public cohorts",
    "Single-cell localization and cross-cohort evaluation of a myeloid host-response transcriptomic signature in sepsis",
    "Strict nested leave-one-dataset-out evaluation of a blood transcriptomic signature for sepsis diagnosis"
  ),
  rationale = c(
    "Balances nested validation and single-cell localization; best fit for BMC Genomics.",
    "Emphasizes the central transportability finding and avoids overclaiming clinical validation.",
    "Highlights biological interpretation but should not overstate mechanism.",
    "Strong methods framing, but less biologically attractive."
  ),
  stringsAsFactors = FALSE
)

recommended_title <- title_options$title[title_options$option == "Recommended"]

keywords <- c(
  "Sepsis",
  "Transcriptomics",
  "Host response",
  "Nested validation",
  "Transportability",
  "Calibration",
  "Single-cell RNA sequencing",
  "Monocyte",
  "Diagnostic signature"
)

title_page <- c(
  "# BMC Genomics title page v0.1",
  "",
  "## Recommended title",
  "",
  recommended_title,
  "",
  "## Alternative titles",
  "",
  paste0(
    seq_len(nrow(title_options)), ". ",
    title_options$title,
    "\n   - Rationale: ",
    title_options$rationale
  ),
  "",
  "## Article type",
  "",
  "Research article",
  "",
  "## Target journal",
  "",
  "BMC Genomics",
  "",
  "## Running title",
  "",
  "Transcriptomic sepsis signature transportability",
  "",
  "## Keywords",
  "",
  paste(keywords, collapse = "; "),
  "",
  "## Author information placeholder",
  "",
  "- First author: [Fill in]",
  "- Corresponding author: [Fill in]",
  "- Affiliations: [Fill in]",
  "- ORCID: [Fill in if available]",
  "",
  "## Study identity locked for writing",
  "",
  "Cross-cohort transportability evaluation of a blood transcriptomic host-response signature for sepsis with independent PBMC single-cell biological localization.",
  "",
  "## Claims boundary",
  "",
  "- Do not claim clinical deployment readiness.",
  "- Do not claim same-intended-use external validation.",
  "- Do not claim fixed-threshold transportability.",
  "- Do not treat scRNA module score analysis as diagnostic validation.",
  "- Do not state that RNA-seq validation was performed."
)

# ============================================================
# Abstract skeleton
# ============================================================

abstract_skeleton <- c(
  "# BMC Genomics structured abstract skeleton v0.1",
  "",
  "> BMC Genomics Research article abstract should use Background, Results and Conclusions. Target length: <=350 words. No references in abstract.",
  "",
  "## Background",
  "",
  paste0(
    "Sepsis diagnosis remains challenging because host-response biomarkers derived in one transcriptomic cohort may not transport reliably across cohorts, platforms and control definitions. ",
    "This study aimed to evaluate the cross-cohort transportability of a blood transcriptomic host-response signature for sepsis under strict nested leave-one-dataset-out validation and to localize the resulting signal in an independent single-cell RNA-seq dataset."
  ),
  "",
  "## Results",
  "",
  paste0(
    "The frozen bulk analysis included ",
    bulk_total, " whole-blood samples from ",
    n_bulk_datasets, " public cohorts, comprising ",
    bulk_sepsis, " sepsis cases and ",
    bulk_control, " controls. ",
    "Under strict nested leave-one-dataset-out evaluation, the median held-out AUROC was ",
    nested_median_auc,
    ", with a minimum AUROC of ",
    nested_min_auc,
    "; ",
    nested_auc_ge_080,
    " of six datasets achieved AUROC >=0.80. ",
    "However, pooled held-out performance was lower (AUROC ",
    pooled_auc,
    "; Brier score ",
    pooled_brier,
    "), and fixed-threshold behavior was unstable. ",
    "The median absolute threshold shift was ",
    median_thr_shift,
    ", with fixed-threshold sensitivity below 0.20 in ",
    fixed_sens_fail,
    " datasets and fixed-threshold specificity below 0.20 in ",
    fixed_spec_fail,
    " dataset. ",
    "Genes selected in all nested folds included ",
    all_fold_genes,
    ". ",
    "In an independent PBMC single-cell RNA-seq dataset, all final and recurrent signature genes were detectable; both final and recurrent signature module scores were highest in ",
    scrna_top_final,
    " compartments across ",
    scrna_cells,
    " cells and ",
    scrna_clusters,
    " clusters."
  ),
  "",
  "## Conclusions",
  "",
  paste0(
    "The blood transcriptomic host-response signature showed recurrent discrimination across several held-out cohorts, but calibration and fixed-threshold transportability were unstable. ",
    "Single-cell localization supported a predominantly monocyte/myeloid origin of the transported signal. ",
    "These findings support further evaluation of transcriptomic host-response signatures under clinically matched external validation designs before deployment-oriented use."
  ),
  "",
  "## Keywords",
  "",
  paste(keywords, collapse = "; ")
)

# ============================================================
# Section outline
# ============================================================

section_outline <- c(
  "# BMC Genomics section outline v0.1",
  "",
  "## Background",
  "",
  "### Paragraph 1: Clinical and molecular context",
  "- Sepsis is clinically heterogeneous.",
  "- Blood transcriptomics captures systemic host-response biology.",
  "- Diagnostic signatures often show optimistic performance in derivation-like settings.",
  "",
  "### Paragraph 2: Transportability gap",
  "- Cross-cohort transportability is challenged by platform, phenotype, timing and control-type differences.",
  "- AUROC alone does not establish probability-scale calibration or fixed-threshold transportability.",
  "- State the need for nested, leakage-resistant evaluation.",
  "",
  "### Paragraph 3: Biological localization gap",
  "- Bulk blood signatures mix signals across immune cell populations.",
  "- Single-cell RNA-seq can localize bulk-derived signatures to broad cellular compartments.",
  "- State that scRNA analysis is biological localization, not diagnostic validation.",
  "",
  "### Paragraph 4: Study objective",
  "- Evaluate a blood transcriptomic host-response signature using strict nested LODO.",
  "- Quantify discrimination, calibration, threshold transportability and exploratory net benefit.",
  "- Localize the signature in independent PBMC scRNA-seq data.",
  "",
  "## Results",
  "",
  "### 1. Cohort composition and analysis overview",
  paste0(
    "- Use frozen numbers: ",
    bulk_total, " samples, ",
    bulk_sepsis, " sepsis, ",
    bulk_control, " controls, ",
    n_bulk_datasets, " cohorts."
  ),
  "- Mention heterogeneous platforms and control definitions.",
  "- Mention RNA-seq screening No-Go briefly, with details in supplement.",
  "",
  "### 2. Differential expression and enrichment",
  "- Use previous DEG and GO outputs.",
  "- Emphasize myeloid activation, response to bacterium, hemostasis/coagulation and adaptive immune response changes.",
  "- Avoid turning this into a mechanistic proof.",
  "",
  "### 3. Strict nested LODO validation",
  paste0(
    "- Main frozen result: median AUROC ",
    nested_median_auc,
    "; minimum AUROC ",
    nested_min_auc,
    "; ",
    nested_auc_ge_080,
    "/6 datasets AUROC >=0.80."
  ),
  "- State held-out datasets did not participate in feature selection, scaling, tuning, or threshold selection.",
  "- Retain and discuss GSE54514 as a failure scenario.",
  "",
  "### 4. Calibration and fixed-threshold transportability",
  paste0(
    "- Pooled AUROC ",
    pooled_auc,
    ", Brier ",
    pooled_brier,
    "."
  ),
  paste0(
    "- Median absolute threshold shift ",
    median_thr_shift,
    "; maximum ",
    max_thr_shift,
    "."
  ),
  "- Explain that discrimination and fixed-threshold operation diverged.",
  "",
  "### 5. Exploratory DCA",
  "- Treat as exploratory.",
  "- Do not claim established clinical utility.",
  "- Link DCA heterogeneity to calibration and threshold instability.",
  "",
  "### 6. Single-cell localization",
  paste0(
    "- scRNA object: ",
    scrna_cells, " cells, ",
    scrna_clusters, " clusters."
  ),
  paste0(
    "- Final and recurrent signatures highest in ",
    scrna_top_final,
    "."
  ),
  "- State biological localization only.",
  "",
  "## Discussion",
  "",
  "### Main findings",
  "- Recurrent per-dataset discrimination.",
  "- Pooled and threshold instability.",
  "- Monocyte/myeloid localization.",
  "",
  "### Methodological implications",
  "- Feature selection and tuning must be nested.",
  "- Calibration and threshold transportability should be reported, not only AUROC.",
  "",
  "### Biological interpretation",
  "- Myeloid/innate immune origin.",
  "- Align with GO enrichment.",
  "- Avoid causal mechanism overclaim.",
  "",
  "### Clinical implications",
  "- Not ready for deployment.",
  "- Needs clinically matched external validation, especially clinical mimics.",
  "",
  "### Limitations",
  "- Healthy-control dominance.",
  "- Archived processed public data.",
  "- Cross-platform microarray heterogeneity.",
  "- No eligible RNA-seq external validation dataset detected.",
  "- scRNA localization is not diagnostic validation.",
  "- GSE54514 failure retained.",
  "",
  "### Conclusions",
  "- Discrimination can recur while calibration and thresholds fail.",
  "- Biological signal localized to monocyte/myeloid compartment.",
  "- Further clinically matched validation required.",
  "",
  "## Methods",
  "",
  "### Data sources and cohort eligibility",
  "### Bulk transcriptome preprocessing and gene-level harmonization",
  "### Differential expression and enrichment analysis",
  "### Candidate gene screening",
  "### Strict nested leave-one-dataset-out modeling",
  "### Performance metrics",
  "### Calibration and threshold transportability",
  "### Decision-curve analysis",
  "### RNA-seq external validation screening",
  "### Single-cell RNA-seq processing and module score analysis",
  "### Reproducibility and final result freeze"
)

# ============================================================
# Figure and table mapping
# ============================================================

figure_table_mapping <- data.frame(
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
    "Supplementary Table S1",
    "Supplementary Table S2",
    "Supplementary Table S3",
    "Supplementary Table S4",
    "Supplementary Table S5",
    "Supplementary Figure S1",
    "Supplementary Figure S2",
    "Supplementary Figure S3",
    "Supplementary Figure S4"
  ),
  title = c(
    "Study design and analysis workflow",
    "Strict nested leave-one-dataset-out validation performance",
    "Calibration and fixed-threshold transportability across held-out cohorts",
    "Functional enrichment of sepsis-associated transcriptomic alterations",
    "Single-cell localization of the host-response signature",
    "Cohort characteristics and control definitions",
    "Nested LODO validation performance",
    "Calibration and threshold transportability",
    "Single-cell localization of signature module scores",
    "Dataset eligibility and cohort-control context",
    "Differential expression and enrichment results",
    "Candidate gene screening and recurrence",
    "Nested LODO selected genes by fold",
    "RNA-seq eligibility screening and No-Go decision",
    "Bulk QC and PCA summaries",
    "Candidate selection plots",
    "Exploratory decision-curve analysis",
    "Canonical marker-based scRNA cluster annotation"
  ),
  source = c(
    "final_freeze decision log; manuscript source index",
    "T20 Table 2; T14 nested LODO outputs",
    "T20 Table 3; T17 calibration and threshold outputs",
    "T05 GO BP enrichment outputs",
    "T20 Table 4; T19 scRNA outputs",
    "T20 Table 1",
    "T20 Table 2",
    "T20 Table 3",
    "T20 Table 4",
    "T15b and T20 Table 1",
    "T05 and DEG outputs",
    "T06, T07, T14 gene frequency",
    "T14 selected genes by fold",
    "T16b RNA-seq eligibility outputs",
    "T03 QC outputs",
    "T06/T07 candidate-selection figures",
    "T17 DCA outputs",
    "T19 canonical marker outputs"
  ),
  manuscript_role = c(
    "Main",
    "Main",
    "Main",
    "Main or Supplement depending on space",
    "Main",
    "Main",
    "Main",
    "Main",
    "Main",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement",
    "Supplement"
  ),
  notes = c(
    "Should show bulk arm, RNA-seq No-Go arm, scRNA localization arm and final freeze.",
    "Must visually indicate held-out exclusion from feature selection and tuning.",
    "Central figure. Highlight AUROC versus threshold failure.",
    "Use concise top GO terms. Avoid over-mechanistic claims.",
    "Use UMAP, module score, violin/dotplot combination.",
    "Include control type and limitation column.",
    "Use rounded AUROC/AUPRC/Brier and threshold metrics.",
    "Focus on calibration-in-the-large, mean predicted, observed rate, threshold shift.",
    "Sort by Final10 score; Monocyte/Myeloid first.",
    "Useful for reviewers challenging healthy controls.",
    "Full DEG and GO outputs.",
    "Candidate screening transparency.",
    "No hiding fold-level variability.",
    "State attempted screening and No-Go.",
    "Quality control transparency.",
    "Optional.",
    "Exploratory only.",
    "Supports coarse annotation."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Claim control sheet
# ============================================================

claim_control <- data.frame(
  category = c(
    "Allowed",
    "Allowed",
    "Allowed",
    "Allowed with caution",
    "Allowed with caution",
    "Forbidden",
    "Forbidden",
    "Forbidden",
    "Forbidden",
    "Forbidden"
  ),
  statement = c(
    "The signature showed recurrent discrimination under strict nested LODO validation.",
    "Calibration and fixed-threshold transportability were unstable across cohorts.",
    "The signature was predominantly localized to monocyte/myeloid compartments in scRNA-seq.",
    "DCA suggested heterogeneous threshold-dependent net benefit.",
    "RNA-seq external validation was attempted through eligibility screening but not performed due to lack of eligible datasets.",
    "The model is ready for clinical deployment.",
    "The fixed threshold was externally validated.",
    "The study provides same-intended-use clinical validation.",
    "The scRNA dataset validated diagnostic performance.",
    "The signature has proven clinical utility."
  ),
  recommended_wording = c(
    "showed recurrent discrimination across several held-out cohorts",
    "showed unstable calibration and fixed-threshold behavior",
    "provided biological localization to monocyte/myeloid compartments",
    "was exploratory and heterogeneous across datasets",
    "no eligible local RNA-seq dataset was detected after screening",
    "Do not write.",
    "Do not write.",
    "Do not write.",
    "Do not write.",
    "Do not write."
  ),
  reason = c(
    "Supported by frozen nested LODO results.",
    "Supported by frozen threshold and calibration results.",
    "Supported by frozen scRNA module score analysis.",
    "Limited by calibration drift and healthy-control contrasts.",
    "Supported by 16b No-Go screening.",
    "Unsupported by available evidence.",
    "Contradicted by threshold drift results.",
    "Cohorts differ in intended use, control type, platform and population.",
    "scRNA is localization only.",
    "DCA and clinical validation are insufficient."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Declarations skeleton
# ============================================================

declarations <- c(
  "# BMC Genomics declarations skeleton v0.1",
  "",
  "## Ethics approval and consent to participate",
  "",
  "This study used publicly available, de-identified transcriptomic datasets and did not involve new recruitment of human participants or generation of new human subject data. Ethics approval and consent procedures for the original studies were described in the respective source datasets or publications. No additional institutional ethics approval was required for this secondary analysis of public data.",
  "",
  "## Consent for publication",
  "",
  "Not applicable.",
  "",
  "## Availability of data and materials",
  "",
  "All public datasets analyzed in this study are available from the Gene Expression Omnibus under the accession numbers listed in the manuscript. The processed analysis outputs, code and reproducibility materials will be made available at [repository link to be filled before submission].",
  "",
  "## Competing interests",
  "",
  "The authors declare that they have no competing interests.",
  "",
  "## Funding",
  "",
  "[Fill in according to actual funding status.]",
  "",
  "## Authors' contributions",
  "",
  "[Fill in author-specific contributions before submission.]",
  "",
  "## Acknowledgements",
  "",
  "[Fill in if applicable.]"
)

# ============================================================
# Writer brief
# ============================================================

writer_brief <- c(
  "# BMC Genomics writer brief v0.1",
  "",
  "## Journal requirements to follow",
  "",
  "- Article type: Research article.",
  "- Abstract sections: Background, Results, Conclusions.",
  "- Abstract length target: <=350 words.",
  "- Keywords: 3 to 10.",
  "- Include Availability of data and materials.",
  "- Use final_freeze as the only numeric source.",
  "",
  "## Frozen writing identity",
  "",
  "Nested cross-cohort evaluation and single-cell localization of a blood transcriptomic host-response signature for sepsis.",
  "",
  "## Core message",
  "",
  "A blood transcriptomic host-response signature showed recurrent discrimination across several held-out cohorts under strict nested evaluation, but calibration and fixed-threshold transportability were unstable; independent PBMC scRNA-seq localized the signal mainly to monocyte/myeloid compartments.",
  "",
  "## Tone",
  "",
  "- Defensive.",
  "- Transparent.",
  "- Methodologically rigorous.",
  "- No deployment overclaim.",
  "- No same-intended-use claim.",
  "",
  "## Numbers to use",
  "",
  paste0("- Bulk samples: ", bulk_total, "."),
  paste0("- Sepsis/control: ", bulk_sepsis, "/", bulk_control, "."),
  paste0("- Cohorts: ", n_bulk_datasets, "."),
  paste0("- Nested LODO median AUROC: ", nested_median_auc, "."),
  paste0("- Nested LODO minimum AUROC: ", nested_min_auc, "."),
  paste0("- Datasets with AUROC >=0.80: ", nested_auc_ge_080, "/6."),
  paste0("- Pooled nested LODO AUROC: ", pooled_auc, "."),
  paste0("- Pooled Brier score: ", pooled_brier, "."),
  paste0("- Median absolute threshold shift: ", median_thr_shift, "."),
  paste0("- Maximum absolute threshold shift: ", max_thr_shift, "."),
  paste0("- Fixed sensitivity <0.20 datasets: ", fixed_sens_fail, "."),
  paste0("- Fixed specificity <0.20 datasets: ", fixed_spec_fail, "."),
  paste0("- Genes selected in all nested folds: ", all_fold_genes, "."),
  paste0("- RNA-seq validation status: ", rnaseq_status, "."),
  paste0("- scRNA cells/clusters: ", scrna_cells, "/", scrna_clusters, "."),
  paste0("- Top scRNA compartment: ", scrna_top_final, "."),
  "",
  "## Must mention limitations",
  "",
  "- Healthy-control dominance.",
  "- GSE54514 transportability failure.",
  "- Calibration and threshold drift.",
  "- No eligible RNA-seq external validation dataset.",
  "- scRNA localization is not diagnostic validation.",
  "- Archived processed public data and platform heterogeneity.",
  "",
  "## Next writing step",
  "",
  "Generate manuscript_v0.3 section by section, starting with Background and Methods. Do not generate final Word before the markdown manuscript is stable."
)

# ============================================================
# Framework index table
# ============================================================

framework_index <- data.frame(
  file_name = c(
    "BMC_Genomics_title_page_v0.1.md",
    "BMC_Genomics_abstract_skeleton_v0.1.md",
    "BMC_Genomics_section_outline_v0.1.md",
    "BMC_Genomics_figure_table_mapping_v0.1.md",
    "BMC_Genomics_claim_control_sheet_v0.1.md",
    "BMC_Genomics_declarations_skeleton_v0.1.md",
    "BMC_Genomics_writer_brief_v0.1.md",
    "BMC_Genomics_framework_index.xlsx"
  ),
  purpose = c(
    "Title options, article identity, keywords and claim boundary.",
    "Structured abstract draft skeleton for BMC Genomics.",
    "Full section-level manuscript outline.",
    "Main and supplementary figure/table mapping.",
    "Allowed and forbidden claims.",
    "Declarations section placeholders.",
    "One-page writing brief with frozen numbers.",
    "Workbook index of all framework materials."
  ),
  output_path = file.path(framework_dir, c(
    "BMC_Genomics_title_page_v0.1.md",
    "BMC_Genomics_abstract_skeleton_v0.1.md",
    "BMC_Genomics_section_outline_v0.1.md",
    "BMC_Genomics_figure_table_mapping_v0.1.md",
    "BMC_Genomics_claim_control_sheet_v0.1.md",
    "BMC_Genomics_declarations_skeleton_v0.1.md",
    "BMC_Genomics_writer_brief_v0.1.md",
    "BMC_Genomics_framework_index.xlsx"
  )),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存 markdown
# ============================================================

message("Writing BMC Genomics framework files...")

write_text_file(
  title_page,
  file.path(framework_dir, "BMC_Genomics_title_page_v0.1.md")
)

write_text_file(
  abstract_skeleton,
  file.path(framework_dir, "BMC_Genomics_abstract_skeleton_v0.1.md")
)

write_text_file(
  section_outline,
  file.path(framework_dir, "BMC_Genomics_section_outline_v0.1.md")
)

figure_table_lines <- c(
  "# BMC Genomics figure and table mapping v0.1",
  "",
  apply(
    figure_table_mapping,
    1,
    function(x) {
      paste0(
        "## ", x[["item_id"]], ". ", x[["title"]], "\n",
        "- Role: ", x[["manuscript_role"]], "\n",
        "- Source: ", x[["source"]], "\n",
        "- Notes: ", x[["notes"]], "\n"
      )
    }
  )
)

write_text_file(
  figure_table_lines,
  file.path(framework_dir, "BMC_Genomics_figure_table_mapping_v0.1.md")
)

claim_lines <- c(
  "# BMC Genomics claim control sheet v0.1",
  "",
  apply(
    claim_control,
    1,
    function(x) {
      paste0(
        "## ", x[["category"]], ": ", x[["statement"]], "\n",
        "- Recommended wording: ", x[["recommended_wording"]], "\n",
        "- Reason: ", x[["reason"]], "\n"
      )
    }
  )
)

write_text_file(
  claim_lines,
  file.path(framework_dir, "BMC_Genomics_claim_control_sheet_v0.1.md")
)

write_text_file(
  declarations,
  file.path(framework_dir, "BMC_Genomics_declarations_skeleton_v0.1.md")
)

write_text_file(
  writer_brief,
  file.path(framework_dir, "BMC_Genomics_writer_brief_v0.1.md")
)

# ============================================================
# 保存 Excel workbook
# ============================================================

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "framework_index")
openxlsx::writeData(wb, "framework_index", framework_index)

openxlsx::addWorksheet(wb, "title_options")
openxlsx::writeData(wb, "title_options", title_options)

openxlsx::addWorksheet(wb, "figure_table_mapping")
openxlsx::writeData(wb, "figure_table_mapping", figure_table_mapping)

openxlsx::addWorksheet(wb, "claim_control")
openxlsx::writeData(wb, "claim_control", claim_control)

openxlsx::addWorksheet(wb, "key_snapshot")
openxlsx::writeData(wb, "key_snapshot", key_snapshot)

openxlsx::addWorksheet(wb, "decision_log")
openxlsx::writeData(wb, "decision_log", decision_log)

openxlsx::addWorksheet(wb, "source_index")
openxlsx::writeData(wb, "source_index", source_index)

openxlsx::saveWorkbook(
  wb,
  file.path(framework_dir, "BMC_Genomics_framework_index.xlsx"),
  overwrite = TRUE
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_22_prepare_BMC_Genomics_manuscript_framework.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 22 BMC Genomics manuscript framework 完成 ============")
message("输出目录：", framework_dir)

message("\nRecommended title:")
print(recommended_title)

message("\nKeywords:")
print(keywords)

message("\nFigure/table mapping:")
print(figure_table_mapping)

message("\nClaim control:")
print(claim_control)

message("\nFramework index:")
print(framework_index)

message("\n关键输出：")
message("1) ", file.path(framework_dir, "BMC_Genomics_title_page_v0.1.md"))
message("2) ", file.path(framework_dir, "BMC_Genomics_abstract_skeleton_v0.1.md"))
message("3) ", file.path(framework_dir, "BMC_Genomics_section_outline_v0.1.md"))
message("4) ", file.path(framework_dir, "BMC_Genomics_figure_table_mapping_v0.1.md"))
message("5) ", file.path(framework_dir, "BMC_Genomics_claim_control_sheet_v0.1.md"))
message("6) ", file.path(framework_dir, "BMC_Genomics_declarations_skeleton_v0.1.md"))
message("7) ", file.path(framework_dir, "BMC_Genomics_writer_brief_v0.1.md"))
message("8) ", file.path(framework_dir, "BMC_Genomics_framework_index.xlsx"))

message("\n下一步：")
message("把 Recommended title、Figure/table mapping、Claim control 贴给我。")
message("我确认框架后，再开始逐段写 manuscript_v0.3。")