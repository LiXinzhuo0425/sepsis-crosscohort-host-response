# ============================================================
# 20_freeze_final_results_and_tables.R
# Final freeze of all completed analyses before manuscript drafting
#
# 目的：
# 1. 创建 04_results/final_freeze/
# 2. 冻结最终 key numbers、tables、figures、decision log
# 3. 记录 RNA-seq No-Go 决策
# 4. 记录 allowed claims / restrictions
# 5. 生成 final_freeze_manifest 和 manuscript_source_index
#
# 注意：
# 本脚本不重新分析、不重新建模、不重新筛基因。
# 它只复制、索引和冻结已经完成的结果。
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
rna_dir <- file.path(project_dir, "04_results", "RNAseq_validation")
sc_dir <- file.path(project_dir, "04_results", "single_cell")
enrich_dir <- file.path(project_dir, "04_results", "enrichment")
candidate_dir <- file.path(project_dir, "04_results", "candidate_selection")
de_dir <- file.path(project_dir, "04_results", "differential_expression")
sens_dir <- file.path(project_dir, "04_results", "sensitivity")
fig_root <- file.path(project_dir, "05_figures")
manuscript_dir <- file.path(project_dir, "07_manuscript")
log_dir <- file.path(project_dir, "04_results", "logs")

freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
freeze_tables_dir <- file.path(freeze_dir, "tables")
freeze_figures_dir <- file.path(freeze_dir, "figures")
freeze_logs_dir <- file.path(freeze_dir, "logs")
freeze_manuscript_dir <- file.path(freeze_dir, "manuscript_source")

dir.create(freeze_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(freeze_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(freeze_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(freeze_logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(freeze_manuscript_dir, recursive = TRUE, showWarnings = FALSE)

freeze_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# ============================================================
# 工具函数
# ============================================================

copy_if_exists <- function(from, to_dir, new_name = NULL, required = TRUE) {
  if (!file.exists(from)) {
    if (required) {
      stop("必要文件不存在：", from)
    } else {
      return(data.frame(
        source_path = from,
        frozen_path = NA_character_,
        copied = FALSE,
        required = required,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (is.null(new_name)) {
    new_name <- basename(from)
  }
  
  to <- file.path(to_dir, new_name)
  ok <- file.copy(from, to, overwrite = TRUE)
  
  data.frame(
    source_path = from,
    frozen_path = to,
    copied = ok,
    required = required,
    stringsAsFactors = FALSE
  )
}

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
}

get_value <- function(df, key_col, value_col, key) {
  x <- df[[value_col]][df[[key_col]] == key]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

list_figures <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    return(data.frame())
  }
  
  f <- list.files(
    dir_path,
    pattern = "\\.(png|pdf|tiff|tif|jpg|jpeg)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(f) == 0) {
    return(data.frame())
  }
  
  data.frame(
    figure_path = f,
    figure_name = basename(f),
    relative_dir = dirname(sub(paste0("^", gsub("\\\\", "/", fig_root), "/?"), "", gsub("\\\\", "/", f))),
    size_mb = round(file.info(f)$size / 1024^2, 3),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 必要输入
# ============================================================

required_files <- list(
  T20_integrated_workbook = file.path(report_dir, "T20_manuscript_ready_integrated_tables.xlsx"),
  T20_key_numbers = file.path(report_dir, "T20_key_results_numbers.csv"),
  T20_table1 = file.path(report_dir, "T20_table1_cohort_context.csv"),
  T20_table2 = file.path(report_dir, "T20_table2_nested_LODO_validation.csv"),
  T20_table3 = file.path(report_dir, "T20_table3_calibration_threshold_transport.csv"),
  T20_table4 = file.path(report_dir, "T20_table4_scRNA_localization.csv"),
  T20_figure_plan = file.path(report_dir, "T20_figure_plan_final.csv"),
  T21_sanity_workbook = file.path(report_dir, "T21_final_sanity_check_summary.xlsx"),
  T21_consistency = file.path(report_dir, "T21_key_consistency_checks.csv"),
  T21_risk_flags = file.path(report_dir, "T21_manuscript_risk_flags.csv"),
  T21_claims = file.path(report_dir, "T21_recommended_claims_and_restrictions.csv"),
  T16b_go_no_go = file.path(rna_dir, "T16b_RNAseq_go_no_go_summary.csv"),
  T16b_eligibility = file.path(rna_dir, "T16b_RNAseq_eligibility_table.csv"),
  T14_nested_metrics = file.path(nested_dir, "T14_nested_LODO_validation_metrics.csv"),
  T14_nested_predictions = file.path(nested_dir, "T14_nested_LODO_predictions.csv"),
  T14_gene_frequency = file.path(nested_dir, "T14_nested_LODO_gene_selection_frequency.csv"),
  T17_pooled_perf = file.path(dca_dir, "T17_pooled_nested_LODO_performance.csv"),
  T17_calibration = file.path(dca_dir, "T17_nested_LODO_calibration_metrics_final.csv"),
  T17_threshold = file.path(dca_dir, "T17_nested_LODO_threshold_drift_final.csv"),
  T17_dca_summary = file.path(dca_dir, "T17_nested_LODO_DCA_summary.csv"),
  T19_scRNA_summary = file.path(sc_dir, "T19_scRNA_signature_localization_summary.csv"),
  T19_scRNA_celltype = file.path(sc_dir, "T19_scRNA_signature_score_by_celltype.csv"),
  T19_scRNA_cluster_annotation = file.path(sc_dir, "T19_scRNA_cluster_annotation.csv")
)

optional_files <- list(
  T05_GO_BP = file.path(enrich_dir, "T05_GO_BP_enrichment.xlsx"),
  T05_gene_lists = file.path(enrich_dir, "T05_DEG_gene_lists.xlsx"),
  T06_candidate_screening = file.path(candidate_dir, "T06_candidate_gene_screening_table.xlsx"),
  T07_refined_candidates = file.path(candidate_dir, "T07_refined_candidate_summary.xlsx"),
  T04_outlier_sensitivity = file.path(sens_dir, "T04_outlier_sensitivity_summary.xlsx"),
  limma_DEG_main = file.path(de_dir, "limma_DEG_Sepsis_vs_Control.csv"),
  results_draft_v02 = file.path(manuscript_dir, "results_draft_v0.2_after_nested_LODO_scRNA.md"),
  figure_plan_v02 = file.path(manuscript_dir, "figure_table_plan_v0.2_after_nested_LODO_scRNA.md"),
  interpretation_v02 = file.path(manuscript_dir, "interpretation_notes_v0.2_after_nested_LODO_scRNA.md"),
  sanity_md = file.path(manuscript_dir, "final_sanity_check_before_manuscript_v0.1.md")
)

missing_required <- names(required_files)[!file.exists(unlist(required_files))]

if (length(missing_required) > 0) {
  stop(
    "缺少必要冻结文件：\n",
    paste(missing_required, unlist(required_files)[missing_required], sep = ": ", collapse = "\n")
  )
}

# ============================================================
# 读取关键表
# ============================================================

message("Reading key final tables...")

key_numbers <- read_csv_df(required_files$T20_key_numbers)
table2 <- read_csv_df(required_files$T20_table2)
table3 <- read_csv_df(required_files$T20_table3)
table4 <- read_csv_df(required_files$T20_table4)
risk_flags <- read_csv_df(required_files$T21_risk_flags)
claims <- read_csv_df(required_files$T21_claims)
consistency <- read_csv_df(required_files$T21_consistency)
rna_go_no_go <- read_csv_df(required_files$T16b_go_no_go)

n_consistency_flags <- sum(consistency$status != "PASS", na.rm = TRUE)
rna_decision <- get_value(rna_go_no_go, "metric", "value", "go_no_go_decision")
rna_next_step <- get_value(rna_go_no_go, "metric", "value", "recommended_next_step")

if (n_consistency_flags > 0) {
  stop("T21 consistency checks 存在非 PASS 项。请先处理后再冻结。")
}

# ============================================================
# 冻结核心表
# ============================================================

message("Copying final tables into final_freeze...")

copy_records <- list()

for (nm in names(required_files)) {
  copy_records[[nm]] <- copy_if_exists(
    from = required_files[[nm]],
    to_dir = freeze_tables_dir,
    new_name = paste0(nm, "_", basename(required_files[[nm]])),
    required = TRUE
  )
}

for (nm in names(optional_files)) {
  copy_records[[paste0("optional_", nm)]] <- copy_if_exists(
    from = optional_files[[nm]],
    to_dir = freeze_tables_dir,
    new_name = paste0(nm, "_", basename(optional_files[[nm]])),
    required = FALSE
  )
}

copy_manifest <- do.call(rbind, copy_records)
copy_manifest$file_key <- names(copy_records)
copy_manifest <- copy_manifest[, c("file_key", "source_path", "frozen_path", "copied", "required")]

# ============================================================
# 冻结图像索引，不复制所有图，避免重复占空间
# 同时复制关键图目录中已有图
# ============================================================

message("Indexing and copying key figures...")

figure_dirs <- c(
  nested_LODO = file.path(fig_root, "nested_LODO"),
  dca = file.path(fig_root, "dca"),
  single_cell = file.path(fig_root, "single_cell"),
  reporting = file.path(fig_root, "reporting"),
  enrichment = file.path(fig_root, "enrichment"),
  candidate_selection = file.path(fig_root, "candidate_selection"),
  qc_bulk = file.path(fig_root, "qc_bulk")
)

figure_inventory_list <- list()

for (nm in names(figure_dirs)) {
  z <- list_figures(figure_dirs[[nm]])
  if (nrow(z) > 0) {
    z$figure_group <- nm
    figure_inventory_list[[nm]] <- z
  }
}

figure_inventory <- if (length(figure_inventory_list) > 0) {
  do.call(rbind, figure_inventory_list)
} else {
  data.frame()
}

figure_copy_manifest <- data.frame()

if (nrow(figure_inventory) > 0) {
  for (i in seq_len(nrow(figure_inventory))) {
    group_dir <- file.path(freeze_figures_dir, figure_inventory$figure_group[i])
    dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
    
    copied <- copy_if_exists(
      from = figure_inventory$figure_path[i],
      to_dir = group_dir,
      new_name = figure_inventory$figure_name[i],
      required = FALSE
    )
    
    copied$figure_group <- figure_inventory$figure_group[i]
    figure_copy_manifest <- rbind(figure_copy_manifest, copied)
  }
}

# ============================================================
# Freeze decision log
# ============================================================

message("Creating final decision log...")

decision_log <- data.frame(
  decision_id = c(
    "D01_primary_analysis_identity",
    "D02_main_validation_framework",
    "D03_RNAseq_external_validation",
    "D04_scRNA_role",
    "D05_DCA_role",
    "D06_threshold_claim",
    "D07_clinical_deployment_claim",
    "D08_failed_transport_dataset",
    "D09_final_result_source",
    "D10_post_freeze_rule"
  ),
  decision = c(
    "Cross-cohort transportability evaluation of a host-response transcriptomic sepsis diagnostic signature.",
    "Strict nested leave-one-dataset-out validation is the primary validation framework.",
    "RNA-seq external platform validation was not performed because local screening detected no eligible RNA-seq dataset.",
    "scRNA-seq is used for biological localization only, not diagnostic validation.",
    "Decision-curve analysis is exploratory because calibration and threshold behavior are unstable and cohort contrasts are heterogeneous.",
    "Fixed threshold is not considered transportable.",
    "No claim of clinical deployment readiness will be made.",
    "GSE54514 is retained and interpreted as a transportability failure scenario.",
    "Manuscript should use final_freeze tables as the sole numeric source.",
    "After this freeze, no gene re-selection, threshold re-selection, cohort removal, or untracked rerun is allowed."
  ),
  evidence_or_source = c(
    "T20 key numbers; T21 claims/restrictions",
    "T14 nested LODO outputs; T20 Table 2",
    paste0("T16b Go/No-Go: ", rna_decision),
    "T19 scRNA localization outputs; T20 Table 4",
    "T17 DCA outputs; T21 risk flags",
    "T17 threshold drift; T20 Table 3; T21 risk flags",
    "T21 recommended claims and restrictions",
    "T20 Table 2 and T21 risk flags",
    "Current final_freeze manifest",
    "Current final freeze rule"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Manuscript source index
# ============================================================

message("Creating manuscript source index...")

manuscript_source_index <- data.frame(
  manuscript_section = c(
    "Abstract",
    "Introduction",
    "Methods: data sources",
    "Methods: preprocessing",
    "Methods: nested LODO modeling",
    "Methods: calibration and threshold analysis",
    "Methods: DCA",
    "Methods: scRNA localization",
    "Results: cohort composition",
    "Results: nested LODO",
    "Results: calibration and threshold",
    "Results: DCA",
    "Results: scRNA localization",
    "Discussion: main interpretation",
    "Discussion: limitations"
  ),
  frozen_source = c(
    "T20_key_results_numbers; T21_claims",
    "No numeric freeze source required",
    "T20_table1_cohort_context; T15b cohort table",
    "Earlier preprocessing scripts and T03/T05/T14 outputs",
    "T20_table2_nested_LODO_validation; T14 outputs",
    "T20_table3_calibration_threshold_transport; T17 outputs",
    "T17_nested_LODO_DCA_summary",
    "T20_table4_scRNA_localization; T19 outputs",
    "T20_table1_cohort_context",
    "T20_table2_nested_LODO_validation",
    "T20_table3_calibration_threshold_transport",
    "T17_nested_LODO_DCA_summary",
    "T20_table4_scRNA_localization",
    "T21_recommended_claims_and_restrictions",
    "T21_manuscript_risk_flags; T16b RNAseq No-Go"
  ),
  allowed_use = c(
    "Use only rounded values from key numbers.",
    "Background only.",
    "Describe cohort heterogeneity and control-type limitations.",
    "Describe archived public processed data and harmonization.",
    "Primary model validation result.",
    "Central transportability result.",
    "Exploratory only.",
    "Biological localization only.",
    "Main result.",
    "Main result.",
    "Main result.",
    "Supplementary or cautious main result.",
    "Main biological interpretation.",
    "Defensive synthesis.",
    "Must explicitly state limitations."
  ),
  restricted_claims = c(
    "Do not claim deployment readiness.",
    "Do not overstate novelty.",
    "Do not claim same-intended-use validation.",
    "Do not claim raw-level reprocessing if not done.",
    "Do not imply held-out data participated in feature selection.",
    "Do not claim fixed threshold is validated.",
    "Do not claim clinical utility is established.",
    "Do not call this diagnostic validation.",
    "Do not hide healthy-control dominance.",
    "Do not hide GSE54514 failure.",
    "Do not soften threshold failure.",
    "Do not overinterpret DCA.",
    "Do not infer patient-level diagnostic performance.",
    "Do not claim universal robustness.",
    "Do not treat RNA-seq as completed validation."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Final key result snapshot
# ============================================================

message("Creating final key result snapshot...")

key_snapshot <- data.frame(
  item = c(
    "freeze_date",
    "bulk_total_samples",
    "bulk_sepsis",
    "bulk_control",
    "n_bulk_datasets",
    "nested_LODO_median_AUROC",
    "nested_LODO_min_AUROC",
    "nested_LODO_AUROC_ge_0.80_datasets",
    "pooled_nested_LODO_AUROC",
    "pooled_nested_LODO_Brier",
    "median_abs_threshold_shift",
    "max_abs_threshold_shift",
    "fixed_sensitivity_lt_0.20_datasets",
    "fixed_specificity_lt_0.20_datasets",
    "genes_selected_all_nested_folds",
    "RNAseq_validation_status",
    "scRNA_cells",
    "scRNA_clusters",
    "scRNA_top_Final10_celltype",
    "scRNA_top_NestedRecurrent_celltype",
    "overall_ready_status"
  ),
  value = c(
    freeze_date,
    get_value(key_numbers, "item", "value", "n_total_samples_bulk"),
    get_value(key_numbers, "item", "value", "n_sepsis_bulk"),
    get_value(key_numbers, "item", "value", "n_control_bulk"),
    get_value(key_numbers, "item", "value", "n_datasets"),
    get_value(key_numbers, "item", "value", "nested_LODO_median_AUROC"),
    get_value(key_numbers, "item", "value", "nested_LODO_min_AUROC"),
    get_value(key_numbers, "item", "value", "nested_LODO_n_AUROC_ge_0.80"),
    get_value(key_numbers, "item", "value", "pooled_nested_LODO_AUROC"),
    get_value(key_numbers, "item", "value", "pooled_nested_LODO_Brier"),
    get_value(key_numbers, "item", "value", "median_abs_threshold_shift"),
    get_value(key_numbers, "item", "value", "max_abs_threshold_shift"),
    get_value(key_numbers, "item", "value", "n_fixed_sensitivity_lt_0.20"),
    get_value(key_numbers, "item", "value", "n_fixed_specificity_lt_0.20"),
    get_value(key_numbers, "item", "value", "genes_selected_in_all_nested_LODO_folds"),
    rna_decision,
    get_value(key_numbers, "item", "value", "scRNA_n_cells"),
    get_value(key_numbers, "item", "value", "scRNA_n_clusters"),
    get_value(key_numbers, "item", "value", "scRNA_top_celltype_Final10"),
    get_value(key_numbers, "item", "value", "scRNA_top_celltype_NestedRecurrent"),
    "READY_FOR_MANUSCRIPT_WITH_RESTRICTIONS"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Freeze summary markdown
# ============================================================

freeze_md <- c(
  "# Final freeze summary",
  "",
  paste0("Freeze date: ", freeze_date),
  "",
  "## Frozen study identity",
  "",
  "Cross-cohort transportability evaluation of a host-response transcriptomic sepsis diagnostic signature with independent single-cell biological localization.",
  "",
  "## Frozen primary validation framework",
  "",
  "Strict nested leave-one-dataset-out validation. In each outer fold, the held-out dataset was excluded from feature screening, redundancy reduction, standardization, model fitting, tuning and threshold selection.",
  "",
  "## Frozen RNA-seq decision",
  "",
  paste0("RNA-seq validation status: ", rna_decision),
  "",
  paste0("Recommended handling: ", rna_next_step),
  "",
  "## Frozen main conclusions",
  "",
  paste0(
    "1. Bulk analysis included ",
    get_value(key_numbers, "item", "value", "n_total_samples_bulk"),
    " samples from ",
    get_value(key_numbers, "item", "value", "n_datasets"),
    " cohorts."
  ),
  paste0(
    "2. Median nested LODO AUROC was ",
    round(as.numeric(get_value(key_numbers, "item", "value", "nested_LODO_median_AUROC")), 3),
    ", with minimum AUROC ",
    round(as.numeric(get_value(key_numbers, "item", "value", "nested_LODO_min_AUROC")), 3),
    "."
  ),
  paste0(
    "3. Pooled nested LODO AUROC was ",
    round(as.numeric(get_value(key_numbers, "item", "value", "pooled_nested_LODO_AUROC")), 3),
    ", supporting weaker pooled transport behavior than per-dataset discrimination."
  ),
  paste0(
    "4. Median absolute threshold shift was ",
    round(as.numeric(get_value(key_numbers, "item", "value", "median_abs_threshold_shift")), 3),
    ", so fixed threshold transportability is not supported."
  ),
  paste0(
    "5. scRNA localization showed Final10 and NestedRecurrent signatures both highest in ",
    get_value(key_numbers, "item", "value", "scRNA_top_celltype_Final10"),
    " compartments."
  ),
  "",
  "## Frozen restrictions",
  "",
  "- Do not claim clinical deployment readiness.",
  "- Do not claim same-intended-use external validation.",
  "- Do not claim fixed threshold transportability.",
  "- Do not treat scRNA module score analysis as independent diagnostic validation.",
  "- Do not overinterpret healthy-control contrasts as clinical mimic discrimination.",
  "- Do not state that RNA-seq validation was performed.",
  "",
  "## Post-freeze rule",
  "",
  "After this freeze, manuscript drafting should use only files in 04_results/final_freeze as the numeric source. Any further rerun must be logged as a new freeze version."
)

# ============================================================
# 保存 freeze 输出
# ============================================================

message("Saving final freeze outputs...")

data.table::fwrite(
  copy_manifest,
  file.path(freeze_dir, "final_freeze_file_copy_manifest.csv")
)

data.table::fwrite(
  figure_inventory,
  file.path(freeze_dir, "final_freeze_figure_inventory.csv")
)

data.table::fwrite(
  figure_copy_manifest,
  file.path(freeze_dir, "final_freeze_figure_copy_manifest.csv")
)

data.table::fwrite(
  decision_log,
  file.path(freeze_dir, "final_freeze_decision_log.csv")
)

data.table::fwrite(
  manuscript_source_index,
  file.path(freeze_dir, "manuscript_source_index.csv")
)

data.table::fwrite(
  key_snapshot,
  file.path(freeze_dir, "final_key_result_snapshot.csv")
)

write_text_file(
  freeze_md,
  file.path(freeze_dir, "final_freeze_summary.md")
)

# 复制 manuscript source markdown 到 manuscript_source 文件夹
for (nm in names(optional_files)) {
  if (grepl("\\.md$", optional_files[[nm]]) && file.exists(optional_files[[nm]])) {
    copy_if_exists(
      from = optional_files[[nm]],
      to_dir = freeze_manuscript_dir,
      new_name = basename(optional_files[[nm]]),
      required = FALSE
    )
  }
}

# Workbook
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "key_snapshot")
openxlsx::writeData(wb, "key_snapshot", key_snapshot)

openxlsx::addWorksheet(wb, "decision_log")
openxlsx::writeData(wb, "decision_log", decision_log)

openxlsx::addWorksheet(wb, "manuscript_source_index")
openxlsx::writeData(wb, "manuscript_source_index", manuscript_source_index)

openxlsx::addWorksheet(wb, "copy_manifest")
openxlsx::writeData(wb, "copy_manifest", copy_manifest)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

openxlsx::addWorksheet(wb, "risk_flags")
openxlsx::writeData(wb, "risk_flags", risk_flags)

openxlsx::addWorksheet(wb, "claims_restrictions")
openxlsx::writeData(wb, "claims_restrictions", claims)

openxlsx::addWorksheet(wb, "RNAseq_go_no_go")
openxlsx::writeData(wb, "RNAseq_go_no_go", rna_go_no_go)

openxlsx::saveWorkbook(
  wb,
  file.path(freeze_dir, "final_freeze_summary_workbook.xlsx"),
  overwrite = TRUE
)

# sessionInfo
sink(file.path(freeze_logs_dir, "sessionInfo_20_freeze_final_results_and_tables.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 20 Final freeze 完成 ============")
message("Freeze 目录：", freeze_dir)
message("Freeze date：", freeze_date)

message("\nFinal key result snapshot:")
print(key_snapshot)

message("\nRNA-seq freeze decision:")
print(rna_go_no_go)

message("\nDecision log:")
print(decision_log)

message("\nManuscript source index:")
print(manuscript_source_index)

message("\nFile copy manifest summary:")
print(table(copy_manifest$copied, copy_manifest$required, useNA = "ifany"))

message("\nFigure inventory summary:")
if (nrow(figure_inventory) > 0) {
  print(table(figure_inventory$figure_group))
} else {
  message("No figures indexed.")
}

message("\n关键输出：")
message("1) ", file.path(freeze_dir, "final_freeze_summary_workbook.xlsx"))
message("2) ", file.path(freeze_dir, "final_key_result_snapshot.csv"))
message("3) ", file.path(freeze_dir, "final_freeze_decision_log.csv"))
message("4) ", file.path(freeze_dir, "manuscript_source_index.csv"))
message("5) ", file.path(freeze_dir, "final_freeze_file_copy_manifest.csv"))
message("6) ", file.path(freeze_dir, "final_freeze_figure_inventory.csv"))
message("7) ", file.path(freeze_dir, "final_freeze_summary.md"))
message("8) ", freeze_tables_dir)
message("9) ", freeze_figures_dir)

message("\n下一步：")
message("把 Final key result snapshot、RNA-seq freeze decision、File copy manifest summary 贴给我。")
message("我确认 freeze 正常后，再对照计划书判断是否还有研究环节未完成。")