# ============================================================
# 25_BMC_Genomics_full_manuscript_assembly.R
# Assemble full BMC Genomics manuscript draft v0.3
#
# 目的：
# 1. 组装 Title page、Abstract、Background、Methods、Results、Discussion、Declarations
# 2. 不新增分析，不改 frozen numbers
# 3. 输出完整 markdown 初稿
# 4. 输出 manuscript readiness checklist
#
# 输入：
# 07_manuscript/BMC_Genomics_framework/
# 07_manuscript/BMC_Genomics_draft/
# 04_results/final_freeze/
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.3.md
#   BMC_Genomics_main_text_v0.3.md
#   BMC_Genomics_submission_checklist_v0.3.csv
#   BMC_Genomics_full_manuscript_v0.3_index.xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
framework_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_framework")
draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

files <- list(
  title_page = file.path(framework_dir, "BMC_Genomics_title_page_v0.1.md"),
  abstract = file.path(framework_dir, "BMC_Genomics_abstract_skeleton_v0.1.md"),
  declarations = file.path(framework_dir, "BMC_Genomics_declarations_skeleton_v0.1.md"),
  background = file.path(draft_dir, "BMC_Genomics_background_v0.3.md"),
  methods = file.path(draft_dir, "BMC_Genomics_methods_v0.3.md"),
  results = file.path(draft_dir, "BMC_Genomics_results_v0.3.md"),
  discussion = file.path(draft_dir, "BMC_Genomics_discussion_v0.3.md"),
  key_snapshot = file.path(freeze_dir, "final_key_result_snapshot.csv"),
  decision_log = file.path(freeze_dir, "final_freeze_decision_log.csv"),
  source_index = file.path(freeze_dir, "manuscript_source_index.csv"),
  figure_inventory = file.path(freeze_dir, "final_freeze_figure_inventory.csv"),
  copy_manifest = file.path(freeze_dir, "final_freeze_file_copy_manifest.csv")
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

count_words_simple <- function(lines) {
  txt <- paste(lines, collapse = " ")
  txt <- gsub("[[:punct:]]+", " ", txt)
  words <- unlist(strsplit(txt, "\\s+"))
  words <- words[words != ""]
  length(words)
}

extract_section <- function(lines, start_pattern, end_pattern = NULL) {
  start_idx <- grep(start_pattern, lines)
  if (length(start_idx) == 0) return(character())
  start_idx <- start_idx[1]
  
  if (is.null(end_pattern)) {
    return(lines[start_idx:length(lines)])
  }
  
  end_idx <- grep(end_pattern, lines)
  end_idx <- end_idx[end_idx > start_idx]
  if (length(end_idx) == 0) {
    return(lines[start_idx:length(lines)])
  }
  
  lines[start_idx:(end_idx[1] - 1)]
}

remove_first_h1 <- function(lines) {
  if (length(lines) == 0) return(lines)
  if (grepl("^# ", lines[1])) {
    return(lines[-1])
  }
  lines
}

# ============================================================
# 读取内容
# ============================================================

message("Reading manuscript components...")

title_lines <- read_lines_utf8(files$title_page)
abstract_lines_raw <- read_lines_utf8(files$abstract)
declarations_lines_raw <- read_lines_utf8(files$declarations)
background_lines <- read_lines_utf8(files$background)
methods_lines <- read_lines_utf8(files$methods)
results_lines <- read_lines_utf8(files$results)
discussion_lines <- read_lines_utf8(files$discussion)

key_snapshot <- read_csv_df(files$key_snapshot)
decision_log <- read_csv_df(files$decision_log)
source_index <- read_csv_df(files$source_index)
figure_inventory <- read_csv_df(files$figure_inventory)
copy_manifest <- read_csv_df(files$copy_manifest)

# ============================================================
# 题目和摘要处理
# ============================================================

recommended_title <- "Nested cross-cohort evaluation and single-cell localization of a blood transcriptomic host-response signature for sepsis"

abstract_core <- abstract_lines_raw
abstract_core <- abstract_core[!grepl("^# ", abstract_core)]
abstract_core <- abstract_core[!grepl("^> ", abstract_core)]

# 删除重复关键词标题前的空余可以保留
abstract_word_count <- count_words_simple(
  abstract_core[!grepl("^## Keywords", abstract_core) & !grepl("^Sepsis;", abstract_core)]
)

# ============================================================
# Main text
# ============================================================

main_text <- c(
  "# Background",
  "",
  remove_first_h1(background_lines),
  "",
  "# Methods",
  "",
  remove_first_h1(methods_lines),
  "",
  "# Results",
  "",
  remove_first_h1(results_lines),
  "",
  "# Discussion",
  "",
  remove_first_h1(discussion_lines)
)

# 去掉因 remove_first_h1 后可能产生的重复标题
main_text <- gsub("^## ", "## ", main_text)

main_text_word_count <- count_words_simple(main_text)

# ============================================================
# Full manuscript
# ============================================================

title_page_clean <- c(
  "# Title page",
  "",
  "## Title",
  "",
  recommended_title,
  "",
  "## Running title",
  "",
  "Transcriptomic sepsis signature transportability",
  "",
  "## Article type",
  "",
  "Research article",
  "",
  "## Target journal",
  "",
  "BMC Genomics",
  "",
  "## Authors",
  "",
  "[Author names to be completed]",
  "",
  "## Affiliations",
  "",
  "[Affiliations to be completed]",
  "",
  "## Corresponding author",
  "",
  "[Name, address, email to be completed]",
  "",
  "## Keywords",
  "",
  "Sepsis; Transcriptomics; Host response; Nested validation; Transportability; Calibration; Single-cell RNA sequencing; Myeloid cell; Diagnostic signature"
)

abbrev_lines <- c(
  "# List of abbreviations",
  "",
  "AUPRC: area under the precision-recall curve",
  "AUROC: area under the receiver operating characteristic curve",
  "DEG: differentially expressed gene",
  "DCA: decision-curve analysis",
  "GEO: Gene Expression Omnibus",
  "GO: Gene Ontology",
  "LODO: leave-one-dataset-out",
  "PBMC: peripheral blood mononuclear cell",
  "RNA-seq: RNA sequencing",
  "scRNA-seq: single-cell RNA sequencing",
  "SIRS: systemic inflammatory response syndrome"
)

references_placeholder <- c(
  "# References",
  "",
  "[References to be inserted after citation replacement. Current draft contains citation placeholders such as REF_SEPSIS_DEFINITION, REF_TRIPOD_AI and REF_DECISION_CURVE_ANALYSIS.]"
)

full_manuscript <- c(
  title_page_clean,
  "",
  "# Abstract",
  "",
  abstract_core,
  "",
  main_text,
  "",
  abbrev_lines,
  "",
  remove_first_h1(declarations_lines_raw),
  "",
  references_placeholder
)

# ============================================================
# 自查表
# ============================================================

key_items <- data.frame(
  item = c(
    "target_journal",
    "article_type",
    "title",
    "abstract_word_count_estimate",
    "main_text_word_count_estimate",
    "bulk_total_samples",
    "bulk_sepsis",
    "bulk_control",
    "n_bulk_datasets",
    "nested_LODO_median_AUROC",
    "nested_LODO_min_AUROC",
    "pooled_nested_LODO_AUROC",
    "median_abs_threshold_shift",
    "RNAseq_validation_status",
    "scRNA_cells",
    "scRNA_top_celltype",
    "final_freeze_source"
  ),
  value = c(
    "BMC Genomics",
    "Research article",
    recommended_title,
    abstract_word_count,
    main_text_word_count,
    get_value(key_snapshot, "bulk_total_samples"),
    get_value(key_snapshot, "bulk_sepsis"),
    get_value(key_snapshot, "bulk_control"),
    get_value(key_snapshot, "n_bulk_datasets"),
    get_value(key_snapshot, "nested_LODO_median_AUROC"),
    get_value(key_snapshot, "nested_LODO_min_AUROC"),
    get_value(key_snapshot, "pooled_nested_LODO_AUROC"),
    get_value(key_snapshot, "median_abs_threshold_shift"),
    get_value(key_snapshot, "RNAseq_validation_status"),
    get_value(key_snapshot, "scRNA_cells"),
    get_value(key_snapshot, "scRNA_top_Final10_celltype"),
    freeze_dir
  ),
  stringsAsFactors = FALSE
)

checklist <- data.frame(
  check_id = c(
    "abstract_structured",
    "abstract_word_limit",
    "keywords_3_to_10",
    "has_background",
    "has_methods",
    "has_results",
    "has_discussion",
    "has_declarations",
    "has_data_availability",
    "has_abbreviations",
    "citation_placeholders_present",
    "no_clinical_deployment_claim",
    "RNAseq_NoGo_recorded",
    "scRNA_localization_boundary",
    "final_freeze_used"
  ),
  requirement = c(
    "Abstract has Background, Results, Conclusions.",
    "Abstract <=350 words.",
    "Keywords between 3 and 10.",
    "Main text has Background.",
    "Main text has Methods.",
    "Main text has Results.",
    "Main text has Discussion.",
    "Declarations section present.",
    "Availability of data and materials section present.",
    "List of abbreviations present.",
    "Citation placeholders still need replacement.",
    "Avoid clinical deployment claim.",
    "RNA-seq external validation No-Go recorded.",
    "scRNA stated as biological localization only.",
    "Final freeze used as numeric source."
  ),
  status = c(
    ifelse(
      any(grepl("^## Background", abstract_core)) &&
        any(grepl("^## Results", abstract_core)) &&
        any(grepl("^## Conclusions", abstract_core)),
      "PASS", "CHECK"
    ),
    ifelse(abstract_word_count <= 350, "PASS", "CHECK"),
    "PASS",
    ifelse(any(grepl("^# Background", main_text)), "PASS", "CHECK"),
    ifelse(any(grepl("^# Methods", main_text)), "PASS", "CHECK"),
    ifelse(any(grepl("^# Results", main_text)), "PASS", "CHECK"),
    ifelse(any(grepl("^# Discussion", main_text)), "PASS", "CHECK"),
    ifelse(any(grepl("Ethics approval|Availability of data", declarations_lines_raw)), "PASS", "CHECK"),
    ifelse(any(grepl("Availability of data and materials", declarations_lines_raw)), "PASS", "CHECK"),
    "PASS",
    ifelse(any(grepl("\\[REF_", full_manuscript)), "ACTION_NEEDED", "PASS"),
    ifelse(any(grepl("ready for clinical deployment|clinically ready|proven clinical utility", full_manuscript, ignore.case = TRUE)), "CHECK", "PASS"),
    ifelse(any(grepl("RNA-seq external validation was not performed|No-Go", full_manuscript)), "PASS", "CHECK"),
    ifelse(any(grepl("biological localization only|not considered independent diagnostic validation|rather than patient-level validation", full_manuscript)), "PASS", "CHECK"),
    "PASS"
  ),
  action = c(
    "None if PASS.",
    "Trim abstract if CHECK.",
    "Confirm final keyword list.",
    "None if PASS.",
    "None if PASS.",
    "None if PASS.",
    "None if PASS.",
    "Complete author-specific fields.",
    "Fill repository and accession details.",
    "Check abbreviations after final editing.",
    "Replace all citation placeholders with formal references.",
    "Revise if CHECK.",
    "None if PASS.",
    "None if PASS.",
    "Do not use non-freeze numeric sources."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 输出
# ============================================================

message("Writing full manuscript draft...")

full_path <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.3.md")
main_text_path <- file.path(draft_dir, "BMC_Genomics_main_text_v0.3.md")
checklist_path <- file.path(draft_dir, "BMC_Genomics_submission_checklist_v0.3.csv")
key_items_path <- file.path(draft_dir, "BMC_Genomics_key_items_v0.3.csv")

write_text_file(full_manuscript, full_path)
write_text_file(main_text, main_text_path)
data.table::fwrite(checklist, checklist_path)
data.table::fwrite(key_items, key_items_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_full_manuscript_v0.3.md",
    "BMC_Genomics_main_text_v0.3.md",
    "BMC_Genomics_submission_checklist_v0.3.csv",
    "BMC_Genomics_key_items_v0.3.csv",
    "BMC_Genomics_full_manuscript_v0.3_index.xlsx"
  ),
  path = c(
    full_path,
    main_text_path,
    checklist_path,
    key_items_path,
    file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.3_index.xlsx")
  ),
  purpose = c(
    "Full manuscript markdown draft",
    "Main text only markdown draft",
    "BMC Genomics submission structure checklist",
    "Frozen key manuscript items",
    "Workbook index"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "key_items")
openxlsx::writeData(wb, "key_items", key_items)

openxlsx::addWorksheet(wb, "checklist")
openxlsx::writeData(wb, "checklist", checklist)

openxlsx::addWorksheet(wb, "decision_log")
openxlsx::writeData(wb, "decision_log", decision_log)

openxlsx::addWorksheet(wb, "source_index")
openxlsx::writeData(wb, "source_index", source_index)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

openxlsx::addWorksheet(wb, "copy_manifest")
openxlsx::writeData(wb, "copy_manifest", copy_manifest)

openxlsx::saveWorkbook(
  wb,
  file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.3_index.xlsx"),
  overwrite = TRUE
)

sink(file.path(log_dir, "sessionInfo_25_BMC_Genomics_full_manuscript_assembly.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 25 BMC Genomics full manuscript assembly 完成 ============")
message("输出目录：", draft_dir)

message("\nKey items:")
print(key_items)

message("\nSubmission checklist:")
print(checklist)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", full_path)
message("2) ", main_text_path)
message("3) ", checklist_path)
message("4) ", key_items_path)
message("5) ", file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.3_index.xlsx"))

message("\n下一步：")
message("打开 BMC_Genomics_full_manuscript_v0.3.md 粗读全文。")
message("如果结构正常，下一步做 26_BMC_Genomics_citation_replacement_plan.R，系统补参考文献和引用位置。")