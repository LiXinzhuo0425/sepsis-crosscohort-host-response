# ============================================================
# 32_BMC_Genomics_manual_fields_completion_plan.R
# Manual fields completion plan for BMC Genomics submission
#
# 目的：
# 1. 基于 v0.6 Word 导出状态，生成投稿前人工补全计划
# 2. 生成 title page、Declarations、Data availability、reference finalization 模板
# 3. 不修改 Word 文件，不修改 manuscript，不改 frozen numbers
# 4. 明确哪些项目阻止最终投稿，哪些项目只需投稿前人工确认
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.6_preSubmission.docx
#   BMC_Genomics_full_manuscript_v0.6_preSubmission_combined.md
#   BMC_Genomics_word_export_manual_placeholder_hits_v0.8.csv
#   BMC_Genomics_word_export_manual_actions_v0.8.csv
#   BMC_Genomics_reference_audit_table_v0.4.csv
#   BMC_Genomics_submission_package_check_v0.7.1.xlsx
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_manual_completion_plan_v0.9.md
#   BMC_Genomics_author_title_page_template_v0.9.md
#   BMC_Genomics_declarations_fillin_template_v0.9.md
#   BMC_Genomics_data_availability_template_v0.9.md
#   BMC_Genomics_reference_finalization_checklist_v0.9.csv
#   BMC_Genomics_manual_completion_tasks_v0.9.csv
#   BMC_Genomics_manual_completion_index_v0.9.xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

files <- list(
  docx = file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.6_preSubmission.docx"),
  combined_md = file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.6_preSubmission_combined.md"),
  manual_placeholder_hits = file.path(draft_dir, "BMC_Genomics_word_export_manual_placeholder_hits_v0.8.csv"),
  manual_actions = file.path(draft_dir, "BMC_Genomics_word_export_manual_actions_v0.8.csv"),
  reference_audit = file.path(draft_dir, "BMC_Genomics_reference_audit_table_v0.4.csv"),
  package_check = file.path(draft_dir, "BMC_Genomics_submission_package_check_v0.7.1.xlsx"),
  key_snapshot = file.path(freeze_dir, "final_key_result_snapshot.csv"),
  decision_log = file.path(freeze_dir, "final_freeze_decision_log.csv"),
  source_index = file.path(freeze_dir, "manuscript_source_index.csv")
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

safe_yesno <- function(x) {
  ifelse(isTRUE(x), "YES", "NO")
}

# ============================================================
# 读取输入
# ============================================================

message("Reading manual completion inputs...")

combined_md <- read_lines_utf8(files$combined_md)
manual_placeholder_hits <- read_csv_df(files$manual_placeholder_hits)
manual_actions_old <- read_csv_df(files$manual_actions)
reference_audit <- read_csv_df(files$reference_audit)
key_snapshot <- read_csv_df(files$key_snapshot)
decision_log <- read_csv_df(files$decision_log)
source_index <- read_csv_df(files$source_index)

docx_exists <- file.exists(files$docx)
docx_size <- ifelse(docx_exists, file.info(files$docx)$size, NA)

# ============================================================
# frozen values
# ============================================================

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
bulk_sepsis <- get_value(key_snapshot, "bulk_sepsis")
bulk_control <- get_value(key_snapshot, "bulk_control")
n_bulk_datasets <- get_value(key_snapshot, "n_bulk_datasets")
rnaseq_status <- get_value(key_snapshot, "RNAseq_validation_status")
scRNA_cells <- get_value(key_snapshot, "scRNA_cells")
scRNA_top <- get_value(key_snapshot, "scRNA_top_Final10_celltype")

# ============================================================
# manual task table
# ============================================================

manual_tasks <- data.frame(
  task_id = sprintf("M%02d", 1:14),
  priority = c(
    "Critical",
    "Critical",
    "Critical",
    "Critical",
    "Critical",
    "Critical",
    "High",
    "High",
    "High",
    "High",
    "High",
    "Medium",
    "Medium",
    "Medium"
  ),
  task = c(
    "Complete author names on title page.",
    "Complete author affiliations on title page.",
    "Complete corresponding author name, postal address and email.",
    "Replace GSE167363 placeholder reference with exact source publication or GEO record citation.",
    "Complete Ethics approval and consent to participate statement.",
    "Complete Availability of data and materials statement.",
    "Complete Funding statement.",
    "Complete Competing interests statement.",
    "Complete Authors' contributions statement.",
    "Complete Acknowledgements.",
    "Verify all references in Zotero or manually.",
    "Confirm supplementary files to upload separately.",
    "Confirm figure files and figure format for submission system.",
    "Confirm article-processing charge route and institutional payment plan."
  ),
  manuscript_location = c(
    "Title page",
    "Title page",
    "Title page",
    "References, current reference 9",
    "Declarations",
    "Declarations",
    "Declarations",
    "Declarations",
    "Declarations",
    "Declarations",
    "References",
    "Supplementary material upload",
    "Figure upload",
    "Administrative"
  ),
  blocks_word_review = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  blocks_final_submission = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE
  ),
  recommended_handling = c(
    "Replace placeholder with final author list agreed by all contributors.",
    "Use full institutional names and department/hospital/university hierarchy.",
    "Use one corresponding author unless the team decides otherwise.",
    "Verify the exact GSE167363 source from GEO or publication and replace placeholder reference.",
    "Use public-data secondary-analysis wording and specify waiver/not-required status according to your institution.",
    "List GEO accessions, code repository and Zenodo DOI if available.",
    "Use exact grant name, grant number and funder role statement.",
    "State either no competing interests or disclose details.",
    "Use CRediT-style role wording or journal-compatible contribution wording.",
    "Acknowledge data contributors, GEO and any non-author assistance if appropriate.",
    "Verify DOI, PMID, journal, year, volume, pages and author order.",
    "Decide whether supplementary tables are uploaded as one workbook or separate files.",
    "Use final_freeze figures, export submission-quality image files if needed.",
    "Confirm who pays APC before submission."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# title page template
# ============================================================

title_page_template <- c(
  "# BMC Genomics title page fill-in template v0.9",
  "",
  "## Article title",
  "",
  "Nested cross-cohort evaluation and single-cell localization of a blood transcriptomic host-response signature for sepsis",
  "",
  "## Author names",
  "",
  "[Replace with final author names in agreed order]",
  "",
  "Example format:",
  "",
  "Xin-zhuo Li1, [Author 2]2, [Author 3]1, [Corresponding Author]1*",
  "",
  "## Affiliations",
  "",
  "1. Department of [Department], The First Affiliated Hospital of Chongqing Medical University, Chongqing, China.",
  "2. [Department], [Institution], [City], [Country].",
  "",
  "## Corresponding author",
  "",
  "Correspondence to: [Name]",
  "",
  "Postal address: [Full department, hospital/university, street or institutional address, city, postal code, country]",
  "",
  "Email: [institutional email preferred]",
  "",
  "## Notes for manual completion",
  "",
  "1. Confirm author order before submission.",
  "2. Confirm each author's affiliation and spelling.",
  "3. Confirm whether there is one corresponding author or multiple corresponding authors.",
  "4. Ensure the author list in the manuscript, submission system and cover letter is identical."
)

# ============================================================
# declarations template
# ============================================================

declarations_template <- c(
  "# BMC Genomics declarations fill-in template v0.9",
  "",
  "## Ethics approval and consent to participate",
  "",
  "Option A, if institutional review determined that ethical approval was not required for public de-identified data:",
  "",
  "This study was a secondary analysis of publicly available, de-identified transcriptomic datasets obtained from the Gene Expression Omnibus and related public repositories. No new human participants were recruited, no new biological samples were collected, and no individual-level identifiable information was accessed. According to [institutional/local policy or ethics committee decision], additional ethics approval and informed consent were not required for this secondary analysis of public de-identified data.",
  "",
  "Option B, if an ethics committee issued a waiver or confirmation:",
  "",
  "This study was reviewed by [name of ethics committee/institution], which determined that additional ethics approval and informed consent were waived/not required because the analysis used publicly available, de-identified datasets and involved no new participant recruitment or sample collection. Approval or waiver reference number: [insert if available].",
  "",
  "Manual decision needed:",
  "",
  "- Use Option A only if your institution allows this wording.",
  "- Use Option B if you can obtain an ethics waiver/confirmation number.",
  "- Do not invent an approval number.",
  "",
  "## Consent for publication",
  "",
  "Not applicable.",
  "",
  "## Availability of data and materials",
  "",
  "Use the dedicated data availability template in BMC_Genomics_data_availability_template_v0.9.md.",
  "",
  "## Competing interests",
  "",
  "The authors declare that they have no competing interests.",
  "",
  "## Funding",
  "",
  "Option A, if no specific funding supported this study:",
  "",
  "This research received no specific grant from any funding agency in the public, commercial or not-for-profit sectors.",
  "",
  "Option B, if a grant should be reported:",
  "",
  "This work was supported by [full funder name] [grant number: xxxx]. The funder had no role in study design, data collection, data analysis, data interpretation, manuscript preparation or the decision to submit the manuscript for publication.",
  "",
  "Manual decision needed:",
  "",
  "- Confirm whether this project can cite NSFC 82370009 or another grant.",
  "- If the grant did not support this analysis, use no-specific-funding wording.",
  "",
  "## Authors' contributions",
  "",
  "Draft CRediT-style template:",
  "",
  "X.L. conceived the study, designed the analysis, performed data processing and statistical analysis, generated figures and tables, and drafted the manuscript. [Author 2] contributed to study supervision, interpretation of results and manuscript revision. [Author 3] contributed to clinical interpretation and manuscript revision. [Corresponding Author] supervised the study, reviewed the analysis strategy and critically revised the manuscript. All authors read and approved the final manuscript.",
  "",
  "Manual decision needed:",
  "",
  "- Replace initials with actual author initials.",
  "- Ensure every listed author has a defensible contribution.",
  "- Ensure all authors approve the final manuscript.",
  "",
  "## Acknowledgements",
  "",
  "The authors thank the investigators who generated and deposited the public datasets used in this study in the Gene Expression Omnibus and related repositories.",
  "",
  "Optional addition:",
  "",
  "The authors also thank [name/person/team] for [specific non-author contribution], if applicable."
)

# ============================================================
# data availability template
# ============================================================

data_availability_template <- c(
  "# BMC Genomics data availability template v0.9",
  "",
  "## Recommended final wording",
  "",
  "The datasets analyzed in this study are publicly available from the Gene Expression Omnibus under accession numbers GSE137340, GSE236713, GSE54514, GSE57065, GSE65682, GSE95233 and GSE167363. Processed analysis outputs, final frozen result tables, figure source files and analysis scripts are available at [repository name/link], DOI: [insert Zenodo DOI or repository DOI if available].",
  "",
  "## If code repository is GitHub plus Zenodo",
  "",
  "The analysis code is available at [GitHub URL]. A versioned archival copy of the code and frozen analysis outputs is available at Zenodo, DOI: [insert DOI].",
  "",
  "## If no public code repository is ready yet",
  "",
  "The public input datasets analyzed during the current study are available in the Gene Expression Omnibus under accession numbers GSE137340, GSE236713, GSE54514, GSE57065, GSE65682, GSE95233 and GSE167363. Analysis scripts and processed result tables are available from the corresponding author on reasonable request.",
  "",
  "## Strong recommendation",
  "",
  "For BMC Genomics, a public versioned repository is preferable. Use Zenodo DOI if possible.",
  "",
  "## Manual fields to fill",
  "",
  "- Repository URL:",
  "- Zenodo DOI:",
  "- Whether processed outputs are deposited:",
  "- Whether raw downloaded public data are redistributed or only accession numbers are provided:",
  "- Whether figure source data are included:"
)

# ============================================================
# reference finalization checklist
# ============================================================

reference_finalization <- reference_audit

if (!"manual_check_required" %in% names(reference_finalization)) {
  reference_finalization$manual_check_required <- TRUE
}

reference_finalization$finalization_task <- ifelse(
  grepl("placeholder|pending|Exact", paste(reference_finalization$ref_key, reference_finalization$working_reference, reference_finalization$doi_or_identifier), ignore.case = TRUE),
  "Replace placeholder with exact source citation.",
  ifelse(
    reference_finalization$manual_check_required,
    "Verify bibliographic details in Zotero/manual source.",
    "Already acceptable as working reference; still verify formatting."
  )
)

reference_finalization$blocks_final_submission <- TRUE

# ============================================================
# manual plan markdown
# ============================================================

manual_plan <- c(
  "# BMC Genomics manual completion plan v0.9",
  "",
  "## Current status",
  "",
  paste0("- Word draft exists: ", safe_yesno(docx_exists)),
  paste0("- Word draft size bytes: ", docx_size),
  paste0("- Bulk samples: ", bulk_total, " total, ", bulk_sepsis, " sepsis, ", bulk_control, " controls across ", n_bulk_datasets, " datasets."),
  paste0("- RNA-seq validation status: ", rnaseq_status),
  paste0("- scRNA analysis: ", scRNA_cells, " cells; top localized cell type: ", scRNA_top, "."),
  "",
  "## Interpretation",
  "",
  "The Word draft is ready for manual review. It is not ready for final submission because title-page fields, declarations, data availability, funding/authorship details and exact source references require manual completion.",
  "",
  "## Critical manual completion items",
  "",
  paste0(
    seq_len(nrow(manual_tasks)),
    ". [",
    manual_tasks$priority,
    "] ",
    manual_tasks$task,
    " Location: ",
    manual_tasks$manuscript_location,
    ". Blocks final submission: ",
    manual_tasks$blocks_final_submission,
    "."
  ),
  "",
  "## Recommended order",
  "",
  "1. Fill title page author information.",
  "2. Decide ethics statement wording with the supervisor or ethics office if needed.",
  "3. Complete data availability with GEO accessions and code/Zenodo link.",
  "4. Verify GSE167363 and all reference details.",
  "5. Complete funding, competing interests and author contributions.",
  "6. Re-open the Word file and replace manual placeholders.",
  "7. Run a final submission check after manual edits.",
  "",
  "## Do not do yet",
  "",
  "- Do not submit before manual placeholders are replaced.",
  "- Do not claim clinical deployment readiness.",
  "- Do not claim the fixed threshold was externally validated.",
  "- Do not call the scRNA analysis diagnostic validation.",
  "- Do not remove GSE54514 because it is a failure scenario that strengthens transparency."
)

# ============================================================
# output
# ============================================================

message("Writing manual completion plan outputs...")

manual_plan_path <- file.path(draft_dir, "BMC_Genomics_manual_completion_plan_v0.9.md")
title_template_path <- file.path(draft_dir, "BMC_Genomics_author_title_page_template_v0.9.md")
declarations_template_path <- file.path(draft_dir, "BMC_Genomics_declarations_fillin_template_v0.9.md")
data_availability_template_path <- file.path(draft_dir, "BMC_Genomics_data_availability_template_v0.9.md")
reference_checklist_path <- file.path(draft_dir, "BMC_Genomics_reference_finalization_checklist_v0.9.csv")
manual_tasks_path <- file.path(draft_dir, "BMC_Genomics_manual_completion_tasks_v0.9.csv")
index_path <- file.path(draft_dir, "BMC_Genomics_manual_completion_index_v0.9.xlsx")

write_text_file(manual_plan, manual_plan_path)
write_text_file(title_page_template, title_template_path)
write_text_file(declarations_template, declarations_template_path)
write_text_file(data_availability_template, data_availability_template_path)

data.table::fwrite(reference_finalization, reference_checklist_path)
data.table::fwrite(manual_tasks, manual_tasks_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_manual_completion_plan_v0.9.md",
    "BMC_Genomics_author_title_page_template_v0.9.md",
    "BMC_Genomics_declarations_fillin_template_v0.9.md",
    "BMC_Genomics_data_availability_template_v0.9.md",
    "BMC_Genomics_reference_finalization_checklist_v0.9.csv",
    "BMC_Genomics_manual_completion_tasks_v0.9.csv",
    "BMC_Genomics_manual_completion_index_v0.9.xlsx"
  ),
  path = c(
    manual_plan_path,
    title_template_path,
    declarations_template_path,
    data_availability_template_path,
    reference_checklist_path,
    manual_tasks_path,
    index_path
  ),
  purpose = c(
    "Overall manual completion plan",
    "Title page author/affiliation template",
    "Declarations fill-in template",
    "Data availability fill-in template",
    "Reference finalization checklist",
    "Manual task table",
    "Workbook index"
  ),
  stringsAsFactors = FALSE
)

overall_status <- data.frame(
  metric = c(
    "word_draft_exists",
    "docx_size_bytes",
    "n_manual_tasks",
    "n_tasks_blocking_final_submission",
    "n_references_requiring_finalization",
    "ready_for_manual_completion",
    "ready_for_final_submission"
  ),
  value = c(
    safe_yesno(docx_exists),
    docx_size,
    nrow(manual_tasks),
    sum(manual_tasks$blocks_final_submission),
    sum(reference_finalization$blocks_final_submission),
    "YES",
    "NO_MANUAL_COMPLETION_REQUIRED"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "manual_tasks")
openxlsx::writeData(wb, "manual_tasks", manual_tasks)

openxlsx::addWorksheet(wb, "manual_placeholder_hits")
openxlsx::writeData(wb, "manual_placeholder_hits", manual_placeholder_hits)

openxlsx::addWorksheet(wb, "reference_finalization")
openxlsx::writeData(wb, "reference_finalization", reference_finalization)

openxlsx::addWorksheet(wb, "previous_manual_actions")
openxlsx::writeData(wb, "previous_manual_actions", manual_actions_old)

openxlsx::addWorksheet(wb, "decision_log")
openxlsx::writeData(wb, "decision_log", decision_log)

openxlsx::addWorksheet(wb, "source_index")
openxlsx::writeData(wb, "source_index", source_index)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_32_BMC_Genomics_manual_fields_completion_plan.txt"))
print(sessionInfo())
sink()

# ============================================================
# console output
# ============================================================

message("\n============ 32 BMC Genomics manual fields completion plan 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nManual tasks:")
print(manual_tasks)

message("\nReference finalization checklist:")
print(reference_finalization)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", manual_plan_path)
message("2) ", title_template_path)
message("3) ", declarations_template_path)
message("4) ", data_availability_template_path)
message("5) ", reference_checklist_path)
message("6) ", manual_tasks_path)
message("7) ", index_path)

message("\n下一步：")
message("打开 BMC_Genomics_manual_completion_plan_v0.9.md，按任务逐项人工补全 Word 稿。")
message("把 Overall status、Manual tasks、Reference finalization checklist 贴给我。")
message("补全作者信息和 Declarations 后，再做 33_BMC_Genomics_final_submission_check_after_manual_edit.R。")