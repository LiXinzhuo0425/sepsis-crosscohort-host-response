# ============================================================
# 33_BMC_Genomics_final_submission_check_after_manual_edit.R
# Final submission check after manual Word edits
#
# 目的：
# 1. 检查人工补全后的 Word 文件是否仍有占位符
# 2. 检查作者页、单位、通讯作者、Declarations、Data availability、Funding、Authors' contributions
# 3. 检查是否还有 Zihao Yan / 晏梓豪 残留
# 4. 检查是否已出现 Xinyue Tu / 涂馨月 / 1209883368@qq.com
# 5. 检查 Reference 9 是否已替换 GSE167363 placeholder
# 6. 检查是否还有 REF/TODO/TBD 和高风险 claim
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.6_preSubmission.docx
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_final_submission_check_after_manual_edit_v1.0.xlsx
#   BMC_Genomics_final_submission_checklist_after_manual_edit_v1.0.csv
#   BMC_Genomics_final_manual_action_items_v1.0.csv
#   BMC_Genomics_final_submission_check_report_v1.0.md
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

docx_file <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.6_preSubmission.docx")

if (!file.exists(docx_file)) {
  stop("缺少人工补全后的 Word 文件：", docx_file)
}

# ============================================================
# 工具函数
# ============================================================

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

has_pattern <- function(text, pattern, ignore.case = TRUE) {
  grepl(pattern, text, ignore.case = ignore.case, perl = TRUE)
}

count_pattern <- function(text, pattern, ignore.case = TRUE) {
  m <- gregexpr(pattern, text, ignore.case = ignore.case, perl = TRUE)
  if (length(m) == 0 || m[[1]][1] == -1) return(0L)
  length(m[[1]])
}

line_hits_text <- function(lines, pattern, ignore.case = TRUE) {
  idx <- grep(pattern, lines, ignore.case = ignore.case, perl = TRUE)
  if (length(idx) == 0) {
    return(data.frame(
      pattern = character(),
      line_number = integer(),
      line_text = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    pattern = pattern,
    line_number = idx,
    line_text = lines[idx],
    stringsAsFactors = FALSE
  )
}

read_docx_text <- function(path) {
  if (!requireNamespace("docxtractr", quietly = TRUE) &&
      !requireNamespace("officer", quietly = TRUE)) {
    stop("需要安装 docxtractr 或 officer 之一才能读取 Word 文本。建议安装 officer：install.packages('officer')")
  }
  
  if (requireNamespace("officer", quietly = TRUE)) {
    doc <- officer::read_docx(path)
    s <- officer::docx_summary(doc)
    if ("text" %in% names(s)) {
      txt <- s$text
      txt <- txt[!is.na(txt)]
      return(txt)
    }
  }
  
  if (requireNamespace("docxtractr", quietly = TRUE)) {
    doc <- docxtractr::read_docx(path)
    txt <- docxtractr::docx_extract_all_text(doc)
    txt <- unlist(strsplit(txt, "\n", fixed = TRUE))
    txt <- txt[!is.na(txt)]
    return(txt)
  }
  
  character()
}

collapse_nonempty <- function(lines) {
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  paste(lines, collapse = "\n")
}

status_from_bool <- function(x) {
  ifelse(isTRUE(x), "PASS", "CHECK")
}

# ============================================================
# 读取 Word 文本
# ============================================================

message("Reading manually edited Word file...")

doc_lines <- read_docx_text(docx_file)
doc_text <- collapse_nonempty(doc_lines)

docx_size <- file.info(docx_file)$size

# ============================================================
# 检查项
# ============================================================

placeholder_patterns <- c(
  "\\[Author names to be completed\\]",
  "\\[Affiliations to be completed\\]",
  "\\[Name, address, email to be completed\\]",
  "\\[repository link to be filled before submission\\]",
  "\\[Fill in according to actual funding status\\.?\\]",
  "\\[Fill in author-specific contributions before submission\\.?\\]",
  "\\[Fill in if applicable\\.?\\]",
  "Exact bibliographic details to be inserted",
  "Original publication or GEO record for GSE167363",
  "REF_[A-Z0-9_]+",
  "TODO",
  "TBD"
)

placeholder_hits <- do.call(
  rbind,
  lapply(placeholder_patterns, function(p) line_hits_text(doc_lines, p, ignore.case = TRUE))
)

if (is.null(placeholder_hits) || nrow(placeholder_hits) == 0) {
  placeholder_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

forbidden_claim_patterns <- c(
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

forbidden_claim_hits <- do.call(
  rbind,
  lapply(forbidden_claim_patterns, function(p) line_hits_text(doc_lines, p, ignore.case = TRUE))
)

if (is.null(forbidden_claim_hits) || nrow(forbidden_claim_hits) == 0) {
  forbidden_claim_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 作者与替换检查
# ============================================================

author_checks <- data.frame(
  check_id = c(
    "author_xinzhuo_li_present",
    "author_xinyue_tu_present",
    "author_yi_gong_present",
    "xinyue_email_present",
    "zihao_yan_absent",
    "yan_chinese_absent",
    "corresponding_author_present",
    "corresponding_email_present"
  ),
  requirement = c(
    "Xinzhuo Li appears in title page or author information",
    "Xinyue Tu appears in title page or author information",
    "Yi Gong appears in title page or author information",
    "Xinyue Tu email 1209883368@qq.com appears",
    "Zihao Yan is absent",
    "晏梓豪 is absent",
    "Corresponding author information appears",
    "Corresponding email appears"
  ),
  observed = c(
    has_pattern(doc_text, "Xinzhuo\\s+Li", TRUE),
    has_pattern(doc_text, "Xinyue\\s+Tu", TRUE),
    has_pattern(doc_text, "Yi\\s+Gong", TRUE),
    has_pattern(doc_text, "1209883368@qq\\.com", TRUE),
    !has_pattern(doc_text, "Zihao\\s+Yan", TRUE),
    !has_pattern(doc_text, "晏梓豪", TRUE),
    has_pattern(doc_text, "Correspondence to|Corresponding author|Corresponding Author", TRUE),
    has_pattern(doc_text, "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", TRUE)
  ),
  stringsAsFactors = FALSE
)

author_checks$status <- status_from_bool(author_checks$observed)

# ============================================================
# Declarations 检查
# ============================================================

declaration_checks <- data.frame(
  check_id = c(
    "declarations_heading",
    "ethics_approval_heading",
    "consent_publication_heading",
    "availability_heading",
    "competing_interests_heading",
    "funding_heading",
    "authors_contributions_heading",
    "acknowledgements_heading",
    "no_specific_funding_or_funding_statement",
    "no_competing_interests_statement",
    "authors_contributions_contains_XL_XT_YG",
    "data_availability_contains_GEO",
    "data_availability_mentions_code_or_repository"
  ),
  requirement = c(
    "Declarations heading present",
    "Ethics approval and consent to participate heading present",
    "Consent for publication heading present",
    "Availability of data and materials heading present",
    "Competing interests heading present",
    "Funding heading present",
    "Authors' contributions heading present",
    "Acknowledgements heading present",
    "Funding statement completed",
    "Competing interests statement completed",
    "Authors' contributions includes XL, XT and YG",
    "Data availability mentions GEO",
    "Data availability mentions code/repository/Zenodo or availability on request"
  ),
  observed = c(
    has_pattern(doc_text, "\\bDeclarations\\b", TRUE),
    has_pattern(doc_text, "Ethics approval and consent to participate", TRUE),
    has_pattern(doc_text, "Consent for publication", TRUE),
    has_pattern(doc_text, "Availability of data and materials", TRUE),
    has_pattern(doc_text, "Competing interests", TRUE),
    has_pattern(doc_text, "Funding", TRUE),
    has_pattern(doc_text, "Authors'? contributions", TRUE),
    has_pattern(doc_text, "Acknowledgements", TRUE),
    has_pattern(doc_text, "no specific grant|no specific funding|received no specific", TRUE),
    has_pattern(doc_text, "no competing interests|declare that they have no competing interests", TRUE),
    has_pattern(doc_text, "\\bXL\\b", TRUE) &&
      has_pattern(doc_text, "\\bXT\\b", TRUE) &&
      has_pattern(doc_text, "\\bYG\\b", TRUE),
    has_pattern(doc_text, "Gene Expression Omnibus|\\bGEO\\b|GSE137340|GSE167363", TRUE),
    has_pattern(doc_text, "code|repository|Zenodo|corresponding author on reasonable request|reasonable request", TRUE)
  ),
  stringsAsFactors = FALSE
)

declaration_checks$status <- status_from_bool(declaration_checks$observed)

# ============================================================
# Reference 9 / GSE167363 检查
# ============================================================

reference_checks <- data.frame(
  check_id = c(
    "gse167363_placeholder_absent",
    "gse167363_present",
    "qiu_reference_present",
    "journal_leukocyte_biology_present",
    "pmid_or_doi_for_gse167363_present"
  ),
  requirement = c(
    "GSE167363 placeholder is absent",
    "GSE167363 is still mentioned",
    "Qiu et al. source reference appears",
    "Journal of Leukocyte Biology appears",
    "PMID or DOI for GSE167363 source appears"
  ),
  observed = c(
    !has_pattern(doc_text, "Original publication or GEO record for GSE167363|Exact bibliographic details to be inserted", TRUE),
    has_pattern(doc_text, "GSE167363", TRUE),
    has_pattern(doc_text, "Qiu", TRUE),
    has_pattern(doc_text, "Journal of Leukocyte Biology|J Leukoc Biol", TRUE),
    has_pattern(doc_text, "34558746|10\\.", TRUE)
  ),
  stringsAsFactors = FALSE
)

reference_checks$status <- status_from_bool(reference_checks$observed)

# ============================================================
# 临时 Data availability 检查
# ============================================================

temporary_data_patterns <- c(
  "will be deposited before journal submission",
  "repository link to be filled",
  "before submission",
  "to be filled before submission",
  "will be made available"
)

temporary_data_hits <- do.call(
  rbind,
  lapply(temporary_data_patterns, function(p) line_hits_text(doc_lines, p, ignore.case = TRUE))
)

if (is.null(temporary_data_hits) || nrow(temporary_data_hits) == 0) {
  temporary_data_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

temporary_data_status <- ifelse(nrow(temporary_data_hits) == 0, "PASS", "CHECK")

# ============================================================
# 综合 checklist
# ============================================================

core_checks <- rbind(
  data.frame(category = "author", author_checks[, c("check_id", "requirement", "status")], stringsAsFactors = FALSE),
  data.frame(category = "declarations", declaration_checks[, c("check_id", "requirement", "status")], stringsAsFactors = FALSE),
  data.frame(category = "references", reference_checks[, c("check_id", "requirement", "status")], stringsAsFactors = FALSE)
)

extra_checks <- data.frame(
  category = c(
    "placeholders",
    "claims",
    "data_availability_temporary_language",
    "docx"
  ),
  check_id = c(
    "no_placeholders",
    "no_forbidden_claims",
    "no_temporary_data_availability_language",
    "docx_nonzero_size"
  ),
  requirement = c(
    "No manual or critical placeholders remain",
    "No forbidden high-risk claim phrases remain",
    "No temporary data availability language remains",
    "Word file exists and size > 0"
  ),
  status = c(
    ifelse(nrow(placeholder_hits) == 0, "PASS", "CHECK"),
    ifelse(nrow(forbidden_claim_hits) == 0, "PASS", "CHECK"),
    temporary_data_status,
    ifelse(file.exists(docx_file) && docx_size > 0, "PASS", "CHECK")
  ),
  stringsAsFactors = FALSE
)

full_checklist <- rbind(core_checks, extra_checks)

# ============================================================
# Manual action items
# ============================================================

manual_action_items <- data.frame(
  priority = character(),
  action_item = character(),
  blocks_final_submission = logical(),
  stringsAsFactors = FALSE
)

failed_checks <- full_checklist[full_checklist$status != "PASS", , drop = FALSE]

if (nrow(failed_checks) > 0) {
  manual_action_items <- rbind(
    manual_action_items,
    data.frame(
      priority = "Critical",
      action_item = paste0("Fix failed check: ", failed_checks$check_id, " | ", failed_checks$requirement),
      blocks_final_submission = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

if (nrow(placeholder_hits) > 0) {
  manual_action_items <- rbind(
    manual_action_items,
    data.frame(
      priority = "Critical",
      action_item = paste0(
        "Remove placeholder at Word text line ",
        placeholder_hits$line_number,
        ": ",
        substr(placeholder_hits$line_text, 1, 180)
      ),
      blocks_final_submission = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

if (nrow(temporary_data_hits) > 0) {
  manual_action_items <- rbind(
    manual_action_items,
    data.frame(
      priority = "High",
      action_item = paste0(
        "Resolve temporary Data availability wording at Word text line ",
        temporary_data_hits$line_number,
        ": ",
        substr(temporary_data_hits$line_text, 1, 180)
      ),
      blocks_final_submission = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

if (nrow(forbidden_claim_hits) > 0) {
  manual_action_items <- rbind(
    manual_action_items,
    data.frame(
      priority = "Critical",
      action_item = paste0(
        "Remove forbidden/high-risk claim at Word text line ",
        forbidden_claim_hits$line_number,
        ": ",
        substr(forbidden_claim_hits$line_text, 1, 180)
      ),
      blocks_final_submission = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

if (nrow(manual_action_items) == 0) {
  manual_action_items <- data.frame(
    priority = "None",
    action_item = "No blocking manual action detected by automated check. Manual visual review in Word is still recommended before submission.",
    blocks_final_submission = FALSE,
    stringsAsFactors = FALSE
  )
}

# ============================================================
# Overall status
# ============================================================

n_failed_checks <- sum(full_checklist$status != "PASS")
n_placeholder_hits <- nrow(placeholder_hits)
n_forbidden_hits <- nrow(forbidden_claim_hits)
n_temporary_data_hits <- nrow(temporary_data_hits)

ready_for_submission_by_script <- (
  n_failed_checks == 0 &&
    n_placeholder_hits == 0 &&
    n_forbidden_hits == 0 &&
    n_temporary_data_hits == 0
)

overall_status <- data.frame(
  metric = c(
    "docx_file",
    "docx_size_bytes",
    "n_failed_checks",
    "n_placeholder_hits",
    "n_forbidden_claim_hits",
    "n_temporary_data_availability_hits",
    "ready_for_submission_by_script",
    "recommended_next_step"
  ),
  value = c(
    docx_file,
    docx_size,
    n_failed_checks,
    n_placeholder_hits,
    n_forbidden_hits,
    n_temporary_data_hits,
    ifelse(ready_for_submission_by_script, "YES_AFTER_MANUAL_VISUAL_REVIEW", "NO_FIX_ITEMS_FIRST"),
    ifelse(
      ready_for_submission_by_script,
      "Proceed to manual visual Word review, figure export and final reference formatting.",
      "Fix listed manual action items, then rerun this script."
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Report
# ============================================================

report_lines <- c(
  "# BMC Genomics final submission check after manual edit v1.0",
  "",
  "## Overall status",
  "",
  paste0("- Word file: ", docx_file),
  paste0("- DOCX size bytes: ", docx_size),
  paste0("- Failed checks: ", n_failed_checks),
  paste0("- Placeholder hits: ", n_placeholder_hits),
  paste0("- Forbidden claim hits: ", n_forbidden_hits),
  paste0("- Temporary data availability hits: ", n_temporary_data_hits),
  paste0("- Ready for submission by script: ", ifelse(ready_for_submission_by_script, "YES_AFTER_MANUAL_VISUAL_REVIEW", "NO_FIX_ITEMS_FIRST")),
  "",
  "## Interpretation",
  "",
  "This script checks the manually edited Word manuscript for submission-blocking placeholders, missing declarations, reference placeholders and high-risk claims. It does not evaluate figure image quality, journal upload metadata, APC payment route or supervisor approval.",
  "",
  "## Manual action items",
  "",
  paste0(seq_len(nrow(manual_action_items)), ". [", manual_action_items$priority, "] ", manual_action_items$action_item),
  "",
  "## Next step",
  "",
  ifelse(
    ready_for_submission_by_script,
    "Open the Word file for final visual review. Then prepare separate figure files, supplementary files, cover letter and submission-system metadata.",
    "Fix the listed action items in the Word file and rerun 33."
  )
)

# ============================================================
# 输出
# ============================================================

message("Writing final submission check outputs...")

checklist_path <- file.path(draft_dir, "BMC_Genomics_final_submission_checklist_after_manual_edit_v1.0.csv")
manual_actions_path <- file.path(draft_dir, "BMC_Genomics_final_manual_action_items_v1.0.csv")
placeholder_hits_path <- file.path(draft_dir, "BMC_Genomics_final_placeholder_hits_v1.0.csv")
forbidden_hits_path <- file.path(draft_dir, "BMC_Genomics_final_forbidden_claim_hits_v1.0.csv")
temporary_data_hits_path <- file.path(draft_dir, "BMC_Genomics_final_temporary_data_availability_hits_v1.0.csv")
report_path <- file.path(draft_dir, "BMC_Genomics_final_submission_check_report_v1.0.md")
index_path <- file.path(draft_dir, "BMC_Genomics_final_submission_check_after_manual_edit_v1.0.xlsx")

data.table::fwrite(full_checklist, checklist_path)
data.table::fwrite(manual_action_items, manual_actions_path)
data.table::fwrite(placeholder_hits, placeholder_hits_path)
data.table::fwrite(forbidden_claim_hits, forbidden_hits_path)
data.table::fwrite(temporary_data_hits, temporary_data_hits_path)
write_text_file(report_lines, report_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "full_checklist")
openxlsx::writeData(wb, "full_checklist", full_checklist)

openxlsx::addWorksheet(wb, "author_checks")
openxlsx::writeData(wb, "author_checks", author_checks)

openxlsx::addWorksheet(wb, "declaration_checks")
openxlsx::writeData(wb, "declaration_checks", declaration_checks)

openxlsx::addWorksheet(wb, "reference_checks")
openxlsx::writeData(wb, "reference_checks", reference_checks)

openxlsx::addWorksheet(wb, "placeholder_hits")
openxlsx::writeData(wb, "placeholder_hits", placeholder_hits)

openxlsx::addWorksheet(wb, "temporary_data_hits")
openxlsx::writeData(wb, "temporary_data_hits", temporary_data_hits)

openxlsx::addWorksheet(wb, "forbidden_claim_hits")
openxlsx::writeData(wb, "forbidden_claim_hits", forbidden_claim_hits)

openxlsx::addWorksheet(wb, "manual_actions")
openxlsx::writeData(wb, "manual_actions", manual_action_items)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_33_BMC_Genomics_final_submission_check_after_manual_edit.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 33 BMC Genomics final submission check after manual edit 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nFull checklist failed items:")
print(full_checklist[full_checklist$status != "PASS", , drop = FALSE])

message("\nAuthor checks:")
print(author_checks)

message("\nDeclaration checks:")
print(declaration_checks)

message("\nReference checks:")
print(reference_checks)

message("\nPlaceholder hits:")
print(placeholder_hits)

message("\nTemporary data availability hits:")
print(temporary_data_hits)

message("\nForbidden claim hits:")
print(forbidden_claim_hits)

message("\nManual action items:")
print(manual_action_items)

message("\n关键输出：")
message("1) ", index_path)
message("2) ", checklist_path)
message("3) ", manual_actions_path)
message("4) ", report_path)
message("5) ", placeholder_hits_path)
message("6) ", temporary_data_hits_path)
message("7) ", forbidden_hits_path)

message("\n下一步：")
message("把 Overall status、Full checklist failed items、Placeholder hits、Temporary data availability hits 贴给我。")
message("如果 ready_for_submission_by_script = YES_AFTER_MANUAL_VISUAL_REVIEW，再进入 figure/supplement/cover letter 最终打包。")