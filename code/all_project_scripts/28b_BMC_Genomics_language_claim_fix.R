# ============================================================
# 28b_BMC_Genomics_language_claim_fix.R
# Fix residual language audit issues after v0.5 polish
#
# 目的：
# 1. 修正 same-intended-use clinical validation 的误报风险表达
# 2. 修正 pooled AUROC spotcheck 对 rounded number 的识别
# 3. 输出 v0.5.1 polished manuscript
# 4. 不改任何分析结果，不重算模型，不改引用编号
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5_polished.md
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.1_polished.md
#   BMC_Genomics_language_claim_audit_v0.5.1.csv
#   BMC_Genomics_forbidden_phrase_hits_v0.5.1.csv
#   BMC_Genomics_number_consistency_spotcheck_v0.5.1.csv
#   BMC_Genomics_language_claim_fix_report_v0.5.1.md
#   BMC_Genomics_language_claim_fix_v0.5.1_index.xlsx
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

manuscript_v05 <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5_polished.md")
key_snapshot_file <- file.path(freeze_dir, "final_key_result_snapshot.csv")

if (!file.exists(manuscript_v05)) {
  stop("缺少 manuscript v0.5 文件：", manuscript_v05)
}

if (!file.exists(key_snapshot_file)) {
  stop("缺少 final freeze key snapshot：", key_snapshot_file)
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

count_words_simple <- function(lines) {
  txt <- paste(lines, collapse = " ")
  txt <- gsub("#+", " ", txt)
  txt <- gsub("\\[[0-9,\\-]+\\]", " ", txt)
  txt <- gsub("[[:punct:]]+", " ", txt)
  words <- unlist(strsplit(txt, "\\s+"))
  words <- words[words != ""]
  length(words)
}

extract_between_headers <- function(lines, start_header, end_header = NULL) {
  start_idx <- grep(paste0("^", start_header, "$"), lines)
  if (length(start_idx) == 0) return(character())
  start_idx <- start_idx[1]
  
  if (is.null(end_header)) {
    return(lines[start_idx:length(lines)])
  }
  
  end_idx <- grep(paste0("^", end_header, "$"), lines)
  end_idx <- end_idx[end_idx > start_idx]
  
  if (length(end_idx) == 0) {
    return(lines[start_idx:length(lines)])
  }
  
  lines[start_idx:(end_idx[1] - 1)]
}

contains_any <- function(lines, patterns, ignore.case = TRUE) {
  txt <- paste(lines, collapse = "\n")
  vapply(
    patterns,
    function(p) grepl(p, txt, ignore.case = ignore.case, perl = TRUE),
    logical(1)
  )
}

line_hits <- function(lines, pattern, ignore.case = TRUE) {
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

replace_all_fixed <- function(lines, pattern, replacement) {
  gsub(pattern, replacement, lines, fixed = TRUE)
}

# ============================================================
# 读取稿件
# ============================================================

message("Reading v0.5 manuscript...")

ms <- read_lines_utf8(manuscript_v05)
key_snapshot <- read_csv_df(key_snapshot_file)

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
nested_median_auc <- get_value(key_snapshot, "nested_LODO_median_AUROC")
pooled_auc <- get_value(key_snapshot, "pooled_nested_LODO_AUROC")
median_threshold_shift <- get_value(key_snapshot, "median_abs_threshold_shift")
rna_status <- get_value(key_snapshot, "RNAseq_validation_status")
scRNA_top <- get_value(key_snapshot, "scRNA_top_Final10_celltype")

nested_median_auc_rounded <- fmt_num(nested_median_auc, 3)
pooled_auc_rounded <- fmt_num(pooled_auc, 3)
threshold_shift_rounded <- fmt_num(median_threshold_shift, 3)

# ============================================================
# 修正高风险误报表达
# ============================================================

message("Fixing residual claim wording...")

ms_fixed <- ms

ms_fixed <- replace_all_fixed(
  ms_fixed,
  "rather than same-intended-use clinical validation",
  "rather than deployment-oriented clinical validation"
)

ms_fixed <- replace_all_fixed(
  ms_fixed,
  "These differences were retained and summarized explicitly to support a transportability-oriented interpretation rather than same-intended-use clinical validation.",
  "These differences were retained and summarized explicitly to support a transportability-oriented interpretation rather than a deployment-oriented clinical validation claim."
)

# 进一步统一一个常见语句，防止 clinical validation 被误读为本文目的
ms_fixed <- replace_all_fixed(
  ms_fixed,
  "We framed the analysis as a transportability evaluation rather than deployment-oriented clinical validation, with emphasis on leakage-resistant nested validation, calibration, threshold behavior and cellular localization.",
  "We framed the analysis as a transportability evaluation rather than a deployment-oriented validation study, with emphasis on leakage-resistant nested validation, calibration, threshold behavior and cellular localization."
)

# ============================================================
# Audit
# ============================================================

message("Running v0.5.1 audit...")

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

required_patterns <- c(
  "strict nested leave-one-dataset-out",
  "held-out dataset",
  "threshold",
  "calibration",
  "biological localization",
  "not considered independent diagnostic validation",
  "RNA-seq validation was therefore recorded as a No-Go decision",
  "does not support immediate clinical deployment"
)

forbidden_hits <- do.call(
  rbind,
  lapply(forbidden_patterns, function(p) line_hits(ms_fixed, p, ignore.case = TRUE))
)

if (is.null(forbidden_hits) || nrow(forbidden_hits) == 0) {
  forbidden_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

required_present <- contains_any(ms_fixed, required_patterns, ignore.case = TRUE)

required_audit <- data.frame(
  check_type = "required_boundary_phrase",
  pattern = required_patterns,
  present = as.logical(required_present),
  status = ifelse(required_present, "PASS", "CHECK"),
  stringsAsFactors = FALSE
)

forbidden_audit <- data.frame(
  check_type = "forbidden_or_high_risk_phrase",
  pattern = forbidden_patterns,
  present = forbidden_patterns %in% unique(forbidden_hits$pattern),
  status = ifelse(forbidden_patterns %in% unique(forbidden_hits$pattern), "CHECK", "PASS"),
  stringsAsFactors = FALSE
)

claim_audit <- rbind(required_audit, forbidden_audit)

abstract_lines <- extract_between_headers(ms_fixed, "# Abstract", "# Background")
background_lines <- extract_between_headers(ms_fixed, "# Background", "# Methods")
methods_lines <- extract_between_headers(ms_fixed, "# Methods", "# Results")
results_lines <- extract_between_headers(ms_fixed, "# Results", "# Discussion")
discussion_lines <- extract_between_headers(ms_fixed, "# Discussion", "# List of abbreviations")

word_count_table <- data.frame(
  section = c("Abstract", "Background", "Methods", "Results", "Discussion"),
  word_count = c(
    count_words_simple(abstract_lines),
    count_words_simple(background_lines),
    count_words_simple(methods_lines),
    count_words_simple(results_lines),
    count_words_simple(discussion_lines)
  ),
  notes = c(
    "BMC Genomics abstract should be <=350 words.",
    "Should establish gap without overclaiming novelty.",
    "Should clearly document nested LODO and no leakage.",
    "Should report results without clinical deployment language.",
    "Should emphasize limitations and restricted claims."
  ),
  stringsAsFactors = FALSE
)

number_checks <- data.frame(
  item = c(
    "bulk_total_samples",
    "nested_LODO_median_AUROC",
    "pooled_nested_LODO_AUROC",
    "median_abs_threshold_shift",
    "RNAseq_validation_status",
    "scRNA_top_celltype"
  ),
  expected = c(
    bulk_total,
    nested_median_auc_rounded,
    pooled_auc_rounded,
    threshold_shift_rounded,
    rna_status,
    scRNA_top
  ),
  detected_in_text = c(
    any(grepl(bulk_total, ms_fixed, fixed = TRUE)),
    any(grepl(nested_median_auc_rounded, ms_fixed, fixed = TRUE)),
    any(grepl(pooled_auc_rounded, ms_fixed, fixed = TRUE)),
    any(grepl(threshold_shift_rounded, ms_fixed, fixed = TRUE)),
    any(grepl("No-Go", ms_fixed, fixed = TRUE)),
    any(grepl(scRNA_top, ms_fixed, fixed = TRUE))
  ),
  status = ifelse(
    c(
      any(grepl(bulk_total, ms_fixed, fixed = TRUE)),
      any(grepl(nested_median_auc_rounded, ms_fixed, fixed = TRUE)),
      any(grepl(pooled_auc_rounded, ms_fixed, fixed = TRUE)),
      any(grepl(threshold_shift_rounded, ms_fixed, fixed = TRUE)),
      any(grepl("No-Go", ms_fixed, fixed = TRUE)),
      any(grepl(scRNA_top, ms_fixed, fixed = TRUE))
    ),
    "PASS",
    "CHECK"
  ),
  stringsAsFactors = FALSE
)

overall_status <- data.frame(
  metric = c(
    "n_forbidden_phrase_hits",
    "n_required_boundary_checks_failed",
    "abstract_word_count",
    "ready_for_next_step"
  ),
  value = c(
    nrow(forbidden_hits),
    sum(required_audit$status != "PASS"),
    word_count_table$word_count[word_count_table$section == "Abstract"],
    ifelse(
      nrow(forbidden_hits) == 0 &&
        sum(required_audit$status != "PASS") == 0 &&
        word_count_table$word_count[word_count_table$section == "Abstract"] <= 350 &&
        all(number_checks$status == "PASS"),
      "YES",
      "CHECK_BEFORE_NEXT_STEP"
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Report
# ============================================================

report_lines <- c(
  "# BMC Genomics language claim fix report v0.5.1",
  "",
  "## Input",
  "",
  paste0("- Manuscript v0.5: ", manuscript_v05),
  "",
  "## Main fixes",
  "",
  "1. Replaced 'same-intended-use clinical validation' with safer deployment-oriented wording.",
  "2. Updated number spotcheck to compare rounded manuscript values against rounded frozen values.",
  "3. No analysis was rerun.",
  "4. No frozen numeric result was intentionally changed.",
  "5. Citation numbering was preserved.",
  "",
  "## Overall status",
  "",
  paste0("- Forbidden phrase hits: ", nrow(forbidden_hits)),
  paste0("- Required boundary checks failed: ", sum(required_audit$status != "PASS")),
  paste0("- Abstract word count: ", word_count_table$word_count[word_count_table$section == "Abstract"]),
  paste0("- Ready for next step: ", overall_status$value[overall_status$metric == "ready_for_next_step"]),
  "",
  "## Next step",
  "",
  "If ready_for_next_step = YES, proceed to 29_BMC_Genomics_table_figure_caption_draft.R."
)

# ============================================================
# 输出
# ============================================================

message("Writing v0.5.1 fixed manuscript and audit outputs...")

manuscript_v051_path <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.1_polished.md")
claim_audit_path <- file.path(draft_dir, "BMC_Genomics_language_claim_audit_v0.5.1.csv")
forbidden_hits_path <- file.path(draft_dir, "BMC_Genomics_forbidden_phrase_hits_v0.5.1.csv")
word_count_path <- file.path(draft_dir, "BMC_Genomics_section_word_counts_v0.5.1.csv")
number_check_path <- file.path(draft_dir, "BMC_Genomics_number_consistency_spotcheck_v0.5.1.csv")
report_path <- file.path(draft_dir, "BMC_Genomics_language_claim_fix_report_v0.5.1.md")
index_path <- file.path(draft_dir, "BMC_Genomics_language_claim_fix_v0.5.1_index.xlsx")

write_text_file(ms_fixed, manuscript_v051_path)
data.table::fwrite(claim_audit, claim_audit_path)
data.table::fwrite(forbidden_hits, forbidden_hits_path)
data.table::fwrite(word_count_table, word_count_path)
data.table::fwrite(number_checks, number_check_path)
write_text_file(report_lines, report_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_full_manuscript_v0.5.1_polished.md",
    "BMC_Genomics_language_claim_audit_v0.5.1.csv",
    "BMC_Genomics_forbidden_phrase_hits_v0.5.1.csv",
    "BMC_Genomics_section_word_counts_v0.5.1.csv",
    "BMC_Genomics_number_consistency_spotcheck_v0.5.1.csv",
    "BMC_Genomics_language_claim_fix_report_v0.5.1.md",
    "BMC_Genomics_language_claim_fix_v0.5.1_index.xlsx"
  ),
  path = c(
    manuscript_v051_path,
    claim_audit_path,
    forbidden_hits_path,
    word_count_path,
    number_check_path,
    report_path,
    index_path
  ),
  purpose = c(
    "Fixed polished manuscript markdown",
    "Required and forbidden claim audit",
    "Line-level forbidden/high-risk phrase hits",
    "Section word counts",
    "Rounded frozen number spotcheck",
    "Human-readable fix report",
    "Workbook index"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "claim_audit")
openxlsx::writeData(wb, "claim_audit", claim_audit)

openxlsx::addWorksheet(wb, "forbidden_hits")
openxlsx::writeData(wb, "forbidden_hits", forbidden_hits)

openxlsx::addWorksheet(wb, "word_counts")
openxlsx::writeData(wb, "word_counts", word_count_table)

openxlsx::addWorksheet(wb, "number_spotcheck")
openxlsx::writeData(wb, "number_spotcheck", number_checks)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_28b_BMC_Genomics_language_claim_fix.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 28b BMC Genomics language claim fix 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nForbidden phrase hits:")
print(forbidden_hits)

message("\nNumber consistency spotcheck:")
print(number_checks)

message("\nWord counts:")
print(word_count_table)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", manuscript_v051_path)
message("2) ", claim_audit_path)
message("3) ", forbidden_hits_path)
message("4) ", number_check_path)
message("5) ", report_path)
message("6) ", index_path)

message("\n下一步：")
message("把 Overall status、Forbidden phrase hits、Number consistency spotcheck 贴给我。")
message("如果 ready_for_next_step = YES，就继续 29_BMC_Genomics_table_figure_caption_draft.R。")