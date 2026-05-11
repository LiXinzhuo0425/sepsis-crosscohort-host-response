# ============================================================
# 28_BMC_Genomics_language_and_claim_polish.R
# Language and claim polish for BMC Genomics manuscript v0.4
#
# 目的：
# 1. 基于 v0.4 referenced manuscript 进行语言和 claim 风险打磨
# 2. 不新增分析，不重算任何结果，不改 frozen numbers
# 3. 保留引用编号，不重新排序 references
# 4. 输出 v0.5 polished markdown
# 5. 输出 claim/language audit table
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.4_referenced.md
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5_polished.md
#   BMC_Genomics_language_claim_audit_v0.5.csv
#   BMC_Genomics_language_polish_report_v0.5.md
#   BMC_Genomics_language_polish_v0.5_index.xlsx
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

manuscript_v04 <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.4_referenced.md")
key_snapshot_file <- file.path(freeze_dir, "final_key_result_snapshot.csv")

if (!file.exists(manuscript_v04)) {
  stop("缺少 manuscript v0.4 文件：", manuscript_v04)
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

replace_line_exact <- function(lines, old, new) {
  idx <- which(lines == old)
  if (length(idx) > 0) {
    lines[idx] <- new
  }
  lines
}

replace_all_fixed <- function(lines, pattern, replacement) {
  gsub(pattern, replacement, lines, fixed = TRUE)
}

# ============================================================
# 读取稿件和 frozen numbers
# ============================================================

message("Reading v0.4 manuscript...")

ms <- read_lines_utf8(manuscript_v04)
key_snapshot <- read_csv_df(key_snapshot_file)

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
nested_median_auc <- get_value(key_snapshot, "nested_LODO_median_AUROC")
pooled_auc <- get_value(key_snapshot, "pooled_nested_LODO_AUROC")
median_threshold_shift <- get_value(key_snapshot, "median_abs_threshold_shift")
rna_status <- get_value(key_snapshot, "RNAseq_validation_status")
scRNA_top <- get_value(key_snapshot, "scRNA_top_Final10_celltype")

# ============================================================
# 语言和 claim 打磨
# ============================================================

message("Applying conservative language polish...")

ms_polished <- ms

# ------------------------------------------------------------
# 1. 统一关键词 Monocyte -> Myeloid cell
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "Sepsis; Transcriptomics; Host response; Nested validation; Transportability; Calibration; Single-cell RNA sequencing; Monocyte; Diagnostic signature",
  "Sepsis; Transcriptomics; Host response; Nested validation; Transportability; Calibration; Single-cell RNA sequencing; Myeloid cell; Diagnostic signature"
)

# ------------------------------------------------------------
# 2. 强化非部署 claim
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "This study was designed as a secondary analysis of publicly available blood transcriptomic datasets to evaluate the cross-cohort transportability of a host-response signature for sepsis.",
  "This study was designed as a secondary analysis of publicly available blood transcriptomic datasets to evaluate the cross-cohort transportability of a host-response signature for sepsis, without making a clinical deployment claim."
)

ms_polished <- replace_all_fixed(
  ms_polished,
  "The study does not support immediate clinical deployment of the signature.",
  "The study does not support immediate clinical deployment of the signature or direct adoption of a fixed operating threshold."
)

# ------------------------------------------------------------
# 3. 强化 nested LODO 防泄漏表达
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "The held-out dataset was used only once for validation.",
  "The held-out dataset was used only once for validation, after the training-only feature selection, preprocessing, model fitting and threshold-selection steps had been completed."
)

ms_polished <- replace_all_fixed(
  ms_polished,
  "This design is more conservative than workflows in which genes are selected using all datasets before external validation.",
  "This design is more conservative than workflows in which genes are selected using all datasets before external validation, because it evaluates the transportability of the entire modeling procedure rather than a model built after observing the validation cohorts."
)

# ------------------------------------------------------------
# 4. 降低 single-cell 过度解释
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "Single-cell analysis was interpreted as biological localization only and was not considered independent diagnostic validation.",
  "Single-cell analysis was interpreted as biological localization only and was not considered independent diagnostic validation or evidence of patient-level diagnostic accuracy."
)

ms_polished <- replace_all_fixed(
  ms_polished,
  "These findings supported a predominantly monocyte/myeloid cellular origin of the transported host-response signal.",
  "These findings supported a predominantly monocyte/myeloid cellular localization of the transported host-response signal."
)

ms_polished <- replace_all_fixed(
  ms_polished,
  "This supports the interpretation that the transported bulk signal is predominantly driven by monocyte/myeloid transcriptional programs.",
  "This supports the interpretation that the transported bulk signal is predominantly localized to monocyte/myeloid transcriptional programs."
)

# ------------------------------------------------------------
# 5. 降低 DCA 过度解释
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "Across pooled nested LODO predictions, the model was clinically preferable over both treat-all and treat-none strategies across",
  "Across pooled nested LODO predictions, the model showed higher exploratory net benefit than both treat-all and treat-none strategies across"
)

ms_polished <- replace_all_fixed(
  ms_polished,
  "Exploratory decision-curve analysis showed heterogeneous net-benefit patterns.",
  "Exploratory decision-curve analysis showed heterogeneous threshold-dependent net-benefit patterns."
)

# ------------------------------------------------------------
# 6. RNA-seq No-Go 表述更稳
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "RNA-seq validation was therefore recorded as a No-Go decision and was not used to modify the model, thresholds or conclusions.",
  "RNA-seq validation was therefore recorded as a No-Go decision and was not used to modify the model, thresholds, result interpretation or conclusions."
)

# ------------------------------------------------------------
# 7. 减少万能性表述
# ------------------------------------------------------------

ms_polished <- replace_all_fixed(
  ms_polished,
  "This pattern indicated that the signature could transport in several settings but was not universally robust across all available public cohorts.",
  "This pattern indicated that the signature transported in several available settings but did not show uniform robustness across all public cohorts."
)

ms_polished <- replace_all_fixed(
  ms_polished,
  "The latter point is central for diagnostic translation, because a biomarker or model that ranks patients well may still fail when a fixed decision threshold is applied in a new cohort.",
  "The latter point is central for diagnostic translation, because a biomarker or model that ranks patients well may still behave poorly when a fixed decision threshold is transferred to a new cohort."
)

# ------------------------------------------------------------
# 8. Background 末段补一句 novelty boundary
# ------------------------------------------------------------

old_bg_sentence <- "We framed the analysis as a transportability evaluation rather than same-intended-use clinical validation."
new_bg_sentence <- paste0(
  "We framed the analysis as a transportability evaluation rather than same-intended-use clinical validation, ",
  "with emphasis on leakage-resistant nested validation, calibration, threshold behavior and cellular localization."
)

ms_polished <- replace_all_fixed(ms_polished, old_bg_sentence, new_bg_sentence)

# ------------------------------------------------------------
# 9. References placeholder 已经替换，避免残留说明句
# ------------------------------------------------------------

ms_polished <- ms_polished[!grepl("References to be inserted after citation replacement", ms_polished, fixed = TRUE)]

# ============================================================
# Audit: 风险表达检查
# ============================================================

message("Running claim and language audit...")

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
  lapply(forbidden_patterns, function(p) line_hits(ms_polished, p, ignore.case = TRUE))
)

if (is.null(forbidden_hits) || nrow(forbidden_hits) == 0) {
  forbidden_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

required_present <- contains_any(ms_polished, required_patterns, ignore.case = TRUE)

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

# ============================================================
# Section word counts
# ============================================================

abstract_lines <- extract_between_headers(ms_polished, "# Abstract", "# Background")
background_lines <- extract_between_headers(ms_polished, "# Background", "# Methods")
methods_lines <- extract_between_headers(ms_polished, "# Methods", "# Results")
results_lines <- extract_between_headers(ms_polished, "# Results", "# Discussion")
discussion_lines <- extract_between_headers(ms_polished, "# Discussion", "# List of abbreviations")

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

# ============================================================
# 数字一致性粗查
# ============================================================

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
    nested_median_auc,
    pooled_auc,
    median_threshold_shift,
    rna_status,
    scRNA_top
  ),
  detected_in_text = c(
    any(grepl(bulk_total, ms_polished, fixed = TRUE)),
    any(grepl(substr(nested_median_auc, 1, 5), ms_polished, fixed = TRUE)),
    any(grepl(substr(pooled_auc, 1, 5), ms_polished, fixed = TRUE)),
    any(grepl(substr(median_threshold_shift, 1, 5), ms_polished, fixed = TRUE)),
    any(grepl("No-Go", ms_polished, fixed = TRUE)),
    any(grepl(scRNA_top, ms_polished, fixed = TRUE))
  ),
  status = ifelse(
    c(
      any(grepl(bulk_total, ms_polished, fixed = TRUE)),
      any(grepl(substr(nested_median_auc, 1, 5), ms_polished, fixed = TRUE)),
      any(grepl(substr(pooled_auc, 1, 5), ms_polished, fixed = TRUE)),
      any(grepl(substr(median_threshold_shift, 1, 5), ms_polished, fixed = TRUE)),
      any(grepl("No-Go", ms_polished, fixed = TRUE)),
      any(grepl(scRNA_top, ms_polished, fixed = TRUE))
    ),
    "PASS",
    "CHECK"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 综合 audit table
# ============================================================

claim_audit <- rbind(
  required_audit,
  forbidden_audit
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
        word_count_table$word_count[word_count_table$section == "Abstract"] <= 350,
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
  "# BMC Genomics language and claim polish report v0.5",
  "",
  "## Input",
  "",
  paste0("- Manuscript v0.4: ", manuscript_v04),
  "",
  "## Output",
  "",
  "- Manuscript v0.5 has conservative language polish.",
  "- No analysis was rerun.",
  "- Frozen numbers were not intentionally changed.",
  "- Citation numbering was preserved.",
  "",
  "## Main edits applied",
  "",
  "1. Replaced keyword Monocyte with Myeloid cell.",
  "2. Strengthened non-deployment language.",
  "3. Strengthened nested LODO leakage-control wording.",
  "4. Reduced overinterpretation of single-cell localization.",
  "5. Reduced overinterpretation of decision-curve analysis.",
  "6. Preserved RNA-seq No-Go framing.",
  "7. Added novelty-boundary wording in Background.",
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
# 输出文件
# ============================================================

message("Writing polished manuscript and audit outputs...")

manuscript_v05_path <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5_polished.md")
claim_audit_path <- file.path(draft_dir, "BMC_Genomics_language_claim_audit_v0.5.csv")
forbidden_hits_path <- file.path(draft_dir, "BMC_Genomics_forbidden_phrase_hits_v0.5.csv")
word_count_path <- file.path(draft_dir, "BMC_Genomics_section_word_counts_v0.5.csv")
number_check_path <- file.path(draft_dir, "BMC_Genomics_number_consistency_spotcheck_v0.5.csv")
report_path <- file.path(draft_dir, "BMC_Genomics_language_polish_report_v0.5.md")
index_path <- file.path(draft_dir, "BMC_Genomics_language_polish_v0.5_index.xlsx")

write_text_file(ms_polished, manuscript_v05_path)
data.table::fwrite(claim_audit, claim_audit_path)
data.table::fwrite(forbidden_hits, forbidden_hits_path)
data.table::fwrite(word_count_table, word_count_path)
data.table::fwrite(number_checks, number_check_path)
write_text_file(report_lines, report_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_full_manuscript_v0.5_polished.md",
    "BMC_Genomics_language_claim_audit_v0.5.csv",
    "BMC_Genomics_forbidden_phrase_hits_v0.5.csv",
    "BMC_Genomics_section_word_counts_v0.5.csv",
    "BMC_Genomics_number_consistency_spotcheck_v0.5.csv",
    "BMC_Genomics_language_polish_report_v0.5.md",
    "BMC_Genomics_language_polish_v0.5_index.xlsx"
  ),
  path = c(
    manuscript_v05_path,
    claim_audit_path,
    forbidden_hits_path,
    word_count_path,
    number_check_path,
    report_path,
    index_path
  ),
  purpose = c(
    "Polished full manuscript markdown",
    "Required and forbidden claim audit",
    "Line-level forbidden/high-risk phrase hits",
    "Section word counts",
    "Spot check for key frozen numbers",
    "Human-readable polish report",
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

sink(file.path(log_dir, "sessionInfo_28_BMC_Genomics_language_and_claim_polish.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 28 BMC Genomics language and claim polish 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nWord counts:")
print(word_count_table)

message("\nClaim audit:")
print(claim_audit)

message("\nForbidden phrase hits:")
print(forbidden_hits)

message("\nNumber consistency spotcheck:")
print(number_checks)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", manuscript_v05_path)
message("2) ", claim_audit_path)
message("3) ", forbidden_hits_path)
message("4) ", word_count_path)
message("5) ", number_check_path)
message("6) ", report_path)
message("7) ", index_path)

message("\n下一步：")
message("把 Overall status、Forbidden phrase hits、Number consistency spotcheck 贴给我。")
message("如果 ready_for_next_step = YES，就继续 29_BMC_Genomics_table_figure_caption_draft.R。")