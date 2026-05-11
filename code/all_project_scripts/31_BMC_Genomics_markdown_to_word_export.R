# ============================================================
# 31_BMC_Genomics_markdown_to_word_export.R
# Export BMC Genomics markdown manuscript to Word
#
# 目的：
# 1. 将 BMC_Genomics_full_manuscript_v0.5.3_preWord.md 导出为 Word
# 2. 优先使用 pandoc 转换，若不可用则使用 officer 兜底
# 3. 生成 Word 导出审计表
# 4. 不改 frozen numbers，不改分析结果，不改引用编号
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.3_preWord.md
#   BMC_Genomics_table_figure_captions_v0.6.md
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.6_preSubmission.docx
#   BMC_Genomics_full_manuscript_v0.6_preSubmission_combined.md
#   BMC_Genomics_word_export_check_v0.8.xlsx
#   BMC_Genomics_word_export_report_v0.8.md
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

manuscript_md <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.3_preWord.md")
caption_md <- file.path(draft_dir, "BMC_Genomics_table_figure_captions_v0.6.md")
preword_check <- file.path(draft_dir, "BMC_Genomics_preWord_check_v0.7.2.xlsx")

if (!file.exists(manuscript_md)) {
  stop("缺少 manuscript markdown 文件：", manuscript_md)
}

if (!file.exists(caption_md)) {
  stop("缺少 caption markdown 文件：", caption_md)
}

if (!file.exists(preword_check)) {
  warning("未发现 preWord check 文件，将继续导出，但建议先确认 30d 已通过：", preword_check)
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

has_pattern <- function(lines, pattern, ignore.case = TRUE) {
  any(grepl(pattern, lines, ignore.case = ignore.case, perl = TRUE))
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

clean_inline_markdown <- function(x) {
  x <- gsub("\\*\\*(.*?)\\*\\*", "\\1", x, perl = TRUE)
  x <- gsub("\\*(.*?)\\*", "\\1", x, perl = TRUE)
  x <- gsub("`(.*?)`", "\\1", x, perl = TRUE)
  x
}

find_pandoc <- function() {
  p <- Sys.which("pandoc")
  if (!is.na(p) && nzchar(p)) return(p)
  
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    if (rmarkdown::pandoc_available()) {
      return(rmarkdown::find_pandoc()$dir)
    }
  }
  
  return("")
}

# ============================================================
# 合并正文与图表标题
# ============================================================

message("Reading markdown files...")

ms <- read_lines_utf8(manuscript_md)
cap <- read_lines_utf8(caption_md)

combined_lines <- c(
  ms,
  "",
  "# Figure and table captions",
  "",
  cap
)

combined_md <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.6_preSubmission_combined.md")
write_text_file(combined_lines, combined_md)

# ============================================================
# 输出路径
# ============================================================

docx_out <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.6_preSubmission.docx")
docx_method <- NA_character_

# ============================================================
# 方法 A：优先使用 pandoc
# ============================================================

message("Checking pandoc availability...")

pandoc_path <- find_pandoc()
pandoc_available <- nzchar(pandoc_path)

if (pandoc_available) {
  message("Pandoc detected. Trying pandoc export...")
  
  cmd <- c(
    shQuote(combined_md),
    "-f", "markdown",
    "-t", "docx",
    "-o", shQuote(docx_out),
    "--standalone"
  )
  
  pandoc_exe <- if (file.exists(pandoc_path) && basename(pandoc_path) == "pandoc") {
    pandoc_path
  } else {
    Sys.which("pandoc")
  }
  
  status <- tryCatch(
    system2(pandoc_exe, args = cmd, stdout = TRUE, stderr = TRUE),
    error = function(e) {
      attr(e$message, "status") <- 1
      e$message
    }
  )
  
  if (file.exists(docx_out) && file.info(docx_out)$size > 0) {
    docx_method <- "pandoc"
  } else {
    warning("Pandoc export failed or produced no DOCX. Will try officer fallback.")
    docx_method <- NA_character_
  }
}

# ============================================================
# 方法 B：officer 兜底
# ============================================================

if (is.na(docx_method)) {
  message("Using officer fallback export...")
  
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("未安装 officer 包，且 pandoc 不可用。请安装 officer 或 pandoc 后重试。")
  }
  
  library(officer)
  
  doc <- officer::read_docx()
  available_styles <- officer::styles_info(doc)$style_name
  
  pick_style <- function(candidates, fallback = "Normal") {
    hit <- candidates[candidates %in% available_styles]
    if (length(hit) > 0) return(hit[1])
    fallback
  }
  
  style_h1 <- pick_style(c("heading 1", "Heading 1"), "Normal")
  style_h2 <- pick_style(c("heading 2", "Heading 2"), "Normal")
  style_h3 <- pick_style(c("heading 3", "Heading 3"), "Normal")
  style_normal <- pick_style(c("Normal", "normal"), "Normal")
  
  add_par_safe <- function(doc, value, style = style_normal) {
    value <- clean_inline_markdown(value)
    officer::body_add_par(doc, value = value, style = style)
  }
  
  add_markdown_line <- function(doc, line) {
    if (grepl("^###\\s+", line)) {
      txt <- sub("^###\\s+", "", line)
      doc <- add_par_safe(doc, txt, style_h3)
    } else if (grepl("^##\\s+", line)) {
      txt <- sub("^##\\s+", "", line)
      doc <- add_par_safe(doc, txt, style_h2)
    } else if (grepl("^#\\s+", line)) {
      txt <- sub("^#\\s+", "", line)
      doc <- add_par_safe(doc, txt, style_h1)
    } else if (grepl("^\\s*$", line)) {
      doc <- officer::body_add_par(doc, value = "", style = style_normal)
    } else if (grepl("^[-*]\\s+", line)) {
      txt <- sub("^[-*]\\s+", "", line)
      doc <- add_par_safe(doc, paste0("• ", txt), style_normal)
    } else if (grepl("^[0-9]+\\.\\s+", line)) {
      doc <- add_par_safe(doc, line, style_normal)
    } else {
      doc <- add_par_safe(doc, line, style_normal)
    }
    doc
  }
  
  for (ln in combined_lines) {
    doc <- add_markdown_line(doc, ln)
  }
  
  print(doc, target = docx_out)
  
  if (file.exists(docx_out) && file.info(docx_out)$size > 0) {
    docx_method <- "officer_fallback"
  } else {
    stop("officer 兜底导出失败，没有生成有效 DOCX。")
  }
}

# ============================================================
# 导出后审计
# ============================================================

message("Running export audit...")

docx_exists <- file.exists(docx_out)
docx_size <- ifelse(docx_exists, file.info(docx_out)$size, NA)

abstract_lines <- extract_between_headers(ms, "# Abstract", "# Background")
main_text_lines <- c(
  extract_between_headers(ms, "# Background", "# Methods"),
  extract_between_headers(ms, "# Methods", "# Results"),
  extract_between_headers(ms, "# Results", "# Discussion"),
  extract_between_headers(ms, "# Discussion", "# List of abbreviations")
)

placeholder_patterns <- c(
  "REF_[A-Z0-9_]+",
  "TODO",
  "TBD"
)

placeholder_hits <- do.call(
  rbind,
  lapply(placeholder_patterns, function(p) line_hits(combined_lines, p, ignore.case = TRUE))
)

if (is.null(placeholder_hits) || nrow(placeholder_hits) == 0) {
  placeholder_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

manual_placeholder_patterns <- c(
  "\\[Author names to be completed\\]",
  "\\[Affiliations to be completed\\]",
  "\\[Name, address, email to be completed\\]",
  "Exact bibliographic details to be inserted"
)

manual_placeholder_hits <- do.call(
  rbind,
  lapply(manual_placeholder_patterns, function(p) line_hits(combined_lines, p, ignore.case = TRUE))
)

if (is.null(manual_placeholder_hits) || nrow(manual_placeholder_hits) == 0) {
  manual_placeholder_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

required_strings <- c(
  "# Title page",
  "# Abstract",
  "# Background",
  "# Methods",
  "# Results",
  "# Discussion",
  "# Declarations",
  "# References",
  "# Figure and table captions"
)

required_check <- data.frame(
  item = required_strings,
  present_in_markdown = vapply(required_strings, function(x) any(combined_lines == x), logical(1)),
  stringsAsFactors = FALSE
)

required_check$status <- ifelse(required_check$present_in_markdown, "PASS", "CHECK")

export_check <- data.frame(
  check_id = c(
    "docx_exists",
    "docx_nonzero_size",
    "export_method_recorded",
    "abstract_word_count_le_350",
    "main_text_present",
    "no_critical_REF_TODO_TBD",
    "manual_placeholders_recorded"
  ),
  requirement = c(
    "Word file exists",
    "Word file size greater than zero",
    "Export method recorded",
    "Abstract word count <=350",
    "Main text sections present",
    "No REF/TODO/TBD critical placeholders remain",
    "Manual placeholders are recorded for later completion"
  ),
  observed = c(
    docx_exists,
    ifelse(!is.na(docx_size), docx_size > 0, FALSE),
    !is.na(docx_method),
    count_words_simple(abstract_lines) <= 350,
    length(main_text_lines) > 0,
    nrow(placeholder_hits) == 0,
    nrow(manual_placeholder_hits) >= 0
  ),
  status = c(
    ifelse(docx_exists, "PASS", "CHECK"),
    ifelse(!is.na(docx_size) && docx_size > 0, "PASS", "CHECK"),
    ifelse(!is.na(docx_method), "PASS", "CHECK"),
    ifelse(count_words_simple(abstract_lines) <= 350, "PASS", "CHECK"),
    ifelse(length(main_text_lines) > 0, "PASS", "CHECK"),
    ifelse(nrow(placeholder_hits) == 0, "PASS", "CHECK"),
    "PASS"
  ),
  stringsAsFactors = FALSE
)

manual_action_items <- data.frame(
  priority = c(
    "Critical",
    "Critical",
    "Critical",
    "Critical",
    "High",
    "High",
    "High"
  ),
  action_item = c(
    "Fill author names.",
    "Fill author affiliations.",
    "Fill corresponding author name, address and email.",
    "Replace GSE167363 placeholder reference with exact source publication or GEO record citation.",
    "Complete Ethics approval and consent to participate statement.",
    "Complete Availability of data and materials statement with GEO accessions and code repository details.",
    "Verify all references in Zotero before submission."
  ),
  blocks_word_export = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  blocks_final_submission = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

overall_status <- data.frame(
  metric = c(
    "docx_output",
    "docx_size_bytes",
    "export_method",
    "n_failed_export_checks",
    "n_critical_placeholder_hits",
    "n_manual_placeholder_hits",
    "ready_for_manual_word_review",
    "ready_for_final_submission"
  ),
  value = c(
    docx_out,
    docx_size,
    docx_method,
    sum(export_check$status != "PASS"),
    nrow(placeholder_hits),
    nrow(manual_placeholder_hits),
    ifelse(sum(export_check$status != "PASS") == 0, "YES", "CHECK"),
    "NO_MANUAL_COMPLETION_REQUIRED"
  ),
  stringsAsFactors = FALSE
)

report_lines <- c(
  "# BMC Genomics Word export report v0.8",
  "",
  "## Output",
  "",
  paste0("- DOCX: ", docx_out),
  paste0("- Combined markdown: ", combined_md),
  paste0("- Export method: ", docx_method),
  paste0("- DOCX size bytes: ", docx_size),
  "",
  "## Status",
  "",
  paste0("- Failed export checks: ", sum(export_check$status != "PASS")),
  paste0("- Critical REF/TODO/TBD placeholder hits: ", nrow(placeholder_hits)),
  paste0("- Manual placeholder hits: ", nrow(manual_placeholder_hits)),
  paste0("- Ready for manual Word review: ", ifelse(sum(export_check$status != "PASS") == 0, "YES", "CHECK")),
  "- Ready for final submission: NO, manual completion still required.",
  "",
  "## Manual completion before final submission",
  "",
  paste0(seq_len(nrow(manual_action_items)), ". ", manual_action_items$action_item),
  "",
  "## Next step",
  "",
  "Open the Word file and check title page, section headings, references, declarations and figure/table captions. Then proceed to 32_BMC_Genomics_manual_fields_completion_plan.R."
)

# ============================================================
# 输出审计文件
# ============================================================

audit_xlsx <- file.path(draft_dir, "BMC_Genomics_word_export_check_v0.8.xlsx")
export_check_csv <- file.path(draft_dir, "BMC_Genomics_word_export_checklist_v0.8.csv")
manual_actions_csv <- file.path(draft_dir, "BMC_Genomics_word_export_manual_actions_v0.8.csv")
manual_placeholder_csv <- file.path(draft_dir, "BMC_Genomics_word_export_manual_placeholder_hits_v0.8.csv")
critical_placeholder_csv <- file.path(draft_dir, "BMC_Genomics_word_export_critical_placeholder_hits_v0.8.csv")
report_md <- file.path(draft_dir, "BMC_Genomics_word_export_report_v0.8.md")

data.table::fwrite(export_check, export_check_csv)
data.table::fwrite(manual_action_items, manual_actions_csv)
data.table::fwrite(manual_placeholder_hits, manual_placeholder_csv)
data.table::fwrite(placeholder_hits, critical_placeholder_csv)
write_text_file(report_lines, report_md)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "export_check")
openxlsx::writeData(wb, "export_check", export_check)

openxlsx::addWorksheet(wb, "required_sections")
openxlsx::writeData(wb, "required_sections", required_check)

openxlsx::addWorksheet(wb, "manual_actions")
openxlsx::writeData(wb, "manual_actions", manual_action_items)

openxlsx::addWorksheet(wb, "manual_placeholder_hits")
openxlsx::writeData(wb, "manual_placeholder_hits", manual_placeholder_hits)

openxlsx::addWorksheet(wb, "critical_placeholder_hits")
openxlsx::writeData(wb, "critical_placeholder_hits", placeholder_hits)

openxlsx::saveWorkbook(wb, audit_xlsx, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_31_BMC_Genomics_markdown_to_word_export.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 31 BMC Genomics markdown to Word export 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nExport check:")
print(export_check)

message("\nManual placeholder hits:")
print(manual_placeholder_hits)

message("\nCritical placeholder hits:")
print(placeholder_hits)

message("\nManual action items:")
print(manual_action_items)

message("\n关键输出：")
message("1) ", docx_out)
message("2) ", combined_md)
message("3) ", audit_xlsx)
message("4) ", export_check_csv)
message("5) ", manual_actions_csv)
message("6) ", report_md)

message("\n下一步：")
message("打开 Word 文件，人工检查正文、参考文献、Declarations 和图表标题。")
message("把 Overall status、Export check、Manual placeholder hits 贴给我。")
message("如果 ready_for_manual_word_review = YES，就继续 32_BMC_Genomics_manual_fields_completion_plan.R。")