# ============================================================
# 30d_BMC_Genomics_declarations_heading_fix.R
# Fix missing exact "# Declarations" heading before Word conversion
#
# 目的：
# 1. 将 manuscript 中的 Declarations 总标题统一为精确的 "# Declarations"
# 2. 不改分析结果，不改引用编号，不改数据
# 3. 重新检查 Word conversion readiness
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.2_preWord.md
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.3_preWord.md
#   BMC_Genomics_preWord_check_v0.7.2.xlsx
#   BMC_Genomics_preWord_checklist_v0.7.2.csv
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

input_md <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.2_preWord.md")

if (!file.exists(input_md)) {
  stop("缺少输入文件：", input_md)
}

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

insert_or_fix_declarations_heading <- function(lines) {
  if (any(lines == "# Declarations")) {
    return(lines)
  }
  
  declaration_subheading_patterns <- c(
    "^## Ethics approval and consent to participate$",
    "^## Consent for publication$",
    "^## Availability of data and materials$",
    "^## Competing interests$",
    "^## Funding$",
    "^## Authors' contributions$",
    "^## Acknowledgements$"
  )
  
  sub_idx <- unlist(lapply(declaration_subheading_patterns, function(p) grep(p, lines, perl = TRUE)))
  sub_idx <- sort(unique(sub_idx))
  
  if (length(sub_idx) == 0) {
    stop("未找到 Declarations 小标题，无法自动插入 # Declarations。")
  }
  
  first_sub <- sub_idx[1]
  
  # 如果前面一行是不同层级的 Declarations，就替换
  nearby_start <- max(1, first_sub - 5)
  nearby <- nearby_start:(first_sub - 1)
  
  dec_idx <- nearby[grepl("^#+\\s*Declarations\\s*$", lines[nearby], ignore.case = TRUE, perl = TRUE)]
  
  if (length(dec_idx) > 0) {
    lines[dec_idx[length(dec_idx)]] <- "# Declarations"
    return(lines)
  }
  
  # 否则在第一个 Declarations 小标题前插入总标题
  lines <- append(lines, values = c("", "# Declarations", ""), after = first_sub - 1)
  lines
}

message("Reading manuscript...")
ms <- read_lines_utf8(input_md)

message("Fixing Declarations heading...")
ms_fixed <- insert_or_fix_declarations_heading(ms)

output_md <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.3_preWord.md")
write_text_file(ms_fixed, output_md)

# ============================================================
# Re-check core headings and critical conditions
# ============================================================

required_sections <- c(
  "# Title page",
  "# Abstract",
  "# Background",
  "# Methods",
  "# Results",
  "# Discussion",
  "# List of abbreviations",
  "# Declarations",
  "## Ethics approval and consent to participate",
  "## Consent for publication",
  "## Availability of data and materials",
  "## Competing interests",
  "## Funding",
  "## Authors' contributions",
  "## Acknowledgements",
  "# References"
)

section_check <- data.frame(
  check_id = paste0("section_", seq_along(required_sections)),
  requirement = required_sections,
  present = vapply(
    required_sections,
    function(x) has_pattern(ms_fixed, paste0("^", x, "$"), ignore.case = FALSE),
    logical(1)
  ),
  stringsAsFactors = FALSE
)

section_check$status <- ifelse(section_check$present, "PASS", "CHECK")

critical_placeholder_patterns <- c(
  "REF_[A-Z0-9_]+",
  "TODO",
  "TBD"
)

critical_placeholder_hits <- do.call(
  rbind,
  lapply(critical_placeholder_patterns, function(p) line_hits(ms_fixed, p, ignore.case = TRUE))
)

if (is.null(critical_placeholder_hits) || nrow(critical_placeholder_hits) == 0) {
  critical_placeholder_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

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

boundary_checks <- data.frame(
  check_id = c(
    "nested_LODO_boundary",
    "threshold_boundary",
    "scRNA_boundary",
    "RNAseq_NoGo_boundary",
    "no_deployment_boundary",
    "GSE54514_failure_retained"
  ),
  requirement = c(
    "Held-out dataset excluded from feature selection/modeling/tuning/threshold selection",
    "Fixed threshold not considered transportable",
    "scRNA described as localization only",
    "RNA-seq No-Go decision reported",
    "No clinical deployment claim",
    "GSE54514 retained as failure scenario"
  ),
  present_or_pass = c(
    has_pattern(ms_fixed, "held-out dataset.*excluded|held-out dataset did not participate", TRUE),
    has_pattern(ms_fixed, "single fixed operating threshold.*not transportable|fixed.*threshold.*not.*transportable|not transportable across held-out datasets", TRUE),
    has_pattern(ms_fixed, "biological localization only|not considered independent diagnostic validation", TRUE),
    has_pattern(ms_fixed, "No-Go|NO_GO_no_eligible_RNAseq_dataset_detected", TRUE),
    nrow(forbidden_hits) == 0,
    has_pattern(ms_fixed, "GSE54514.*failure|failure scenario", TRUE)
  ),
  stringsAsFactors = FALSE
)

boundary_checks$status <- ifelse(boundary_checks$present_or_pass, "PASS", "CHECK")

overall_status <- data.frame(
  metric = c(
    "n_failed_section_checks",
    "n_failed_boundary_checks",
    "n_critical_placeholder_hits",
    "n_forbidden_claim_hits",
    "ready_for_word_conversion",
    "ready_for_submission"
  ),
  value = c(
    sum(section_check$status != "PASS"),
    sum(boundary_checks$status != "PASS"),
    nrow(critical_placeholder_hits),
    nrow(forbidden_hits),
    ifelse(
      sum(section_check$status != "PASS") == 0 &&
        sum(boundary_checks$status != "PASS") == 0 &&
        nrow(critical_placeholder_hits) == 0 &&
        nrow(forbidden_hits) == 0,
      "YES",
      "CHECK"
    ),
    "NO_MANUAL_COMPLETION_REQUIRED"
  ),
  stringsAsFactors = FALSE
)

checklist_path <- file.path(draft_dir, "BMC_Genomics_preWord_checklist_v0.7.2.csv")
index_path <- file.path(draft_dir, "BMC_Genomics_preWord_check_v0.7.2.xlsx")

data.table::fwrite(section_check, checklist_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "section_check")
openxlsx::writeData(wb, "section_check", section_check)

openxlsx::addWorksheet(wb, "boundary_checks")
openxlsx::writeData(wb, "boundary_checks", boundary_checks)

openxlsx::addWorksheet(wb, "critical_placeholder_hits")
openxlsx::writeData(wb, "critical_placeholder_hits", critical_placeholder_hits)

openxlsx::addWorksheet(wb, "forbidden_hits")
openxlsx::writeData(wb, "forbidden_hits", forbidden_hits)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_30d_BMC_Genomics_declarations_heading_fix.txt"))
print(sessionInfo())
sink()

message("\n============ 30d BMC Genomics declarations heading fix 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nSection checks:")
print(section_check)

message("\nBoundary checks:")
print(boundary_checks)

message("\nCritical placeholder hits:")
print(critical_placeholder_hits)

message("\nForbidden claim hits:")
print(forbidden_hits)

message("\n关键输出：")
message("1) ", output_md)
message("2) ", checklist_path)
message("3) ", index_path)

message("\n下一步：")
message("把 Overall status、Critical placeholder hits 贴给我。")
message("如果 ready_for_word_conversion = YES，就继续 31_BMC_Genomics_markdown_to_word_export.R。")