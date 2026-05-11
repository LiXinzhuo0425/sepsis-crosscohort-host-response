# ============================================================
# 30b_BMC_Genomics_submission_package_fix.R
# Fix submission package check issues before Word conversion
#
# 目的：
# 1. 在 v0.5.1 稿件中补充明确的 fixed-threshold non-transportability 句子
# 2. 重新执行投稿包检查
# 3. 允许作者信息/单位/通讯作者/GSE167363 精确文献作为 manual completion items
# 4. 若核心结构、claim boundary、摘要、图表计划均通过，则允许 Word conversion
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.1_polished.md
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.5.2_preWord.md
#   BMC_Genomics_submission_package_check_v0.7.1.xlsx
#   BMC_Genomics_submission_package_checklist_v0.7.1.csv
#   BMC_Genomics_manual_action_items_v0.7.1.csv
#   BMC_Genomics_submission_package_check_report_v0.7.1.md
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
  manuscript = file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.1_polished.md"),
  captions = file.path(draft_dir, "BMC_Genomics_table_figure_captions_v0.6.md"),
  main_table_plan = file.path(draft_dir, "BMC_Genomics_main_table_plan_v0.6.csv"),
  main_figure_plan = file.path(draft_dir, "BMC_Genomics_main_figure_plan_v0.6.csv"),
  supplementary_plan = file.path(draft_dir, "BMC_Genomics_supplementary_material_plan_v0.6.csv"),
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

insert_after_first_match <- function(lines, pattern, insert_line) {
  if (any(grepl(insert_line, lines, fixed = TRUE))) {
    return(lines)
  }
  
  idx <- grep(pattern, lines, ignore.case = TRUE, perl = TRUE)
  
  if (length(idx) == 0) {
    return(c(lines, "", insert_line))
  }
  
  i <- idx[1]
  append(lines, values = insert_line, after = i)
}

# ============================================================
# 读取输入
# ============================================================

message("Reading manuscript package materials...")

ms <- read_lines_utf8(files$manuscript)
captions <- read_lines_utf8(files$captions)
main_table_plan <- read_csv_df(files$main_table_plan)
main_figure_plan <- read_csv_df(files$main_figure_plan)
supplementary_plan <- read_csv_df(files$supplementary_plan)
key_snapshot <- read_csv_df(files$key_snapshot)
decision_log <- read_csv_df(files$decision_log)
source_index <- read_csv_df(files$source_index)
figure_inventory <- read_csv_df(files$figure_inventory)
copy_manifest <- read_csv_df(files$copy_manifest)

# ============================================================
# 修正文稿：补充明确 threshold boundary
# ============================================================

message("Adding explicit fixed-threshold non-transportability statement...")

threshold_boundary_sentence <- paste0(
  "Taken together, these results indicate that a single fixed operating threshold ",
  "was not transportable across held-out datasets and would require setting-specific ",
  "evaluation before any deployment-oriented use."
)

ms_fixed <- insert_after_first_match(
  lines = ms,
  pattern = "median absolute threshold shift|fixed-threshold|threshold shift",
  insert_line = threshold_boundary_sentence
)

manuscript_v052_path <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.5.2_preWord.md")
write_text_file(ms_fixed, manuscript_v052_path)

combined_text <- c(ms_fixed, captions)

# ============================================================
# Section checks
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
section_check$action <- ifelse(section_check$status == "PASS", "None", "Add or correct this manuscript section.")

for (i in seq_len(nrow(section_check))) {
  if (section_check$status[i] == "CHECK" && grepl("^## ", section_check$requirement[i])) {
    clean_req <- gsub("^## ", "", section_check$requirement[i])
    if (has_pattern(ms_fixed, clean_req, ignore.case = TRUE)) {
      section_check$present[i] <- TRUE
      section_check$status[i] <- "PASS"
      section_check$action[i] <- "Heading detected flexibly; confirm formatting manually."
    }
  }
}

# ============================================================
# Abstract and word counts
# ============================================================

abstract_lines <- extract_between_headers(ms_fixed, "# Abstract", "# Background")
background_lines <- extract_between_headers(ms_fixed, "# Background", "# Methods")
methods_lines <- extract_between_headers(ms_fixed, "# Methods", "# Results")
results_lines <- extract_between_headers(ms_fixed, "# Results", "# Discussion")
discussion_lines <- extract_between_headers(ms_fixed, "# Discussion", "# List of abbreviations")

word_count_table <- data.frame(
  section = c("Abstract", "Background", "Methods", "Results", "Discussion", "Main text total"),
  word_count = c(
    count_words_simple(abstract_lines),
    count_words_simple(background_lines),
    count_words_simple(methods_lines),
    count_words_simple(results_lines),
    count_words_simple(discussion_lines),
    count_words_simple(c(background_lines, methods_lines, results_lines, discussion_lines))
  ),
  target_or_note = c(
    "BMC structured abstract should be <=350 words.",
    "No strict journal limit; keep concise.",
    "No strict journal limit; methods should be reproducible.",
    "No strict journal limit; report frozen numbers only.",
    "No strict journal limit; emphasize restrictions.",
    "No strict journal limit; current length is acceptable if concise."
  ),
  status = c(
    ifelse(count_words_simple(abstract_lines) <= 350, "PASS", "CHECK"),
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS"
  ),
  stringsAsFactors = FALSE
)

abstract_structure_check <- data.frame(
  check_id = c("abstract_background", "abstract_results", "abstract_conclusions", "abstract_no_refs"),
  requirement = c(
    "Abstract includes Background subsection",
    "Abstract includes Results subsection",
    "Abstract includes Conclusions subsection",
    "Abstract does not cite references"
  ),
  present_or_pass = c(
    has_pattern(abstract_lines, "^## Background$", ignore.case = FALSE),
    has_pattern(abstract_lines, "^## Results$", ignore.case = FALSE),
    has_pattern(abstract_lines, "^## Conclusions$", ignore.case = FALSE),
    !has_pattern(abstract_lines, "\\[[0-9,\\-]+\\]", ignore.case = FALSE)
  ),
  stringsAsFactors = FALSE
)

abstract_structure_check$status <- ifelse(abstract_structure_check$present_or_pass, "PASS", "CHECK")

# ============================================================
# Placeholder / TODO / claim checks
# ============================================================

manual_placeholder_patterns <- c(
  "\\[Author names to be completed\\]",
  "\\[Affiliations to be completed\\]",
  "\\[Name, address, email to be completed\\]",
  "\\[.*to be completed.*\\]",
  "Exact bibliographic details to be inserted"
)

critical_placeholder_patterns <- c(
  "REF_[A-Z0-9_]+",
  "TODO",
  "TBD"
)

manual_placeholder_hits <- do.call(
  rbind,
  lapply(manual_placeholder_patterns, function(p) line_hits(ms_fixed, p, ignore.case = TRUE))
)

if (is.null(manual_placeholder_hits) || nrow(manual_placeholder_hits) == 0) {
  manual_placeholder_hits <- data.frame(
    pattern = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

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
  lapply(forbidden_patterns, function(p) line_hits(combined_text, p, ignore.case = TRUE))
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

# ============================================================
# Table / figure / supplement checks
# ============================================================

tf_check <- data.frame(
  check_id = c(
    "main_table_count",
    "main_figure_count",
    "supplement_count",
    "figure_caption_file_present",
    "figure_inventory_present",
    "frozen_required_files_copied"
  ),
  requirement = c(
    "Four main tables planned",
    "Five main figures planned",
    "At least eight supplementary items planned",
    "Caption file exists",
    "Final freeze figure inventory exists",
    "Required final-freeze files copied"
  ),
  observed = c(
    nrow(main_table_plan),
    nrow(main_figure_plan),
    nrow(supplementary_plan),
    file.exists(files$captions),
    file.exists(files$figure_inventory),
    ifelse("required" %in% names(copy_manifest), sum(copy_manifest$required == TRUE & copy_manifest$copied == TRUE), NA)
  ),
  status = c(
    ifelse(nrow(main_table_plan) == 4, "PASS", "CHECK"),
    ifelse(nrow(main_figure_plan) == 5, "PASS", "CHECK"),
    ifelse(nrow(supplementary_plan) >= 8, "PASS", "CHECK"),
    ifelse(file.exists(files$captions), "PASS", "CHECK"),
    ifelse(file.exists(files$figure_inventory), "PASS", "CHECK"),
    ifelse(
      "required" %in% names(copy_manifest) && all(copy_manifest$copied[copy_manifest$required == TRUE] == TRUE),
      "PASS",
      "CHECK"
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Declarations specific check
# ============================================================

declaration_checks <- data.frame(
  check_id = c(
    "ethics_statement_present",
    "consent_publication_present",
    "data_availability_present",
    "competing_interests_present",
    "funding_present",
    "author_contributions_present",
    "acknowledgements_present"
  ),
  requirement = c(
    "Ethics approval and consent to participate statement exists",
    "Consent for publication statement exists",
    "Availability of data and materials statement exists",
    "Competing interests statement exists",
    "Funding statement exists",
    "Authors' contributions statement exists",
    "Acknowledgements statement exists"
  ),
  present = c(
    has_pattern(ms_fixed, "Ethics approval and consent to participate", TRUE),
    has_pattern(ms_fixed, "Consent for publication", TRUE),
    has_pattern(ms_fixed, "Availability of data and materials", TRUE),
    has_pattern(ms_fixed, "Competing interests", TRUE),
    has_pattern(ms_fixed, "Funding", TRUE),
    has_pattern(ms_fixed, "Authors' contributions", TRUE),
    has_pattern(ms_fixed, "Acknowledgements", TRUE)
  ),
  manual_completion_needed = c(
    TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE
  ),
  stringsAsFactors = FALSE
)

declaration_checks$status <- ifelse(declaration_checks$present, "PASS", "CHECK")
declaration_checks$action <- ifelse(
  declaration_checks$status == "PASS" & declaration_checks$manual_completion_needed,
  "Present but must be completed manually before final submission.",
  ifelse(declaration_checks$status == "PASS", "None", "Add this declaration section.")
)

# ============================================================
# Manual action items
# ============================================================

manual_action_items <- data.frame(
  priority = c(
    "Critical",
    "Critical",
    "Critical",
    "Critical",
    "High",
    "High",
    "High",
    "High",
    "Medium",
    "Medium"
  ),
  action_item = c(
    "Fill author names, affiliations and corresponding author details.",
    "Complete Ethics approval and consent statement for secondary public human data analysis, including whether approval was waived or not required according to local/institutional policy.",
    "Complete Availability of data and materials with GEO accessions, code repository and Zenodo/DOI if available.",
    "Verify exact references for GSE167363 and all included GEO cohorts.",
    "Verify all working references in Zotero, including DOI, PMID, volume, pages and author order.",
    "Confirm funding statement and grant numbers.",
    "Complete authors' contributions using CRediT-style roles if appropriate.",
    "Prepare separate figure files from final_freeze/figures according to journal upload workflow.",
    "Check whether supplementary materials should be combined or uploaded separately.",
    "Check BMC Genomics article-processing charge and institutional payment route."
  ),
  current_status = c(
    "Manual completion before final submission",
    "Needs manual institutional wording",
    "Needs final repository/accession details",
    "Needs source verification",
    "Needs Zotero/manual check",
    "Needs author confirmation",
    "Needs author confirmation",
    "Needs file export/upload preparation",
    "Needs submission-system decision",
    "Needs administrative confirmation"
  ),
  blocks_word_conversion = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  blocks_final_submission = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE
  ),
  stringsAsFactors = FALSE
)

if (nrow(manual_placeholder_hits) > 0) {
  manual_placeholder_actions <- data.frame(
    priority = "Critical",
    action_item = paste0(
      "Resolve manual placeholder before final submission at manuscript line ",
      manual_placeholder_hits$line_number,
      ": ",
      substr(manual_placeholder_hits$line_text, 1, 160)
    ),
    current_status = "Manual placeholder detected",
    blocks_word_conversion = FALSE,
    blocks_final_submission = TRUE,
    stringsAsFactors = FALSE
  )
  manual_action_items <- rbind(manual_action_items, manual_placeholder_actions)
}

if (nrow(critical_placeholder_hits) > 0) {
  critical_placeholder_actions <- data.frame(
    priority = "Critical",
    action_item = paste0(
      "Resolve critical placeholder before Word conversion at manuscript line ",
      critical_placeholder_hits$line_number,
      ": ",
      substr(critical_placeholder_hits$line_text, 1, 160)
    ),
    current_status = "Critical placeholder detected",
    blocks_word_conversion = TRUE,
    blocks_final_submission = TRUE,
    stringsAsFactors = FALSE
  )
  manual_action_items <- rbind(manual_action_items, critical_placeholder_actions)
}

# ============================================================
# Overall status
# ============================================================

all_core_status <- c(
  section_check$status,
  word_count_table$status,
  abstract_structure_check$status,
  boundary_checks$status,
  tf_check$status,
  declaration_checks$status
)

n_failed_core_checks <- sum(all_core_status != "PASS")
n_manual_placeholder_hits <- nrow(manual_placeholder_hits)
n_critical_placeholder_hits <- nrow(critical_placeholder_hits)
n_forbidden_hits <- nrow(forbidden_hits)

ready_for_word_conversion <- (
  n_failed_core_checks == 0 &&
    n_forbidden_hits == 0 &&
    n_critical_placeholder_hits == 0
)

ready_for_submission <- (
  ready_for_word_conversion &&
    n_manual_placeholder_hits == 0 &&
    !any(manual_action_items$blocks_final_submission)
)

overall_status <- data.frame(
  metric = c(
    "n_failed_core_checks",
    "n_manual_placeholder_hits",
    "n_critical_placeholder_hits",
    "n_forbidden_claim_hits",
    "ready_for_word_conversion",
    "ready_for_submission",
    "recommended_next_step"
  ),
  value = c(
    n_failed_core_checks,
    n_manual_placeholder_hits,
    n_critical_placeholder_hits,
    n_forbidden_hits,
    ifelse(ready_for_word_conversion, "YES", "CHECK"),
    ifelse(ready_for_submission, "YES", "NO_MANUAL_COMPLETION_REQUIRED"),
    ifelse(
      ready_for_word_conversion,
      "Proceed to Word conversion; complete manual fields before final submission.",
      "Fix failed core or critical checks before Word conversion."
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Report
# ============================================================

report_lines <- c(
  "# BMC Genomics submission package check report v0.7.1",
  "",
  "## Overall status",
  "",
  paste0("- Failed core checks: ", n_failed_core_checks),
  paste0("- Manual placeholder hits: ", n_manual_placeholder_hits),
  paste0("- Critical placeholder hits: ", n_critical_placeholder_hits),
  paste0("- Forbidden claim hits: ", n_forbidden_hits),
  paste0("- Ready for Word conversion: ", ifelse(ready_for_word_conversion, "YES", "CHECK")),
  paste0("- Ready for final submission: ", ifelse(ready_for_submission, "YES", "NO, manual completion still required")),
  "",
  "## Interpretation",
  "",
  "The manuscript can proceed to Word conversion if all core structure, claim-boundary, word-count and figure/table checks pass, and if no critical REF/TODO/TBD placeholders remain.",
  "",
  "Author names, affiliations, corresponding author details and exact GSE167363 bibliographic details are manual completion items. They do not block Word conversion, but they must be completed before final submission.",
  "",
  "## Manual action items",
  "",
  paste0(seq_len(nrow(manual_action_items)), ". [", manual_action_items$priority, "] ", manual_action_items$action_item),
  "",
  "## Recommended next step",
  "",
  ifelse(
    ready_for_word_conversion,
    "Proceed to 31_BMC_Genomics_markdown_to_word_export.R.",
    "Fix failed checks before Word export."
  )
)

# ============================================================
# 输出
# ============================================================

message("Writing submission package fix outputs...")

checklist_path <- file.path(draft_dir, "BMC_Genomics_submission_package_checklist_v0.7.1.csv")
manual_actions_path <- file.path(draft_dir, "BMC_Genomics_manual_action_items_v0.7.1.csv")
manual_placeholder_hits_path <- file.path(draft_dir, "BMC_Genomics_manual_placeholder_hits_v0.7.1.csv")
critical_placeholder_hits_path <- file.path(draft_dir, "BMC_Genomics_critical_placeholder_hits_v0.7.1.csv")
forbidden_hits_path <- file.path(draft_dir, "BMC_Genomics_forbidden_claim_hits_v0.7.1.csv")
report_path <- file.path(draft_dir, "BMC_Genomics_submission_package_check_report_v0.7.1.md")
index_path <- file.path(draft_dir, "BMC_Genomics_submission_package_check_v0.7.1.xlsx")

full_checklist <- rbind(
  data.frame(category = "sections", section_check[, c("check_id", "requirement", "status", "action")], stringsAsFactors = FALSE),
  data.frame(category = "abstract_structure", abstract_structure_check[, c("check_id", "requirement", "status")], action = ifelse(abstract_structure_check$status == "PASS", "None", "Fix abstract structure."), stringsAsFactors = FALSE),
  data.frame(category = "boundary", boundary_checks[, c("check_id", "requirement", "status")], action = ifelse(boundary_checks$status == "PASS", "None", "Fix claim-boundary language."), stringsAsFactors = FALSE),
  data.frame(category = "figures_tables", tf_check[, c("check_id", "requirement", "status")], action = ifelse(tf_check$status == "PASS", "None", "Fix figure/table package."), stringsAsFactors = FALSE),
  data.frame(category = "declarations", declaration_checks[, c("check_id", "requirement", "status", "action")], stringsAsFactors = FALSE)
)

data.table::fwrite(full_checklist, checklist_path)
data.table::fwrite(manual_action_items, manual_actions_path)
data.table::fwrite(manual_placeholder_hits, manual_placeholder_hits_path)
data.table::fwrite(critical_placeholder_hits, critical_placeholder_hits_path)
data.table::fwrite(forbidden_hits, forbidden_hits_path)
write_text_file(report_lines, report_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "checklist")
openxlsx::writeData(wb, "checklist", full_checklist)

openxlsx::addWorksheet(wb, "word_counts")
openxlsx::writeData(wb, "word_counts", word_count_table)

openxlsx::addWorksheet(wb, "abstract_structure")
openxlsx::writeData(wb, "abstract_structure", abstract_structure_check)

openxlsx::addWorksheet(wb, "boundary_checks")
openxlsx::writeData(wb, "boundary_checks", boundary_checks)

openxlsx::addWorksheet(wb, "declarations")
openxlsx::writeData(wb, "declarations", declaration_checks)

openxlsx::addWorksheet(wb, "manual_actions")
openxlsx::writeData(wb, "manual_actions", manual_action_items)

openxlsx::addWorksheet(wb, "manual_placeholder_hits")
openxlsx::writeData(wb, "manual_placeholder_hits", manual_placeholder_hits)

openxlsx::addWorksheet(wb, "critical_placeholder_hits")
openxlsx::writeData(wb, "critical_placeholder_hits", critical_placeholder_hits)

openxlsx::addWorksheet(wb, "forbidden_hits")
openxlsx::writeData(wb, "forbidden_hits", forbidden_hits)

openxlsx::addWorksheet(wb, "table_plan")
openxlsx::writeData(wb, "table_plan", main_table_plan)

openxlsx::addWorksheet(wb, "figure_plan")
openxlsx::writeData(wb, "figure_plan", main_figure_plan)

openxlsx::addWorksheet(wb, "supplement_plan")
openxlsx::writeData(wb, "supplement_plan", supplementary_plan)

openxlsx::addWorksheet(wb, "source_index")
openxlsx::writeData(wb, "source_index", source_index)

openxlsx::addWorksheet(wb, "decision_log")
openxlsx::writeData(wb, "decision_log", decision_log)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_30b_BMC_Genomics_submission_package_fix.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 30b BMC Genomics submission package fix 完成 ============")
message("输出目录：", draft_dir)

message("\nOverall status:")
print(overall_status)

message("\nBoundary checks:")
print(boundary_checks)

message("\nManual placeholder hits:")
print(manual_placeholder_hits)

message("\nCritical placeholder hits:")
print(critical_placeholder_hits)

message("\nForbidden claim hits:")
print(forbidden_hits)

message("\nManual action items:")
print(manual_action_items)

message("\n关键输出：")
message("1) ", manuscript_v052_path)
message("2) ", index_path)
message("3) ", checklist_path)
message("4) ", manual_actions_path)
message("5) ", report_path)
message("6) ", manual_placeholder_hits_path)
message("7) ", critical_placeholder_hits_path)
message("8) ", forbidden_hits_path)

message("\n下一步：")
message("把 Overall status、Boundary checks、Critical placeholder hits 贴给我。")
message("如果 ready_for_word_conversion = YES，就继续 31_BMC_Genomics_markdown_to_word_export.R。")