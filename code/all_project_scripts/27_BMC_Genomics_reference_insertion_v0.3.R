# ============================================================
# 27_BMC_Genomics_reference_insertion_v0.3.R
# Insert working numeric references into BMC Genomics manuscript
#
# 目的：
# 1. 将 v0.3 稿件中的 [REF_...] 占位符替换为数字引用
# 2. 生成带参考文献清单的 v0.4 markdown
# 3. 生成 reference audit table，方便后续 Zotero/手动核对
#
# 注意：
# 1. 这是 working reference insertion，不是最终投稿参考文献格式
# 2. GEO 队列原始文献暂以 GEO accession/source placeholder 形式保留在 audit table
# 3. 投稿前仍需用 Zotero 或手动核对 DOI、PMID、卷期页码和 BMC Genomics 格式
#
# 输入：
# 07_manuscript/BMC_Genomics_draft/BMC_Genomics_full_manuscript_v0.3.md
# 07_manuscript/BMC_Genomics_draft/BMC_Genomics_citation_replacement_map_v0.3.csv
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_full_manuscript_v0.4_referenced.md
#   BMC_Genomics_reference_audit_table_v0.4.csv
#   BMC_Genomics_reference_insertion_report_v0.4.md
#   BMC_Genomics_reference_insertion_v0.4_index.xlsx
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

manuscript_v03 <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.3.md")
citation_map_file <- file.path(draft_dir, "BMC_Genomics_citation_replacement_map_v0.3.csv")
placeholder_locations_file <- file.path(draft_dir, "BMC_Genomics_citation_placeholder_locations_v0.3.csv")

if (!file.exists(manuscript_v03)) {
  stop("缺少 manuscript v0.3 文件：", manuscript_v03)
}

if (!file.exists(citation_map_file)) {
  stop("缺少 citation map 文件：", citation_map_file)
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

extract_ref_keys <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  hits <- gregexpr("REF_[A-Z0-9_]+", txt, perl = TRUE)
  refs <- regmatches(txt, hits)[[1]]
  if (length(refs) == 0 || all(refs == "")) {
    return(character())
  }
  sort(unique(refs))
}

count_ref_key_occurrences <- function(lines, ref_key) {
  hits <- gregexpr(ref_key, lines, fixed = TRUE)
  sum(vapply(
    hits,
    function(z) {
      if (length(z) == 1 && z[1] == -1) return(0L)
      length(z)
    },
    integer(1)
  ))
}

replace_fixed <- function(lines, pattern, replacement) {
  gsub(pattern, replacement, lines, fixed = TRUE)
}

# ============================================================
# 读取 manuscript
# ============================================================

message("Reading manuscript and citation map...")

manuscript_lines <- read_lines_utf8(manuscript_v03)
citation_map <- data.table::fread(citation_map_file, data.table = FALSE)

if (file.exists(placeholder_locations_file)) {
  placeholder_locations <- data.table::fread(placeholder_locations_file, data.table = FALSE)
} else {
  placeholder_locations <- data.frame()
}

detected_before <- extract_ref_keys(manuscript_lines)

# ============================================================
# Working reference library
# ============================================================
# 编号策略：
# 1. 按正文首次出现的大致顺序排列
# 2. 避免过多引用堆砌
# 3. GEO exact source paper 后续可补充，此处先保留 audit 标记
# ============================================================

reference_library <- data.frame(
  ref_no = 1:13,
  ref_key = c(
    "Singer_Sepsis3_JAMA_2016",
    "van_der_Poll_Sepsis_Immunology_NatRevImmunol_2017",
    "Sweeney_Sepsis_GeneExpression_SciTranslMed_2015",
    "Scicluna_MARS_Sepsis_AJRespCritCareMed_2017",
    "VanCalster_Calibration_StatMed_2019",
    "Collins_TRIPOD_AI_BMJ_2024",
    "Moons_TRIPOD_AnnInternMed_2015",
    "Wolff_PROBAST_AnnInternMed_2019",
    "GSE167363_source_placeholder",
    "Vickers_DCA_MDM_2006",
    "Vickers_DCA_Guide_DiagnPrognRes_2019",
    "Hao_Seurat5_NatBiotechnol_2024",
    "Satija_Seurat_NatBiotechnol_2015"
  ),
  working_reference = c(
    "Singer M, Deutschman CS, Seymour CW, Shankar-Hari M, Annane D, Bauer M, et al. The Third International Consensus Definitions for Sepsis and Septic Shock (Sepsis-3). JAMA. 2016;315:801-810.",
    "van der Poll T, van de Veerdonk FL, Scicluna BP, Netea MG. The immunopathology of sepsis and potential therapeutic targets. Nat Rev Immunol. 2017;17:407-420.",
    "Sweeney TE, Shidham A, Wong HR, Khatri P. A comprehensive time-course-based multicohort analysis of sepsis and sterile inflammation reveals a robust diagnostic gene set. Sci Transl Med. 2015;7:287ra71.",
    "Scicluna BP, van Vught LA, Zwinderman AH, Wiewel MA, Davenport EE, Burnham KL, et al. Classification of patients with sepsis according to blood genomic endotype: a prospective cohort study. Lancet Respir Med. 2017;5:816-826.",
    "Van Calster B, McLernon DJ, van Smeden M, Wynants L, Steyerberg EW, on behalf of Topic Group 'Evaluating diagnostic tests and prediction models' of the STRATOS initiative. Calibration: the Achilles heel of predictive analytics. BMC Med. 2019;17:230.",
    "Collins GS, Dhiman P, Ma J, Andaur Navarro CL, Hooft L, Reitsma JB, et al. TRIPOD+AI statement: updated guidance for reporting clinical prediction models that use regression or machine learning methods. BMJ. 2024;385:e078378.",
    "Moons KGM, Altman DG, Reitsma JB, Ioannidis JPA, Macaskill P, Steyerberg EW, et al. Transparent Reporting of a multivariable prediction model for Individual Prognosis or Diagnosis (TRIPOD): explanation and elaboration. Ann Intern Med. 2015;162:W1-W73.",
    "Wolff RF, Moons KGM, Riley RD, Whiting PF, Westwood M, Collins GS, et al. PROBAST: a tool to assess the risk of bias and applicability of prediction model studies. Ann Intern Med. 2019;170:51-58.",
    "Original publication or GEO record for GSE167363. Exact bibliographic details to be inserted after manual verification.",
    "Vickers AJ, Elkin EB. Decision curve analysis: a novel method for evaluating prediction models. Med Decis Making. 2006;26:565-574.",
    "Vickers AJ, van Calster B, Steyerberg EW. A simple, step-by-step guide to interpreting decision curve analysis. Diagn Progn Res. 2019;3:18.",
    "Hao Y, Stuart T, Kowalski MH, Choudhary S, Hoffman P, Hartman A, et al. Dictionary learning for integrative, multimodal and scalable single-cell analysis. Nat Biotechnol. 2024;42:293-304.",
    "Satija R, Farrell JA, Gennert D, Schier AF, Regev A. Spatial reconstruction of single-cell gene expression data. Nat Biotechnol. 2015;33:495-502."
  ),
  doi_or_identifier = c(
    "doi:10.1001/jama.2016.0287; PMID:26903338",
    "doi:10.1038/nri.2017.36",
    "doi:10.1126/scitranslmed.aaa5993",
    "doi:10.1016/S2213-2600(17)30294-1",
    "doi:10.1186/s12916-019-1466-7; PMID:31842878",
    "doi:10.1136/bmj-2023-078378; PMID:38626948",
    "doi:10.7326/M14-0698",
    "doi:10.7326/M18-1376",
    "GSE167363 exact source pending",
    "doi:10.1177/0272989X06295361; PMID:17099194",
    "doi:10.1186/s41512-019-0064-7",
    "doi:10.1038/s41587-023-01767-y; PMID:37231261",
    "doi:10.1038/nbt.3192"
  ),
  manual_check_required = c(
    FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Placeholder replacement map
# ============================================================

placeholder_replacements <- data.frame(
  ref_key = c(
    "REF_SEPSIS_DEFINITION",
    "REF_SEPSIS_HOST_RESPONSE",
    "REF_SEPSIS_TRANSCRIPTOMIC_SIGNATURES",
    "REF_SEPSIS_SINGLE_CELL",
    "REF_PREDICTION_MODEL_REPORTING",
    "REF_TRIPOD_AI",
    "REF_CALIBRATION_PREDICTION_MODELS",
    "REF_DECISION_CURVE_ANALYSIS",
    "REF_SINGLE_CELL_MODULE_SCORE"
  ),
  replacement = c(
    "[1]",
    "[2]",
    "[3,4]",
    "[9]",
    "[6-8]",
    "[6]",
    "[5]",
    "[10,11]",
    "[12,13]"
  ),
  rationale = c(
    "Sepsis-3 consensus definition.",
    "Sepsis host-response and immunopathology review.",
    "Representative sepsis transcriptomic diagnostic/endotype studies.",
    "GSE167363 scRNA source placeholder, to be verified.",
    "Prediction model reporting, TRIPOD and PROBAST.",
    "TRIPOD+AI statement.",
    "Calibration-focused prediction model reference.",
    "Decision-curve analysis original method and interpretation guide.",
    "Seurat v5 and classic Seurat references."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 替换正文
# ============================================================

message("Replacing citation placeholders with numeric references...")

manuscript_v04 <- manuscript_lines

# 先处理带完整括号的单一 placeholder
for (i in seq_len(nrow(placeholder_replacements))) {
  full_pattern <- paste0("[", placeholder_replacements$ref_key[i], "]")
  manuscript_v04 <- replace_fixed(
    manuscript_v04,
    full_pattern,
    placeholder_replacements$replacement[i]
  )
}

# 再处理复合括号内的 key，例如 [REF_PREDICTION_MODEL_REPORTING; REF_TRIPOD_AI]
# 替换后可能出现 [[6-8]; [6]]，所以后面做规范化
for (i in seq_len(nrow(placeholder_replacements))) {
  key <- placeholder_replacements$ref_key[i]
  repl <- placeholder_replacements$replacement[i]
  manuscript_v04 <- replace_fixed(manuscript_v04, key, repl)
}

# 规范化复合引用遗留格式
manuscript_v04 <- gsub("\\[\\[", "[", manuscript_v04)
manuscript_v04 <- gsub("\\]\\]", "]", manuscript_v04)
manuscript_v04 <- gsub("\\[6-8\\]; \\[6\\]", "[6-8]", manuscript_v04, fixed = TRUE)
manuscript_v04 <- gsub("\\[5\\]\\. Transparent", "[5]. Transparent", manuscript_v04, fixed = TRUE)

# 删除旧 References placeholder 段，并替换为正式 references
ref_header_idx <- grep("^# References$", manuscript_v04)

if (length(ref_header_idx) > 0) {
  ref_header_idx <- ref_header_idx[1]
  manuscript_v04 <- manuscript_v04[seq_len(ref_header_idx - 1)]
}

references_lines <- c(
  "# References",
  "",
  paste0(
    reference_library$ref_no,
    ". ",
    reference_library$working_reference
  )
)

manuscript_v04 <- c(
  manuscript_v04,
  "",
  references_lines
)

detected_after <- extract_ref_keys(manuscript_v04)

# ============================================================
# Audit table
# ============================================================

audit_table <- merge(
  placeholder_replacements,
  data.frame(
    ref_key = paste0("REF_", c(
      "SEPSIS_DEFINITION",
      "SEPSIS_HOST_RESPONSE",
      "SEPSIS_TRANSCRIPTOMIC_SIGNATURES",
      "SEPSIS_SINGLE_CELL",
      "PREDICTION_MODEL_REPORTING",
      "TRIPOD_AI",
      "CALIBRATION_PREDICTION_MODELS",
      "DECISION_CURVE_ANALYSIS",
      "SINGLE_CELL_MODULE_SCORE"
    )),
    occurrences_before = vapply(
      paste0("REF_", c(
        "SEPSIS_DEFINITION",
        "SEPSIS_HOST_RESPONSE",
        "SEPSIS_TRANSCRIPTOMIC_SIGNATURES",
        "SEPSIS_SINGLE_CELL",
        "PREDICTION_MODEL_REPORTING",
        "TRIPOD_AI",
        "CALIBRATION_PREDICTION_MODELS",
        "DECISION_CURVE_ANALYSIS",
        "SINGLE_CELL_MODULE_SCORE"
      )),
      function(x) count_ref_key_occurrences(manuscript_lines, x),
      integer(1)
    ),
    occurrences_after = vapply(
      paste0("REF_", c(
        "SEPSIS_DEFINITION",
        "SEPSIS_HOST_RESPONSE",
        "SEPSIS_TRANSCRIPTOMIC_SIGNATURES",
        "SEPSIS_SINGLE_CELL",
        "PREDICTION_MODEL_REPORTING",
        "TRIPOD_AI",
        "CALIBRATION_PREDICTION_MODELS",
        "DECISION_CURVE_ANALYSIS",
        "SINGLE_CELL_MODULE_SCORE"
      )),
      function(x) count_ref_key_occurrences(manuscript_v04, x),
      integer(1)
    ),
    stringsAsFactors = FALSE
  ),
  by = "ref_key",
  all.x = TRUE
)

reference_audit <- reference_library
reference_audit$used_in_current_text <- reference_audit$ref_no %in% c(1:13)
reference_audit$status_note <- ifelse(
  reference_audit$manual_check_required,
  "Manual verification required before submission.",
  "Working reference inserted."
)

# ============================================================
# Report
# ============================================================

report_lines <- c(
  "# BMC Genomics reference insertion report v0.4",
  "",
  "## Input",
  "",
  paste0("- Manuscript v0.3: ", manuscript_v03),
  paste0("- Citation map: ", citation_map_file),
  "",
  "## Output",
  "",
  "- Manuscript v0.4 contains numeric working references.",
  "- All REF placeholders should be removed from the manuscript body.",
  "- References still require Zotero/manual verification before submission.",
  "",
  "## Detected REF keys before replacement",
  "",
  if (length(detected_before) > 0) paste0("- ", detected_before) else "- None",
  "",
  "## Detected REF keys after replacement",
  "",
  if (length(detected_after) > 0) paste0("- ", detected_after) else "- None",
  "",
  "## Important manual checks before submission",
  "",
  "1. Verify exact source paper or GEO record for GSE167363.",
  "2. Verify whether Seurat v5 reference or classic Seurat reference best matches the actual package version used.",
  "3. Verify Sweeney and Scicluna references are appropriate examples for the novelty-positioning sentence.",
  "4. Verify all DOI, PMID, volume, issue, pages and author lists in Zotero.",
  "5. Add exact GEO dataset references for GSE137340, GSE236713, GSE54514, GSE57065, GSE65682 and GSE95233 in Methods or supplementary data-source table if required.",
  "6. Do not cite weak repetitive GEO biomarker papers unless necessary."
)

# ============================================================
# 输出文件
# ============================================================

message("Writing v0.4 referenced manuscript and audit files...")

manuscript_v04_path <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.4_referenced.md")
audit_path <- file.path(draft_dir, "BMC_Genomics_reference_audit_table_v0.4.csv")
replacement_audit_path <- file.path(draft_dir, "BMC_Genomics_placeholder_replacement_audit_v0.4.csv")
report_path <- file.path(draft_dir, "BMC_Genomics_reference_insertion_report_v0.4.md")
index_path <- file.path(draft_dir, "BMC_Genomics_reference_insertion_v0.4_index.xlsx")

write_text_file(manuscript_v04, manuscript_v04_path)
data.table::fwrite(reference_audit, audit_path)
data.table::fwrite(audit_table, replacement_audit_path)
write_text_file(report_lines, report_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_full_manuscript_v0.4_referenced.md",
    "BMC_Genomics_reference_audit_table_v0.4.csv",
    "BMC_Genomics_placeholder_replacement_audit_v0.4.csv",
    "BMC_Genomics_reference_insertion_report_v0.4.md",
    "BMC_Genomics_reference_insertion_v0.4_index.xlsx"
  ),
  path = c(
    manuscript_v04_path,
    audit_path,
    replacement_audit_path,
    report_path,
    index_path
  ),
  purpose = c(
    "Full manuscript with working numeric references",
    "Reference list audit table",
    "Placeholder replacement audit",
    "Human-readable replacement report",
    "Workbook index"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "reference_library")
openxlsx::writeData(wb, "reference_library", reference_library)

openxlsx::addWorksheet(wb, "placeholder_replacements")
openxlsx::writeData(wb, "placeholder_replacements", placeholder_replacements)

openxlsx::addWorksheet(wb, "replacement_audit")
openxlsx::writeData(wb, "replacement_audit", audit_table)

openxlsx::addWorksheet(wb, "reference_audit")
openxlsx::writeData(wb, "reference_audit", reference_audit)

if (nrow(placeholder_locations) > 0) {
  openxlsx::addWorksheet(wb, "placeholder_locations_v03")
  openxlsx::writeData(wb, "placeholder_locations_v03", placeholder_locations)
}

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_27_BMC_Genomics_reference_insertion.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 27 BMC Genomics reference insertion 完成 ============")
message("输出目录：", draft_dir)

message("\nDetected REF keys before replacement:")
print(detected_before)

message("\nDetected REF keys after replacement:")
print(detected_after)

message("\nPlaceholder replacement audit:")
print(audit_table)

message("\nReference audit:")
print(reference_audit)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", manuscript_v04_path)
message("2) ", audit_path)
message("3) ", replacement_audit_path)
message("4) ", report_path)
message("5) ", index_path)

message("\n下一步：")
message("打开 BMC_Genomics_full_manuscript_v0.4_referenced.md，检查正文引用是否自然。")
message("如果 detected_after 为空，下一步做 28_BMC_Genomics_language_and_claim_polish.R。")