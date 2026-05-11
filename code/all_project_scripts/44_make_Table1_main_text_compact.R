# ============================================================
# 44_make_Table1_main_text_compact.R
# Purpose:
#   Generate a compact, main-text-ready Table 1 for BMC Genomics.
#
# Input:
#   04_results/table1_clean_for_BMC_Genomics/
#     T43_Table1_BMC_Genomics_main_text_clean.csv
#     T43_Table1_BMC_Genomics_supplementary_audit.csv
#
# Output:
#   04_results/table1_compact_for_BMC_Genomics/
#     T44_Table1_BMC_Genomics_main_text_compact.csv
#     T44_Table1_BMC_Genomics_main_text_compact.docx
#     T44_Table1_BMC_Genomics_compact_workbook.xlsx
#     T44_Table1_BMC_Genomics_additional_file_S1.csv
#     T44_Table1_compact_checks.csv
#     T44_Table1_compact_overall_status.csv
#
# Key fixes in this version:
#   1) Use flextable::width explicitly to avoid namespace conflict.
#   2) Recalculate n_failed_checks immediately before overall_status.
#   3) Avoid relying on partial reruns.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(openxlsx)
  library(flextable)
  library(officer)
  library(tidyr)
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

dir_in <- file.path(project_dir, "04_results", "table1_clean_for_BMC_Genomics")
dir_out <- file.path(project_dir, "04_results", "table1_compact_for_BMC_Genomics")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

main_clean_in_path <- file.path(dir_in, "T43_Table1_BMC_Genomics_main_text_clean.csv")
supp_audit_in_path <- file.path(dir_in, "T43_Table1_BMC_Genomics_supplementary_audit.csv")

csv_compact_path <- file.path(dir_out, "T44_Table1_BMC_Genomics_main_text_compact.csv")
docx_compact_path <- file.path(dir_out, "T44_Table1_BMC_Genomics_main_text_compact.docx")
xlsx_compact_path <- file.path(dir_out, "T44_Table1_BMC_Genomics_compact_workbook.xlsx")
csv_additional_s1_path <- file.path(dir_out, "T44_Table1_BMC_Genomics_additional_file_S1.csv")
checks_path <- file.path(dir_out, "T44_Table1_compact_checks.csv")
overall_path <- file.path(dir_out, "T44_Table1_compact_overall_status.csv")

# -----------------------------
# 2. Read inputs
# -----------------------------
if (!file.exists(main_clean_in_path)) {
  stop("Input clean Table 1 not found: ", main_clean_in_path)
}

table1_clean <- readr::read_csv(main_clean_in_path, show_col_types = FALSE)

if (file.exists(supp_audit_in_path)) {
  supp_audit <- readr::read_csv(supp_audit_in_path, show_col_types = FALSE)
} else {
  supp_audit <- NULL
}

expected_datasets <- c(
  "GSE137340",
  "GSE236713",
  "GSE54514",
  "GSE57065",
  "GSE65682",
  "GSE95233"
)

required_cols <- c(
  "Dataset",
  "Platform",
  "Sample source",
  "Case definition",
  "Comparator group used in this analysis",
  "Intermediate or repeated samples handled in this analysis",
  "Cohort role in this study"
)

missing_cols <- setdiff(required_cols, names(table1_clean))
if (length(missing_cols) > 0) {
  stop("The following required columns are missing from T43 input: ", paste(missing_cols, collapse = ", "))
}

# -----------------------------
# 3. Build compact main-text Table 1
# -----------------------------
table1_compact <- table1_clean %>%
  dplyr::transmute(
    Dataset,
    Platform,
    `Sample source`,
    `Case definition`,
    `Comparator used` = `Comparator group used in this analysis`,
    `Sample-handling note` = `Intermediate or repeated samples handled in this analysis`,
    `Role in this study` = `Cohort role in this study`
  ) %>%
  dplyr::mutate(
    Dataset = factor(Dataset, levels = expected_datasets),
    
    `Case definition` = dplyr::case_when(
      as.character(Dataset) == "GSE137340" ~ "Sepsis cases sampled at diagnosis and after 24 h.",
      as.character(Dataset) == "GSE236713" ~ "Adult severe systemic inflammation cohort including sepsis and SIRS-related comparisons.",
      as.character(Dataset) == "GSE54514" ~ "Critically ill ICU patients with sepsis sampled longitudinally.",
      as.character(Dataset) == "GSE57065" ~ "Patients with septic shock profiled for early dynamic gene-expression changes.",
      as.character(Dataset) == "GSE65682" ~ "Critically ill patients with sepsis or septic shock in a prospective systems-biology cohort.",
      as.character(Dataset) == "GSE95233" ~ "Patients with septic shock profiled for 28-day mortality-associated biomarkers.",
      TRUE ~ `Case definition`
    ),
    
    `Comparator used` = dplyr::case_when(
      as.character(Dataset) == "GSE137340" ~ "Age- and sex-matched healthy controls.",
      as.character(Dataset) == "GSE236713" ~ "Control samples defined by original study metadata.",
      as.character(Dataset) == "GSE54514" ~ "Healthy or non-sepsis comparator samples selected by sample-level phenotype annotation.",
      as.character(Dataset) == "GSE57065" ~ "Healthy or non-sepsis control samples selected by phenotype annotation.",
      as.character(Dataset) == "GSE65682" ~ "Healthy controls selected for the current binary diagnostic comparison.",
      as.character(Dataset) == "GSE95233" ~ "Healthy controls or volunteers selected by phenotype annotation.",
      TRUE ~ `Comparator used`
    ),
    
    `Sample-handling note` = dplyr::case_when(
      as.character(Dataset) == "GSE137340" ~ "Only the diagnostic or predefined sepsis time point was retained.",
      as.character(Dataset) == "GSE236713" ~ "SIRS or other intermediate inflammatory samples were handled by the predefined binary selection rule.",
      as.character(Dataset) == "GSE54514" ~ "Repeated longitudinal sepsis samples were restricted by the predefined selection rule.",
      as.character(Dataset) == "GSE57065" ~ "Early or serial samples were restricted by the predefined selection rule.",
      as.character(Dataset) == "GSE65682" ~ "Other ICU diagnostic categories were not used in the primary binary comparison.",
      as.character(Dataset) == "GSE95233" ~ "No SIRS intermediate group was retained in the current binary comparison.",
      TRUE ~ `Sample-handling note`
    ),
    
    `Role in this study` = dplyr::case_when(
      as.character(Dataset) == "GSE54514" ~ "Held-out transportability stress scenario.",
      TRUE ~ "Included in cross-dataset model development and held-out validation."
    )
  ) %>%
  dplyr::arrange(Dataset) %>%
  dplyr::mutate(Dataset = as.character(Dataset))

# -----------------------------
# 4. Build Additional file S1
# -----------------------------
if (!is.null(supp_audit)) {
  additional_file_s1 <- supp_audit
} else {
  additional_file_s1 <- table1_clean
}

additional_file_s1 <- additional_file_s1 %>%
  dplyr::mutate(
    supplementary_table = "Supplementary Table S1. Source references and author-side verification notes for bulk transcriptomic datasets"
  ) %>%
  dplyr::relocate(supplementary_table, .before = 1)

# -----------------------------
# 5. Checks
# -----------------------------
collapse_table_text <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::unite("all_text", dplyr::everything(), sep = " ", remove = TRUE, na.rm = TRUE) %>%
    dplyr::pull(all_text) %>%
    paste(collapse = " ")
}

main_text_all <- collapse_table_text(table1_compact)

forbidden_patterns_main <- c(
  "manual verify",
  "manually verify",
  "before final submission",
  "should whether",
  "DRAFT_WITH_MANUAL",
  "CHECK",
  "exact control group",
  "current selected controls from GSM metadata",
  "PMID:",
  "DOI:",
  "10\\."
)

contains_forbidden <- function(text, patterns) {
  any(stringr::str_detect(stringr::str_to_lower(text), stringr::str_to_lower(patterns)))
}

gse54514_role_ok <- table1_compact %>%
  dplyr::filter(Dataset == "GSE54514") %>%
  dplyr::pull(`Role in this study`) %>%
  stringr::str_detect(stringr::fixed("Held-out transportability stress scenario."))

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:16),
  check_item = c(
    "All six datasets retained",
    "Compact table has seven columns",
    "No missing dataset",
    "No missing platform",
    "No missing sample source",
    "No missing case definition",
    "No missing comparator used",
    "No missing sample-handling note",
    "No missing role in this study",
    "Dataset order matches expected order",
    "No PMID or DOI strings in compact main-text table",
    "No forbidden internal-audit phrases in compact table",
    "GSE54514 role shortened as transportability stress scenario",
    "Additional file S1 generated",
    "DOCX compact table generated",
    "XLSX workbook generated"
  ),
  observed = c(
    setequal(table1_compact$Dataset, expected_datasets) && nrow(table1_compact) == 6,
    ncol(table1_compact) == 7,
    all(!is.na(table1_compact$Dataset) & table1_compact$Dataset != ""),
    all(!is.na(table1_compact$Platform) & table1_compact$Platform != ""),
    all(!is.na(table1_compact$`Sample source`) & table1_compact$`Sample source` != ""),
    all(!is.na(table1_compact$`Case definition`) & table1_compact$`Case definition` != ""),
    all(!is.na(table1_compact$`Comparator used`) & table1_compact$`Comparator used` != ""),
    all(!is.na(table1_compact$`Sample-handling note`) & table1_compact$`Sample-handling note` != ""),
    all(!is.na(table1_compact$`Role in this study`) & table1_compact$`Role in this study` != ""),
    identical(table1_compact$Dataset, expected_datasets),
    !stringr::str_detect(main_text_all, stringr::regex("PMID:|DOI:|10\\.", ignore_case = TRUE)),
    !contains_forbidden(main_text_all, forbidden_patterns_main),
    isTRUE(gse54514_role_ok),
    nrow(additional_file_s1) == 6,
    FALSE,
    FALSE
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 6. Export CSV files
# -----------------------------
readr::write_csv(table1_compact, csv_compact_path)
readr::write_csv(additional_file_s1, csv_additional_s1_path)

# -----------------------------
# 7. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Table1_Main_Text_Compact")
openxlsx::writeData(wb, "Table1_Main_Text_Compact", table1_compact)

openxlsx::addWorksheet(wb, "Additional_File_S1")
openxlsx::writeData(wb, "Additional_File_S1", additional_file_s1)

openxlsx::addWorksheet(wb, "Checks")
openxlsx::writeData(wb, "Checks", checks)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  fgFill = "#D9EAF7",
  border = "Bottom"
)

body_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

for (sheet in names(wb)) {
  sheet_df <- openxlsx::readWorkbook(wb, sheet)
  openxlsx::addStyle(
    wb,
    sheet,
    header_style,
    rows = 1,
    cols = seq_len(ncol(sheet_df)),
    gridExpand = TRUE
  )
  if (nrow(sheet_df) > 0) {
    openxlsx::addStyle(
      wb,
      sheet,
      body_style,
      rows = 2:(nrow(sheet_df) + 1),
      cols = seq_len(ncol(sheet_df)),
      gridExpand = TRUE
    )
  }
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(sheet_df)), widths = "auto")
}

openxlsx::saveWorkbook(wb, xlsx_compact_path, overwrite = TRUE)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "XLSX workbook generated" ~ file.exists(xlsx_compact_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 8. Export DOCX main-text compact Table 1
# -----------------------------
ft <- flextable::flextable(table1_compact)

ft <- flextable::theme_booktabs(ft)
ft <- flextable::fontsize(ft, size = 8, part = "all")
ft <- flextable::fontsize(ft, size = 8.5, part = "header")
ft <- flextable::bold(ft, part = "header")
ft <- flextable::align(ft, align = "center", part = "header")
ft <- flextable::align(ft, j = c("Dataset", "Platform"), align = "center", part = "body")
ft <- flextable::valign(ft, valign = "top", part = "all")

ft <- flextable::width(ft, j = "Dataset", width = 0.8)
ft <- flextable::width(ft, j = "Platform", width = 0.75)
ft <- flextable::width(ft, j = "Sample source", width = 1.25)
ft <- flextable::width(ft, j = "Case definition", width = 2.0)
ft <- flextable::width(ft, j = "Comparator used", width = 1.7)
ft <- flextable::width(ft, j = "Sample-handling note", width = 2.1)
ft <- flextable::width(ft, j = "Role in this study", width = 1.8)

ft <- flextable::set_table_properties(ft, layout = "fixed", width = 1)

doc <- officer::read_docx()
doc <- officer::body_add_par(
  doc,
  "Table 1. Bulk transcriptomic datasets included in the cross-dataset sepsis analysis",
  style = "heading 1"
)
doc <- officer::body_add_par(
  doc,
  "Comparator groups and intermediate or repeated samples were defined according to the predefined binary sample-selection framework used in the current analysis. Detailed source references and cohort-specific verification notes are provided in Supplementary Table S1.",
  style = "Normal"
)
doc <- flextable::body_add_flextable(doc, ft)
doc <- officer::body_add_par(
  doc,
  "Abbreviations: GEO, Gene Expression Omnibus; ICU, intensive care unit; SIRS, systemic inflammatory response syndrome.",
  style = "Normal"
)

print(doc, target = docx_compact_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "DOCX compact table generated" ~ file.exists(docx_compact_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 9. Finalize checks before overall status
# -----------------------------
n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)
readr::write_csv(checks, checks_path)

# -----------------------------
# 10. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "n_table_rows",
    "n_table_columns",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "main_remaining_risk",
    "recommended_next_step"
  ),
  value = c(
    "44_make_Table1_main_text_compact.R",
    project_dir,
    as.character(nrow(table1_compact)),
    as.character(ncol(table1_compact)),
    as.character(n_failed_checks),
    dplyr::if_else(n_failed_checks == 0, "YES_AFTER_AUTHOR_VERIFICATION", "NO_FIX_CHECK_ITEMS"),
    "Exact sample-selection rules must still match the analysis code, especially GSE137340 time-point retention, GSE236713 SIRS handling, GSE54514 repeated samples, and GSE65682 comparator selection.",
    "Open the DOCX output and inspect whether the 7-column table fits the manuscript page. Use Additional file S1 for detailed references and audit notes."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 11. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nCompact main-text Table 1:\n")
print(table1_compact, n = Inf, width = Inf)

cat("\nAdditional file S1 preview:\n")
print(
  additional_file_s1 %>%
    dplyr::select(dplyr::any_of(c("Dataset", "Primary reference or data source", "final_author_verification_item"))),
  n = Inf,
  width = Inf
)

cat("\n关键输出：\n")
cat("1) ", csv_compact_path, "\n", sep = "")
cat("2) ", docx_compact_path, "\n", sep = "")
cat("3) ", xlsx_compact_path, "\n", sep = "")
cat("4) ", csv_additional_s1_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Compact main-text Table 1 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你做最终人工审稿式判断，并给出 Table 1 标题、脚注和正文引用句。\n")

cat("\n============ 44 compact Table 1 generation complete ============\n")