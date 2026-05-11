# ============================================================
# 43_cleanup_Table1_manuscript_language.R
# Purpose:
#   Clean Table 1 language for BMC Genomics manuscript insertion.
#   Separate concise main-text Table 1 from supplementary audit table.
#
# Input:
#   04_results/table1_final_for_BMC_Genomics/
#     T42_Table1_BMC_Genomics_submission_ready.csv
#     T42_Table1_manual_audit_items.csv
#
# Output:
#   04_results/table1_clean_for_BMC_Genomics/
#     T43_Table1_BMC_Genomics_main_text_clean.csv
#     T43_Table1_BMC_Genomics_main_text_clean.docx
#     T43_Table1_BMC_Genomics_clean_workbook.xlsx
#     T43_Table1_BMC_Genomics_supplementary_audit.csv
#     T43_Table1_language_cleanup_checks.csv
#     T43_Table1_language_cleanup_overall_status.csv
#
# Notes:
#   This script does NOT re-download GEO data.
#   It only cleans wording and separates manuscript table from audit material.
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
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

dir_in <- file.path(project_dir, "04_results", "table1_final_for_BMC_Genomics")
dir_out <- file.path(project_dir, "04_results", "table1_clean_for_BMC_Genomics")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

table1_in_path <- file.path(dir_in, "T42_Table1_BMC_Genomics_submission_ready.csv")
manual_audit_in_path <- file.path(dir_in, "T42_Table1_manual_audit_items.csv")

csv_main_clean_path <- file.path(dir_out, "T43_Table1_BMC_Genomics_main_text_clean.csv")
docx_main_clean_path <- file.path(dir_out, "T43_Table1_BMC_Genomics_main_text_clean.docx")
xlsx_clean_path <- file.path(dir_out, "T43_Table1_BMC_Genomics_clean_workbook.xlsx")
csv_supp_audit_path <- file.path(dir_out, "T43_Table1_BMC_Genomics_supplementary_audit.csv")
checks_path <- file.path(dir_out, "T43_Table1_language_cleanup_checks.csv")
overall_path <- file.path(dir_out, "T43_Table1_language_cleanup_overall_status.csv")

# -----------------------------
# 2. Read inputs
# -----------------------------
if (!file.exists(table1_in_path)) {
  stop("Input Table 1 not found: ", table1_in_path)
}

table1_raw <- read_csv(table1_in_path, show_col_types = FALSE)

manual_audit_raw <- NULL
if (file.exists(manual_audit_in_path)) {
  manual_audit_raw <- read_csv(manual_audit_in_path, show_col_types = FALSE)
}

expected_datasets <- c(
  "GSE137340",
  "GSE236713",
  "GSE54514",
  "GSE57065",
  "GSE65682",
  "GSE95233"
)

# -----------------------------
# 3. Clean main-text Table 1 manually
# -----------------------------
# Important:
#   The main text table must be concise and auditable.
#   It should not contain internal reminders such as:
#   "manual verify", "whether", "unless explicitly selected", "requires manual check".

table1_main_clean <- tribble(
  ~Dataset, ~`GEO accession`, ~Platform, ~`Sample source`, ~`Case definition`, ~`Comparator group used in this analysis`, ~`Intermediate or repeated samples handled in this analysis`, ~`Cohort role in this study`, ~`Primary reference or data source`,
  
  "GSE137340",
  "GSE137340",
  "GPL10558",
  "Peripheral blood or whole-blood-derived expression matrix",
  "Sepsis cases sampled at diagnosis and again after 24 hours.",
  "Age- and sex-matched healthy controls without inflammatory disease.",
  "Only the diagnostic or predefined sepsis time point was retained for the current binary comparison.",
  "Included bulk transcriptomic cohort for cross-dataset model development and held-out validation.",
  "GEO accession record; no linked PMID or DOI was indexed in the retrieved GEO metadata.",
  
  "GSE236713",
  "GSE236713",
  "GPL17077",
  "Blood or whole-blood-derived expression matrix",
  "Adult multi-center severe systemic inflammation cohort designed to evaluate mRNA biomarkers for sepsis and SIRS-related comparisons.",
  "Control samples defined by the original study metadata.",
  "SIRS or other intermediate inflammatory samples were handled according to the predefined binary sample-selection rule.",
  "Included bulk transcriptomic cohort for cross-dataset model development and held-out validation.",
  "PMID: 38332914; DOI: 10.3389/fimmu.2023.1308530",
  
  "GSE54514",
  "GSE54514",
  "GPL6947",
  "Whole blood",
  "Critically ill ICU patients with sepsis, with whole blood collected daily for up to 5 days.",
  "Non-sepsis comparator samples selected according to GSM-level phenotype annotation.",
  "Repeated longitudinal sepsis samples were handled according to the predefined sample-selection rule.",
  "Held-out validation cohort with low discrimination performance, retained as a transportability stress scenario.",
  "PMID: 23807251; DOI: 10.1097/SHK.0b013e31829ee604",
  
  "GSE57065",
  "GSE57065",
  "GPL570",
  "Whole blood or whole-blood-derived expression matrix",
  "Patients with septic shock profiled to characterize early and dynamic gene-expression changes.",
  "Healthy or non-sepsis control samples selected according to phenotype annotation.",
  "Early or serial sampling features were handled according to the predefined sample-selection rule.",
  "Included bulk transcriptomic cohort for cross-dataset model development and held-out validation.",
  "PMID: 30671061; DOI: 10.3389/fimmu.2018.03091",
  
  "GSE65682",
  "GSE65682",
  "GPL13667",
  "Whole blood or whole-blood leukocyte expression profiling",
  "Critically ill patients with sepsis or septic shock in a large prospective systems-biology cohort.",
  "Healthy controls selected for the current binary diagnostic comparison.",
  "Non-infectious ICU categories or other non-primary diagnostic groups were not used in the primary binary comparison.",
  "Included bulk transcriptomic cohort for cross-dataset model development and held-out validation.",
  "PMID: 26121490; DOI: 10.1164/rccm.201502-0355OC",
  
  "GSE95233",
  "GSE95233",
  "GPL570",
  "Whole blood or whole-blood-derived expression matrix",
  "Patients with septic shock profiled early to identify prognostic biomarkers according to 28-day mortality.",
  "Healthy volunteers or healthy controls selected according to phenotype annotation.",
  "No SIRS intermediate group was retained in the current binary comparison.",
  "Included bulk transcriptomic cohort for cross-dataset model development and held-out validation.",
  "PMID: 30671061; DOI: 10.3389/fimmu.2018.03091"
)

# Preserve expected order
table1_main_clean <- table1_main_clean %>%
  mutate(Dataset = factor(Dataset, levels = expected_datasets)) %>%
  arrange(Dataset) %>%
  mutate(Dataset = as.character(Dataset))

# -----------------------------
# 4. Supplementary audit table
# -----------------------------
# This table keeps limitations and author-side audit notes outside the main manuscript table.

audit_base <- table1_raw %>%
  select(
    Dataset,
    `GEO accession`,
    Platform,
    everything()
  )

table1_supp_audit <- table1_main_clean %>%
  select(
    Dataset,
    `GEO accession`,
    Platform,
    `Primary reference or data source`
  ) %>%
  left_join(
    audit_base %>%
      select(
        Dataset,
        raw_sample_source = `Sample source`,
        raw_case_definition = `Case definition`,
        raw_control_context = `Control context`,
        raw_intermediate_group_handling = `Intermediate groups handled in this analysis`,
        raw_cohort_role = `Cohort role in this study`,
        raw_original_pmid_doi = `Original PMID/DOI`,
        raw_main_limitation = `Main limitation for transportability analysis`
      ),
    by = "Dataset"
  )

if (!is.null(manual_audit_raw)) {
  # Join only columns that exist, avoiding fragile assumptions about the audit file structure.
  audit_cols <- intersect(
    c(
      "Dataset",
      "requires_manual_check",
      "manual_check_priority",
      "recommended_action"
    ),
    names(manual_audit_raw)
  )
  
  if ("Dataset" %in% audit_cols) {
    table1_supp_audit <- table1_supp_audit %>%
      left_join(
        manual_audit_raw %>% select(all_of(audit_cols)),
        by = "Dataset"
      )
  }
}

table1_supp_audit <- table1_supp_audit %>%
  mutate(
    final_author_verification_item = case_when(
      Dataset == "GSE137340" ~ "Confirm final retained time point and whether citation should remain GEO-only.",
      Dataset == "GSE236713" ~ "Confirm SIRS handling in the binary analysis and ensure SIRS was not ambiguously treated as healthy control.",
      Dataset == "GSE54514" ~ "Confirm repeated-sample handling and comparator annotation from GSM-level metadata.",
      Dataset == "GSE57065" ~ "Confirm early or serial sampling rule and comparator annotation.",
      Dataset == "GSE65682" ~ "Confirm healthy-control subset and exclusion of other ICU diagnostic categories from the primary binary comparison.",
      Dataset == "GSE95233" ~ "Confirm healthy-control definition and prognosis-oriented septic-shock context.",
      TRUE ~ "Confirm cohort-level metadata before final submission."
    )
  )

# -----------------------------
# 5. Language and integrity checks
# -----------------------------
collapse_table_text <- function(df) {
  df %>%
    mutate(across(everything(), as.character)) %>%
    tidyr::unite("all_text", everything(), sep = " ", remove = TRUE, na.rm = TRUE) %>%
    pull(all_text) %>%
    paste(collapse = " ")
}

main_text_all <- collapse_table_text(table1_main_clean)

forbidden_patterns <- c(
  "manual verify",
  "manually verify",
  "before final submission",
  "whether",
  "unless explicitly selected",
  "should whether",
  "requiring manual",
  "requires manual",
  "DRAFT_WITH_MANUAL",
  "CHECK",
  "exact control group",
  "current selected controls from GSM metadata"
)

contains_forbidden <- function(text, patterns) {
  any(str_detect(str_to_lower(text), str_to_lower(patterns)))
}

checks <- tibble(
  check_id = sprintf("C%02d", 1:14),
  check_item = c(
    "All six datasets retained",
    "No missing GEO accession",
    "No missing platform",
    "No missing sample source",
    "No missing case definition",
    "No missing comparator group",
    "No missing intermediate/repeated sample handling",
    "No missing cohort role",
    "No missing primary reference or data source",
    "No forbidden internal-audit phrases in main-text table",
    "GSE137340 grammar fixed",
    "GSE65682 primary reference shortened",
    "Supplementary audit table generated",
    "DOCX main-text table generated"
  ),
  observed = c(
    setequal(table1_main_clean$Dataset, expected_datasets) && nrow(table1_main_clean) == 6,
    all(!is.na(table1_main_clean$`GEO accession`) & table1_main_clean$`GEO accession` != ""),
    all(!is.na(table1_main_clean$Platform) & table1_main_clean$Platform != ""),
    all(!is.na(table1_main_clean$`Sample source`) & table1_main_clean$`Sample source` != ""),
    all(!is.na(table1_main_clean$`Case definition`) & table1_main_clean$`Case definition` != ""),
    all(!is.na(table1_main_clean$`Comparator group used in this analysis`) & table1_main_clean$`Comparator group used in this analysis` != ""),
    all(!is.na(table1_main_clean$`Intermediate or repeated samples handled in this analysis`) & table1_main_clean$`Intermediate or repeated samples handled in this analysis` != ""),
    all(!is.na(table1_main_clean$`Cohort role in this study`) & table1_main_clean$`Cohort role in this study` != ""),
    all(!is.na(table1_main_clean$`Primary reference or data source`) & table1_main_clean$`Primary reference or data source` != ""),
    !contains_forbidden(main_text_all, forbidden_patterns),
    !str_detect(main_text_all, fixed("should whether")),
    str_detect(
      table1_main_clean %>% filter(Dataset == "GSE65682") %>% pull(`Primary reference or data source`),
      fixed("10.1164/rccm.201502-0355OC")
    ) &&
      !str_detect(
        table1_main_clean %>% filter(Dataset == "GSE65682") %>% pull(`Primary reference or data source`),
        fixed("10.1016/S2213-2600")
      ),
    nrow(table1_supp_audit) == 6,
    TRUE
  )
) %>%
  mutate(status = if_else(observed, "PASS", "CHECK"))

n_failed_checks <- sum(checks$status != "PASS")

# -----------------------------
# 6. Export CSV files
# -----------------------------
write_csv(table1_main_clean, csv_main_clean_path)
write_csv(table1_supp_audit, csv_supp_audit_path)
write_csv(checks, checks_path)

# -----------------------------
# 7. Export XLSX workbook
# -----------------------------
wb <- createWorkbook()

addWorksheet(wb, "Main_Text_Table1_Clean")
writeData(wb, "Main_Text_Table1_Clean", table1_main_clean)

addWorksheet(wb, "Supplementary_Audit")
writeData(wb, "Supplementary_Audit", table1_supp_audit)

addWorksheet(wb, "Checks")
writeData(wb, "Checks", checks)

# Basic workbook styling
header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  fgFill = "#D9EAF7",
  border = "Bottom"
)

body_style <- createStyle(
  valign = "top",
  wrapText = TRUE
)

for (sheet in names(wb)) {
  addStyle(wb, sheet, header_style, rows = 1, cols = 1:ncol(readWorkbook(wb, sheet)), gridExpand = TRUE)
  addStyle(
    wb,
    sheet,
    body_style,
    rows = 2:(nrow(readWorkbook(wb, sheet)) + 1),
    cols = 1:ncol(readWorkbook(wb, sheet)),
    gridExpand = TRUE
  )
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:ncol(readWorkbook(wb, sheet)), widths = "auto")
}

saveWorkbook(wb, xlsx_clean_path, overwrite = TRUE)

# -----------------------------
# 8. Export DOCX main-text Table 1
# -----------------------------
ft <- flextable(table1_main_clean)

ft <- ft %>%
  theme_booktabs() %>%
  fontsize(size = 8, part = "all") %>%
  fontsize(size = 8.5, part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = c("Dataset", "GEO accession", "Platform"), align = "center", part = "body") %>%
  valign(valign = "top", part = "all") %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table 1. Bulk transcriptomic datasets included in the cross-dataset sepsis analysis", style = "heading 1") %>%
  body_add_par(
    "The table summarizes the datasets used for model development and held-out validation. Comparator and intermediate-group handling reflect the predefined binary sample-selection framework used in the current analysis.",
    style = "Normal"
  ) %>%
  body_add_flextable(ft) %>%
  body_add_par(
    "Abbreviations: GEO, Gene Expression Omnibus; ICU, intensive care unit; SIRS, systemic inflammatory response syndrome.",
    style = "Normal"
  )

print(doc, target = docx_main_clean_path)

# Re-check DOCX after writing
checks <- checks %>%
  mutate(
    observed = if_else(
      check_item == "DOCX main-text table generated",
      file.exists(docx_main_clean_path),
      observed
    ),
    status = if_else(observed, "PASS", "CHECK")
  )

n_failed_checks <- sum(checks$status != "PASS")
write_csv(checks, checks_path)

# -----------------------------
# 9. Overall status
# -----------------------------
overall_status <- tibble(
  metric = c(
    "script",
    "project_dir",
    "n_table_rows",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "main_remaining_risk",
    "recommended_next_step"
  ),
  value = c(
    "43_cleanup_Table1_manuscript_language.R",
    project_dir,
    as.character(nrow(table1_main_clean)),
    as.character(n_failed_checks),
    if_else(n_failed_checks == 0, "YES_AFTER_AUTHOR_VERIFICATION", "NO_FIX_CHECK_ITEMS"),
    "Final author verification is still needed for exact sample-selection rules, especially SIRS handling, repeated samples, and comparator definitions.",
    "Open the DOCX and XLSX outputs. Paste the clean main-text table into the manuscript, and keep the supplementary audit table for internal verification or supplementary material."
  )
)

write_csv(overall_status, overall_path)

# -----------------------------
# 10. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nClean main-text Table 1:\n")
print(table1_main_clean, n = Inf, width = Inf)

cat("\nSupplementary audit table preview:\n")
print(
  table1_supp_audit %>%
    select(Dataset, final_author_verification_item),
  n = Inf,
  width = Inf
)

cat("\n关键输出：\n")
cat("1) ", csv_main_clean_path, "\n", sep = "")
cat("2) ", docx_main_clean_path, "\n", sep = "")
cat("3) ", xlsx_clean_path, "\n", sep = "")
cat("4) ", csv_supp_audit_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Clean main-text Table 1 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你判断这张 clean Table 1 是否可以直接放入正文，还是应继续压缩成更适合 BMC Genomics 的主文表格。\n")

cat("\n============ 43 Table 1 manuscript-language cleanup complete ============\n")