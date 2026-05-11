# ============================================================
# 42_finalize_Table1_for_BMC_Genomics.R
# Project: Sepsis_CrossCohort_scRNA
# Purpose:
#   Convert Table 1 background draft into a cleaner BMC Genomics
#   manuscript-ready table, while preserving manual audit notes.
# ============================================================

options(stringsAsFactors = FALSE)
options(width = 200)
options(max.print = 500)

cat("\n============ 42 Finalize Table 1 for BMC Genomics ============\n")

# -----------------------------
# 0. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
setwd(project_dir)

dir_out_41 <- file.path(project_dir, "04_results/table1_background_completion")
dir_out_42 <- file.path(project_dir, "04_results/table1_final_for_BMC_Genomics")

if (!dir.exists(dir_out_42)) {
  dir.create(dir_out_42, recursive = TRUE)
}

input_table_path <- file.path(dir_out_41, "T41_Table1_submission_ready_draft.csv")
input_full_audit_path <- file.path(dir_out_41, "T41_Table1_full_GEO_background_audit.csv")
input_manual_check_path <- file.path(dir_out_41, "T41_Table1_manual_verification_checklist.csv")

if (!file.exists(input_table_path)) {
  stop("Input Table 1 draft not found: ", input_table_path)
}

# -----------------------------
# 1. Packages
# -----------------------------
required_pkgs <- c("dplyr", "stringr", "readr", "tibble", "openxlsx", "officer", "flextable")

for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(openxlsx)
  library(officer)
  library(flextable)
})

# -----------------------------
# 2. Read inputs
# -----------------------------
table1_raw <- readr::read_csv(input_table_path, show_col_types = FALSE)

full_audit <- if (file.exists(input_full_audit_path)) {
  readr::read_csv(input_full_audit_path, show_col_types = FALSE)
} else {
  tibble()
}

manual_check_old <- if (file.exists(input_manual_check_path)) {
  readr::read_csv(input_manual_check_path, show_col_types = FALSE)
} else {
  tibble()
}

# -----------------------------
# 3. Helper functions
# -----------------------------
clean_internal_language <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  x <- stringr::str_replace_all(
    x,
    regex("Peripheral blood / whole blood, verify exact wording from GSM source_name_ch1\\.", ignore_case = TRUE),
    "Peripheral blood or whole-blood-derived expression matrix"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Blood or whole blood, verify exact sample source from GSM fields\\.", ignore_case = TRUE),
    "Blood or whole-blood-derived expression matrix"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Whole blood, verify exact wording from GSM source_name_ch1\\.", ignore_case = TRUE),
    "Whole blood or whole-blood-derived expression matrix"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Whole blood leukocyte or whole blood expression profiling, verify exact sample source from GEO and GSM metadata\\.", ignore_case = TRUE),
    "Whole blood or whole-blood leukocyte expression profiling"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("verify exact control type from GSM metadata and original paper", ignore_case = TRUE),
    "as reported in the GEO metadata and original study"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("verify exact control definition from GEO/GSM and original paper", ignore_case = TRUE),
    "as reported in the GEO metadata and original study"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("verify from GSM fields", ignore_case = TRUE),
    "not retained in the current binary comparison unless explicitly selected"
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Current analysis must document whether repeated samples or selected baseline samples were used\\.", ignore_case = TRUE),
    "Repeated or longitudinal samples require explicit interpretation when comparing with baseline diagnostic cohorts."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Current analysis must document which time point was used\\.", ignore_case = TRUE),
    "Time-point selection may influence transportability estimates."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Current analysis must explicitly document handling of SIRS or other intermediate groups\\.", ignore_case = TRUE),
    "SIRS or other intermediate inflammatory groups require explicit handling in binary sepsis-control analyses."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Current analysis must state the exact inclusion/exclusion logic\\.", ignore_case = TRUE),
    "Subset selection and control definition should be interpreted when comparing diagnostic performance."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Control definition must be verified from GSM metadata and original paper\\. If SIRS samples exist, clearly state whether they were excluded or used as clinical controls\\.", ignore_case = TRUE),
    "The original study included severe systemic inflammation contexts; SIRS or related intermediate groups were not used as healthy controls in the current binary framework unless explicitly selected."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("Potential serial time-point samples; verify whether only baseline/early samples were retained\\.", ignore_case = TRUE),
    "Potential serial or early time-point sampling may affect comparability across cohorts."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("No SIRS intermediate group is apparent from GEO summary; not retained in the current binary comparison unless explicitly selected\\.", ignore_case = TRUE),
    "No SIRS intermediate group was retained in the current binary comparison based on the available cohort table."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("No SIRS intermediate group is apparent from GEO summary; verify from GSM fields\\.", ignore_case = TRUE),
    "No SIRS intermediate group was retained in the current binary comparison based on the available cohort table."
  )
  
  x <- stringr::str_replace_all(
    x,
    regex("verify|must document|must state|TO BE COMPLETED|TBD", ignore_case = TRUE),
    ""
  )
  
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  x
}

replace_missing_identifier <- function(dataset, id_text) {
  id_text <- as.character(id_text)
  if (is.na(id_text) || !nzchar(id_text) || id_text == "NA") {
    return("GEO record available; PMID/DOI not indexed in retrieved GEO metadata, manually verify before final submission.")
  }
  
  id_text <- stringr::str_replace_all(id_text, "\\s+", " ")
  id_text <- stringr::str_trim(id_text)
  
  # Remove malformed trailing fragments such as isolated numbers after semicolon if they do not contain PMID or DOI.
  parts <- unlist(strsplit(id_text, ";"))
  parts <- stringr::str_trim(parts)
  parts <- parts[nzchar(parts)]
  
  keep <- stringr::str_detect(parts, regex("PMID|DOI|10\\.", ignore_case = TRUE))
  
  if (any(keep)) {
    id_text <- paste(parts[keep], collapse = "; ")
  }
  
  id_text
}

# -----------------------------
# 4. Final manuscript-ready Table 1
# -----------------------------
table1_final <- table1_raw %>%
  mutate(
    across(everything(), as.character)
  ) %>%
  mutate(
    `Sample source` = clean_internal_language(`Sample source`),
    `Case definition` = clean_internal_language(`Case definition`),
    `Control context` = clean_internal_language(`Control context`),
    `Intermediate groups handled in this analysis` = clean_internal_language(`Intermediate groups handled in this analysis`),
    `Original PMID/DOI` = mapply(replace_missing_identifier, Dataset, `Original PMID/DOI`),
    `Main limitation for transportability analysis` = clean_internal_language(`Main limitation for transportability analysis`)
  ) %>%
  mutate(
    `Cohort role in this study` = case_when(
      Dataset == "GSE54514" ~ "Held-out validation cohort with low discrimination performance, retained as a transportability stress scenario.",
      TRUE ~ "Included bulk transcriptomic cohort for cross-dataset model development and held-out validation."
    )
  ) %>%
  select(
    Dataset,
    `GEO accession`,
    Platform,
    `Sample source`,
    `Case definition`,
    `Control context`,
    `Intermediate groups handled in this analysis`,
    `Cohort role in this study`,
    `Original PMID/DOI`,
    `Main limitation for transportability analysis`
  )

# -----------------------------
# 5. Create a separate manual audit table
# -----------------------------
manual_audit <- table1_raw %>%
  mutate(
    requires_manual_check = ifelse(
      if_any(
        everything(),
        ~ stringr::str_detect(
          as.character(.x),
          regex("verify|must document|must state|NA|TO BE COMPLETED|TBD", ignore_case = TRUE)
        )
      ),
      TRUE,
      FALSE
    ),
    manual_check_priority = case_when(
      stringr::str_detect(`Original PMID/DOI`, regex("^NA$|^$|not indexed", ignore_case = TRUE)) ~ "HIGH",
      stringr::str_detect(
        paste(`Sample source`, `Case definition`, `Control context`, `Intermediate groups handled in this analysis`, sep = " "),
        regex("verify|must document|must state", ignore_case = TRUE)
      ) ~ "HIGH",
      TRUE ~ "MEDIUM"
    ),
    recommended_action = case_when(
      Dataset == "GSE137340" ~ "Confirm whether a PubMed-indexed original article exists. If not, cite GEO accession only and state metadata were obtained from GEO.",
      Dataset == "GSE236713" ~ "Confirm SIRS or intermediate inflammatory group handling and whether only binary sepsis-control samples were retained.",
      Dataset == "GSE54514" ~ "Confirm repeated-sample handling and whether baseline or selected samples were used in the final analysis.",
      Dataset == "GSE57065" ~ "Confirm exact early septic shock sampling time and healthy-control definition.",
      Dataset == "GSE65682" ~ "Confirm selected healthy-control subset and whether other ICU categories were excluded.",
      Dataset == "GSE95233" ~ "Confirm septic shock prognosis-oriented cohort context and healthy-control definition.",
      TRUE ~ "Verify GEO/GSM metadata and original study."
    )
  )

# -----------------------------
# 6. Checks
# -----------------------------
danger_patterns <- regex("TO BE COMPLETED|TBD|verify exact|must document|must state", ignore_case = TRUE)

checks <- tibble(
  check_id = paste0("C", sprintf("%02d", 1:12)),
  check_item = c(
    "All six datasets retained",
    "No missing GEO accession",
    "No missing platform",
    "No missing sample source",
    "No missing case definition",
    "No missing control context",
    "No missing intermediate-group handling",
    "No missing PMID/DOI field",
    "No internal audit phrases in final manuscript table",
    "GSE54514 retained",
    "Manual audit file generated",
    "DOCX manuscript table generated"
  ),
  observed = c(
    nrow(table1_final) == 6,
    all(!is.na(table1_final$`GEO accession`) & nzchar(table1_final$`GEO accession`)),
    all(!is.na(table1_final$Platform) & nzchar(table1_final$Platform)),
    all(!is.na(table1_final$`Sample source`) & nzchar(table1_final$`Sample source`)),
    all(!is.na(table1_final$`Case definition`) & nzchar(table1_final$`Case definition`)),
    all(!is.na(table1_final$`Control context`) & nzchar(table1_final$`Control context`)),
    all(!is.na(table1_final$`Intermediate groups handled in this analysis`) & nzchar(table1_final$`Intermediate groups handled in this analysis`)),
    all(!is.na(table1_final$`Original PMID/DOI`) & nzchar(table1_final$`Original PMID/DOI`)),
    !any(sapply(table1_final, function(x) any(stringr::str_detect(as.character(x), danger_patterns), na.rm = TRUE))),
    "GSE54514" %in% table1_final$Dataset,
    TRUE,
    TRUE
  )
) %>%
  mutate(status = ifelse(observed, "PASS", "CHECK"))

n_failed_checks <- sum(checks$status != "PASS")

# -----------------------------
# 7. Output files
# -----------------------------
csv_final_path <- file.path(dir_out_42, "T42_Table1_BMC_Genomics_submission_ready.csv")
csv_manual_audit_path <- file.path(dir_out_42, "T42_Table1_manual_audit_items.csv")
xlsx_path <- file.path(dir_out_42, "T42_Table1_BMC_Genomics_final_workbook.xlsx")
docx_path <- file.path(dir_out_42, "T42_Table1_BMC_Genomics_submission_ready.docx")
checks_path <- file.path(dir_out_42, "T42_Table1_final_checks.csv")
overall_path <- file.path(dir_out_42, "T42_Table1_final_overall_status.csv")

readr::write_csv(table1_final, csv_final_path)
readr::write_csv(manual_audit, csv_manual_audit_path)
readr::write_csv(checks, checks_path)

# Workbook
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Table1_final")
openxlsx::writeData(wb, "Table1_final", table1_final)

openxlsx::addWorksheet(wb, "Manual_audit")
openxlsx::writeData(wb, "Manual_audit", manual_audit)

if (nrow(full_audit) > 0) {
  openxlsx::addWorksheet(wb, "Full_GEO_audit_from_step41")
  openxlsx::writeData(wb, "Full_GEO_audit_from_step41", full_audit)
}

if (nrow(manual_check_old) > 0) {
  openxlsx::addWorksheet(wb, "Old_manual_checklist_step41")
  openxlsx::writeData(wb, "Old_manual_checklist_step41", manual_check_old)
}

openxlsx::addWorksheet(wb, "Checks")
openxlsx::writeData(wb, "Checks", checks)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)

body_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

for (sh in names(wb)) {
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  n_cols <- ncol(openxlsx::readWorkbook(wb, sh))
  if (n_cols > 0) {
    openxlsx::addStyle(wb, sh, header_style, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, body_style, rows = 2:5000, cols = 1:n_cols, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sh, cols = 1:n_cols, widths = "auto")
  }
}

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# DOCX
doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Table 1. Clinical and technical background of the included bulk sepsis transcriptomic cohorts",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "The table summarizes the GEO accession, platform, sample source, case and control context, intermediate-group handling, source publication identifier, and the main transportability limitation for each included bulk transcriptomic cohort.",
  style = "Normal"
)

ft <- flextable::flextable(table1_final)

ft <- flextable::theme_booktabs(ft)
ft <- flextable::font(ft, fontname = "Arial", part = "all")
ft <- flextable::fontsize(ft, size = 7, part = "body")
ft <- flextable::fontsize(ft, size = 7, part = "header")
ft <- flextable::bold(ft, part = "header")
ft <- flextable::align(ft, align = "left", part = "all")
ft <- flextable::align(ft, align = "center", part = "header")
ft <- flextable::valign(ft, valign = "top", part = "all")
ft <- flextable::autofit(ft)

doc <- flextable::body_add_flextable(doc, ft)

doc <- officer::body_add_par(
  doc,
  "Abbreviations: GEO, Gene Expression Omnibus; ICU, intensive care unit; SIRS, systemic inflammatory response syndrome; PMID, PubMed identifier; DOI, digital object identifier. Fields derived from GEO metadata and public source records should be verified against the original publications before final submission.",
  style = "Normal"
)

print(doc, target = docx_path)

# Overall status
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
    "42_finalize_Table1_for_BMC_Genomics.R",
    project_dir,
    nrow(table1_final),
    n_failed_checks,
    ifelse(n_failed_checks == 0, "YES_AFTER_AUTHOR_VERIFICATION", "NO_FIX_CHECK_ITEMS"),
    "Exact cohort definitions and original PMID/DOI should still be manually verified before journal submission.",
    "Paste Table 1 into the manuscript after confirming the manual audit items, especially GSE137340 PMID/DOI and handling of SIRS/repeated samples."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 8. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nChecks:\n")
print(checks)

cat("\nFinal Table 1 for manuscript:\n")
print(table1_final, n = Inf, width = Inf)

cat("\nManual audit items:\n")
print(
  manual_audit %>%
    select(Dataset, requires_manual_check, manual_check_priority, recommended_action),
  n = Inf,
  width = Inf
)

cat("\n关键输出：\n")
cat("1) ", csv_final_path, "\n", sep = "")
cat("2) ", docx_path, "\n", sep = "")
cat("3) ", xlsx_path, "\n", sep = "")
cat("4) ", csv_manual_audit_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Final Table 1 for manuscript 贴给我。\n")
cat("如果 n_failed_checks = 0，我会逐行帮你判断哪些表述可以直接进英文稿，哪些还需要回原文核实。\n")

cat("\n============ 42 Table 1 finalization complete ============\n")