# ============================================================
# 45_make_Table1_insertion_package.R
# Purpose:
#   Generate manuscript insertion package for Table 1 and
#   Additional file 1 / Supplementary Table S1.
#
# Input:
#   04_results/table1_compact_for_BMC_Genomics/
#     T44_Table1_BMC_Genomics_main_text_compact.csv
#     T44_Table1_BMC_Genomics_main_text_compact.docx
#     T44_Table1_BMC_Genomics_additional_file_S1.csv
#
# Output:
#   04_results/table1_insertion_package_for_BMC_Genomics/
#     T45_Table1_insertion_text.docx
#     T45_Table1_insertion_text.txt
#     T45_Table1_caption_and_footnote.csv
#     T45_Methods_Table1_reference_paragraph.txt
#     T45_Additional_file_1_description.txt
#     T45_Table1_insertion_package_checks.csv
#     T45_Table1_insertion_package_overall_status.csv
#
# Important:
#   This script does NOT modify the Table 1 data generated in Step 44.
#   It only prepares text and checks for manuscript insertion.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(officer)
  library(flextable)
  library(openxlsx)
  library(tidyr)
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

dir_in <- file.path(project_dir, "04_results", "table1_compact_for_BMC_Genomics")
dir_out <- file.path(project_dir, "04_results", "table1_insertion_package_for_BMC_Genomics")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

compact_csv_path <- file.path(dir_in, "T44_Table1_BMC_Genomics_main_text_compact.csv")
compact_docx_path <- file.path(dir_in, "T44_Table1_BMC_Genomics_main_text_compact.docx")
additional_s1_csv_path <- file.path(dir_in, "T44_Table1_BMC_Genomics_additional_file_S1.csv")

docx_insertion_path <- file.path(dir_out, "T45_Table1_insertion_text.docx")
txt_insertion_path <- file.path(dir_out, "T45_Table1_insertion_text.txt")
caption_csv_path <- file.path(dir_out, "T45_Table1_caption_and_footnote.csv")
methods_txt_path <- file.path(dir_out, "T45_Methods_Table1_reference_paragraph.txt")
additional_file_desc_txt_path <- file.path(dir_out, "T45_Additional_file_1_description.txt")
xlsx_package_path <- file.path(dir_out, "T45_Table1_insertion_package_workbook.xlsx")
checks_path <- file.path(dir_out, "T45_Table1_insertion_package_checks.csv")
overall_path <- file.path(dir_out, "T45_Table1_insertion_package_overall_status.csv")

# -----------------------------
# 2. Read Step 44 outputs
# -----------------------------
if (!file.exists(compact_csv_path)) {
  stop("Step 44 compact CSV not found: ", compact_csv_path)
}

if (!file.exists(additional_s1_csv_path)) {
  stop("Step 44 Additional file S1 CSV not found: ", additional_s1_csv_path)
}

table1_compact <- readr::read_csv(compact_csv_path, show_col_types = FALSE)
additional_file_s1 <- readr::read_csv(additional_s1_csv_path, show_col_types = FALSE)

expected_datasets <- c(
  "GSE137340",
  "GSE236713",
  "GSE54514",
  "GSE57065",
  "GSE65682",
  "GSE95233"
)

# -----------------------------
# 3. Define insertion texts
# -----------------------------
table1_title <- "Table 1. Bulk transcriptomic datasets included in the cross-dataset sepsis analysis"

table1_footnote_main <- paste0(
  "Comparator groups and intermediate or repeated samples were defined according to the predefined ",
  "binary sample-selection framework used in the current analysis. Detailed source references and ",
  "cohort-specific verification notes are provided in Supplementary Table S1."
)

table1_abbreviations <- paste0(
  "Abbreviations: GEO, Gene Expression Omnibus; ICU, intensive care unit; ",
  "SIRS, systemic inflammatory response syndrome."
)

methods_reference_paragraph <- paste0(
  "To minimize ambiguity in cross-cohort comparisons, each dataset was documented with respect to ",
  "platform, sample source, case definition, comparator definition, intermediate or repeated sample ",
  "handling, and analytical role in the present study (Table 1). Detailed source references and ",
  "cohort-specific verification notes are provided in Supplementary Table S1."
)

additional_file_1_section <- paste0(
  "Additional file 1: Supplementary Table S1. Source references and cohort-specific verification notes ",
  "for the included bulk transcriptomic datasets.\n",
  "File format: CSV.\n",
  "Description: This file provides the primary reference or GEO data source, detailed cohort-background ",
  "information, and author-side verification notes for the six bulk transcriptomic datasets included ",
  "in the cross-dataset sepsis analysis."
)

additional_file_1_in_text_sentence <- paste0(
  "Detailed source references and cohort-specific verification notes are provided in Supplementary Table S1 ",
  "[see Additional file 1]."
)

# -----------------------------
# 4. Build caption and insertion text tables
# -----------------------------
caption_and_footnote <- tibble::tibble(
  item = c(
    "table_title",
    "table_footnote",
    "abbreviations",
    "methods_reference_paragraph",
    "additional_file_1_in_text_sentence",
    "additional_file_1_section"
  ),
  text = c(
    table1_title,
    table1_footnote_main,
    table1_abbreviations,
    methods_reference_paragraph,
    additional_file_1_in_text_sentence,
    additional_file_1_section
  )
)

# -----------------------------
# 5. Write plain text outputs
# -----------------------------
insertion_text <- paste(
  "TABLE 1 TITLE",
  table1_title,
  "",
  "TABLE 1 FOOTNOTE",
  table1_footnote_main,
  "",
  table1_abbreviations,
  "",
  "METHODS REFERENCE PARAGRAPH",
  methods_reference_paragraph,
  "",
  "ADDITIONAL FILE 1 IN-TEXT SENTENCE",
  additional_file_1_in_text_sentence,
  "",
  "ADDITIONAL FILES SECTION",
  additional_file_1_section,
  sep = "\n"
)

writeLines(insertion_text, con = txt_insertion_path)
writeLines(methods_reference_paragraph, con = methods_txt_path)
writeLines(additional_file_1_section, con = additional_file_desc_txt_path)
readr::write_csv(caption_and_footnote, caption_csv_path)

# -----------------------------
# 6. Create Word insertion package
# -----------------------------
# The Word document is an insertion guide, not a replacement for the manuscript.
# Table 1 data itself remains the Step 44 output.

ft_caption <- flextable::flextable(caption_and_footnote)
ft_caption <- flextable::theme_booktabs(ft_caption)
ft_caption <- flextable::fontsize(ft_caption, size = 9, part = "all")
ft_caption <- flextable::fontsize(ft_caption, size = 9.5, part = "header")
ft_caption <- flextable::bold(ft_caption, part = "header")
ft_caption <- flextable::valign(ft_caption, valign = "top", part = "all")
ft_caption <- flextable::width(ft_caption, j = "item", width = 2.0)
ft_caption <- flextable::width(ft_caption, j = "text", width = 5.8)
ft_caption <- flextable::set_table_properties(ft_caption, layout = "fixed", width = 1)

ft_table1_preview <- flextable::flextable(table1_compact)
ft_table1_preview <- flextable::theme_booktabs(ft_table1_preview)
ft_table1_preview <- flextable::fontsize(ft_table1_preview, size = 7.5, part = "all")
ft_table1_preview <- flextable::fontsize(ft_table1_preview, size = 8, part = "header")
ft_table1_preview <- flextable::bold(ft_table1_preview, part = "header")
ft_table1_preview <- flextable::align(ft_table1_preview, align = "center", part = "header")
ft_table1_preview <- flextable::align(ft_table1_preview, j = c("Dataset", "Platform"), align = "center", part = "body")
ft_table1_preview <- flextable::valign(ft_table1_preview, valign = "top", part = "all")
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Dataset", width = 0.75)
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Platform", width = 0.7)
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Sample source", width = 1.25)
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Case definition", width = 1.85)
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Comparator used", width = 1.65)
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Sample-handling note", width = 2.0)
ft_table1_preview <- flextable::width(ft_table1_preview, j = "Role in this study", width = 1.6)
ft_table1_preview <- flextable::set_table_properties(ft_table1_preview, layout = "fixed", width = 1)

doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Table 1 insertion package for BMC Genomics manuscript",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "This document contains the text that should accompany the compact Table 1 generated in Step 44. It does not alter the table data.",
  style = "Normal"
)

doc <- officer::body_add_par(doc, "1. Caption, footnote, and insertion text", style = "heading 2")
doc <- flextable::body_add_flextable(doc, ft_caption)

doc <- officer::body_add_par(doc, "2. Methods reference paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_reference_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "3. Additional file 1 in-text sentence", style = "heading 2")
doc <- officer::body_add_par(doc, additional_file_1_in_text_sentence, style = "Normal")

doc <- officer::body_add_par(doc, "4. Additional files section", style = "heading 2")
doc <- officer::body_add_par(doc, additional_file_1_section, style = "Normal")

doc <- officer::body_add_par(doc, "5. Compact Table 1 preview", style = "heading 2")
doc <- officer::body_add_par(doc, table1_title, style = "Normal")
doc <- officer::body_add_par(doc, table1_footnote_main, style = "Normal")
doc <- flextable::body_add_flextable(doc, ft_table1_preview)
doc <- officer::body_add_par(doc, table1_abbreviations, style = "Normal")

print(doc, target = docx_insertion_path)

# -----------------------------
# 7. Create XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Insertion_Text")
openxlsx::writeData(wb, "Insertion_Text", caption_and_footnote)

openxlsx::addWorksheet(wb, "Table1_Compact")
openxlsx::writeData(wb, "Table1_Compact", table1_compact)

openxlsx::addWorksheet(wb, "Additional_File_S1")
openxlsx::writeData(wb, "Additional_File_S1", additional_file_s1)

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

openxlsx::saveWorkbook(wb, xlsx_package_path, overwrite = TRUE)

# -----------------------------
# 8. Consistency checks
# -----------------------------
all_insertion_text <- paste(
  table1_title,
  table1_footnote_main,
  table1_abbreviations,
  methods_reference_paragraph,
  additional_file_1_in_text_sentence,
  additional_file_1_section,
  collapse = " "
)

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:18),
  check_item = c(
    "Step 44 compact CSV exists",
    "Step 44 compact DOCX exists",
    "Step 44 Additional file S1 CSV exists",
    "Compact Table 1 has six rows",
    "Compact Table 1 has seven columns",
    "All expected datasets present",
    "Methods paragraph mentions Table 1",
    "Methods paragraph mentions Supplementary Table S1",
    "Additional file in-text sentence mentions Additional file 1",
    "Additional files section includes file name",
    "Additional files section includes file format",
    "Additional files section includes title",
    "Additional files section includes description",
    "Table footnote mentions predefined binary sample-selection framework",
    "Abbreviations include GEO, ICU and SIRS",
    "Insertion Word document generated",
    "Insertion plain-text document generated",
    "Insertion workbook generated"
  ),
  observed = c(
    file.exists(compact_csv_path),
    file.exists(compact_docx_path),
    file.exists(additional_s1_csv_path),
    nrow(table1_compact) == 6,
    ncol(table1_compact) == 7,
    setequal(table1_compact$Dataset, expected_datasets),
    stringr::str_detect(methods_reference_paragraph, stringr::fixed("Table 1")),
    stringr::str_detect(methods_reference_paragraph, stringr::fixed("Supplementary Table S1")),
    stringr::str_detect(additional_file_1_in_text_sentence, stringr::fixed("Additional file 1")),
    stringr::str_detect(additional_file_1_section, stringr::fixed("Additional file 1")),
    stringr::str_detect(additional_file_1_section, stringr::fixed("File format: CSV")),
    stringr::str_detect(additional_file_1_section, stringr::fixed("Supplementary Table S1")),
    stringr::str_detect(additional_file_1_section, stringr::fixed("Description:")),
    stringr::str_detect(table1_footnote_main, stringr::fixed("predefined binary sample-selection framework")),
    all(stringr::str_detect(table1_abbreviations, c("GEO", "ICU", "SIRS"))),
    file.exists(docx_insertion_path),
    file.exists(txt_insertion_path),
    file.exists(xlsx_package_path)
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

readr::write_csv(checks, checks_path)

# -----------------------------
# 9. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "table1_source",
    "additional_file_s1_source",
    "recommended_next_step"
  ),
  value = c(
    "45_make_Table1_insertion_package.R",
    project_dir,
    as.character(n_failed_checks),
    dplyr::if_else(n_failed_checks == 0, "YES_INSERT_TABLE1_AND_TEXT", "NO_FIX_CHECK_ITEMS"),
    compact_csv_path,
    additional_s1_csv_path,
    "Open T45_Table1_insertion_text.docx and copy the title, footnote, Methods paragraph, and Additional file 1 section into the manuscript. Keep the Step 44 compact DOCX as the main Table 1 source."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 10. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nTable 1 title:\n")
cat(table1_title, "\n")

cat("\nTable 1 footnote:\n")
cat(table1_footnote_main, "\n")
cat(table1_abbreviations, "\n")

cat("\nMethods reference paragraph:\n")
cat(methods_reference_paragraph, "\n")

cat("\nAdditional file 1 in-text sentence:\n")
cat(additional_file_1_in_text_sentence, "\n")

cat("\nAdditional files section:\n")
cat(additional_file_1_section, "\n")

cat("\n关键输出：\n")
cat("1) ", docx_insertion_path, "\n", sep = "")
cat("2) ", txt_insertion_path, "\n", sep = "")
cat("3) ", caption_csv_path, "\n", sep = "")
cat("4) ", methods_txt_path, "\n", sep = "")
cat("5) ", additional_file_desc_txt_path, "\n", sep = "")
cat("6) ", xlsx_package_path, "\n", sep = "")
cat("7) ", checks_path, "\n", sep = "")
cat("8) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Table 1 title、Methods reference paragraph 和 Additional files section 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你确认 Table 1 插入包是否可以正式并入 manuscript。\n")

cat("\n============ 45 Table 1 insertion package complete ============\n")