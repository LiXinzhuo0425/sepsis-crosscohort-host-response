# ============================================================
# 46d_finalize_signature_text_package.R
# Purpose:
#   Finalize manuscript-ready text package for the final 10-gene
#   host-response signature after Step 46c.
#
# This script does NOT modify the 46c data.
# It reads:
#   04_results/signature_definition_final_10_genes_46c/
#     T46c_final_10_gene_signature_definition.csv
#
# It generates:
#   1) Final Table 2 title
#   2) Final Table 2 footnote
#   3) Methods signature-definition paragraph
#   4) Results signature-definition paragraph
#   5) Terminology consistency statement
#   6) Additional file 2 section
#   7) A DOCX insertion package
#   8) A TXT insertion package
#   9) Checks and overall status
#
# Output:
#   04_results/signature_definition_final_10_genes_46d/
#     T46d_signature_text_package.docx
#     T46d_signature_text_package.txt
#     T46d_signature_text_package.csv
#     T46d_signature_text_package_workbook.xlsx
#     T46d_signature_text_checks.csv
#     T46d_signature_text_overall_status.csv
#
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(openxlsx)
  library(flextable)
  library(officer)
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

dir_in <- file.path(
  project_dir,
  "04_results",
  "signature_definition_final_10_genes_46c"
)

dir_out <- file.path(
  project_dir,
  "04_results",
  "signature_definition_final_10_genes_46d"
)

dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

signature_in_path <- file.path(
  dir_in,
  "T46c_final_10_gene_signature_definition.csv"
)

docx_package_path <- file.path(dir_out, "T46d_signature_text_package.docx")
txt_package_path <- file.path(dir_out, "T46d_signature_text_package.txt")
csv_text_package_path <- file.path(dir_out, "T46d_signature_text_package.csv")
xlsx_package_path <- file.path(dir_out, "T46d_signature_text_package_workbook.xlsx")
checks_path <- file.path(dir_out, "T46d_signature_text_checks.csv")
overall_path <- file.path(dir_out, "T46d_signature_text_overall_status.csv")

# -----------------------------
# 2. Read 46c final signature table
# -----------------------------
if (!file.exists(signature_in_path)) {
  stop("46c final signature table not found: ", signature_in_path)
}

signature_tbl <- readr::read_csv(signature_in_path, show_col_types = FALSE)

required_cols <- c(
  "gene_order",
  "gene",
  "nested_lodo_occurrence_count",
  "selected_folds",
  "sepsis_direction",
  "used_in_final_bulk_model",
  "used_in_single_cell_module_score",
  "evidence_status"
)

missing_cols <- setdiff(required_cols, names(signature_tbl))

if (length(missing_cols) > 0) {
  stop(
    "The following required columns are missing from 46c signature table: ",
    paste(missing_cols, collapse = ", ")
  )
}

signature_tbl <- signature_tbl %>%
  dplyr::arrange(gene_order)

# -----------------------------
# 3. Derive final gene-set summaries
# -----------------------------
final_genes <- signature_tbl %>%
  dplyr::arrange(gene_order) %>%
  dplyr::pull(gene)

final_gene_list_text <- paste(final_genes, collapse = ", ")

six_fold_genes <- signature_tbl %>%
  dplyr::filter(nested_lodo_occurrence_count == 6) %>%
  dplyr::arrange(gene_order) %>%
  dplyr::pull(gene)

non_six_fold_tbl <- signature_tbl %>%
  dplyr::filter(nested_lodo_occurrence_count != 6) %>%
  dplyr::arrange(gene_order)

six_fold_gene_list_text <- paste(six_fold_genes, collapse = ", ")

non_six_fold_text <- if (nrow(non_six_fold_tbl) == 0) {
  "None"
} else {
  non_six_fold_tbl %>%
    dplyr::mutate(
      item = paste0(gene, " (", nested_lodo_occurrence_count, "/6)")
    ) %>%
    dplyr::pull(item) %>%
    paste(collapse = "; ")
}

all_upregulated <- all(signature_tbl$sepsis_direction == "Upregulated in sepsis")
all_used_bulk <- all(signature_tbl$used_in_final_bulk_model == "YES")
all_used_sc <- all(signature_tbl$used_in_single_cell_module_score == "YES")
all_complete <- all(signature_tbl$evidence_status == "COMPLETE")

final_equals_six_fold_subset <- setequal(final_genes, six_fold_genes) &&
  length(final_genes) == length(six_fold_genes)

n_final_genes <- length(final_genes)
n_six_fold_genes <- length(six_fold_genes)
n_non_six_fold_genes <- nrow(non_six_fold_tbl)

# -----------------------------
# 4. Final manuscript-ready text
# -----------------------------
# Table title should stay <= 15 words for BMC-style table requirements.
table2_title <- "Table 2. Definition and nested recurrence of the final 10-gene signature"

methods_signature_paragraph <- paste0(
  "The final 10-gene host-response signature was defined from the consolidated FINAL10 gene set after ",
  "nested leave-one-dataset-out feature selection and downstream signature consolidation. For each gene, ",
  "we recorded the number of nested LODO folds in which it was retained, the direction of differential ",
  "expression in sepsis, whether it was included in the final bulk analysis, and whether it was used for ",
  "single-cell module-score calculation. The six-fold recurrent nested LODO subset was defined as FINAL10 ",
  "genes retained in all six nested LODO folds and was reported separately from the complete final ",
  "10-gene signature."
)

results_signature_paragraph <- paste0(
  "The final 10-gene host-response signature comprised ",
  final_gene_list_text,
  ". All 10 genes were upregulated in sepsis, included in the final bulk analysis, and used for ",
  "single-cell module-score calculation. Nine genes were retained in all six nested LODO folds, including ",
  six_fold_gene_list_text,
  "; ",
  non_six_fold_text,
  " was retained in fewer folds. The recurrence, expression-direction, final bulk-analysis inclusion, ",
  "and single-cell module-score inclusion status of all signature genes are summarized in Table 2 and ",
  "Supplementary Table S2."
)

# If more than one non-6/6 gene exists, grammar is adjusted.
if (n_non_six_fold_genes != 1) {
  results_signature_paragraph <- paste0(
    "The final 10-gene host-response signature comprised ",
    final_gene_list_text,
    ". All 10 genes were upregulated in sepsis, included in the final bulk analysis, and used for ",
    "single-cell module-score calculation. ",
    n_six_fold_genes,
    " genes were retained in all six nested LODO folds, including ",
    six_fold_gene_list_text,
    ". Genes retained in fewer than six folds were: ",
    non_six_fold_text,
    ". The recurrence, expression-direction, final bulk-analysis inclusion, and single-cell module-score ",
    "inclusion status of all signature genes are summarized in Table 2 and Supplementary Table S2."
  )
}

terminology_consistency_statement <- paste0(
  "In this manuscript, the terms final 10-gene host-response signature and FINAL10 gene set refer to the ",
  "same 10 genes used in both the final bulk analysis and single-cell module-score analysis. The recurrent ",
  "nested LODO gene subset refers specifically to the subset of FINAL10 genes retained across all six ",
  "nested LODO folds."
)

table2_footnote <- paste0(
  "Nested LODO occurrence indicates the number of leave-one-dataset-out feature-selection folds in which ",
  "the gene was retained, with possible values from 0 to 6. Direction refers to differential expression in ",
  "sepsis relative to the comparator group in the main bulk differential-expression analysis. Bulk-analysis ",
  "and single-cell module-score columns indicate whether the gene was used in the final bulk analysis and ",
  "single-cell module-score calculation, respectively. The final 10-gene host-response signature is not ",
  "identical to the six-fold recurrent nested LODO subset because ",
  non_six_fold_text,
  " was retained in fewer than six folds."
)

# If no non-6/6 gene exists, adjust table footnote.
if (n_non_six_fold_genes == 0) {
  table2_footnote <- paste0(
    "Nested LODO occurrence indicates the number of leave-one-dataset-out feature-selection folds in which ",
    "the gene was retained, with possible values from 0 to 6. Direction refers to differential expression in ",
    "sepsis relative to the comparator group in the main bulk differential-expression analysis. Bulk-analysis ",
    "and single-cell module-score columns indicate whether the gene was used in the final bulk analysis and ",
    "single-cell module-score calculation, respectively. In this analysis, all FINAL10 genes were retained ",
    "across all six nested LODO folds."
  )
}

additional_file_2_section <- paste0(
  "Additional file 2: Supplementary Table S2. Final 10-gene signature definition and nested LODO recurrence audit.\n",
  "File format: XLSX.\n",
  "Description: This file provides the final 10-gene signature list, nested LODO occurrence counts, ",
  "sepsis-associated expression direction, final bulk-analysis inclusion status, single-cell module-score ",
  "inclusion status, source-file evidence, and consistency checks comparing the FINAL10 signature with ",
  "the six-fold recurrent nested LODO subset."
)

additional_file_2_in_text_sentence <- paste0(
  "Detailed recurrence counts, expression-direction annotations, source-file evidence, and consistency ",
  "checks for the final 10-gene signature are provided in Supplementary Table S2 [see Additional file 2]."
)

# -----------------------------
# 5. Text package table
# -----------------------------
text_package <- tibble::tibble(
  item = c(
    "table2_title",
    "methods_signature_paragraph",
    "results_signature_paragraph",
    "terminology_consistency_statement",
    "table2_footnote",
    "additional_file_2_in_text_sentence",
    "additional_file_2_section"
  ),
  text = c(
    table2_title,
    methods_signature_paragraph,
    results_signature_paragraph,
    terminology_consistency_statement,
    table2_footnote,
    additional_file_2_in_text_sentence,
    additional_file_2_section
  )
)

summary_tbl <- tibble::tibble(
  metric = c(
    "n_final_genes",
    "final_gene_list",
    "n_six_fold_recurrent_genes",
    "six_fold_recurrent_genes",
    "n_non_six_fold_genes",
    "non_six_fold_genes",
    "final_signature_equals_six_fold_recurrent_subset",
    "all_genes_upregulated_in_sepsis",
    "all_genes_used_in_final_bulk_analysis",
    "all_genes_used_in_single_cell_module_score",
    "all_evidence_status_complete"
  ),
  value = c(
    as.character(n_final_genes),
    final_gene_list_text,
    as.character(n_six_fold_genes),
    six_fold_gene_list_text,
    as.character(n_non_six_fold_genes),
    non_six_fold_text,
    as.character(final_equals_six_fold_subset),
    as.character(all_upregulated),
    as.character(all_used_bulk),
    as.character(all_used_sc),
    as.character(all_complete)
  )
)

# -----------------------------
# 6. Write TXT and CSV outputs
# -----------------------------
plain_text <- paste(
  "TABLE 2 TITLE",
  table2_title,
  "",
  "METHODS: SIGNATURE DEFINITION",
  methods_signature_paragraph,
  "",
  "RESULTS: SIGNATURE DEFINITION",
  results_signature_paragraph,
  "",
  "TERMINOLOGY CONSISTENCY STATEMENT",
  terminology_consistency_statement,
  "",
  "TABLE 2 FOOTNOTE",
  table2_footnote,
  "",
  "ADDITIONAL FILE 2 IN-TEXT SENTENCE",
  additional_file_2_in_text_sentence,
  "",
  "ADDITIONAL FILE 2 SECTION",
  additional_file_2_section,
  sep = "\n"
)

writeLines(plain_text, con = txt_package_path)
readr::write_csv(text_package, csv_text_package_path)

# -----------------------------
# 7. Create DOCX insertion package
# -----------------------------
ft_text <- flextable::flextable(text_package)
ft_text <- flextable::theme_booktabs(ft_text)
ft_text <- flextable::fontsize(ft_text, size = 9, part = "all")
ft_text <- flextable::fontsize(ft_text, size = 9.5, part = "header")
ft_text <- flextable::bold(ft_text, part = "header")
ft_text <- flextable::valign(ft_text, valign = "top", part = "all")
ft_text <- flextable::width(ft_text, j = "item", width = 2.2)
ft_text <- flextable::width(ft_text, j = "text", width = 5.8)
ft_text <- flextable::set_table_properties(ft_text, layout = "fixed", width = 1)

ft_summary <- flextable::flextable(summary_tbl)
ft_summary <- flextable::theme_booktabs(ft_summary)
ft_summary <- flextable::fontsize(ft_summary, size = 8.5, part = "all")
ft_summary <- flextable::fontsize(ft_summary, size = 9, part = "header")
ft_summary <- flextable::bold(ft_summary, part = "header")
ft_summary <- flextable::valign(ft_summary, valign = "top", part = "all")
ft_summary <- flextable::width(ft_summary, j = "metric", width = 2.4)
ft_summary <- flextable::width(ft_summary, j = "value", width = 5.6)
ft_summary <- flextable::set_table_properties(ft_summary, layout = "fixed", width = 1)

ft_sig <- flextable::flextable(signature_tbl)
ft_sig <- flextable::theme_booktabs(ft_sig)
ft_sig <- flextable::fontsize(ft_sig, size = 7, part = "all")
ft_sig <- flextable::fontsize(ft_sig, size = 7.5, part = "header")
ft_sig <- flextable::bold(ft_sig, part = "header")
ft_sig <- flextable::valign(ft_sig, valign = "top", part = "all")
ft_sig <- flextable::set_table_properties(ft_sig, layout = "autofit", width = 1)

doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Final 10-gene signature text package",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "This document provides manuscript-ready text derived from the validated Step 46c signature definition table.",
  style = "Normal"
)

doc <- officer::body_add_par(doc, "1. Summary", style = "heading 2")
doc <- flextable::body_add_flextable(doc, ft_summary)

doc <- officer::body_add_par(doc, "2. Manuscript insertion text", style = "heading 2")
doc <- flextable::body_add_flextable(doc, ft_text)

doc <- officer::body_add_par(doc, "3. Table 2 preview", style = "heading 2")
doc <- officer::body_add_par(doc, table2_title, style = "Normal")
doc <- officer::body_add_par(doc, table2_footnote, style = "Normal")
doc <- flextable::body_add_flextable(doc, ft_sig)

print(doc, target = docx_package_path)

# -----------------------------
# 8. Create XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Text_Package")
openxlsx::writeData(wb, "Text_Package", text_package)

openxlsx::addWorksheet(wb, "Signature_Summary")
openxlsx::writeData(wb, "Signature_Summary", summary_tbl)

openxlsx::addWorksheet(wb, "Final10_Signature_46c")
openxlsx::writeData(wb, "Final10_Signature_46c", signature_tbl)

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
# 9. Checks
# -----------------------------
title_word_count <- stringr::str_count(table2_title, "\\S+")

footnote_word_count <- stringr::str_count(table2_footnote, "\\S+")

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:24),
  check_item = c(
    "46c signature input exists",
    "Final signature has 10 genes",
    "All genes have COMPLETE evidence status",
    "All genes are upregulated in sepsis",
    "All genes used in final bulk analysis",
    "All genes used in single-cell module-score calculation",
    "Six-fold recurrent gene subset detected",
    "Non-six-fold gene subset detected",
    "Final signature is explicitly distinguished from six-fold recurrent subset",
    "Table 2 title generated",
    "Table 2 title has 15 words or fewer",
    "Table 2 footnote generated",
    "Table 2 footnote has 300 words or fewer",
    "Table 2 footnote mentions 0 to 6 occurrence range",
    "Table 2 footnote mentions non-six-fold gene",
    "Methods paragraph mentions FINAL10 gene set",
    "Methods paragraph distinguishes recurrent nested LODO subset",
    "Results paragraph lists all final genes",
    "Results paragraph mentions nine six-fold recurrent genes",
    "Additional file 2 section includes file name",
    "Additional file 2 section includes file format",
    "TXT package generated",
    "DOCX package generated",
    "XLSX package generated"
  ),
  observed = c(
    file.exists(signature_in_path),
    n_final_genes == 10,
    all_complete,
    all_upregulated,
    all_used_bulk,
    all_used_sc,
    n_six_fold_genes > 0,
    n_non_six_fold_genes > 0,
    !final_equals_six_fold_subset,
    !is.na(table2_title) && table2_title != "",
    title_word_count <= 15,
    !is.na(table2_footnote) && table2_footnote != "",
    footnote_word_count <= 300,
    stringr::str_detect(table2_footnote, stringr::fixed("0 to 6")),
    all(non_six_fold_tbl$gene %in% unlist(stringr::str_extract_all(table2_footnote, "[A-Z0-9]+"))),
    stringr::str_detect(methods_signature_paragraph, stringr::fixed("FINAL10 gene set")),
    stringr::str_detect(methods_signature_paragraph, stringr::fixed("reported separately")),
    all(final_genes %in% unlist(stringr::str_extract_all(results_signature_paragraph, "[A-Z0-9]+"))),
    stringr::str_detect(results_signature_paragraph, stringr::fixed("Nine genes were retained in all six")),
    stringr::str_detect(additional_file_2_section, stringr::fixed("Additional file 2")),
    stringr::str_detect(additional_file_2_section, stringr::fixed("File format: XLSX")),
    file.exists(txt_package_path),
    file.exists(docx_package_path),
    file.exists(xlsx_package_path)
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

readr::write_csv(checks, checks_path)

# -----------------------------
# 10. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "signature_source",
    "n_final_genes",
    "final_gene_list",
    "n_six_fold_recurrent_genes",
    "six_fold_recurrent_genes",
    "n_non_six_fold_genes",
    "non_six_fold_genes",
    "final_signature_equals_six_fold_recurrent_subset",
    "table2_title_word_count",
    "table2_footnote_word_count",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "46d_finalize_signature_text_package.R",
    project_dir,
    signature_in_path,
    as.character(n_final_genes),
    final_gene_list_text,
    as.character(n_six_fold_genes),
    six_fold_gene_list_text,
    as.character(n_non_six_fold_genes),
    non_six_fold_text,
    as.character(final_equals_six_fold_subset),
    as.character(title_word_count),
    as.character(footnote_word_count),
    as.character(n_failed_checks),
    dplyr::if_else(n_failed_checks == 0, "YES_INSERT_SIGNATURE_TEXT", "NO_FIX_CHECK_ITEMS"),
    "Use T46d_signature_text_package.docx or TXT output to replace inconsistent signature terminology in the manuscript. Keep T46c final signature table as the data source for Table 2 and Supplementary Table S2."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 11. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nSignature summary:\n")
print(summary_tbl, n = Inf, width = Inf)

cat("\nTable 2 title:\n")
cat(table2_title, "\n")

cat("\nMethods signature paragraph:\n")
cat(methods_signature_paragraph, "\n")

cat("\nResults signature paragraph:\n")
cat(results_signature_paragraph, "\n")

cat("\nTerminology consistency statement:\n")
cat(terminology_consistency_statement, "\n")

cat("\nTable 2 footnote:\n")
cat(table2_footnote, "\n")

cat("\nAdditional file 2 section:\n")
cat(additional_file_2_section, "\n")

cat("\n关键输出：\n")
cat("1) ", docx_package_path, "\n", sep = "")
cat("2) ", txt_package_path, "\n", sep = "")
cat("3) ", csv_text_package_path, "\n", sep = "")
cat("4) ", xlsx_package_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Signature summary、Methods signature paragraph、Results signature paragraph 和 Table 2 footnote 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你确认 signature 定义模块是否正式收尾，并给出正文中需要统一替换的术语清单。\n")

cat("\n============ 46d signature text package finalized ============\n")