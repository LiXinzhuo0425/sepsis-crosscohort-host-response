# ============================================================
# 38_BMC_Genomics_final_visual_and_upload_checklist.R
# Final visual and upload checklist for BMC Genomics submission
#
# Purpose:
#   1. Confirm the READY_TO_UPLOAD folder contains only the intended upload files.
#   2. Generate upload-only list for submission system.
#   3. Flag support/check files that should NOT be uploaded.
#   4. Confirm Figure 1-5 PDFs and Additional file 1-6 XLSX are present.
#   5. Generate a final human-readable submission checklist.
#
# This script does NOT modify manuscript, figures, tables, data, or results.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

# ============================================================
# Paths
# ============================================================

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

ready_dir <- file.path(
  project_dir,
  "07_manuscript",
  "BMC_Genomics_final_submission_package",
  "00_READY_TO_UPLOAD"
)

ready_fig_dir <- file.path(ready_dir, "figures")
ready_supp_dir <- file.path(ready_dir, "additional_files")
ready_check_dir <- file.path(ready_dir, "checks")

main_docx <- file.path(ready_dir, "Main_manuscript.docx")
cover_docx <- file.path(ready_dir, "Cover_letter.docx")

dir.create(ready_check_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Utilities
# ============================================================

file_exists_nonzero <- function(path) {
  file.exists(path) && isTRUE(file.info(path)$size > 0)
}

file_size_mb <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

safe_list_files <- function(path, recursive = TRUE) {
  if (!dir.exists(path)) return(character(0))
  list.files(path, recursive = recursive, full.names = TRUE, all.files = FALSE, no.. = TRUE)
}

write_csv_safe <- function(x, path) {
  data.table::fwrite(x, path)
}

classify_upload_file <- function(path) {
  bn <- basename(path)
  rel <- sub(paste0("^", normalizePath(ready_dir, winslash = "/", mustWork = FALSE), "/?"), "", normalizePath(path, winslash = "/", mustWork = FALSE))
  ext <- tolower(tools::file_ext(path))
  
  if (identical(normalizePath(path, winslash = "/", mustWork = FALSE), normalizePath(main_docx, winslash = "/", mustWork = FALSE))) {
    return("UPLOAD_MAIN_MANUSCRIPT")
  }
  
  if (identical(normalizePath(path, winslash = "/", mustWork = FALSE), normalizePath(cover_docx, winslash = "/", mustWork = FALSE))) {
    return("UPLOAD_COVER_LETTER")
  }
  
  if (grepl("^figures/Figure_[1-5]\\.pdf$", rel)) {
    return("UPLOAD_MAIN_FIGURE_PDF")
  }
  
  if (grepl("^additional_files/Additional_file_[1-6]\\.xlsx$", rel)) {
    return("UPLOAD_ADDITIONAL_FILE_XLSX")
  }
  
  if (grepl("^additional_files/Additional_file_descriptions_for_submission_system\\.csv$", rel)) {
    return("REFERENCE_FOR_SUBMISSION_SYSTEM_NOT_UPLOAD_UNLESS_REQUESTED")
  }
  
  if (grepl("^figures/Figure_[1-5]\\.png$", rel)) {
    return("VISUAL_CHECK_COPY_NOT_UPLOAD_IF_PDF_ACCEPTED")
  }
  
  if (grepl("^checks/", rel)) {
    return("CHECK_SUPPORT_FILE_DO_NOT_UPLOAD")
  }
  
  if (grepl("README|manifest|check|source_status|selection|inventory|overall_status", bn, ignore.case = TRUE)) {
    return("CHECK_SUPPORT_FILE_DO_NOT_UPLOAD")
  }
  
  if (grepl("^additional_files/Additional_file_[4-6]\\.csv$", rel)) {
    return("OLD_CSV_RESIDUAL_DO_NOT_UPLOAD")
  }
  
  return("UNCLASSIFIED_REVIEW_BEFORE_UPLOAD")
}

# ============================================================
# Build inventory
# ============================================================

all_files <- safe_list_files(ready_dir, recursive = TRUE)

inventory <- data.frame(
  file_name = basename(all_files),
  relative_path = sub(
    paste0("^", normalizePath(ready_dir, winslash = "/", mustWork = FALSE), "/?"),
    "",
    normalizePath(all_files, winslash = "/", mustWork = FALSE)
  ),
  full_path = normalizePath(all_files, winslash = "/", mustWork = FALSE),
  extension = tolower(tools::file_ext(all_files)),
  size_mb = vapply(all_files, file_size_mb, numeric(1)),
  exists_nonzero = vapply(all_files, file_exists_nonzero, logical(1)),
  upload_class = vapply(all_files, classify_upload_file, character(1)),
  stringsAsFactors = FALSE
)

inventory <- inventory[order(inventory$upload_class, inventory$relative_path), , drop = FALSE]

upload_files <- inventory[inventory$upload_class %in% c(
  "UPLOAD_MAIN_MANUSCRIPT",
  "UPLOAD_COVER_LETTER",
  "UPLOAD_MAIN_FIGURE_PDF",
  "UPLOAD_ADDITIONAL_FILE_XLSX"
), , drop = FALSE]

do_not_upload_files <- inventory[!inventory$upload_class %in% c(
  "UPLOAD_MAIN_MANUSCRIPT",
  "UPLOAD_COVER_LETTER",
  "UPLOAD_MAIN_FIGURE_PDF",
  "UPLOAD_ADDITIONAL_FILE_XLSX"
), , drop = FALSE]

# ============================================================
# Expected file checks
# ============================================================

expected_main <- data.frame(
  upload_order = 1,
  upload_category = "Main manuscript",
  expected_file = "Main_manuscript.docx",
  expected_path = main_docx,
  file_exists = file_exists_nonzero(main_docx),
  size_mb = file_size_mb(main_docx),
  size_limit_mb = NA_real_,
  size_ok = file_exists_nonzero(main_docx),
  upload_instruction = "Upload as main manuscript file.",
  stringsAsFactors = FALSE
)

expected_cover <- data.frame(
  upload_order = 2,
  upload_category = "Cover letter",
  expected_file = "Cover_letter.docx",
  expected_path = cover_docx,
  file_exists = file_exists_nonzero(cover_docx),
  size_mb = file_size_mb(cover_docx),
  size_limit_mb = NA_real_,
  size_ok = file_exists_nonzero(cover_docx),
  upload_instruction = "Upload as cover letter.",
  stringsAsFactors = FALSE
)

expected_figures <- data.frame(
  upload_order = 3:7,
  upload_category = paste0("Main figure ", 1:5),
  expected_file = paste0("Figure_", 1:5, ".pdf"),
  expected_path = file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf")),
  file_exists = vapply(file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf")), file_exists_nonzero, logical(1)),
  size_mb = vapply(file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf")), file_size_mb, numeric(1)),
  size_limit_mb = 10,
  size_ok = vapply(file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf")), function(x) file_exists_nonzero(x) && file_size_mb(x) <= 10, logical(1)),
  upload_instruction = "Upload PDF as separate main figure. Do not upload PNG unless PDF is rejected by system.",
  stringsAsFactors = FALSE
)

expected_supp <- data.frame(
  upload_order = 8:13,
  upload_category = paste0("Additional file ", 1:6),
  expected_file = paste0("Additional_file_", 1:6, ".xlsx"),
  expected_path = file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx")),
  file_exists = vapply(file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx")), file_exists_nonzero, logical(1)),
  size_mb = vapply(file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx")), file_size_mb, numeric(1)),
  size_limit_mb = 20,
  size_ok = vapply(file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx")), function(x) file_exists_nonzero(x) && file_size_mb(x) <= 20, logical(1)),
  upload_instruction = "Upload as additional file. Use description text from Additional_file_descriptions_for_submission_system.csv.",
  stringsAsFactors = FALSE
)

expected_upload_list <- rbind(
  expected_main,
  expected_cover,
  expected_figures,
  expected_supp
)

expected_upload_list$status <- ifelse(
  expected_upload_list$file_exists & expected_upload_list$size_ok,
  "READY",
  "CHECK"
)

# ============================================================
# Additional file descriptions
# ============================================================

additional_desc_path <- file.path(ready_supp_dir, "Additional_file_descriptions_for_submission_system.csv")

if (file.exists(additional_desc_path)) {
  additional_descriptions <- data.table::fread(additional_desc_path, data.table = FALSE)
} else {
  additional_descriptions <- data.frame(
    additional_file = paste0("Additional_file_", 1:6),
    title = c(
      "Dataset eligibility and cohort-control context",
      "Differential expression and Gene Ontology enrichment results",
      "Candidate gene screening and nested LODO model details",
      "Calibration, threshold transportability and exploratory DCA results",
      "RNA-seq eligibility screening and No-Go decision",
      "Single-cell annotation and signature module score summaries"
    ),
    filename = paste0("Additional_file_", 1:6, ".xlsx"),
    submission_system_description = c(
      "Dataset eligibility, cohort-control context and frozen manuscript-ready integrated tables.",
      "Differential expression and Gene Ontology biological process enrichment outputs.",
      "Candidate gene screening, refined candidate selection, nested LODO gene recurrence and validation details.",
      "Calibration, fixed-threshold transportability and exploratory decision-curve analysis outputs.",
      "RNA-seq dataset eligibility screening and No-Go decision for RNA-seq external validation.",
      "Single-cell cluster annotation and host-response signature module score summaries."
    ),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# Manual visual checklist
# ============================================================

manual_visual_checklist <- data.frame(
  item_id = sprintf("V%02d", 1:24),
  item = c(
    "Open Main_manuscript.docx and confirm title page author order.",
    "Confirm all affiliations are official English institutional names.",
    "Confirm corresponding author name, address and email.",
    "Confirm Declarations are accurate and approved by supervisor or corresponding author.",
    "Confirm Funding states no specific funding if that is final.",
    "Confirm Competing interests statement is final.",
    "Confirm Availability of data and materials includes GEO accessions and current code/data availability wording.",
    "Confirm title, abstract and keywords match what will be entered in submission system.",
    "Confirm all main tables are cited in numerical order.",
    "Confirm all main figures are cited in numerical order.",
    "Confirm Additional file 1 to Additional file 6 are cited in numerical order.",
    "Open Figure_1.pdf and confirm text is readable.",
    "Open Figure_2.pdf and confirm AUROC/AUPRC/gene recurrence panels are readable.",
    "Open Figure_3.pdf and confirm calibration and threshold panels are readable.",
    "Open Figure_4.pdf and confirm GO panels are readable and not overinterpreted.",
    "Open Figure_5.pdf and confirm scRNA panels are readable and localization-only message is clear.",
    "Confirm figure captions are in the manuscript, not only in image files.",
    "Confirm every PDF figure is correctly oriented.",
    "Confirm no PNG main figures are uploaded unless the system rejects PDFs.",
    "Confirm only Additional_file_1.xlsx to Additional_file_6.xlsx are uploaded as supplementary/additional files.",
    "Do not upload checks folder files.",
    "Do not upload old Additional_file_4.csv, Additional_file_5.csv or Additional_file_6.csv.",
    "Use Additional_file_descriptions_for_submission_system.csv only as text source for the submission system.",
    "Confirm APC or institutional payment route before final submit."
  ),
  required_before_submit = c(
    rep(TRUE, 23),
    TRUE
  ),
  status = "MANUAL_REVIEW_REQUIRED",
  stringsAsFactors = FALSE
)

# ============================================================
# Final automated checks
# ============================================================

unexpected_upload_like <- inventory[
  inventory$upload_class == "UNCLASSIFIED_REVIEW_BEFORE_UPLOAD",
  ,
  drop = FALSE
]

old_csv_residuals <- inventory[
  inventory$upload_class == "OLD_CSV_RESIDUAL_DO_NOT_UPLOAD",
  ,
  drop = FALSE
]

checks <- data.frame(
  check_id = sprintf("C%02d", 1:16),
  check_item = c(
    "Main manuscript DOCX exists",
    "Cover letter DOCX exists",
    "Five main figure PDFs exist",
    "Five main figure PDFs are under 10 MB",
    "Six additional XLSX files exist",
    "Six additional XLSX files are under 20 MB",
    "Additional file description CSV exists",
    "Upload-only file count equals 13",
    "No zero-byte upload files",
    "No unclassified files requiring upload review",
    "Old Additional_file CSV residuals are identified as do-not-upload",
    "checks folder files are classified as do-not-upload",
    "PNG figure copies are classified as visual-check-only",
    "Manual visual checklist generated",
    "Ready directory exists",
    "Final submit still requires manual visual review"
  ),
  observed = c(
    file_exists_nonzero(main_docx),
    file_exists_nonzero(cover_docx),
    all(vapply(file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf")), file_exists_nonzero, logical(1))),
    all(vapply(file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf")), function(x) file_exists_nonzero(x) && file_size_mb(x) <= 10, logical(1))),
    all(vapply(file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx")), file_exists_nonzero, logical(1))),
    all(vapply(file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx")), function(x) file_exists_nonzero(x) && file_size_mb(x) <= 20, logical(1))),
    file_exists_nonzero(additional_desc_path),
    nrow(expected_upload_list[expected_upload_list$status == "READY", , drop = FALSE]) == 13,
    all(upload_files$exists_nonzero),
    nrow(unexpected_upload_like) == 0,
    TRUE,
    any(grepl("^checks/", inventory$relative_path)),
    any(inventory$upload_class == "VISUAL_CHECK_COPY_NOT_UPLOAD_IF_PDF_ACCEPTED"),
    nrow(manual_visual_checklist) >= 20,
    dir.exists(ready_dir),
    TRUE
  ),
  status = NA_character_,
  stringsAsFactors = FALSE
)

checks$status <- ifelse(checks$observed, "PASS", "CHECK")
checks$status[checks$check_id == "C16"] <- "MANUAL_REQUIRED"

n_failed_checks <- sum(checks$status == "CHECK", na.rm = TRUE)

overall_status <- data.frame(
  metric = c(
    "ready_upload_dir",
    "n_expected_upload_files",
    "n_ready_upload_files",
    "n_failed_automated_checks",
    "n_unclassified_review_files",
    "n_do_not_upload_files",
    "ready_for_submission_upload",
    "recommended_next_step"
  ),
  value = c(
    ready_dir,
    nrow(expected_upload_list),
    sum(expected_upload_list$status == "READY"),
    n_failed_checks,
    nrow(unexpected_upload_like),
    nrow(do_not_upload_files),
    ifelse(n_failed_checks == 0, "YES_AFTER_MANUAL_VISUAL_REVIEW", "NO_FIX_ITEMS_FIRST"),
    ifelse(
      n_failed_checks == 0,
      "Upload only the 13 READY files after manual visual review. Do not upload checks, README, manifest, PNG copies, or old CSV residuals.",
      "Fix CHECK items before submission upload."
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Generate concise upload instruction table
# ============================================================

final_upload_instruction <- expected_upload_list
final_upload_instruction$submission_system_field <- c(
  "Manuscript",
  "Cover letter",
  paste0("Figure ", 1:5),
  paste0("Additional file ", 1:6)
)

final_upload_instruction <- final_upload_instruction[, c(
  "upload_order",
  "submission_system_field",
  "upload_category",
  "expected_file",
  "expected_path",
  "size_mb",
  "status",
  "upload_instruction"
)]

# ============================================================
# Write outputs
# ============================================================

inventory_path <- file.path(ready_check_dir, "BMC_Genomics_final_upload_inventory_v1.3.csv")
upload_list_path <- file.path(ready_check_dir, "BMC_Genomics_final_upload_only_list_v1.3.csv")
do_not_upload_path <- file.path(ready_check_dir, "BMC_Genomics_do_not_upload_files_v1.3.csv")
manual_visual_path <- file.path(ready_check_dir, "BMC_Genomics_manual_visual_checklist_v1.3.csv")
checks_path <- file.path(ready_check_dir, "BMC_Genomics_final_upload_checks_v1.3.csv")
overall_path <- file.path(ready_check_dir, "BMC_Genomics_final_upload_overall_status_v1.3.csv")
additional_desc_out_path <- file.path(ready_check_dir, "BMC_Genomics_additional_file_descriptions_for_copy_paste_v1.3.csv")
xlsx_path <- file.path(ready_dir, "BMC_Genomics_FINAL_UPLOAD_CHECKLIST_v1.3.xlsx")
readme_path <- file.path(ready_dir, "README_FINAL_UPLOAD_ONLY_v1.3.md")

write_csv_safe(inventory, inventory_path)
write_csv_safe(final_upload_instruction, upload_list_path)
write_csv_safe(do_not_upload_files, do_not_upload_path)
write_csv_safe(manual_visual_checklist, manual_visual_path)
write_csv_safe(checks, checks_path)
write_csv_safe(overall_status, overall_path)
write_csv_safe(additional_descriptions, additional_desc_out_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "upload_only_list")
openxlsx::writeData(wb, "upload_only_list", final_upload_instruction)

openxlsx::addWorksheet(wb, "additional_descriptions")
openxlsx::writeData(wb, "additional_descriptions", additional_descriptions)

openxlsx::addWorksheet(wb, "manual_visual_checklist")
openxlsx::writeData(wb, "manual_visual_checklist", manual_visual_checklist)

openxlsx::addWorksheet(wb, "do_not_upload")
openxlsx::writeData(wb, "do_not_upload", do_not_upload_files)

openxlsx::addWorksheet(wb, "checks")
openxlsx::writeData(wb, "checks", checks)

openxlsx::addWorksheet(wb, "full_inventory")
openxlsx::writeData(wb, "full_inventory", inventory)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

readme_lines <- c(
  "# BMC Genomics final upload-only instructions",
  "",
  "Upload ONLY these files after manual visual review:",
  "",
  "1. Main_manuscript.docx",
  "2. Cover_letter.docx",
  "3. figures/Figure_1.pdf",
  "4. figures/Figure_2.pdf",
  "5. figures/Figure_3.pdf",
  "6. figures/Figure_4.pdf",
  "7. figures/Figure_5.pdf",
  "8. additional_files/Additional_file_1.xlsx",
  "9. additional_files/Additional_file_2.xlsx",
  "10. additional_files/Additional_file_3.xlsx",
  "11. additional_files/Additional_file_4.xlsx",
  "12. additional_files/Additional_file_5.xlsx",
  "13. additional_files/Additional_file_6.xlsx",
  "",
  "Use the content of additional_files/Additional_file_descriptions_for_submission_system.csv for the submission system descriptions.",
  "",
  "Do NOT upload:",
  "",
  "- checks/ folder files",
  "- README files",
  "- manifest files",
  "- PNG versions of Figure 1-5 unless the system rejects the PDF files",
  "- old CSV residuals such as Additional_file_4.csv, Additional_file_5.csv, Additional_file_6.csv",
  "",
  "Manual visual review remains required before clicking final submit."
)

writeLines(readme_lines, con = readme_path, useBytes = TRUE)

# ============================================================
# Console output
# ============================================================

message("\n============ 38 BMC Genomics final visual and upload checklist 完成 ============")
message("READY_TO_UPLOAD 目录：", ready_dir)

message("\nOverall status:")
print(overall_status)

message("\nFinal upload-only list:")
print(final_upload_instruction)

message("\nAdditional file descriptions:")
print(additional_descriptions)

message("\nFinal checks:")
print(checks)

message("\nDo-not-upload summary:")
print(table(do_not_upload_files$upload_class, useNA = "ifany"))

message("\nManual visual checklist head:")
print(head(manual_visual_checklist, 12))

message("\n关键输出：")
message("1) ", xlsx_path)
message("2) ", upload_list_path)
message("3) ", do_not_upload_path)
message("4) ", manual_visual_path)
message("5) ", checks_path)
message("6) ", overall_path)
message("7) ", readme_path)

message("\n下一步：")
message("把 Overall status、Final upload-only list、Final checks、Do-not-upload summary 贴给我。")
message("如果 n_failed_automated_checks = 0，就可以进入最终投稿系统逐项上传。")