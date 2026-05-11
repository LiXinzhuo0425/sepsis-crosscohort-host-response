# ============================================================
# 34_BMC_Genomics_final_submission_package_builder.R
# Build final submission package for BMC Genomics
#
# 稳定版修正：
# 1. 修复 file.copy(from, to) 中 from == to 导致的报错
# 2. copy_file_safe() 支持 already_in_place 记录
# 3. cover letter md/docx 原地生成后直接登记，不再复制到自身
# 4. 不改正文、不改分析结果、不改 Word 文件
#
# 输出：
# 07_manuscript/BMC_Genomics_final_submission_package/
#   01_main_manuscript/
#   02_figures/
#   03_supplementary_files/
#   04_cover_letter/
#   05_submission_checklists/
#   06_freeze_audit/
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

package_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_final_submission_package")
main_dir <- file.path(package_dir, "01_main_manuscript")
fig_dir <- file.path(package_dir, "02_figures")
supp_dir <- file.path(package_dir, "03_supplementary_files")
cover_dir <- file.path(package_dir, "04_cover_letter")
check_dir <- file.path(package_dir, "05_submission_checklists")
audit_dir <- file.path(package_dir, "06_freeze_audit")

dir.create(package_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cover_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(check_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

files <- list(
  manuscript_docx = file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.6_preSubmission.docx"),
  final_check_xlsx = file.path(draft_dir, "BMC_Genomics_final_submission_check_after_manual_edit_v1.1.xlsx"),
  final_check_csv = file.path(draft_dir, "BMC_Genomics_final_submission_checklist_after_manual_edit_v1.1.csv"),
  final_manual_actions = file.path(draft_dir, "BMC_Genomics_final_manual_action_items_v1.1.csv"),
  captions_md = file.path(draft_dir, "BMC_Genomics_table_figure_captions_v0.6.md"),
  main_table_plan = file.path(draft_dir, "BMC_Genomics_main_table_plan_v0.6.csv"),
  main_figure_plan = file.path(draft_dir, "BMC_Genomics_main_figure_plan_v0.6.csv"),
  supplementary_plan = file.path(draft_dir, "BMC_Genomics_supplementary_material_plan_v0.6.csv"),
  key_snapshot = file.path(freeze_dir, "final_key_result_snapshot.csv"),
  freeze_summary = file.path(freeze_dir, "final_freeze_summary_workbook.xlsx"),
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

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
}

read_lines_utf8 <- function(path) {
  readLines(path, encoding = "UTF-8", warn = FALSE)
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

normalize_path_safe <- function(path, mustWork = FALSE) {
  tryCatch(
    normalizePath(path, winslash = "/", mustWork = mustWork),
    error = function(e) path
  )
}

file_size_mb <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

file_nonzero <- function(path) {
  if (is.na(path) || !file.exists(path)) return(FALSE)
  isTRUE(file.info(path)$size > 0)
}

record_created_in_place <- function(path, required = TRUE, label = "created_in_place") {
  data.frame(
    source_path = path,
    copied_path = path,
    copied = file.exists(path) && file_nonzero(path),
    required = required,
    note = ifelse(file.exists(path) && file_nonzero(path), label, "missing_or_empty_after_creation"),
    stringsAsFactors = FALSE
  )
}

copy_file_safe <- function(from, to_dir, new_name = NULL, required = TRUE) {
  if (!file.exists(from)) {
    return(data.frame(
      source_path = from,
      copied_path = NA_character_,
      copied = FALSE,
      required = required,
      note = "source_missing",
      stringsAsFactors = FALSE
    ))
  }
  
  if (!dir.exists(to_dir)) {
    dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (is.null(new_name) || is.na(new_name) || new_name == "") {
    new_name <- basename(from)
  }
  
  to <- file.path(to_dir, new_name)
  
  from_norm <- normalize_path_safe(from, mustWork = TRUE)
  to_norm <- normalize_path_safe(to, mustWork = FALSE)
  
  if (identical(from_norm, to_norm)) {
    return(data.frame(
      source_path = from,
      copied_path = to,
      copied = file.exists(to) && file_nonzero(to),
      required = required,
      note = ifelse(file.exists(to) && file_nonzero(to), "already_in_place", "already_in_place_but_empty_or_missing"),
      stringsAsFactors = FALSE
    ))
  }
  
  ok <- tryCatch(
    file.copy(from, to, overwrite = TRUE),
    error = function(e) FALSE
  )
  
  copied_ok <- isTRUE(ok) && file.exists(to) && file_nonzero(to)
  
  data.frame(
    source_path = from,
    copied_path = to,
    copied = copied_ok,
    required = required,
    note = ifelse(copied_ok, "copied", "copy_failed_or_empty"),
    stringsAsFactors = FALSE
  )
}

get_value <- function(df, item) {
  if (!all(c("item", "value") %in% colnames(df))) return(NA_character_)
  x <- df$value[df$item == item]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

fmt3 <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  ifelse(is.na(y), as.character(x), sprintf("%.3f", y))
}

# ============================================================
# 读取关键输入
# ============================================================

message("Reading final submission materials...")

key_snapshot <- read_csv_df(files$key_snapshot)
final_check <- read_csv_df(files$final_check_csv)
manual_actions <- read_csv_df(files$final_manual_actions)
main_table_plan <- read_csv_df(files$main_table_plan)
main_figure_plan <- read_csv_df(files$main_figure_plan)
supplementary_plan <- read_csv_df(files$supplementary_plan)
figure_inventory <- read_csv_df(files$figure_inventory)
copy_manifest_old <- read_csv_df(files$copy_manifest)

bulk_total <- get_value(key_snapshot, "bulk_total_samples")
bulk_sepsis <- get_value(key_snapshot, "bulk_sepsis")
bulk_control <- get_value(key_snapshot, "bulk_control")
n_bulk_datasets <- get_value(key_snapshot, "n_bulk_datasets")
median_auc <- get_value(key_snapshot, "nested_LODO_median_AUROC")
min_auc <- get_value(key_snapshot, "nested_LODO_min_AUROC")
pooled_auc <- get_value(key_snapshot, "pooled_nested_LODO_AUROC")
threshold_shift <- get_value(key_snapshot, "median_abs_threshold_shift")
rna_status <- get_value(key_snapshot, "RNAseq_validation_status")
sc_cells <- get_value(key_snapshot, "scRNA_cells")
sc_top <- get_value(key_snapshot, "scRNA_top_Final10_celltype")

# ============================================================
# 复制主文稿和审计文件
# ============================================================

message("Copying main manuscript and audit files...")

copy_manifest <- data.frame()

copy_manifest <- rbind(
  copy_manifest,
  copy_file_safe(
    files$manuscript_docx,
    main_dir,
    "BMC_Genomics_main_manuscript_preSubmission.docx",
    required = TRUE
  ),
  copy_file_safe(
    files$final_check_xlsx,
    check_dir,
    "BMC_Genomics_final_submission_check_after_manual_edit_v1.1.xlsx",
    required = TRUE
  ),
  copy_file_safe(
    files$final_check_csv,
    check_dir,
    "BMC_Genomics_final_submission_checklist_after_manual_edit_v1.1.csv",
    required = TRUE
  ),
  copy_file_safe(
    files$final_manual_actions,
    check_dir,
    "BMC_Genomics_final_manual_action_items_v1.1.csv",
    required = TRUE
  ),
  copy_file_safe(
    files$captions_md,
    check_dir,
    "BMC_Genomics_table_figure_captions_v0.6.md",
    required = TRUE
  ),
  copy_file_safe(
    files$key_snapshot,
    audit_dir,
    "final_key_result_snapshot.csv",
    required = TRUE
  ),
  copy_file_safe(
    files$freeze_summary,
    audit_dir,
    "final_freeze_summary_workbook.xlsx",
    required = TRUE
  ),
  copy_file_safe(
    files$decision_log,
    audit_dir,
    "final_freeze_decision_log.csv",
    required = TRUE
  ),
  copy_file_safe(
    files$source_index,
    audit_dir,
    "manuscript_source_index.csv",
    required = TRUE
  ),
  copy_file_safe(
    files$figure_inventory,
    audit_dir,
    "final_freeze_figure_inventory.csv",
    required = TRUE
  )
)

# ============================================================
# Figure inventory and copy candidate figures
# ============================================================

message("Indexing figures...")

figure_source_dir <- file.path(freeze_dir, "figures")

all_fig_files <- character()

if (dir.exists(figure_source_dir)) {
  all_fig_files <- list.files(
    figure_source_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
}

allowed_figure_ext <- c("pdf", "png", "tif", "tiff", "jpg", "jpeg", "eps", "pptx", "docx", "bmp")
fig_ext <- tolower(tools::file_ext(all_fig_files))
candidate_fig_files <- all_fig_files[fig_ext %in% allowed_figure_ext]

figure_copy_manifest <- data.frame()

if (length(candidate_fig_files) > 0) {
  for (f in candidate_fig_files) {
    rel <- gsub(
      paste0("^", normalize_path_safe(figure_source_dir, mustWork = TRUE), "/?"),
      "",
      normalize_path_safe(f, mustWork = TRUE)
    )
    safe_name <- gsub("[/\\\\]", "__", rel)
    figure_copy_manifest <- rbind(
      figure_copy_manifest,
      copy_file_safe(f, fig_dir, safe_name, required = FALSE)
    )
  }
} else {
  figure_copy_manifest <- data.frame(
    source_path = character(),
    copied_path = character(),
    copied = logical(),
    required = logical(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

figure_upload_inventory <- data.frame(
  file_name = basename(candidate_fig_files),
  source_path = candidate_fig_files,
  copied_file_name = if (length(candidate_fig_files) > 0) basename(figure_copy_manifest$copied_path) else character(),
  copied_path = if (length(candidate_fig_files) > 0) figure_copy_manifest$copied_path else character(),
  extension = tolower(tools::file_ext(candidate_fig_files)),
  size_mb = vapply(candidate_fig_files, file_size_mb, numeric(1)),
  file_size_ok_under_10MB = vapply(candidate_fig_files, function(x) {
    s <- file_size_mb(x)
    ifelse(is.na(s), FALSE, s <= 10)
  }, logical(1)),
  upload_role = "Candidate figure/source figure. Select final Figure 1 to Figure 5 manually.",
  stringsAsFactors = FALSE
)

# ============================================================
# Supplementary files candidates
# ============================================================

message("Indexing supplementary files...")

freeze_tables_dir <- file.path(freeze_dir, "tables")
supp_candidate_files <- character()

if (dir.exists(freeze_tables_dir)) {
  supp_candidate_files <- list.files(
    freeze_tables_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
}

allowed_supp_ext <- c("xlsx", "xls", "csv", "txt", "pdf", "docx")
supp_ext <- tolower(tools::file_ext(supp_candidate_files))
supp_candidate_files <- supp_candidate_files[supp_ext %in% allowed_supp_ext]

supp_copy_manifest <- data.frame()

if (length(supp_candidate_files) > 0) {
  for (f in supp_candidate_files) {
    rel <- gsub(
      paste0("^", normalize_path_safe(freeze_tables_dir, mustWork = TRUE), "/?"),
      "",
      normalize_path_safe(f, mustWork = TRUE)
    )
    safe_name <- gsub("[/\\\\]", "__", rel)
    supp_copy_manifest <- rbind(
      supp_copy_manifest,
      copy_file_safe(f, supp_dir, safe_name, required = FALSE)
    )
  }
} else {
  supp_copy_manifest <- data.frame(
    source_path = character(),
    copied_path = character(),
    copied = logical(),
    required = logical(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

supplementary_upload_inventory <- data.frame(
  file_name = basename(supp_candidate_files),
  source_path = supp_candidate_files,
  copied_file_name = if (length(supp_candidate_files) > 0) basename(supp_copy_manifest$copied_path) else character(),
  copied_path = if (length(supp_candidate_files) > 0) supp_copy_manifest$copied_path else character(),
  extension = tolower(tools::file_ext(supp_candidate_files)),
  size_mb = vapply(supp_candidate_files, file_size_mb, numeric(1)),
  file_size_ok_under_20MB = vapply(supp_candidate_files, function(x) {
    s <- file_size_mb(x)
    ifelse(is.na(s), FALSE, s <= 20)
  }, logical(1)),
  upload_role = "Candidate additional file or source table. Select final supplementary files manually.",
  stringsAsFactors = FALSE
)

# ============================================================
# Cover letter draft
# ============================================================

message("Writing cover letter draft...")

cover_letter_lines <- c(
  "Dear Editors,",
  "",
  "We are pleased to submit our manuscript entitled \"Nested cross-cohort evaluation and single-cell localization of a blood transcriptomic host-response signature for sepsis\" for consideration as a Research Article in BMC Genomics.",
  "",
  "This study evaluates a blood transcriptomic host-response signature for sepsis using strict nested leave-one-dataset-out validation across public whole-blood cohorts. The final frozen analysis included 484 samples from six public bulk transcriptomic datasets, including 333 sepsis cases and 151 controls. In each held-out validation fold, feature screening, redundancy filtering, standardization, model fitting, tuning and threshold selection were performed using the training datasets only.",
  "",
  paste0(
    "The main finding is that the signature showed recurrent but non-uniform discrimination across held-out cohorts, with a median nested LODO AUROC of ",
    fmt3(median_auc),
    " and a minimum held-out AUROC of ",
    fmt3(min_auc),
    ". The pooled nested LODO AUROC was ",
    fmt3(pooled_auc),
    ", and calibration and fixed-threshold transportability were unstable, with a median absolute threshold shift of ",
    fmt3(threshold_shift),
    ". These results support a transportability-oriented interpretation rather than a clinical deployment claim."
  ),
  "",
  paste0(
    "We also used an independent PBMC single-cell RNA-seq dataset comprising ",
    sc_cells,
    " cells to localize the bulk-derived host-response signal. Both evaluated signature modules were highest in ",
    sc_top,
    " compartments, supporting a myeloid-dominant biological interpretation. This single-cell analysis was used for biological localization only and was not treated as independent diagnostic validation."
  ),
  "",
  "We believe the manuscript is suitable for BMC Genomics because it addresses transcriptomic host-response modeling, cross-cohort transportability, calibration behavior and single-cell localization using reproducible public data analysis. The study is framed conservatively: it does not claim clinical deployment readiness, fixed-threshold validation or same-intended-use clinical validation.",
  "",
  "All authors have approved the submitted version of the manuscript. The authors declare that the manuscript is original, is not under consideration elsewhere, and has not been published previously. The analysis used publicly available de-identified datasets and did not involve new participant recruitment or new sample collection.",
  "",
  "Thank you for considering our manuscript for publication in BMC Genomics.",
  "",
  "Sincerely,",
  "",
  "Yi Gong",
  "Department of Blood Transfusion",
  "The First Affiliated Hospital of Chongqing Medical University",
  "Chongqing, China",
  "Email: 53293936@qq.com"
)

cover_md_path <- file.path(cover_dir, "BMC_Genomics_cover_letter_draft_v1.0.md")
write_text_file(cover_letter_lines, cover_md_path)

cover_docx_path <- file.path(cover_dir, "BMC_Genomics_cover_letter_draft_v1.0.docx")

if (requireNamespace("officer", quietly = TRUE)) {
  doc <- officer::read_docx()
  styles <- officer::styles_info(doc)$style_name
  normal_style <- ifelse("Normal" %in% styles, "Normal", styles[1])
  for (ln in cover_letter_lines) {
    doc <- officer::body_add_par(doc, value = ln, style = normal_style)
  }
  print(doc, target = cover_docx_path)
} else {
  warning("officer 未安装，未生成 cover letter docx，仅生成 md。")
}

# 原地生成文件，直接登记，不复制自身
copy_manifest <- rbind(
  copy_manifest,
  record_created_in_place(cover_md_path, required = TRUE, label = "created_in_place")
)

copy_manifest <- rbind(
  copy_manifest,
  record_created_in_place(cover_docx_path, required = TRUE, label = "created_in_place")
)

# ============================================================
# Upload checklist
# ============================================================

message("Building upload checklist...")

upload_checklist <- data.frame(
  item_id = c(
    "U01", "U02", "U03", "U04", "U05", "U06", "U07", "U08",
    "U09", "U10", "U11", "U12", "U13", "U14", "U15", "U16"
  ),
  upload_item = c(
    "Main manuscript DOCX",
    "Cover letter",
    "Figure 1 file",
    "Figure 2 file",
    "Figure 3 file",
    "Figure 4 file",
    "Figure 5 file",
    "Supplementary Table S1",
    "Supplementary Table S2",
    "Supplementary Table S3",
    "Supplementary Table S4",
    "Supplementary Table S5",
    "Supplementary Table S6",
    "Supplementary Figure S1-S4 or combined supplementary figures",
    "Data/code repository link",
    "Submission-system metadata"
  ),
  status = c(
    ifelse(file.exists(files$manuscript_docx), "READY", "MISSING"),
    ifelse(file.exists(cover_docx_path) || file.exists(cover_md_path), "READY_DRAFT", "MISSING"),
    rep("SELECT_FINAL_FILE", 5),
    rep("SELECT_FINAL_FILE", 7),
    "RECOMMENDED_BEFORE_SUBMISSION",
    "MANUAL_ENTRY_REQUIRED"
  ),
  required_for_submission = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE
  ),
  notes = c(
    "Use BMC_Genomics_main_manuscript_preSubmission.docx after final visual review.",
    "Review and adjust editor name/date if needed.",
    "Select final Figure 1 composite file from package/02_figures or regenerate if needed.",
    "Select final Figure 2 composite file from package/02_figures or regenerate if needed.",
    "Select final Figure 3 composite file from package/02_figures or regenerate if needed.",
    "Select final Figure 4 composite file from package/02_figures or regenerate if needed.",
    "Select final Figure 5 composite file from package/02_figures or regenerate if needed.",
    "Cohort eligibility and control-context supplement.",
    "DEG and enrichment supplement.",
    "Candidate screening and recurrence supplement.",
    "Nested LODO selected genes and model parameters supplement.",
    "RNA-seq eligibility No-Go supplement.",
    "Final freeze decision and claim-boundary supplement.",
    "Decide whether to upload as separate figures or one combined supplementary PDF.",
    "Prefer GitHub plus Zenodo DOI. Current wording allows reasonable request, but repository is stronger.",
    "Enter title, abstract, keywords, author information, declarations, suggested reviewers if requested."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Final visual checklist
# ============================================================

visual_checklist <- data.frame(
  check_id = sprintf("V%02d", 1:18),
  item = c(
    "Open Word and confirm author order",
    "Confirm all affiliations are official English names",
    "Confirm corresponding author address and email",
    "Confirm title and abstract match submission system",
    "Confirm line spacing and page/line numbering if journal system requests",
    "Confirm figure captions are in manuscript and not only image files",
    "Confirm table titles and legends are readable",
    "Confirm all main tables are cited in sequence",
    "Confirm all figures are cited in sequence",
    "Confirm additional files are cited in sequence if uploaded",
    "Confirm Data availability statement is acceptable",
    "Confirm Ethics approval wording with supervisor or institution",
    "Confirm no claim of clinical deployment readiness",
    "Confirm GSE54514 failure scenario is retained",
    "Confirm Reference 9 GSE167363 source is correct",
    "Confirm all reference DOIs and pages in Zotero",
    "Confirm final figure image quality",
    "Confirm APC/payment route"
  ),
  required_before_click_submit = c(
    TRUE, TRUE, TRUE, TRUE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE
  ),
  status = "MANUAL_REVIEW_REQUIRED",
  stringsAsFactors = FALSE
)

# ============================================================
# Package manifest
# ============================================================

package_manifest <- rbind(
  copy_manifest,
  figure_copy_manifest,
  supp_copy_manifest
)

package_manifest$size_mb <- vapply(package_manifest$copied_path, file_size_mb, numeric(1))

# ============================================================
# Overall status
# ============================================================

if (nrow(package_manifest) > 0) {
  required_rows <- package_manifest$required == TRUE
  required_copy_ok <- all(package_manifest$copied[required_rows], na.rm = TRUE)
} else {
  required_copy_ok <- FALSE
}

n_candidate_figures <- nrow(figure_upload_inventory)
n_candidate_supp_files <- nrow(supplementary_upload_inventory)

main_manuscript_copied_path <- file.path(main_dir, "BMC_Genomics_main_manuscript_preSubmission.docx")

overall_status <- data.frame(
  metric = c(
    "package_dir",
    "required_files_copied",
    "n_candidate_figure_files_copied",
    "n_candidate_supplementary_files_copied",
    "cover_letter_md_created",
    "cover_letter_docx_created",
    "main_manuscript_copied",
    "ready_for_final_visual_review",
    "ready_to_click_submit"
  ),
  value = c(
    package_dir,
    ifelse(required_copy_ok, "YES", "CHECK"),
    n_candidate_figures,
    n_candidate_supp_files,
    ifelse(file.exists(cover_md_path) && file_nonzero(cover_md_path), "YES", "CHECK"),
    ifelse(file.exists(cover_docx_path) && file_nonzero(cover_docx_path), "YES", "CHECK"),
    ifelse(file.exists(main_manuscript_copied_path) && file_nonzero(main_manuscript_copied_path), "YES", "CHECK"),
    ifelse(required_copy_ok && file.exists(cover_md_path) && file_nonzero(cover_md_path), "YES", "CHECK"),
    "NO_MANUAL_VISUAL_REVIEW_AND_UPLOAD_SELECTION_REQUIRED"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# README
# ============================================================

readme_lines <- c(
  "# BMC Genomics final submission package v1.0",
  "",
  "## Package structure",
  "",
  "- 01_main_manuscript: main manuscript DOCX for final visual review.",
  "- 02_figures: copied candidate figure/source files from final freeze.",
  "- 03_supplementary_files: copied candidate supplementary tables/files.",
  "- 04_cover_letter: cover letter draft in Markdown and DOCX if officer was available.",
  "- 05_submission_checklists: final automated checks and upload checklists.",
  "- 06_freeze_audit: final freeze audit files and result snapshots.",
  "",
  "## Current status",
  "",
  paste0("- Required files copied: ", ifelse(required_copy_ok, "YES", "CHECK")),
  paste0("- Candidate figure files indexed: ", n_candidate_figures),
  paste0("- Candidate supplementary files indexed: ", n_candidate_supp_files),
  paste0("- Cover letter draft created: ", ifelse(file.exists(cover_md_path), "YES", "CHECK")),
  "",
  "## Required manual actions before submission",
  "",
  "1. Open the main manuscript DOCX and visually inspect title page, declarations, references, tables and figure captions.",
  "2. Select final Figure 1 to Figure 5 files for upload. If multiple candidate figures exist, use the final composite figures only.",
  "3. Select final supplementary files. Avoid uploading unnecessary intermediate audit files unless useful for review transparency.",
  "4. Confirm Data availability. Prefer replacing reasonable-request wording with a public GitHub/Zenodo link before submission.",
  "5. Confirm cover letter content and corresponding author details.",
  "6. Confirm APC/payment route.",
  "",
  "## Claim boundary",
  "",
  "Do not claim clinical deployment readiness, fixed-threshold external validation, same-intended-use clinical validation, or scRNA diagnostic validation.",
  "",
  "## Final note",
  "",
  "This package builder does not modify manuscript content or analysis outputs."
)

readme_path <- file.path(package_dir, "README_BMC_Genomics_final_submission_package_v1.0.md")
write_text_file(readme_lines, readme_path)

# ============================================================
# 输出文件
# ============================================================

message("Writing package outputs...")

package_manifest_path <- file.path(package_dir, "BMC_Genomics_final_package_manifest_v1.0.csv")
figure_inventory_path <- file.path(check_dir, "BMC_Genomics_candidate_figure_upload_inventory_v1.0.csv")
supp_inventory_path <- file.path(check_dir, "BMC_Genomics_candidate_supplementary_upload_inventory_v1.0.csv")
upload_checklist_path <- file.path(check_dir, "BMC_Genomics_submission_upload_checklist_v1.0.csv")
visual_checklist_path <- file.path(check_dir, "BMC_Genomics_final_visual_review_checklist_v1.0.csv")
index_path <- file.path(package_dir, "BMC_Genomics_submission_upload_checklist_v1.0.xlsx")

data.table::fwrite(package_manifest, package_manifest_path)
data.table::fwrite(figure_upload_inventory, figure_inventory_path)
data.table::fwrite(supplementary_upload_inventory, supp_inventory_path)
data.table::fwrite(upload_checklist, upload_checklist_path)
data.table::fwrite(visual_checklist, visual_checklist_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "upload_checklist")
openxlsx::writeData(wb, "upload_checklist", upload_checklist)

openxlsx::addWorksheet(wb, "visual_checklist")
openxlsx::writeData(wb, "visual_checklist", visual_checklist)

openxlsx::addWorksheet(wb, "package_manifest")
openxlsx::writeData(wb, "package_manifest", package_manifest)

openxlsx::addWorksheet(wb, "candidate_figures")
openxlsx::writeData(wb, "candidate_figures", figure_upload_inventory)

openxlsx::addWorksheet(wb, "candidate_supp_files")
openxlsx::writeData(wb, "candidate_supp_files", supplementary_upload_inventory)

openxlsx::addWorksheet(wb, "main_figure_plan")
openxlsx::writeData(wb, "main_figure_plan", main_figure_plan)

openxlsx::addWorksheet(wb, "supplementary_plan")
openxlsx::writeData(wb, "supplementary_plan", supplementary_plan)

openxlsx::addWorksheet(wb, "final_check")
openxlsx::writeData(wb, "final_check", final_check)

openxlsx::addWorksheet(wb, "manual_actions")
openxlsx::writeData(wb, "manual_actions", manual_actions)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_34_BMC_Genomics_final_submission_package_builder.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 34 BMC Genomics final submission package builder 完成 ============")
message("Package 目录：", package_dir)

message("\nOverall status:")
print(overall_status)

message("\nUpload checklist:")
print(upload_checklist)

message("\nCandidate figure files:")
if (nrow(figure_upload_inventory) > 0) {
  print(head(figure_upload_inventory, 30))
} else {
  message("No candidate figure files copied.")
}

message("\nCandidate supplementary files:")
if (nrow(supplementary_upload_inventory) > 0) {
  print(head(supplementary_upload_inventory, 30))
} else {
  message("No candidate supplementary files copied.")
}

message("\nVisual checklist:")
print(visual_checklist)

message("\nPackage manifest summary:")
if (nrow(package_manifest) > 0) {
  print(table(package_manifest$copied, package_manifest$required, useNA = "ifany"))
} else {
  message("Package manifest is empty.")
}

message("\n关键输出：")
message("1) ", package_dir)
message("2) ", main_manuscript_copied_path)
message("3) ", cover_md_path)
message("4) ", cover_docx_path)
message("5) ", index_path)
message("6) ", package_manifest_path)
message("7) ", upload_checklist_path)
message("8) ", visual_checklist_path)
message("9) ", readme_path)

message("\n下一步：")
message("把 Overall status、Upload checklist、Candidate figure files、Candidate supplementary files 贴给我。")
message("然后我会帮你判断哪些图和补充文件应该最终上传，哪些只是审计文件不用上传。")