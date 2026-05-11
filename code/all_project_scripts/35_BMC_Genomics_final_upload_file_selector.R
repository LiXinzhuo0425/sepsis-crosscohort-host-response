# ============================================================
# 35_BMC_Genomics_final_upload_file_selector.R
# Select, rename and organize final upload-ready files for BMC Genomics
#
# 目的：
# 1. 从 34 生成的候选投稿包中筛选真正要上传的文件
# 2. 生成 00_READY_TO_UPLOAD 文件夹
# 3. 复制并标准化命名：
#    Main_manuscript.docx
#    Cover_letter.docx
#    Figure_1.xxx to Figure_5.xxx
#    Additional_file_1.xlsx to Additional_file_6.xlsx
# 4. 生成 upload manifest、missing file report、人工核对表
#
# 注意：
# 本脚本不修改正文、不修改分析结果、不重新生成统计结果。
# 如果没有发现合成版主图，本脚本会把 Figure 1-5 标记为 NEED_MANUAL_SELECTION_OR_COMPOSITION。
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

# ============================================================
# 路径设置
# ============================================================

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

package_dir <- file.path(
  project_dir,
  "07_manuscript",
  "BMC_Genomics_final_submission_package"
)

draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
log_dir <- file.path(project_dir, "04_results", "logs")

main_dir <- file.path(package_dir, "01_main_manuscript")
fig_dir <- file.path(package_dir, "02_figures")
supp_dir <- file.path(package_dir, "03_supplementary_files")
cover_dir <- file.path(package_dir, "04_cover_letter")
check_dir <- file.path(package_dir, "05_submission_checklists")

ready_dir <- file.path(package_dir, "00_READY_TO_UPLOAD")
ready_fig_dir <- file.path(ready_dir, "figures")
ready_supp_dir <- file.path(ready_dir, "additional_files")
ready_check_dir <- file.path(ready_dir, "checks")

dir.create(ready_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_supp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_check_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

input_files <- list(
  main_manuscript = file.path(main_dir, "BMC_Genomics_main_manuscript_preSubmission.docx"),
  cover_letter_docx = file.path(cover_dir, "BMC_Genomics_cover_letter_draft_v1.0.docx"),
  cover_letter_md = file.path(cover_dir, "BMC_Genomics_cover_letter_draft_v1.0.md"),
  candidate_figure_inventory = file.path(check_dir, "BMC_Genomics_candidate_figure_upload_inventory_v1.0.csv"),
  candidate_supp_inventory = file.path(check_dir, "BMC_Genomics_candidate_supplementary_upload_inventory_v1.0.csv"),
  upload_checklist = file.path(check_dir, "BMC_Genomics_submission_upload_checklist_v1.0.csv"),
  visual_checklist = file.path(check_dir, "BMC_Genomics_final_visual_review_checklist_v1.0.csv"),
  table_figure_captions = file.path(draft_dir, "BMC_Genomics_table_figure_captions_v0.6.md"),
  main_figure_plan = file.path(draft_dir, "BMC_Genomics_main_figure_plan_v0.6.csv"),
  supplementary_plan = file.path(draft_dir, "BMC_Genomics_supplementary_material_plan_v0.6.csv")
)

missing_inputs <- names(input_files)[!file.exists(unlist(input_files))]

if (length(missing_inputs) > 0) {
  stop(
    "缺少输入文件：\n",
    paste(missing_inputs, unlist(input_files)[missing_inputs], sep = ": ", collapse = "\n")
  )
}

# ============================================================
# 工具函数
# ============================================================

read_csv_df <- function(path) {
  data.table::fread(path, data.table = FALSE)
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

safe_copy <- function(from, to, required = TRUE, role = NA_character_) {
  if (is.na(from) || !file.exists(from)) {
    return(data.frame(
      upload_role = role,
      source_path = from,
      ready_path = to,
      copied = FALSE,
      required = required,
      size_mb = NA_real_,
      status = ifelse(required, "MISSING_REQUIRED_SOURCE", "MISSING_OPTIONAL_SOURCE"),
      note = "source_missing",
      stringsAsFactors = FALSE
    ))
  }
  
  to_dir <- dirname(to)
  if (!dir.exists(to_dir)) {
    dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  from_norm <- normalize_path_safe(from, mustWork = TRUE)
  to_norm <- normalize_path_safe(to, mustWork = FALSE)
  
  if (identical(from_norm, to_norm)) {
    copied_ok <- file.exists(to) && file_nonzero(to)
    return(data.frame(
      upload_role = role,
      source_path = from,
      ready_path = to,
      copied = copied_ok,
      required = required,
      size_mb = file_size_mb(to),
      status = ifelse(copied_ok, "READY_ALREADY_IN_PLACE", "CHECK_EMPTY_OR_MISSING"),
      note = "already_in_place",
      stringsAsFactors = FALSE
    ))
  }
  
  ok <- tryCatch(
    file.copy(from, to, overwrite = TRUE),
    error = function(e) FALSE
  )
  
  copied_ok <- isTRUE(ok) && file.exists(to) && file_nonzero(to)
  
  data.frame(
    upload_role = role,
    source_path = from,
    ready_path = to,
    copied = copied_ok,
    required = required,
    size_mb = file_size_mb(to),
    status = ifelse(copied_ok, "READY", ifelse(required, "COPY_FAILED_REQUIRED", "COPY_FAILED_OPTIONAL")),
    note = ifelse(copied_ok, "copied", "copy_failed_or_empty"),
    stringsAsFactors = FALSE
  )
}

write_text_file <- function(lines, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
}

detect_best_file <- function(inventory, patterns, preferred_ext = c("pdf", "png", "tiff", "tif", "jpg", "jpeg")) {
  if (nrow(inventory) == 0) return(NA_character_)
  
  required_cols <- c("file_name", "copied_path", "extension", "size_mb")
  if (!all(required_cols %in% colnames(inventory))) return(NA_character_)
  
  x <- inventory
  
  x$file_name_lower <- tolower(x$file_name)
  x$copied_path <- as.character(x$copied_path)
  x$extension <- tolower(as.character(x$extension))
  
  hit <- rep(FALSE, nrow(x))
  for (p in patterns) {
    hit <- hit | grepl(tolower(p), x$file_name_lower, fixed = FALSE)
  }
  
  x <- x[hit & file.exists(x$copied_path), , drop = FALSE]
  if (nrow(x) == 0) return(NA_character_)
  
  x$ext_rank <- match(x$extension, preferred_ext)
  x$ext_rank[is.na(x$ext_rank)] <- 999
  x$size_rank <- ifelse(is.na(x$size_mb), 999, x$size_mb)
  
  x <- x[order(x$ext_rank, x$size_rank), , drop = FALSE]
  x$copied_path[1]
}

detect_supp_file <- function(inventory, patterns, preferred_ext = c("xlsx", "csv", "pdf", "docx")) {
  if (nrow(inventory) == 0) return(NA_character_)
  
  required_cols <- c("file_name", "copied_path", "extension", "size_mb")
  if (!all(required_cols %in% colnames(inventory))) return(NA_character_)
  
  x <- inventory
  
  x$file_name_lower <- tolower(x$file_name)
  x$copied_path <- as.character(x$copied_path)
  x$extension <- tolower(as.character(x$extension))
  
  hit <- rep(FALSE, nrow(x))
  for (p in patterns) {
    hit <- hit | grepl(tolower(p), x$file_name_lower, fixed = FALSE)
  }
  
  x <- x[hit & file.exists(x$copied_path), , drop = FALSE]
  if (nrow(x) == 0) return(NA_character_)
  
  x$ext_rank <- match(x$extension, preferred_ext)
  x$ext_rank[is.na(x$ext_rank)] <- 999
  x$size_rank <- ifelse(is.na(x$size_mb), 999, x$size_mb)
  
  x <- x[order(x$ext_rank, x$size_rank), , drop = FALSE]
  x$copied_path[1]
}

get_ext <- function(path, fallback = "dat") {
  ext <- tolower(tools::file_ext(path))
  if (is.na(ext) || ext == "") return(fallback)
  ext
}

# ============================================================
# 读取 inventory
# ============================================================

message("Reading candidate inventories...")

figure_inventory <- read_csv_df(input_files$candidate_figure_inventory)
supp_inventory <- read_csv_df(input_files$candidate_supp_inventory)
upload_checklist_34 <- read_csv_df(input_files$upload_checklist)
visual_checklist_34 <- read_csv_df(input_files$visual_checklist)
main_figure_plan <- read_csv_df(input_files$main_figure_plan)
supplementary_plan <- read_csv_df(input_files$supplementary_plan)

# ============================================================
# 主文稿和 Cover letter
# ============================================================

message("Selecting main manuscript and cover letter...")

manifest <- data.frame()

manifest <- rbind(
  manifest,
  safe_copy(
    from = input_files$main_manuscript,
    to = file.path(ready_dir, "Main_manuscript.docx"),
    required = TRUE,
    role = "Main manuscript"
  )
)

manifest <- rbind(
  manifest,
  safe_copy(
    from = input_files$cover_letter_docx,
    to = file.path(ready_dir, "Cover_letter.docx"),
    required = TRUE,
    role = "Cover letter"
  )
)

# 同步保存 cover letter md，非正式上传，仅便于编辑
manifest <- rbind(
  manifest,
  safe_copy(
    from = input_files$cover_letter_md,
    to = file.path(ready_check_dir, "Cover_letter_source.md"),
    required = FALSE,
    role = "Cover letter source markdown for editing"
  )
)

# ============================================================
# 自动尝试选择 Figure 1-5
# ============================================================

message("Selecting candidate figures...")

# 说明：
# Figure 1 常常需要流程图，若没有合成图，自动识别可能失败。
# Figure 2 对应 nested LODO 性能
# Figure 3 对应 calibration/threshold
# Figure 4 对应 enrichment
# Figure 5 对应 scRNA localization
#
# 自动选择策略：
# 优先寻找文件名中带 Figure_1/Figure1/Main/F20/F29 等合成图关键词。
# 如果找不到，则寻找最接近的候选单图，但状态会标记为 AUTO_SELECTED_NEEDS_VISUAL_CONFIRMATION。

figure_targets <- data.frame(
  figure_id = paste0("Figure_", 1:5),
  expected_title = c(
    "Study design and analysis workflow",
    "Strict nested leave-one-dataset-out validation performance",
    "Calibration and fixed-threshold transportability across held-out cohorts",
    "Functional enrichment of sepsis-associated transcriptomic alterations",
    "Single-cell localization of the host-response signature"
  ),
  pattern_primary = I(list(
    c("figure[_ -]?1", "fig[_ -]?1", "study.*workflow", "workflow", "cohort.*workflow", "analysis.*workflow"),
    c("figure[_ -]?2", "fig[_ -]?2", "nested.*lodo", "lodo.*validation", "roc", "auroc"),
    c("figure[_ -]?3", "fig[_ -]?3", "calibration", "threshold", "observed.*predicted"),
    c("figure[_ -]?4", "fig[_ -]?4", "go[_ -]?bp", "enrichment", "functional"),
    c("figure[_ -]?5", "fig[_ -]?5", "single[_ -]?cell", "scrna", "module", "umap")
  )),
  pattern_fallback = I(list(
    c("f20", "t20", "study", "reporting"),
    c("f14", "f17a", "f17b", "roc", "pr_curve"),
    c("f17c", "f17d", "f17e", "f17f", "calibration", "threshold"),
    c("t05", "go", "enrichment"),
    c("f19", "scrna", "single_cell", "umap", "dotplot", "violin")
  )),
  stringsAsFactors = FALSE
)

figure_selection <- data.frame()

for (i in seq_len(nrow(figure_targets))) {
  fig_id <- figure_targets$figure_id[i]
  
  selected <- detect_best_file(
    figure_inventory,
    patterns = unlist(figure_targets$pattern_primary[[i]]),
    preferred_ext = c("pdf", "png", "tiff", "tif", "jpg", "jpeg")
  )
  
  selection_basis <- "primary_pattern"
  
  if (is.na(selected) || selected == "") {
    selected <- detect_best_file(
      figure_inventory,
      patterns = unlist(figure_targets$pattern_fallback[[i]]),
      preferred_ext = c("pdf", "png", "tiff", "tif", "jpg", "jpeg")
    )
    selection_basis <- "fallback_pattern"
  }
  
  ext <- get_ext(selected, fallback = "pdf")
  target_path <- file.path(ready_fig_dir, paste0(fig_id, ".", ext))
  
  if (is.na(selected) || selected == "") {
    figure_selection <- rbind(
      figure_selection,
      data.frame(
        figure_id = fig_id,
        expected_title = figure_targets$expected_title[i],
        selected_source = NA_character_,
        ready_path = target_path,
        selection_basis = "not_found",
        status = "NEED_MANUAL_SELECTION_OR_COMPOSITION",
        size_mb = NA_real_,
        file_size_ok_under_10MB = NA,
        stringsAsFactors = FALSE
      )
    )
  } else {
    copy_row <- safe_copy(
      from = selected,
      to = target_path,
      required = TRUE,
      role = fig_id
    )
    
    status <- ifelse(
      copy_row$copied,
      ifelse(selection_basis == "primary_pattern", "AUTO_SELECTED_NEEDS_VISUAL_CONFIRMATION", "FALLBACK_SELECTED_NEEDS_VISUAL_CONFIRMATION"),
      "COPY_FAILED"
    )
    
    figure_selection <- rbind(
      figure_selection,
      data.frame(
        figure_id = fig_id,
        expected_title = figure_targets$expected_title[i],
        selected_source = selected,
        ready_path = target_path,
        selection_basis = selection_basis,
        status = status,
        size_mb = file_size_mb(target_path),
        file_size_ok_under_10MB = ifelse(is.na(file_size_mb(target_path)), NA, file_size_mb(target_path) <= 10),
        stringsAsFactors = FALSE
      )
    )
    
    manifest <- rbind(manifest, copy_row)
  }
}

# ============================================================
# 选择 Supplementary / Additional files
# ============================================================

message("Selecting additional files...")

# 原则：
# 上传干净、有审稿价值的 supplement。
# 不把 T21 风险标记、sanity check、内部审计表直接作为 submission additional file。
# 如果某项无法整合，先复制最有代表性的 workbook 或 CSV，并在说明中标记。

additional_targets <- data.frame(
  additional_id = paste0("Additional_file_", 1:6),
  title = c(
    "Dataset eligibility and cohort-control context",
    "Differential expression and Gene Ontology enrichment results",
    "Candidate gene screening and nested LODO model details",
    "Calibration, threshold transportability and exploratory DCA results",
    "RNA-seq eligibility screening and No-Go decision",
    "Single-cell annotation and signature module score summaries"
  ),
  preferred_name = paste0("Additional_file_", 1:6, ".xlsx"),
  patterns = I(list(
    c("t20_integrated_workbook", "t20_table1", "cohort_context", "t15b"),
    c("t05_go_bp", "t05_gene_lists", "limma_deg"),
    c("t06_candidate", "t07_refined", "t14_gene_frequency", "t14_nested_metrics"),
    c("t17_calibration", "t17_threshold", "t17_dca_summary", "t17_pooled_perf"),
    c("t16b_eligibility", "t16b_go_no_go"),
    c("t19_scrna_celltype", "t19_scrna_cluster", "t19_scrna_summary")
  )),
  stringsAsFactors = FALSE
)

additional_selection <- data.frame()

for (i in seq_len(nrow(additional_targets))) {
  add_id <- additional_targets$additional_id[i]
  
  selected <- detect_supp_file(
    supp_inventory,
    patterns = unlist(additional_targets$patterns[[i]]),
    preferred_ext = c("xlsx", "csv", "pdf", "docx")
  )
  
  ext <- get_ext(selected, fallback = "xlsx")
  target_path <- file.path(ready_supp_dir, paste0(add_id, ".", ext))
  
  if (is.na(selected) || selected == "") {
    additional_selection <- rbind(
      additional_selection,
      data.frame(
        additional_id = add_id,
        title = additional_targets$title[i],
        selected_source = NA_character_,
        ready_path = target_path,
        status = "NEED_MANUAL_SELECTION_OR_COMBINATION",
        size_mb = NA_real_,
        file_size_ok_under_20MB = NA,
        note = "No matching candidate file found.",
        stringsAsFactors = FALSE
      )
    )
  } else {
    copy_row <- safe_copy(
      from = selected,
      to = target_path,
      required = FALSE,
      role = add_id
    )
    
    additional_selection <- rbind(
      additional_selection,
      data.frame(
        additional_id = add_id,
        title = additional_targets$title[i],
        selected_source = selected,
        ready_path = target_path,
        status = ifelse(copy_row$copied, "AUTO_SELECTED_NEEDS_VISUAL_CONFIRMATION", "COPY_FAILED"),
        size_mb = file_size_mb(target_path),
        file_size_ok_under_20MB = ifelse(is.na(file_size_mb(target_path)), NA, file_size_mb(target_path) <= 20),
        note = "Confirm whether this file alone is sufficient or should be combined with related tables.",
        stringsAsFactors = FALSE
      )
    )
    
    manifest <- rbind(manifest, copy_row)
  }
}

# ============================================================
# 生成 Additional files说明
# ============================================================

message("Writing additional file descriptions...")

additional_file_descriptions <- data.frame(
  file_label = additional_targets$additional_id,
  upload_file = basename(additional_selection$ready_path),
  title = additional_targets$title,
  suggested_caption = c(
    "Dataset eligibility, cohort-control context and harmonized phenotype definitions used in the bulk transcriptomic analysis.",
    "Differential expression and Gene Ontology biological process enrichment results supporting the biological interpretation.",
    "Candidate gene screening, redundancy filtering, gene-selection recurrence and nested LODO model details.",
    "Calibration, fixed-threshold transportability and exploratory decision-curve analysis summaries across held-out cohorts.",
    "RNA-seq dataset eligibility screening and the pre-specified No-Go decision for RNA-seq external validation.",
    "Single-cell cluster annotation, broad cell-type assignment and host-response signature module score summaries."
  ),
  current_status = additional_selection$status,
  stringsAsFactors = FALSE
)

additional_desc_path <- file.path(ready_supp_dir, "Additional_file_descriptions_for_submission_system.csv")
data.table::fwrite(additional_file_descriptions, additional_desc_path)

manifest <- rbind(
  manifest,
  safe_copy(
    from = additional_desc_path,
    to = file.path(ready_check_dir, "Additional_file_descriptions_for_submission_system.csv"),
    required = FALSE,
    role = "Additional file descriptions"
  )
)

# ============================================================
# 上传清单和人工检查
# ============================================================

message("Building final upload manifest and checks...")

ready_files <- list.files(
  ready_dir,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

ready_file_inventory <- data.frame(
  file_name = basename(ready_files),
  relative_path = gsub(paste0("^", normalize_path_safe(ready_dir, mustWork = TRUE), "/?"), "", normalize_path_safe(ready_files, mustWork = TRUE)),
  full_path = ready_files,
  extension = tolower(tools::file_ext(ready_files)),
  size_mb = vapply(ready_files, file_size_mb, numeric(1)),
  stringsAsFactors = FALSE
)

ready_file_inventory$upload_category <- ifelse(
  grepl("^Main_manuscript", ready_file_inventory$file_name),
  "Main manuscript",
  ifelse(
    grepl("^Cover_letter", ready_file_inventory$file_name),
    "Cover letter",
    ifelse(
      grepl("^Figure_", ready_file_inventory$file_name),
      "Figure",
      ifelse(
        grepl("^Additional_file_", ready_file_inventory$file_name),
        "Additional file",
        "Check or support file"
      )
    )
  )
)

required_main_exists <- file.exists(file.path(ready_dir, "Main_manuscript.docx"))
required_cover_exists <- file.exists(file.path(ready_dir, "Cover_letter.docx"))

figure_required_status <- data.frame(
  figure_id = figure_targets$figure_id,
  ready_path = file.path(ready_fig_dir, paste0(figure_targets$figure_id, ".", vapply(figure_selection$ready_path, get_ext, character(1)))),
  selection_status = figure_selection$status,
  exists = file.exists(figure_selection$ready_path),
  size_mb = figure_selection$size_mb,
  file_size_ok_under_10MB = figure_selection$file_size_ok_under_10MB,
  stringsAsFactors = FALSE
)

figure_needs_manual <- sum(grepl("NEED_MANUAL|FALLBACK|AUTO_SELECTED", figure_selection$status), na.rm = TRUE)
additional_needs_manual <- sum(grepl("NEED_MANUAL|AUTO_SELECTED", additional_selection$status), na.rm = TRUE)

final_checks <- data.frame(
  check_id = c(
    "C01",
    "C02",
    "C03",
    "C04",
    "C05",
    "C06",
    "C07",
    "C08",
    "C09",
    "C10",
    "C11",
    "C12"
  ),
  check_item = c(
    "Main manuscript copied",
    "Cover letter copied",
    "Five figure slots generated or flagged",
    "All selected figure files under 10 MB",
    "Additional files generated or flagged",
    "Additional file descriptions generated",
    "No required copy failure",
    "Ready folder exists",
    "Manual visual review still required",
    "Figure selection confirmation required",
    "Supplementary file selection confirmation required",
    "Data/code repository should be confirmed before final submit"
  ),
  observed = c(
    required_main_exists,
    required_cover_exists,
    nrow(figure_selection) == 5,
    all(figure_selection$file_size_ok_under_10MB %in% TRUE | is.na(figure_selection$file_size_ok_under_10MB)),
    nrow(additional_selection) == 6,
    file.exists(additional_desc_path),
    !any(manifest$required == TRUE & manifest$copied == FALSE),
    dir.exists(ready_dir),
    TRUE,
    figure_needs_manual > 0,
    additional_needs_manual > 0,
    TRUE
  ),
  status = c(
    ifelse(required_main_exists, "PASS", "CHECK"),
    ifelse(required_cover_exists, "PASS", "CHECK"),
    ifelse(nrow(figure_selection) == 5, "PASS", "CHECK"),
    ifelse(all(figure_selection$file_size_ok_under_10MB %in% TRUE | is.na(figure_selection$file_size_ok_under_10MB)), "PASS", "CHECK"),
    ifelse(nrow(additional_selection) == 6, "PASS", "CHECK"),
    ifelse(file.exists(additional_desc_path), "PASS", "CHECK"),
    ifelse(!any(manifest$required == TRUE & manifest$copied == FALSE), "PASS", "CHECK"),
    ifelse(dir.exists(ready_dir), "PASS", "CHECK"),
    "MANUAL_REQUIRED",
    ifelse(figure_needs_manual > 0, "MANUAL_REQUIRED", "PASS"),
    ifelse(additional_needs_manual > 0, "MANUAL_REQUIRED", "PASS"),
    "MANUAL_RECOMMENDED"
  ),
  stringsAsFactors = FALSE
)

n_hard_failed_checks <- sum(final_checks$status == "CHECK", na.rm = TRUE)

ready_for_submission_status <- ifelse(
  n_hard_failed_checks == 0,
  "YES_AFTER_MANUAL_FIGURE_SUPPLEMENT_REVIEW",
  "NO_FIX_CHECK_ITEMS_FIRST"
)

overall_status <- data.frame(
  metric = c(
    "ready_upload_dir",
    "main_manuscript_ready",
    "cover_letter_ready",
    "n_figure_slots",
    "n_figures_need_manual_confirmation",
    "n_additional_file_slots",
    "n_additional_files_need_manual_confirmation",
    "n_hard_failed_checks",
    "ready_for_submission_upload"
  ),
  value = c(
    ready_dir,
    ifelse(required_main_exists, "YES", "CHECK"),
    ifelse(required_cover_exists, "YES", "CHECK"),
    nrow(figure_selection),
    figure_needs_manual,
    nrow(additional_selection),
    additional_needs_manual,
    n_hard_failed_checks,
    ready_for_submission_status
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# README for READY_TO_UPLOAD
# ============================================================

readme_lines <- c(
  "# 00_READY_TO_UPLOAD",
  "",
  "This folder contains the cleaned upload package for BMC Genomics.",
  "",
  "## Files expected for submission",
  "",
  "1. Main_manuscript.docx",
  "2. Cover_letter.docx",
  "3. Figure_1 to Figure_5 files",
  "4. Additional_file_1 to Additional_file_6 files, if uploaded",
  "",
  "## Important manual checks",
  "",
  "1. Confirm that each Figure file is the final composite figure, not an isolated panel.",
  "2. Confirm that Figure 1 to Figure 5 match the legends in the manuscript.",
  "3. Confirm that additional files are cited in order in the manuscript or submission system.",
  "4. Confirm that no internal-only audit table is uploaded as a formal supplementary file.",
  "5. Confirm the data/code repository statement before clicking submit.",
  "",
  "## BMC Genomics specific practical point",
  "",
  "Individual figure files should be kept below 10 MB. Figures should be uploaded as separate files rather than embedded in the manuscript text.",
  "",
  "## Current script decision",
  "",
  paste0("ready_for_submission_upload = ", ready_for_submission_status)
)

readme_path <- file.path(ready_dir, "README_READY_TO_UPLOAD.md")
write_text_file(readme_lines, readme_path)

# ============================================================
# 写出结果
# ============================================================

manifest_path <- file.path(ready_check_dir, "BMC_Genomics_READY_TO_UPLOAD_manifest_v1.0.csv")
ready_inventory_path <- file.path(ready_check_dir, "BMC_Genomics_READY_TO_UPLOAD_file_inventory_v1.0.csv")
figure_selection_path <- file.path(ready_check_dir, "BMC_Genomics_final_figure_selection_v1.0.csv")
additional_selection_path <- file.path(ready_check_dir, "BMC_Genomics_final_additional_file_selection_v1.0.csv")
final_checks_path <- file.path(ready_check_dir, "BMC_Genomics_READY_TO_UPLOAD_final_checks_v1.0.csv")
overall_status_path <- file.path(ready_check_dir, "BMC_Genomics_READY_TO_UPLOAD_overall_status_v1.0.csv")
xlsx_path <- file.path(ready_dir, "BMC_Genomics_READY_TO_UPLOAD_manifest_v1.0.xlsx")

data.table::fwrite(manifest, manifest_path)
data.table::fwrite(ready_file_inventory, ready_inventory_path)
data.table::fwrite(figure_selection, figure_selection_path)
data.table::fwrite(additional_selection, additional_selection_path)
data.table::fwrite(final_checks, final_checks_path)
data.table::fwrite(overall_status, overall_status_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "final_checks")
openxlsx::writeData(wb, "final_checks", final_checks)

openxlsx::addWorksheet(wb, "ready_file_inventory")
openxlsx::writeData(wb, "ready_file_inventory", ready_file_inventory)

openxlsx::addWorksheet(wb, "figure_selection")
openxlsx::writeData(wb, "figure_selection", figure_selection)

openxlsx::addWorksheet(wb, "additional_selection")
openxlsx::writeData(wb, "additional_selection", additional_selection)

openxlsx::addWorksheet(wb, "additional_descriptions")
openxlsx::writeData(wb, "additional_descriptions", additional_file_descriptions)

openxlsx::addWorksheet(wb, "copy_manifest")
openxlsx::writeData(wb, "copy_manifest", manifest)

openxlsx::addWorksheet(wb, "figure_plan")
openxlsx::writeData(wb, "figure_plan", main_figure_plan)

openxlsx::addWorksheet(wb, "supplementary_plan")
openxlsx::writeData(wb, "supplementary_plan", supplementary_plan)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_35_BMC_Genomics_final_upload_file_selector.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 35 BMC Genomics final upload file selector 完成 ============")
message("READY_TO_UPLOAD 目录：", ready_dir)

message("\nOverall status:")
print(overall_status)

message("\nFigure selection:")
print(figure_selection)

message("\nAdditional file selection:")
print(additional_selection)

message("\nFinal checks:")
print(final_checks)

message("\nREADY file inventory:")
print(ready_file_inventory)

message("\n关键输出：")
message("1) ", ready_dir)
message("2) ", file.path(ready_dir, "Main_manuscript.docx"))
message("3) ", file.path(ready_dir, "Cover_letter.docx"))
message("4) ", ready_fig_dir)
message("5) ", ready_supp_dir)
message("6) ", xlsx_path)
message("7) ", figure_selection_path)
message("8) ", additional_selection_path)
message("9) ", final_checks_path)
message("10) ", readme_path)

message("\n下一步：")
message("把 Overall status、Figure selection、Additional file selection、Final checks 贴给我。")
message("我会判断 Figure 1-5 是否已经能直接上传，还是需要先合成主图。")