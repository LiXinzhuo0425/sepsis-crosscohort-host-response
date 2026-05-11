# ============================================================
# 36_BMC_Genomics_final_figures_and_supplements_builder.R
# Build final composite figures and clean additional files
# for BMC Genomics submission
#
# Fixed version:
# - Corrected grid::grid.segments() argument names in draw_arrow()
# - Uses x0/y0/x1/y1 as required by grid.segments()
# - Keeps all previous Step 36 logic
#
# 输入：
#   34/35 已生成的 final submission package
#
# 输出：
#   00_READY_TO_UPLOAD/
#     Main_manuscript.docx
#     Cover_letter.docx
#     figures/Figure_1.pdf to Figure_5.pdf
#     figures/Figure_1.png to Figure_5.png
#     additional_files/Additional_file_1.xlsx to Additional_file_6.xlsx
#     checks/...
#
# 说明：
#   1. 本脚本不改正文、不改统计结果
#   2. Figure 1 使用 grid 绘制流程图
#   3. Figure 2-5 优先从已有候选 PDF/PNG 面板复制为正式候选图
#   4. Additional files 整理成 6 个干净 xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(grid)
  library(grDevices)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

package_dir <- file.path(
  project_dir,
  "07_manuscript",
  "BMC_Genomics_final_submission_package"
)

ready_dir <- file.path(package_dir, "00_READY_TO_UPLOAD")
ready_fig_dir <- file.path(ready_dir, "figures")
ready_supp_dir <- file.path(ready_dir, "additional_files")
ready_check_dir <- file.path(ready_dir, "checks")

candidate_fig_dir <- file.path(package_dir, "02_figures")
candidate_supp_dir <- file.path(package_dir, "03_supplementary_files")

draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(ready_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_supp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_check_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 工具函数
# ============================================================

file_exists_nonzero <- function(path) {
  file.exists(path) && isTRUE(file.info(path)$size > 0)
}

file_size_mb <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

write_csv_safe <- function(x, path) {
  data.table::fwrite(x, path)
}

safe_copy <- function(from, to, required = TRUE, role = NA_character_) {
  if (is.na(from) || !file.exists(from)) {
    return(data.frame(
      role = role,
      source_path = from,
      output_path = to,
      copied = FALSE,
      required = required,
      size_mb = NA_real_,
      status = ifelse(required, "MISSING_REQUIRED_SOURCE", "MISSING_OPTIONAL_SOURCE"),
      stringsAsFactors = FALSE
    ))
  }
  
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  
  from_norm <- tryCatch(
    normalizePath(from, winslash = "/", mustWork = TRUE),
    error = function(e) from
  )
  to_norm <- tryCatch(
    normalizePath(to, winslash = "/", mustWork = FALSE),
    error = function(e) to
  )
  
  if (identical(from_norm, to_norm)) {
    ok <- file_exists_nonzero(to)
    return(data.frame(
      role = role,
      source_path = from,
      output_path = to,
      copied = ok,
      required = required,
      size_mb = file_size_mb(to),
      status = ifelse(ok, "ALREADY_IN_PLACE", "CHECK_EMPTY_OR_MISSING"),
      stringsAsFactors = FALSE
    ))
  }
  
  ok <- tryCatch(file.copy(from, to, overwrite = TRUE), error = function(e) FALSE)
  copied_ok <- isTRUE(ok) && file_exists_nonzero(to)
  
  data.frame(
    role = role,
    source_path = from,
    output_path = to,
    copied = copied_ok,
    required = required,
    size_mb = file_size_mb(to),
    status = ifelse(copied_ok, "READY", ifelse(required, "COPY_FAILED_REQUIRED", "COPY_FAILED_OPTIONAL")),
    stringsAsFactors = FALSE
  )
}

find_file <- function(patterns, exts = c("pdf", "png"), base_dir = candidate_fig_dir) {
  files <- list.files(base_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  files <- files[tolower(tools::file_ext(files)) %in% tolower(exts)]
  if (length(files) == 0) return(NA_character_)
  
  lower <- tolower(basename(files))
  hit <- rep(FALSE, length(files))
  
  for (p in patterns) {
    hit <- hit | grepl(tolower(p), lower, perl = TRUE)
  }
  
  candidates <- files[hit]
  if (length(candidates) == 0) return(NA_character_)
  
  ext_rank <- match(tolower(tools::file_ext(candidates)), tolower(exts))
  ext_rank[is.na(ext_rank)] <- 999
  size_rank <- vapply(candidates, file_size_mb, numeric(1))
  
  candidates[order(ext_rank, size_rank)][1]
}

find_supp <- function(patterns, exts = c("xlsx", "csv"), base_dir = candidate_supp_dir) {
  files <- list.files(base_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  files <- files[tolower(tools::file_ext(files)) %in% tolower(exts)]
  if (length(files) == 0) return(NA_character_)
  
  lower <- tolower(basename(files))
  hit <- rep(FALSE, length(files))
  
  for (p in patterns) {
    hit <- hit | grepl(tolower(p), lower, perl = TRUE)
  }
  
  candidates <- files[hit]
  if (length(candidates) == 0) return(NA_character_)
  
  ext_rank <- match(tolower(tools::file_ext(candidates)), tolower(exts))
  ext_rank[is.na(ext_rank)] <- 999
  size_rank <- vapply(candidates, file_size_mb, numeric(1))
  
  candidates[order(ext_rank, size_rank)][1]
}

read_table_any <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(data.frame(note = "source_missing"))
  }
  
  ext <- tolower(tools::file_ext(path))
  
  out <- tryCatch({
    if (ext == "csv") {
      data.table::fread(path, data.table = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      sheets <- openxlsx::getSheetNames(path)
      if (length(sheets) == 0) {
        data.frame(note = "empty_workbook")
      } else {
        openxlsx::read.xlsx(path, sheet = sheets[1])
      }
    } else {
      data.frame(note = paste0("unsupported_extension_", ext))
    }
  }, error = function(e) {
    data.frame(note = paste0("read_failed: ", conditionMessage(e)))
  })
  
  as.data.frame(out)
}

write_workbook_from_sources <- function(source_list, output_path, metadata = NULL) {
  wb <- openxlsx::createWorkbook()
  
  if (!is.null(metadata)) {
    openxlsx::addWorksheet(wb, "README")
    openxlsx::writeData(wb, "README", metadata)
  }
  
  for (nm in names(source_list)) {
    path <- source_list[[nm]]
    sheet_name <- gsub("[^A-Za-z0-9_]", "_", nm)
    sheet_name <- substr(sheet_name, 1, 31)
    
    df <- read_table_any(path)
    
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, df)
  }
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}

# ============================================================
# Figure 1 流程图函数
# ============================================================

draw_box <- function(x, y, w, h, label, gp_fill = "grey95", fontsize = 8.5) {
  grid::grid.roundrect(
    x = x,
    y = y,
    width = w,
    height = h,
    r = unit(0.04, "snpc"),
    gp = grid::gpar(fill = gp_fill, col = "grey30", lwd = 1)
  )
  
  grid::grid.text(
    label,
    x = x,
    y = y,
    gp = grid::gpar(fontsize = fontsize, col = "black"),
    just = "center"
  )
}

draw_arrow <- function(x_start, y_start, x_end, y_end) {
  grid::grid.segments(
    x0 = x_start,
    y0 = y_start,
    x1 = x_end,
    y1 = y_end,
    default.units = "npc",
    arrow = grid::arrow(type = "closed", length = unit(0.12, "inches")),
    gp = grid::gpar(col = "grey35", lwd = 1)
  )
}

make_figure1_page <- function() {
  grid::grid.newpage()
  
  grid::grid.text(
    "Figure 1. Study design and analysis workflow",
    x = 0.5,
    y = 0.96,
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
  
  grid::grid.text("A", x = 0.04, y = 0.89, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  draw_box(0.18, 0.84, 0.23, 0.10, "Public whole-blood\ntranscriptomic cohorts\n6 datasets, n = 484", "grey95")
  draw_box(0.50, 0.84, 0.23, 0.10, "Gene-symbol\nharmonization and\ncohort-level screening", "grey95")
  draw_box(0.82, 0.84, 0.23, 0.10, "Final frozen bulk set\n333 sepsis cases\n151 controls", "grey95")
  draw_arrow(0.30, 0.84, 0.38, 0.84)
  draw_arrow(0.62, 0.84, 0.70, 0.84)
  
  grid::grid.text("B", x = 0.04, y = 0.66, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  draw_box(0.18, 0.61, 0.25, 0.11, "Strict nested\nleave-one-dataset-out\nvalidation", "grey95")
  draw_box(0.50, 0.61, 0.25, 0.11, "Training-only\nfeature screening,\nmodel fitting and thresholding", "grey95")
  draw_box(0.82, 0.61, 0.25, 0.11, "Held-out dataset\nused only for\nvalidation", "grey95")
  draw_arrow(0.31, 0.61, 0.38, 0.61)
  draw_arrow(0.63, 0.61, 0.70, 0.61)
  
  grid::grid.text("C", x = 0.04, y = 0.43, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  draw_box(0.18, 0.38, 0.25, 0.11, "RNA-seq screening\n22 datasets scanned", "grey95")
  draw_box(0.50, 0.38, 0.25, 0.11, "No eligible RNA-seq\nexternal validation dataset\ndetected", "grey95")
  draw_box(0.82, 0.38, 0.25, 0.11, "RNA-seq validation\nrecorded as No-Go", "grey95")
  draw_arrow(0.31, 0.38, 0.38, 0.38)
  draw_arrow(0.63, 0.38, 0.70, 0.38)
  
  grid::grid.text("D", x = 0.04, y = 0.20, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  draw_box(0.18, 0.15, 0.25, 0.11, "Independent PBMC\nscRNA-seq dataset\nGSE167363", "grey95")
  draw_box(0.50, 0.15, 0.25, 0.11, "Broad cell-type\nannotation and\nmodule scoring", "grey95")
  draw_box(0.82, 0.15, 0.25, 0.11, "Biological localization\npredominantly\nMonocyte/Myeloid", "grey95")
  draw_arrow(0.31, 0.15, 0.38, 0.15)
  draw_arrow(0.63, 0.15, 0.70, 0.15)
}

make_figure1 <- function(pdf_path, png_path) {
  grDevices::pdf(pdf_path, width = 11, height = 7.5, onefile = TRUE)
  make_figure1_page()
  grDevices::dev.off()
  
  grDevices::png(png_path, width = 3300, height = 2250, res = 300)
  make_figure1_page()
  grDevices::dev.off()
}

# ============================================================
# 合成图逻辑
# ============================================================

message("Building final figures...")

figure_manifest <- data.frame()

fig1_pdf <- file.path(ready_fig_dir, "Figure_1.pdf")
fig1_png <- file.path(ready_fig_dir, "Figure_1.png")

make_figure1(fig1_pdf, fig1_png)

figure_manifest <- rbind(
  figure_manifest,
  data.frame(
    figure = "Figure_1",
    source_files = "Generated by script from frozen workflow design",
    output_pdf = fig1_pdf,
    output_png = fig1_png,
    pdf_exists = file_exists_nonzero(fig1_pdf),
    png_exists = file_exists_nonzero(fig1_png),
    pdf_size_mb = file_size_mb(fig1_pdf),
    png_size_mb = file_size_mb(fig1_png),
    status = ifelse(
      file_exists_nonzero(fig1_pdf) && file_exists_nonzero(fig1_png),
      "READY_NEEDS_VISUAL_CHECK",
      "CHECK"
    ),
    stringsAsFactors = FALSE
  )
)

figure_source_map <- list(
  Figure_2 = c(
    "nested.*lodo",
    "validation",
    "roc",
    "auroc",
    "f17a",
    "f17b",
    "t14"
  ),
  Figure_3 = c(
    "calibration",
    "threshold",
    "observed.*predicted",
    "f17c",
    "f17d",
    "f17e",
    "f17f"
  ),
  Figure_4 = c(
    "go_bp",
    "enrichment",
    "barplot",
    "f07g",
    "f07h",
    "f05"
  ),
  Figure_5 = c(
    "scrna",
    "single_cell",
    "umap",
    "module",
    "dotplot",
    "violin",
    "f19"
  )
)

for (fig in names(figure_source_map)) {
  src_file <- find_file(figure_source_map[[fig]], exts = c("pdf", "png"))
  out_pdf <- file.path(ready_fig_dir, paste0(fig, ".pdf"))
  out_png <- file.path(ready_fig_dir, paste0(fig, ".png"))
  
  if (!is.na(src_file) && file.exists(src_file)) {
    ext <- tolower(tools::file_ext(src_file))
    
    if (ext == "pdf") {
      safe_copy(src_file, out_pdf, required = TRUE, role = fig)
      
      png_src <- sub("\\.pdf$", ".png", src_file, ignore.case = TRUE)
      if (file.exists(png_src)) {
        safe_copy(png_src, out_png, required = FALSE, role = paste0(fig, "_png"))
      } else {
        png_candidate <- find_file(figure_source_map[[fig]], exts = c("png"))
        if (!is.na(png_candidate) && file.exists(png_candidate)) {
          safe_copy(png_candidate, out_png, required = FALSE, role = paste0(fig, "_png"))
        }
      }
    }
    
    if (ext == "png") {
      safe_copy(src_file, out_png, required = TRUE, role = fig)
    }
  }
  
  figure_manifest <- rbind(
    figure_manifest,
    data.frame(
      figure = fig,
      source_files = ifelse(is.na(src_file), NA_character_, src_file),
      output_pdf = out_pdf,
      output_png = out_png,
      pdf_exists = file_exists_nonzero(out_pdf),
      png_exists = file_exists_nonzero(out_png),
      pdf_size_mb = file_size_mb(out_pdf),
      png_size_mb = file_size_mb(out_png),
      status = ifelse(
        file_exists_nonzero(out_pdf) || file_exists_nonzero(out_png),
        "READY_SOURCE_SELECTED_NEEDS_VISUAL_CHECK",
        "MISSING_NEEDS_MANUAL_COMPOSITION"
      ),
      stringsAsFactors = FALSE
    )
  )
}

# ============================================================
# 构建 6 个干净 Additional files
# ============================================================

message("Building clean additional files...")

additional_manifest <- data.frame()

additional_specs <- list(
  Additional_file_1 = list(
    title = "Dataset eligibility and cohort-control context",
    sources = list(
      T20_integrated_workbook = find_supp(c("t20_integrated_workbook"), c("xlsx")),
      T20_table1_cohort_context = find_supp(c("t20_table1.*cohort_context", "cohort_context"), c("csv")),
      T20_table2_nested_LODO_validation = find_supp(c("t20_table2.*nested_lodo_validation"), c("csv")),
      T20_table3_calibration_threshold_transport = find_supp(c("t20_table3.*calibration_threshold_transport"), c("csv")),
      T20_table4_scRNA_localization = find_supp(c("t20_table4.*scrna_localization"), c("csv"))
    )
  ),
  Additional_file_2 = list(
    title = "Differential expression and Gene Ontology enrichment results",
    sources = list(
      DEG_main_limma = find_supp(c("limma_deg.*sepsis_vs_control"), c("csv")),
      DEG_gene_lists = find_supp(c("t05_deg_gene_lists", "t05_gene_lists"), c("xlsx")),
      GO_BP_enrichment = find_supp(c("t05_go_bp", "go_bp_enrichment"), c("xlsx"))
    )
  ),
  Additional_file_3 = list(
    title = "Candidate gene screening and nested LODO model details",
    sources = list(
      Candidate_gene_screening = find_supp(c("t06_candidate"), c("xlsx")),
      Refined_candidate_summary = find_supp(c("t07_refined"), c("xlsx")),
      Nested_LODO_gene_frequency = find_supp(c("t14_gene_frequency"), c("csv")),
      Nested_LODO_validation_metrics = find_supp(c("t14_nested_metrics"), c("csv")),
      Nested_LODO_predictions = find_supp(c("t14_nested_predictions"), c("csv"))
    )
  ),
  Additional_file_4 = list(
    title = "Calibration, threshold transportability and exploratory DCA results",
    sources = list(
      Pooled_nested_LODO_performance = find_supp(c("t17_pooled_perf"), c("csv")),
      Calibration_metrics = find_supp(c("t17_calibration"), c("csv")),
      Threshold_drift = find_supp(c("t17_threshold"), c("csv")),
      DCA_summary = find_supp(c("t17_dca_summary"), c("csv"))
    )
  ),
  Additional_file_5 = list(
    title = "RNA-seq eligibility screening and No-Go decision",
    sources = list(
      RNAseq_eligibility_table = find_supp(c("t16b_eligibility"), c("csv")),
      RNAseq_go_no_go_summary = find_supp(c("t16b_go_no_go"), c("csv"))
    )
  ),
  Additional_file_6 = list(
    title = "Single-cell annotation and signature module score summaries",
    sources = list(
      scRNA_signature_score_by_celltype = find_supp(c("t19_scrna_celltype"), c("csv")),
      scRNA_cluster_annotation = find_supp(c("t19_scrna_cluster"), c("csv")),
      scRNA_localization_summary = find_supp(c("t19_scrna_summary"), c("csv"))
    )
  )
)

additional_descriptions <- data.frame(
  additional_file = names(additional_specs),
  title = vapply(additional_specs, function(x) x$title, character(1)),
  filename = paste0(names(additional_specs), ".xlsx"),
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

for (nm in names(additional_specs)) {
  spec <- additional_specs[[nm]]
  out_path <- file.path(ready_supp_dir, paste0(nm, ".xlsx"))
  
  readme <- data.frame(
    field = c("Additional file", "Title", "Purpose", "Generated_by"),
    value = c(
      nm,
      spec$title,
      "Clean supplementary workbook generated from frozen source outputs.",
      "36_BMC_Genomics_final_figures_and_supplements_builder.R"
    ),
    stringsAsFactors = FALSE
  )
  
  write_workbook_from_sources(
    source_list = spec$sources,
    output_path = out_path,
    metadata = readme
  )
  
  src_status <- data.frame(
    source_label = names(spec$sources),
    source_path = unlist(spec$sources, use.names = FALSE),
    source_exists = file.exists(unlist(spec$sources, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  
  additional_manifest <- rbind(
    additional_manifest,
    data.frame(
      additional_file = nm,
      title = spec$title,
      output_path = out_path,
      output_exists = file_exists_nonzero(out_path),
      output_size_mb = file_size_mb(out_path),
      n_sources = length(spec$sources),
      n_sources_found = sum(src_status$source_exists),
      status = ifelse(file_exists_nonzero(out_path), "READY_NEEDS_VISUAL_CHECK", "CHECK"),
      stringsAsFactors = FALSE
    )
  )
  
  write_csv_safe(
    src_status,
    file.path(ready_check_dir, paste0(nm, "_source_status.csv"))
  )
}

desc_path <- file.path(ready_supp_dir, "Additional_file_descriptions_for_submission_system.csv")
write_csv_safe(additional_descriptions, desc_path)

# ============================================================
# 最终检查
# ============================================================

message("Running final upload checks...")

main_manuscript <- file.path(ready_dir, "Main_manuscript.docx")
cover_letter <- file.path(ready_dir, "Cover_letter.docx")

required_figures <- file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf"))
figure_pdf_exists <- file.exists(required_figures)

required_additional <- file.path(ready_supp_dir, paste0("Additional_file_", 1:6, ".xlsx"))
additional_exists <- file.exists(required_additional)

figure_size_ok <- vapply(required_figures, function(x) {
  if (!file.exists(x)) return(FALSE)
  s <- file_size_mb(x)
  !is.na(s) && s <= 10
}, logical(1))

additional_size_ok <- vapply(required_additional, function(x) {
  if (!file.exists(x)) return(FALSE)
  s <- file_size_mb(x)
  !is.na(s) && s <= 20
}, logical(1))

final_checks <- data.frame(
  check_id = c(
    "C01", "C02", "C03", "C04", "C05",
    "C06", "C07", "C08", "C09", "C10",
    "C11", "C12"
  ),
  check_item = c(
    "Main manuscript exists",
    "Cover letter exists",
    "Figure 1 PDF exists",
    "Figure 2 PDF exists",
    "Figure 3 PDF exists",
    "Figure 4 PDF exists",
    "Figure 5 PDF exists",
    "All figure PDFs under 10 MB",
    "Six additional XLSX files exist",
    "All additional files under 20 MB",
    "Additional file descriptions exist",
    "Manual visual check required before submit"
  ),
  observed = c(
    file_exists_nonzero(main_manuscript),
    file_exists_nonzero(cover_letter),
    figure_pdf_exists[1],
    figure_pdf_exists[2],
    figure_pdf_exists[3],
    figure_pdf_exists[4],
    figure_pdf_exists[5],
    all(figure_size_ok),
    all(additional_exists),
    all(additional_size_ok),
    file_exists_nonzero(desc_path),
    TRUE
  ),
  status = c(
    ifelse(file_exists_nonzero(main_manuscript), "PASS", "CHECK"),
    ifelse(file_exists_nonzero(cover_letter), "PASS", "CHECK"),
    ifelse(figure_pdf_exists[1], "PASS", "CHECK"),
    ifelse(figure_pdf_exists[2], "PASS", "CHECK"),
    ifelse(figure_pdf_exists[3], "PASS", "CHECK"),
    ifelse(figure_pdf_exists[4], "PASS", "CHECK"),
    ifelse(figure_pdf_exists[5], "PASS", "CHECK"),
    ifelse(all(figure_size_ok), "PASS", "CHECK"),
    ifelse(all(additional_exists), "PASS", "CHECK"),
    ifelse(all(additional_size_ok), "PASS", "CHECK"),
    ifelse(file_exists_nonzero(desc_path), "PASS", "CHECK"),
    "MANUAL_REQUIRED"
  ),
  stringsAsFactors = FALSE
)

n_failed_checks <- sum(final_checks$status == "CHECK", na.rm = TRUE)

overall_status <- data.frame(
  metric = c(
    "ready_upload_dir",
    "main_manuscript_ready",
    "cover_letter_ready",
    "n_figure_pdf_ready",
    "n_additional_xlsx_ready",
    "n_failed_checks",
    "ready_for_submission_upload",
    "recommended_next_step"
  ),
  value = c(
    ready_dir,
    ifelse(file_exists_nonzero(main_manuscript), "YES", "CHECK"),
    ifelse(file_exists_nonzero(cover_letter), "YES", "CHECK"),
    sum(figure_pdf_exists),
    sum(additional_exists),
    n_failed_checks,
    ifelse(n_failed_checks == 0, "YES_AFTER_MANUAL_VISUAL_REVIEW", "NO_FIX_ITEMS_FIRST"),
    ifelse(
      n_failed_checks == 0,
      "Open Figure 1-5 and Additional file 1-6 manually, then proceed to submission system.",
      "Fix missing figure or additional file outputs, then rerun script."
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# READY_TO_UPLOAD inventory
# ============================================================

ready_files <- list.files(ready_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)

ready_inventory <- data.frame(
  file_name = basename(ready_files),
  relative_path = gsub(
    paste0("^", normalizePath(ready_dir, winslash = "/", mustWork = TRUE), "/?"),
    "",
    normalizePath(ready_files, winslash = "/", mustWork = TRUE)
  ),
  full_path = ready_files,
  extension = tolower(tools::file_ext(ready_files)),
  size_mb = vapply(ready_files, file_size_mb, numeric(1)),
  stringsAsFactors = FALSE
)

ready_inventory$upload_category <- ifelse(
  ready_inventory$file_name == "Main_manuscript.docx",
  "Main manuscript",
  ifelse(
    ready_inventory$file_name == "Cover_letter.docx",
    "Cover letter",
    ifelse(
      grepl("^Figure_[1-5]\\.(pdf|png|tif|tiff|jpg|jpeg)$", ready_inventory$file_name, ignore.case = TRUE),
      "Main figure",
      ifelse(
        grepl("^Additional_file_[1-6]\\.xlsx$", ready_inventory$file_name, ignore.case = TRUE),
        "Additional file",
        "Check/support file"
      )
    )
  )
)

# ============================================================
# 写出审计文件
# ============================================================

figure_manifest_path <- file.path(ready_check_dir, "BMC_Genomics_final_composite_figure_manifest_v1.1.csv")
additional_manifest_path <- file.path(ready_check_dir, "BMC_Genomics_clean_additional_file_manifest_v1.1.csv")
final_checks_path <- file.path(ready_check_dir, "BMC_Genomics_final_upload_checks_v1.1.csv")
overall_status_path <- file.path(ready_check_dir, "BMC_Genomics_final_upload_overall_status_v1.1.csv")
ready_inventory_path <- file.path(ready_check_dir, "BMC_Genomics_READY_TO_UPLOAD_inventory_v1.1.csv")
desc_check_path <- file.path(ready_check_dir, "Additional_file_descriptions_for_submission_system.csv")
xlsx_manifest_path <- file.path(ready_dir, "BMC_Genomics_READY_TO_UPLOAD_manifest_v1.1.xlsx")

write_csv_safe(figure_manifest, figure_manifest_path)
write_csv_safe(additional_manifest, additional_manifest_path)
write_csv_safe(final_checks, final_checks_path)
write_csv_safe(overall_status, overall_status_path)
write_csv_safe(ready_inventory, ready_inventory_path)
write_csv_safe(additional_descriptions, desc_check_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "final_checks")
openxlsx::writeData(wb, "final_checks", final_checks)

openxlsx::addWorksheet(wb, "figure_manifest")
openxlsx::writeData(wb, "figure_manifest", figure_manifest)

openxlsx::addWorksheet(wb, "additional_manifest")
openxlsx::writeData(wb, "additional_manifest", additional_manifest)

openxlsx::addWorksheet(wb, "additional_descriptions")
openxlsx::writeData(wb, "additional_descriptions", additional_descriptions)

openxlsx::addWorksheet(wb, "ready_inventory")
openxlsx::writeData(wb, "ready_inventory", ready_inventory)

openxlsx::saveWorkbook(wb, xlsx_manifest_path, overwrite = TRUE)

readme_lines <- c(
  "# BMC Genomics READY_TO_UPLOAD package after Step 36",
  "",
  "This folder contains the cleaned upload-ready package.",
  "",
  "Upload candidates:",
  "",
  "- Main_manuscript.docx",
  "- Cover_letter.docx",
  "- figures/Figure_1.pdf to figures/Figure_5.pdf",
  "- additional_files/Additional_file_1.xlsx to Additional_file_6.xlsx",
  "",
  "Manual visual review is still required before clicking submit.",
  "",
  "Check specifically:",
  "",
  "1. Figure 1 workflow accuracy.",
  "2. Whether Figures 2-5 are acceptable as final figure files or need more polished multi-panel composition.",
  "3. Whether Additional files 1-6 open normally and contain the expected sheets.",
  "4. Whether the manuscript cites figures and additional files in correct order.",
  "5. Whether Data availability has been finalized with repository information if available."
)

readme_path <- file.path(ready_dir, "README_READY_TO_UPLOAD_after_step36.md")
writeLines(readme_lines, con = readme_path, useBytes = TRUE)

sink(file.path(log_dir, "sessionInfo_36_BMC_Genomics_final_figures_and_supplements_builder.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 36 BMC Genomics final figures and supplements builder 完成 ============")
message("READY_TO_UPLOAD 目录：", ready_dir)

message("\nOverall status:")
print(overall_status)

message("\nFigure manifest:")
print(figure_manifest)

message("\nAdditional file manifest:")
print(additional_manifest)

message("\nFinal checks:")
print(final_checks)

message("\nReady inventory:")
print(ready_inventory)

message("\nAdditional file descriptions:")
print(additional_descriptions)

message("\n关键输出：")
message("1) ", ready_dir)
message("2) ", file.path(ready_dir, "Main_manuscript.docx"))
message("3) ", file.path(ready_dir, "Cover_letter.docx"))
message("4) ", ready_fig_dir)
message("5) ", ready_supp_dir)
message("6) ", xlsx_manifest_path)
message("7) ", figure_manifest_path)
message("8) ", additional_manifest_path)
message("9) ", final_checks_path)
message("10) ", readme_path)

message("\n下一步：")
message("把 Overall status、Figure manifest、Additional file manifest、Final checks 贴给我。")
message("我会判断 Figure 2-5 是否还需要进一步多面板重绘，还是可以进入最终投稿系统检查。")