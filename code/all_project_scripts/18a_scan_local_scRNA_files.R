# ============================================================
# 18a_scan_local_scRNA_files.R
# Scan local files for possible single-cell RNA-seq datasets
#
# 目的：
# 1. 扫描本地可能的 scRNA-seq 文件
# 2. 识别 10x MEX 三件套、10x h5、h5ad、h5Seurat、Seurat RDS
# 3. 输出候选单细胞文件清单
# 4. 为下一步 18_prepare_scRNA_dataset.R 决定读取方式
#
# 本脚本不读取完整单细胞对象，不做 QC，不做聚类。
#
# 输出：
# 04_results/single_cell/
#   T18a_scRNA_file_inventory.csv
#   T18a_10x_mex_candidate_dirs.csv
#   T18a_h5_candidate_files.csv
#   T18a_h5ad_candidate_files.csv
#   T18a_h5seurat_candidate_files.csv
#   T18a_rds_candidate_files.csv
#   T18a_scRNA_scan_summary.xlsx
#
# 04_results/logs/
#   sessionInfo_18a_scan_local_scRNA_files.txt
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

out_dir <- file.path(project_dir, "04_results", "single_cell")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 扫描目录
# 如果你后续把 scRNA 数据放到其他目录，直接加到这里
# ============================================================

scan_dirs <- c(
  file.path(project_dir, "01_raw_data"),
  file.path(project_dir, "02_processed_data"),
  file.path(project_dir, "single_cell"),
  file.path(project_dir, "scRNA"),
  file.path(project_dir, "scrna"),
  "/Users/felix/Downloads",
  "/Users/felix/Documents"
)

scan_dirs <- unique(scan_dirs[file.exists(scan_dirs)])

# ============================================================
# 目标基因
# 仅用于检查文件名或小型文本对象，不读取大型矩阵
# ============================================================

final_10_genes <- c(
  "RAB31", "VNN1", "TCN1", "EMILIN2", "ZDHHC19",
  "ANKRD22", "PGLYRP1", "FAM20A", "RNASE3", "HK3"
)

nested_recurrent_genes <- c(
  "CD177", "TDRD9", "LILRA6", "RAB31", "VNN1",
  "ANKRD22", "ARG1", "C3AR1", "RNASE3", "WFDC1"
)

target_genes <- unique(c(final_10_genes, nested_recurrent_genes))

# ============================================================
# 工具函数
# ============================================================

safe_file_info <- function(files) {
  if (length(files) == 0) return(data.frame())
  
  info <- file.info(files)
  
  data.frame(
    file_path = files,
    file_name = basename(files),
    dir_name = dirname(files),
    extension = tolower(tools::file_ext(files)),
    size_mb = round(info$size / 1024^2, 3),
    modified_time = as.character(info$mtime),
    stringsAsFactors = FALSE
  )
}

classify_sc_file <- function(file_path) {
  fn <- tolower(basename(file_path))
  ext <- tolower(tools::file_ext(file_path))
  
  if (grepl("\\.h5ad$", fn)) return("h5ad_anndata")
  if (grepl("\\.h5seurat$", fn)) return("h5seurat")
  if (grepl("\\.rds$", fn)) return("rds_possible_seurat_or_sce")
  if (grepl("\\.rda$|\\.rdata$", fn)) return("rdata_possible_seurat_or_sce")
  
  if (grepl("filtered_feature_bc_matrix\\.h5$|raw_feature_bc_matrix\\.h5$|cellranger.*\\.h5$|10x.*\\.h5$", fn)) {
    return("tenx_h5_candidate")
  }
  
  if (grepl("\\.h5$", fn)) return("h5_unknown_candidate")
  
  if (grepl("matrix\\.mtx$|matrix\\.mtx\\.gz$", fn)) return("tenx_mex_matrix")
  if (grepl("features\\.tsv$|features\\.tsv\\.gz$|genes\\.tsv$|genes\\.tsv\\.gz$", fn)) return("tenx_mex_features")
  if (grepl("barcodes\\.tsv$|barcodes\\.tsv\\.gz$", fn)) return("tenx_mex_barcodes")
  
  if (grepl("metadata|meta|pheno|phenotype|clinical|annotation|celltype|cell_type|cluster|barcode", fn)) {
    return("metadata_candidate")
  }
  
  if (grepl("counts|count|expression|expr|matrix|normalized|seurat|singlecell|single_cell|scrna|scRNA", fn, ignore.case = TRUE)) {
    return("tabular_expression_or_metadata_candidate")
  }
  
  "unknown"
}

should_skip_dir_or_file <- function(file_path) {
  p <- tolower(file_path)
  
  skip_patterns <- c(
    "/03_scripts/",
    "/04_results/differential_expression/",
    "/04_results/nested_lodo/",
    "/04_results/reporting/",
    "/04_results/dca/",
    "/04_results/candidate_selection/",
    "/04_results/enrichment/",
    "/04_results/final_model/",
    "/04_results/model_validation/",
    "/04_results/sensitivity/",
    "/04_results/logs/",
    "/05_figures/",
    "/07_manuscript/",
    "/platform_annotation/",
    "/geo_series_matrix/",
    "sessioninfo",
    "\\.pdf$",
    "\\.png$",
    "\\.jpg$",
    "\\.jpeg$",
    "\\.tif$",
    "\\.tiff$"
  )
  
  any(grepl(paste(skip_patterns, collapse = "|"), p))
}

safe_read_small_text_preview <- function(file_path, n_max = 50) {
  out <- try({
    if (grepl("\\.csv$|\\.tsv$|\\.txt$|\\.csv\\.gz$|\\.tsv\\.gz$|\\.txt\\.gz$",
              file_path,
              ignore.case = TRUE)) {
      df <- suppressWarnings(
        data.table::fread(
          file_path,
          nrows = n_max,
          data.table = FALSE,
          showProgress = FALSE,
          fill = TRUE,
          blank.lines.skip = TRUE
        )
      )
      return(as.data.frame(df))
    }
    
    if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
      sheets <- openxlsx::getSheetNames(file_path)
      if (length(sheets) == 0) return(NULL)
      df <- openxlsx::read.xlsx(file_path, sheet = sheets[1], rows = 1:(n_max + 1))
      return(as.data.frame(df))
    }
    
    NULL
  }, silent = TRUE)
  
  if (inherits(out, "try-error")) return(NULL)
  out
}

detect_target_genes_in_text_preview <- function(file_path, target_genes) {
  df <- safe_read_small_text_preview(file_path, n_max = 80)
  
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(character())
  }
  
  vals <- unique(toupper(as.character(unlist(df, use.names = FALSE))))
  target_upper <- toupper(target_genes)
  
  target_genes[target_upper %in% vals]
}

is_possible_metadata_table <- function(file_path) {
  df <- safe_read_small_text_preview(file_path, n_max = 40)
  
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  
  cn <- tolower(colnames(df))
  
  any(grepl(
    "sample|cell|barcode|cluster|celltype|cell_type|condition|group|diagnosis|sepsis|control|patient|donor|orig.ident",
    cn
  ))
}

# ============================================================
# 扫描文件
# ============================================================

message("Scanning directories:")
print(scan_dirs)

file_patterns <- c(
  "\\.h5ad$",
  "\\.h5seurat$",
  "\\.h5$",
  "\\.mtx$",
  "\\.mtx\\.gz$",
  "\\.tsv$",
  "\\.tsv\\.gz$",
  "\\.csv$",
  "\\.csv\\.gz$",
  "\\.txt$",
  "\\.txt\\.gz$",
  "\\.rds$",
  "\\.RData$",
  "\\.rda$",
  "\\.xlsx$"
)

all_files <- character()

for (d in scan_dirs) {
  message("Scanning: ", d)
  
  ff <- list.files(
    d,
    pattern = paste(file_patterns, collapse = "|"),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  all_files <- c(all_files, ff)
}

all_files <- unique(all_files)
all_files <- all_files[!vapply(all_files, should_skip_dir_or_file, logical(1))]

file_inventory <- safe_file_info(all_files)

if (nrow(file_inventory) == 0) {
  stop("没有扫描到候选单细胞文件。请确认 scRNA 数据是否已经下载。")
}

file_inventory$sc_file_class <- vapply(file_inventory$file_path, classify_sc_file, character(1))
file_inventory$is_sc_candidate <- file_inventory$sc_file_class != "unknown"
file_inventory$is_metadata_candidate <- file_inventory$sc_file_class == "metadata_candidate"
file_inventory$detected_target_genes_in_preview <- NA_character_
file_inventory$n_detected_target_genes_in_preview <- NA_integer_
file_inventory$possible_metadata_table <- FALSE

# 只对小型文本表格做预览，避免读取大矩阵
preview_idx <- which(
  file_inventory$size_mb <= 50 &
    grepl("\\.csv$|\\.tsv$|\\.txt$|\\.csv\\.gz$|\\.tsv\\.gz$|\\.txt\\.gz$|\\.xlsx$",
          file_inventory$file_name,
          ignore.case = TRUE)
)

message("Total candidate files after skipping known result folders: ", nrow(file_inventory))
message("Small text files selected for preview: ", length(preview_idx))

for (i in preview_idx) {
  f <- file_inventory$file_path[i]
  detected <- detect_target_genes_in_text_preview(f, target_genes)
  file_inventory$detected_target_genes_in_preview[i] <- paste(detected, collapse = ";")
  file_inventory$n_detected_target_genes_in_preview[i] <- length(detected)
  file_inventory$possible_metadata_table[i] <- is_possible_metadata_table(f)
}

# ============================================================
# 识别 10x MEX 目录
# ============================================================

mex_matrix_files <- file_inventory$file_path[file_inventory$sc_file_class == "tenx_mex_matrix"]
mex_feature_files <- file_inventory$file_path[file_inventory$sc_file_class == "tenx_mex_features"]
mex_barcode_files <- file_inventory$file_path[file_inventory$sc_file_class == "tenx_mex_barcodes"]

candidate_dirs <- unique(dirname(c(mex_matrix_files, mex_feature_files, mex_barcode_files)))

tenx_mex_candidate_dirs <- data.frame()

if (length(candidate_dirs) > 0) {
  mex_list <- list()
  
  for (d in candidate_dirs) {
    files_d <- list.files(d, full.names = FALSE)
    files_low <- tolower(files_d)
    
    has_matrix <- any(grepl("^matrix\\.mtx(\\.gz)?$", files_low))
    has_features <- any(grepl("^(features|genes)\\.tsv(\\.gz)?$", files_low))
    has_barcodes <- any(grepl("^barcodes\\.tsv(\\.gz)?$", files_low))
    
    mex_list[[d]] <- data.frame(
      data_dir = d,
      has_matrix_mtx = has_matrix,
      has_features_or_genes_tsv = has_features,
      has_barcodes_tsv = has_barcodes,
      complete_10x_mex = has_matrix & has_features & has_barcodes,
      n_files_in_dir = length(files_d),
      stringsAsFactors = FALSE
    )
  }
  
  tenx_mex_candidate_dirs <- do.call(rbind, mex_list)
  rownames(tenx_mex_candidate_dirs) <- NULL
}

# ============================================================
# 候选对象文件
# ============================================================

h5_candidate_files <- file_inventory[
  file_inventory$sc_file_class %in% c("tenx_h5_candidate", "h5_unknown_candidate"),
  ,
  drop = FALSE
]

h5ad_candidate_files <- file_inventory[
  file_inventory$sc_file_class == "h5ad_anndata",
  ,
  drop = FALSE
]

h5seurat_candidate_files <- file_inventory[
  file_inventory$sc_file_class == "h5seurat",
  ,
  drop = FALSE
]

rds_candidate_files <- file_inventory[
  file_inventory$sc_file_class %in% c("rds_possible_seurat_or_sce", "rdata_possible_seurat_or_sce"),
  ,
  drop = FALSE
]

metadata_candidate_files <- file_inventory[
  file_inventory$is_metadata_candidate == TRUE | file_inventory$possible_metadata_table == TRUE,
  ,
  drop = FALSE
]

gene_detection_summary <- data.frame(
  gene_symbol = target_genes,
  detected_in_any_preview = vapply(
    target_genes,
    function(g) {
      any(grepl(
        paste0("(^|;)", g, "(;|$)"),
        file_inventory$detected_target_genes_in_preview,
        ignore.case = TRUE
      ), na.rm = TRUE)
    },
    logical(1)
  ),
  stringsAsFactors = FALSE
)

gene_detection_summary$file_hits <- vapply(
  gene_detection_summary$gene_symbol,
  function(g) {
    hits <- file_inventory$file_name[
      grepl(
        paste0("(^|;)", g, "(;|$)"),
        file_inventory$detected_target_genes_in_preview,
        ignore.case = TRUE
      )
    ]
    paste(unique(hits), collapse = "; ")
  },
  character(1)
)

scan_summary <- data.frame(
  metric = c(
    "n_scan_dirs",
    "n_total_files_after_skip",
    "n_sc_candidate_files",
    "n_complete_10x_mex_dirs",
    "n_h5_candidate_files",
    "n_h5ad_candidate_files",
    "n_h5seurat_candidate_files",
    "n_rds_or_rdata_candidate_files",
    "n_metadata_candidate_files",
    "n_target_genes_detected_in_preview"
  ),
  value = c(
    length(scan_dirs),
    nrow(file_inventory),
    sum(file_inventory$is_sc_candidate, na.rm = TRUE),
    ifelse(nrow(tenx_mex_candidate_dirs) == 0, 0, sum(tenx_mex_candidate_dirs$complete_10x_mex, na.rm = TRUE)),
    nrow(h5_candidate_files),
    nrow(h5ad_candidate_files),
    nrow(h5seurat_candidate_files),
    nrow(rds_candidate_files),
    nrow(metadata_candidate_files),
    sum(gene_detection_summary$detected_in_any_preview, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存结果
# ============================================================

data.table::fwrite(
  file_inventory,
  file.path(out_dir, "T18a_scRNA_file_inventory.csv")
)

data.table::fwrite(
  tenx_mex_candidate_dirs,
  file.path(out_dir, "T18a_10x_mex_candidate_dirs.csv")
)

data.table::fwrite(
  h5_candidate_files,
  file.path(out_dir, "T18a_h5_candidate_files.csv")
)

data.table::fwrite(
  h5ad_candidate_files,
  file.path(out_dir, "T18a_h5ad_candidate_files.csv")
)

data.table::fwrite(
  h5seurat_candidate_files,
  file.path(out_dir, "T18a_h5seurat_candidate_files.csv")
)

data.table::fwrite(
  rds_candidate_files,
  file.path(out_dir, "T18a_rds_candidate_files.csv")
)

data.table::fwrite(
  metadata_candidate_files,
  file.path(out_dir, "T18a_metadata_candidate_files.csv")
)

data.table::fwrite(
  gene_detection_summary,
  file.path(out_dir, "T18a_gene_detection_summary.csv")
)

data.table::fwrite(
  scan_summary,
  file.path(out_dir, "T18a_scan_summary.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "scan_summary")
openxlsx::writeData(wb, "scan_summary", scan_summary)

openxlsx::addWorksheet(wb, "10x_mex_dirs")
openxlsx::writeData(wb, "10x_mex_dirs", tenx_mex_candidate_dirs)

openxlsx::addWorksheet(wb, "h5_files")
openxlsx::writeData(wb, "h5_files", h5_candidate_files)

openxlsx::addWorksheet(wb, "h5ad_files")
openxlsx::writeData(wb, "h5ad_files", h5ad_candidate_files)

openxlsx::addWorksheet(wb, "h5seurat_files")
openxlsx::writeData(wb, "h5seurat_files", h5seurat_candidate_files)

openxlsx::addWorksheet(wb, "rds_files")
openxlsx::writeData(wb, "rds_files", rds_candidate_files)

openxlsx::addWorksheet(wb, "metadata_files")
openxlsx::writeData(wb, "metadata_files", metadata_candidate_files)

openxlsx::addWorksheet(wb, "gene_detection")
openxlsx::writeData(wb, "gene_detection", gene_detection_summary)

openxlsx::addWorksheet(wb, "full_inventory")
openxlsx::writeData(wb, "full_inventory", file_inventory)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T18a_scRNA_scan_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_18a_scan_local_scRNA_files.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 18a Local scRNA file scan 完成 ============")

message("\n扫描目录：")
print(scan_dirs)

message("\nScan summary:")
print(scan_summary)

message("\nComplete 10x MEX candidate directories:")
if (nrow(tenx_mex_candidate_dirs) > 0) {
  print(tenx_mex_candidate_dirs[tenx_mex_candidate_dirs$complete_10x_mex == TRUE, , drop = FALSE])
} else {
  message("未发现 10x MEX 三件套目录。")
}

message("\nH5 candidate files:")
if (nrow(h5_candidate_files) > 0) {
  print(h5_candidate_files[, c("file_name", "file_path", "size_mb", "sc_file_class"), drop = FALSE])
} else {
  message("未发现 h5 候选文件。")
}

message("\nH5AD candidate files:")
if (nrow(h5ad_candidate_files) > 0) {
  print(h5ad_candidate_files[, c("file_name", "file_path", "size_mb", "sc_file_class"), drop = FALSE])
} else {
  message("未发现 h5ad 候选文件。")
}

message("\nH5Seurat candidate files:")
if (nrow(h5seurat_candidate_files) > 0) {
  print(h5seurat_candidate_files[, c("file_name", "file_path", "size_mb", "sc_file_class"), drop = FALSE])
} else {
  message("未发现 h5Seurat 候选文件。")
}

message("\nRDS/RData candidate files:")
if (nrow(rds_candidate_files) > 0) {
  print(head(rds_candidate_files[, c("file_name", "file_path", "size_mb", "sc_file_class"), drop = FALSE], 30))
} else {
  message("未发现 RDS/RData 候选文件。")
}

message("\nMetadata candidate files:")
if (nrow(metadata_candidate_files) > 0) {
  print(head(metadata_candidate_files[, c("file_name", "file_path", "size_mb", "sc_file_class"), drop = FALSE], 30))
} else {
  message("未发现 metadata 候选文件。")
}

message("\nTarget gene detection summary:")
print(gene_detection_summary)

message("\n关键输出：")
message("1) ", file.path(out_dir, "T18a_scRNA_scan_summary.xlsx"))
message("2) ", file.path(out_dir, "T18a_10x_mex_candidate_dirs.csv"))
message("3) ", file.path(out_dir, "T18a_h5_candidate_files.csv"))
message("4) ", file.path(out_dir, "T18a_h5ad_candidate_files.csv"))
message("5) ", file.path(out_dir, "T18a_h5seurat_candidate_files.csv"))
message("6) ", file.path(out_dir, "T18a_rds_candidate_files.csv"))
message("7) ", file.path(out_dir, "T18a_metadata_candidate_files.csv"))
message("8) ", file.path(out_dir, "T18a_scRNA_file_inventory.csv"))

message("\n下一步：")
message("请把 Scan summary、Complete 10x MEX candidate directories、H5/H5AD/H5Seurat/RDS candidate files 贴给我。")
message("我会根据扫描结果生成 18_prepare_scRNA_dataset.R。")