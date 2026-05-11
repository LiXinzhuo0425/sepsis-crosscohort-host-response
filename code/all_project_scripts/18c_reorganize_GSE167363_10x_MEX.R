# ============================================================
# 18c_reorganize_GSE167363_10x_MEX.R
# Reorganize GSE167363 prefixed 10x MEX files into per-sample folders
#
# 目的：
# 1. 读取 18b 的 file inventory 和 sample metadata
# 2. 将每个 GSM 的 10x 三件套整理为标准目录结构：
#    per_sample_10x/GSMxxxx_sample/
#      matrix.mtx.gz
#      features.tsv.gz
#      barcodes.tsv.gz
# 3. 输出 Seurat Read10X 可直接读取的目录清单
#
# 输入：
# 01_raw_data/scRNA/GSE167363/RAW/
# 04_results/single_cell/
#   T18b_GSE167363_sample_metadata.csv
#   T18b_GSE167363_10x_file_inventory.csv
#
# 输出：
# 01_raw_data/scRNA/GSE167363/per_sample_10x/
# 04_results/single_cell/
#   T18c_GSE167363_per_sample_10x_dirs.csv
#   T18c_GSE167363_reorganize_summary.xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

raw_sc_dir <- file.path(project_dir, "01_raw_data", "scRNA", "GSE167363")
raw_dir <- file.path(raw_sc_dir, "RAW")
per_sample_dir <- file.path(raw_sc_dir, "per_sample_10x")

out_dir <- file.path(project_dir, "04_results", "single_cell")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(per_sample_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

metadata_file <- file.path(out_dir, "T18b_GSE167363_sample_metadata.csv")
inventory_file <- file.path(out_dir, "T18b_GSE167363_10x_file_inventory.csv")

needed <- c(metadata_file, inventory_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

sample_metadata <- data.table::fread(metadata_file, data.table = FALSE)
file_inventory <- data.table::fread(inventory_file, data.table = FALSE)

required_meta_cols <- c("gsm", "sample_title", "clinical_group_scRNA", "sepsis_status", "outcome_group", "timepoint", "patient_id")
if (!all(required_meta_cols %in% colnames(sample_metadata))) {
  stop("sample metadata 缺少必要列：", paste(setdiff(required_meta_cols, colnames(sample_metadata)), collapse = ", "))
}

required_inventory_cols <- c("file_path", "file_name", "file_class", "gsm_guess")
if (!all(required_inventory_cols %in% colnames(file_inventory))) {
  stop("file inventory 缺少必要列：", paste(setdiff(required_inventory_cols, colnames(file_inventory)), collapse = ", "))
}

safe_sample_label <- function(gsm, sample_title) {
  title_clean <- gsub("[^A-Za-z0-9]+", "_", sample_title)
  title_clean <- gsub("_+", "_", title_clean)
  title_clean <- gsub("^_|_$", "", title_clean)
  paste0(gsm, "_", title_clean)
}

copy_or_overwrite <- function(from, to) {
  if (!file.exists(from)) {
    stop("源文件不存在：", from)
  }
  
  if (file.exists(to)) {
    file.remove(to)
  }
  
  ok <- file.copy(from, to, overwrite = TRUE)
  
  if (!ok) {
    stop("复制失败：\nfrom: ", from, "\nto: ", to)
  }
  
  TRUE
}

message("Reorganizing GSE167363 10x MEX files...")

out_list <- list()
log_list <- list()

for (i in seq_len(nrow(sample_metadata))) {
  
  gsm <- sample_metadata$gsm[i]
  sample_title <- sample_metadata$sample_title[i]
  sample_label <- safe_sample_label(gsm, sample_title)
  
  sample_out_dir <- file.path(per_sample_dir, sample_label)
  dir.create(sample_out_dir, recursive = TRUE, showWarnings = FALSE)
  
  sub <- file_inventory[file_inventory$gsm_guess == gsm, , drop = FALSE]
  
  matrix_files <- sub$file_path[sub$file_class == "matrix_mtx"]
  feature_files <- sub$file_path[sub$file_class == "features_or_genes_tsv"]
  barcode_files <- sub$file_path[sub$file_class == "barcodes_tsv"]
  
  if (length(matrix_files) != 1 || length(feature_files) != 1 || length(barcode_files) != 1) {
    warning(
      "样本文件数量异常：", gsm,
      " matrix=", length(matrix_files),
      " features=", length(feature_files),
      " barcodes=", length(barcode_files)
    )
  }
  
  if (length(matrix_files) >= 1) {
    copy_or_overwrite(matrix_files[1], file.path(sample_out_dir, "matrix.mtx.gz"))
  }
  
  if (length(feature_files) >= 1) {
    copy_or_overwrite(feature_files[1], file.path(sample_out_dir, "features.tsv.gz"))
  }
  
  if (length(barcode_files) >= 1) {
    copy_or_overwrite(barcode_files[1], file.path(sample_out_dir, "barcodes.tsv.gz"))
  }
  
  has_matrix <- file.exists(file.path(sample_out_dir, "matrix.mtx.gz"))
  has_features <- file.exists(file.path(sample_out_dir, "features.tsv.gz"))
  has_barcodes <- file.exists(file.path(sample_out_dir, "barcodes.tsv.gz"))
  
  complete <- has_matrix && has_features && has_barcodes
  
  out_list[[gsm]] <- data.frame(
    gsm = gsm,
    sample_title = sample_title,
    sample_label = sample_label,
    data_dir = sample_out_dir,
    has_matrix_mtx = has_matrix,
    has_features_tsv = has_features,
    has_barcodes_tsv = has_barcodes,
    complete_10x_mex = complete,
    stringsAsFactors = FALSE
  )
  
  log_list[[gsm]] <- data.frame(
    gsm = gsm,
    sample_label = sample_label,
    original_matrix_file = ifelse(length(matrix_files) >= 1, matrix_files[1], NA),
    original_features_file = ifelse(length(feature_files) >= 1, feature_files[1], NA),
    original_barcodes_file = ifelse(length(barcode_files) >= 1, barcode_files[1], NA),
    output_dir = sample_out_dir,
    stringsAsFactors = FALSE
  )
}

per_sample_dirs <- do.call(rbind, out_list)
copy_log <- do.call(rbind, log_list)

per_sample_dirs <- merge(
  per_sample_dirs,
  sample_metadata,
  by = c("gsm", "sample_title"),
  all.x = TRUE,
  sort = FALSE
)

per_sample_dirs <- per_sample_dirs[, c(
  "gsm",
  "sample_title",
  "sample_label",
  "data_dir",
  "complete_10x_mex",
  "has_matrix_mtx",
  "has_features_tsv",
  "has_barcodes_tsv",
  "clinical_group_scRNA",
  "sepsis_status",
  "outcome_group",
  "timepoint",
  "patient_id",
  "dataset"
)]

summary_df <- data.frame(
  metric = c(
    "n_samples_in_metadata",
    "n_samples_reorganized",
    "n_complete_10x_mex_dirs",
    "per_sample_10x_dir"
  ),
  value = c(
    nrow(sample_metadata),
    nrow(per_sample_dirs),
    sum(per_sample_dirs$complete_10x_mex, na.rm = TRUE),
    per_sample_dir
  ),
  stringsAsFactors = FALSE
)

data.table::fwrite(
  per_sample_dirs,
  file.path(out_dir, "T18c_GSE167363_per_sample_10x_dirs.csv")
)

data.table::fwrite(
  copy_log,
  file.path(out_dir, "T18c_GSE167363_10x_copy_log.csv")
)

data.table::fwrite(
  summary_df,
  file.path(out_dir, "T18c_GSE167363_reorganize_summary.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary")
openxlsx::writeData(wb, "summary", summary_df)

openxlsx::addWorksheet(wb, "per_sample_dirs")
openxlsx::writeData(wb, "per_sample_dirs", per_sample_dirs)

openxlsx::addWorksheet(wb, "copy_log")
openxlsx::writeData(wb, "copy_log", copy_log)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T18c_GSE167363_reorganize_summary.xlsx"),
  overwrite = TRUE
)

sink(file.path(log_dir, "sessionInfo_18c_reorganize_GSE167363_10x_MEX.txt"))
print(sessionInfo())
sink()

message("\n============ 18c GSE167363 10x MEX reorganize 完成 ============")

message("\nSummary:")
print(summary_df)

message("\nPer-sample 10x directories:")
print(per_sample_dirs)

message("\n关键输出：")
message("1) ", file.path(out_dir, "T18c_GSE167363_per_sample_10x_dirs.csv"))
message("2) ", file.path(out_dir, "T18c_GSE167363_reorganize_summary.xlsx"))
message("3) ", per_sample_dir)

message("\n下一步：")
message("如果 n_complete_10x_mex_dirs = 12，就继续运行 18_prepare_scRNA_dataset.R。")