# ============================================================
# 18b_download_or_prepare_GSE167363_scRNA.R
# Download and prepare GSE167363 scRNA-seq files
#
# 目的：
# 1. 创建 GSE167363 单细胞数据目录
# 2. 下载 GEO supplementary file: GSE167363_RAW.tar
# 3. 解压并识别 10x MEX 文件结构
# 4. 生成 sample metadata
# 5. 输出后续 Seurat 读取所需的目录清单
#
# 数据集背景：
# GSE167363:
# Dynamic changes in human single cell transcriptional signatures during fatal sepsis
# Human PBMC scRNA-seq from healthy controls, sepsis survivors and non-survivors.
# GEO supplementary file: GSE167363_RAW.tar, TAR of MTX/TSV.
#
# 本脚本不做 Seurat QC、聚类或 module score。
#
# 输出：
# 01_raw_data/scRNA/GSE167363/
#   GSE167363_RAW.tar
#   RAW/
#
# 04_results/single_cell/
#   T18b_GSE167363_sample_metadata.csv
#   T18b_GSE167363_10x_file_inventory.csv
#   T18b_GSE167363_10x_candidate_dirs.csv
#   T18b_GSE167363_prepare_summary.xlsx
#
# 04_results/logs/
#   sessionInfo_18b_download_or_prepare_GSE167363_scRNA.txt
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

raw_sc_dir <- file.path(project_dir, "01_raw_data", "scRNA", "GSE167363")
raw_extract_dir <- file.path(raw_sc_dir, "RAW")

out_dir <- file.path(project_dir, "04_results", "single_cell")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(raw_sc_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_extract_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 下载设置
# ============================================================

gse_id <- "GSE167363"

tar_url <- "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE167nnn/GSE167363/suppl/GSE167363_RAW.tar"
tar_file <- file.path(raw_sc_dir, "GSE167363_RAW.tar")

# 如果网络不稳定，可以手动下载：
# https://ftp.ncbi.nlm.nih.gov/geo/series/GSE167nnn/GSE167363/suppl/GSE167363_RAW.tar
# 放到：
# /Users/felix/Documents/Sepsis_CrossCohort_scRNA/01_raw_data/scRNA/GSE167363/GSE167363_RAW.tar

download_if_missing <- TRUE
force_redownload <- FALSE
force_reextract <- FALSE

# ============================================================
# 样本 metadata
# ============================================================

sample_metadata <- data.frame(
  gsm = c(
    "GSM5102900",
    "GSM5102901",
    "GSM5102902",
    "GSM5102903",
    "GSM5102904",
    "GSM5102905",
    "GSM5511351",
    "GSM5511352",
    "GSM5511353",
    "GSM5511354",
    "GSM5511355",
    "GSM5511356"
  ),
  sample_title = c(
    "HC1",
    "HC2",
    "NS LS T0",
    "NS LS T6",
    "S1 T0",
    "S1 T6",
    "NS ES_T0",
    "NS ES_T6",
    "S2_T0",
    "S2_T6",
    "S3_T0",
    "S3_T6"
  ),
  clinical_group_scRNA = c(
    "Healthy",
    "Healthy",
    "Sepsis_nonsurvivor",
    "Sepsis_nonsurvivor",
    "Sepsis_survivor",
    "Sepsis_survivor",
    "Sepsis_nonsurvivor",
    "Sepsis_nonsurvivor",
    "Sepsis_survivor",
    "Sepsis_survivor",
    "Sepsis_survivor",
    "Sepsis_survivor"
  ),
  sepsis_status = c(
    "Healthy",
    "Healthy",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis",
    "Sepsis"
  ),
  outcome_group = c(
    "Healthy",
    "Healthy",
    "Non_survivor",
    "Non_survivor",
    "Survivor",
    "Survivor",
    "Non_survivor",
    "Non_survivor",
    "Survivor",
    "Survivor",
    "Survivor",
    "Survivor"
  ),
  timepoint = c(
    "Healthy",
    "Healthy",
    "T0",
    "T6",
    "T0",
    "T6",
    "T0",
    "T6",
    "T0",
    "T6",
    "T0",
    "T6"
  ),
  patient_id = c(
    "HC1",
    "HC2",
    "NS_LS",
    "NS_LS",
    "S1",
    "S1",
    "NS_ES",
    "NS_ES",
    "S2",
    "S2",
    "S3",
    "S3"
  ),
  stringsAsFactors = FALSE
)

sample_metadata$dataset <- gse_id

# ============================================================
# 工具函数
# ============================================================

download_file_safely <- function(url, destfile) {
  message("Downloading:")
  message(url)
  message("to:")
  message(destfile)
  
  ok <- FALSE
  
  method_list <- c("libcurl", "curl", "auto")
  
  for (m in method_list) {
    message("Trying download method: ", m)
    
    res <- try(
      utils::download.file(
        url = url,
        destfile = destfile,
        mode = "wb",
        method = m,
        quiet = FALSE
      ),
      silent = TRUE
    )
    
    if (!inherits(res, "try-error") && file.exists(destfile) && file.info(destfile)$size > 1024^2) {
      ok <- TRUE
      break
    }
  }
  
  ok
}

safe_file_info <- function(files) {
  if (length(files) == 0) return(data.frame())
  
  info <- file.info(files)
  
  data.frame(
    file_path = files,
    relative_path = NA_character_,
    file_name = basename(files),
    dir_name = dirname(files),
    extension = tolower(tools::file_ext(files)),
    size_mb = round(info$size / 1024^2, 3),
    modified_time = as.character(info$mtime),
    stringsAsFactors = FALSE
  )
}

classify_10x_file <- function(file_path) {
  fn <- tolower(basename(file_path))
  
  if (grepl("matrix\\.mtx(\\.gz)?$", fn)) return("matrix_mtx")
  if (grepl("(features|genes)\\.tsv(\\.gz)?$", fn)) return("features_or_genes_tsv")
  if (grepl("barcodes\\.tsv(\\.gz)?$", fn)) return("barcodes_tsv")
  if (grepl("\\.mtx(\\.gz)?$", fn)) return("other_mtx")
  if (grepl("\\.tsv(\\.gz)?$", fn)) return("other_tsv")
  if (grepl("\\.csv(\\.gz)?$", fn)) return("csv")
  
  "other"
}

infer_sample_from_path <- function(file_path, sample_metadata) {
  x <- basename(file_path)
  p <- file_path
  
  for (gsm in sample_metadata$gsm) {
    if (grepl(gsm, x, fixed = TRUE) || grepl(gsm, p, fixed = TRUE)) {
      return(gsm)
    }
  }
  
  for (title in sample_metadata$sample_title) {
    title_clean <- gsub("[^A-Za-z0-9]+", "_", title)
    x_clean <- gsub("[^A-Za-z0-9]+", "_", x)
    p_clean <- gsub("[^A-Za-z0-9]+", "_", p)
    
    if (grepl(title_clean, x_clean, ignore.case = TRUE) ||
        grepl(title_clean, p_clean, ignore.case = TRUE)) {
      return(sample_metadata$gsm[sample_metadata$sample_title == title][1])
    }
  }
  
  NA_character_
}

detect_10x_candidate_dirs <- function(file_inventory) {
  dirs <- unique(file_inventory$dir_name)
  
  if (length(dirs) == 0) return(data.frame())
  
  out_list <- list()
  
  for (d in dirs) {
    sub <- file_inventory[file_inventory$dir_name == d, , drop = FALSE]
    
    has_matrix <- any(sub$file_class == "matrix_mtx")
    has_features <- any(sub$file_class == "features_or_genes_tsv")
    has_barcodes <- any(sub$file_class == "barcodes_tsv")
    
    sample_guess <- NA_character_
    
    gsm_hits <- unique(sub$gsm_guess[!is.na(sub$gsm_guess) & sub$gsm_guess != ""])
    if (length(gsm_hits) == 1) {
      sample_guess <- gsm_hits
    }
    
    out_list[[d]] <- data.frame(
      data_dir = d,
      gsm_guess = sample_guess,
      has_matrix_mtx = has_matrix,
      has_features_or_genes_tsv = has_features,
      has_barcodes_tsv = has_barcodes,
      complete_10x_mex = has_matrix & has_features & has_barcodes,
      n_files_in_dir = nrow(sub),
      stringsAsFactors = FALSE
    )
  }
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  
  out[order(!out$complete_10x_mex, out$data_dir), , drop = FALSE]
}

# ============================================================
# 下载
# ============================================================

message("============ GSE167363 scRNA prepare ============")

if (force_redownload && file.exists(tar_file)) {
  message("force_redownload = TRUE, removing existing tar file.")
  file.remove(tar_file)
}

if (!file.exists(tar_file) && download_if_missing) {
  ok <- download_file_safely(tar_url, tar_file)
  
  if (!ok) {
    stop(
      "自动下载失败。\n",
      "请手动下载：\n",
      tar_url, "\n",
      "并保存为：\n",
      tar_file
    )
  }
}

if (!file.exists(tar_file)) {
  stop(
    "未发现 GSE167363_RAW.tar。\n",
    "请手动下载：\n",
    tar_url, "\n",
    "并保存到：\n",
    tar_file
  )
}

tar_size_mb <- round(file.info(tar_file)$size / 1024^2, 3)
message("Found tar file: ", tar_file)
message("Tar size: ", tar_size_mb, " Mb")

# ============================================================
# 解压
# ============================================================

existing_extracted_files <- list.files(raw_extract_dir, recursive = TRUE, full.names = TRUE)

if (force_reextract && length(existing_extracted_files) > 0) {
  message("force_reextract = TRUE, removing existing extracted files.")
  unlink(raw_extract_dir, recursive = TRUE)
  dir.create(raw_extract_dir, recursive = TRUE, showWarnings = FALSE)
  existing_extracted_files <- character()
}

if (length(existing_extracted_files) == 0) {
  message("Extracting tar file to:")
  message(raw_extract_dir)
  
  res <- try(
    utils::untar(tar_file, exdir = raw_extract_dir),
    silent = TRUE
  )
  
  if (inherits(res, "try-error")) {
    stop("解压失败。请检查 tar 文件是否完整：", tar_file)
  }
} else {
  message("Existing extracted files detected. Skip extraction.")
}

# ============================================================
# 文件识别
# ============================================================

all_files <- list.files(raw_extract_dir, recursive = TRUE, full.names = TRUE)

if (length(all_files) == 0) {
  stop("解压目录为空：", raw_extract_dir)
}

file_inventory <- safe_file_info(all_files)
file_inventory$relative_path <- sub(paste0("^", raw_extract_dir, "/?"), "", file_inventory$file_path)
file_inventory$file_class <- vapply(file_inventory$file_path, classify_10x_file, character(1))
file_inventory$gsm_guess <- vapply(file_inventory$file_path, infer_sample_from_path, character(1), sample_metadata = sample_metadata)

candidate_dirs <- detect_10x_candidate_dirs(file_inventory)

candidate_dirs_merged <- merge(
  candidate_dirs,
  sample_metadata,
  by.x = "gsm_guess",
  by.y = "gsm",
  all.x = TRUE,
  sort = FALSE
)

# 如果 GEO 文件结构不是按 GSM 分目录，而是文件名带 GSM，也允许后续脚本按文件名分组重建目录
complete_dirs <- candidate_dirs_merged[candidate_dirs_merged$complete_10x_mex == TRUE, , drop = FALSE]

# ============================================================
# 简单质量检查
# ============================================================

sample_file_counts <- aggregate(
  file_path ~ gsm_guess + file_class,
  data = file_inventory[!is.na(file_inventory$gsm_guess), , drop = FALSE],
  FUN = length
)
colnames(sample_file_counts)[colnames(sample_file_counts) == "file_path"] <- "n_files"

sample_detection_summary <- merge(
  sample_metadata,
  aggregate(
    file_path ~ gsm_guess,
    data = file_inventory[!is.na(file_inventory$gsm_guess), , drop = FALSE],
    FUN = length
  ),
  by.x = "gsm",
  by.y = "gsm_guess",
  all.x = TRUE,
  sort = FALSE
)

colnames(sample_detection_summary)[colnames(sample_detection_summary) == "file_path"] <- "n_detected_files"
sample_detection_summary$n_detected_files[is.na(sample_detection_summary$n_detected_files)] <- 0
sample_detection_summary$file_detected_flag <- sample_detection_summary$n_detected_files > 0

prepare_summary <- data.frame(
  metric = c(
    "dataset",
    "tar_file_exists",
    "tar_size_mb",
    "n_extracted_files",
    "n_matrix_mtx_files",
    "n_features_or_genes_tsv_files",
    "n_barcodes_tsv_files",
    "n_candidate_dirs",
    "n_complete_10x_mex_dirs",
    "n_samples_in_metadata",
    "n_samples_with_detected_files"
  ),
  value = c(
    gse_id,
    file.exists(tar_file),
    tar_size_mb,
    nrow(file_inventory),
    sum(file_inventory$file_class == "matrix_mtx"),
    sum(file_inventory$file_class == "features_or_genes_tsv"),
    sum(file_inventory$file_class == "barcodes_tsv"),
    nrow(candidate_dirs_merged),
    nrow(complete_dirs),
    nrow(sample_metadata),
    sum(sample_detection_summary$file_detected_flag)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存
# ============================================================

data.table::fwrite(
  sample_metadata,
  file.path(out_dir, "T18b_GSE167363_sample_metadata.csv")
)

data.table::fwrite(
  file_inventory,
  file.path(out_dir, "T18b_GSE167363_10x_file_inventory.csv")
)

data.table::fwrite(
  candidate_dirs_merged,
  file.path(out_dir, "T18b_GSE167363_10x_candidate_dirs.csv")
)

data.table::fwrite(
  sample_detection_summary,
  file.path(out_dir, "T18b_GSE167363_sample_detection_summary.csv")
)

data.table::fwrite(
  sample_file_counts,
  file.path(out_dir, "T18b_GSE167363_sample_file_counts.csv")
)

data.table::fwrite(
  prepare_summary,
  file.path(out_dir, "T18b_GSE167363_prepare_summary.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "prepare_summary")
openxlsx::writeData(wb, "prepare_summary", prepare_summary)

openxlsx::addWorksheet(wb, "sample_metadata")
openxlsx::writeData(wb, "sample_metadata", sample_metadata)

openxlsx::addWorksheet(wb, "sample_detection")
openxlsx::writeData(wb, "sample_detection", sample_detection_summary)

openxlsx::addWorksheet(wb, "candidate_10x_dirs")
openxlsx::writeData(wb, "candidate_10x_dirs", candidate_dirs_merged)

openxlsx::addWorksheet(wb, "file_inventory")
openxlsx::writeData(wb, "file_inventory", file_inventory)

openxlsx::addWorksheet(wb, "sample_file_counts")
openxlsx::writeData(wb, "sample_file_counts", sample_file_counts)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T18b_GSE167363_prepare_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_18b_download_or_prepare_GSE167363_scRNA.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 18b GSE167363 scRNA prepare 完成 ============")

message("\nPrepare summary:")
print(prepare_summary)

message("\nSample metadata:")
print(sample_metadata)

message("\nSample detection summary:")
print(sample_detection_summary)

message("\n10x candidate dirs:")
if (nrow(candidate_dirs_merged) > 0) {
  print(candidate_dirs_merged)
} else {
  message("未发现 10x MEX 候选目录。")
}

message("\nFile inventory head:")
print(head(file_inventory, 30))

message("\n关键输出：")
message("1) ", file.path(out_dir, "T18b_GSE167363_prepare_summary.xlsx"))
message("2) ", file.path(out_dir, "T18b_GSE167363_sample_metadata.csv"))
message("3) ", file.path(out_dir, "T18b_GSE167363_10x_file_inventory.csv"))
message("4) ", file.path(out_dir, "T18b_GSE167363_10x_candidate_dirs.csv"))
message("5) ", file.path(out_dir, "T18b_GSE167363_sample_detection_summary.csv"))
message("6) ", raw_extract_dir)

message("\n下一步：")
message("请把 Prepare summary、Sample detection summary、10x candidate dirs 贴给我。")
message("如果 n_complete_10x_mex_dirs > 0，就可以生成 18_prepare_scRNA_dataset.R。")
message("如果没有完整 10x MEX 目录，我会根据 file_inventory 写一个重组 10x 文件夹的脚本。")