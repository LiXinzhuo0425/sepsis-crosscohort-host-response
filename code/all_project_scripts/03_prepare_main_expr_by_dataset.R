# ============================================================
# 03_prepare_main_expr_by_dataset.R
# 按纳入样本提取每个队列的表达矩阵和 phenotype 表
#
# 输入：
# - T01_core_included_sample_map.csv   (包含 include_main TRUE)
# - 各队列表达矩阵 RDS (02_processed_data/eset_rds) 
# - 各队列 phenotype 原始 (02_processed_data/phenotype)
#
# 输出：
# - 每个队列表达矩阵（filtered）CSV 和 phenotype CSV
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

# -------------- 输入路径 --------------
included_map_file <- file.path(
  project_dir,
  "04_results/tables/T01_core_included_sample_map.csv"
)

expr_rds_dir  <- file.path(project_dir, "02_processed_data/eset_rds")
pheno_raw_dir <- file.path(project_dir, "02_processed_data/phenotype")

# -------------- 输出路径 --------------
out_expr_dir  <- file.path(project_dir, "02_processed_data/main_expr")
out_pheno_dir <- file.path(project_dir, "02_processed_data/main_pheno")

dir.create(out_expr_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_pheno_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------
# 读取 sample map
# -------------------------------

message("读取主分析纳入样本表：", included_map_file)
included_map <- read_csv(included_map_file, show_col_types = FALSE)

if (!all(c("dataset", "sample_id", "clinical_group_main") %in% names(included_map))) {
  stop("样本映射表缺少必要列，请检查 'dataset', 'sample_id', 'clinical_group_main'")
}

datasets <- unique(included_map$dataset)
message("发现需处理的队列: ", paste(datasets, collapse = ", "))

# -------------------------------
# 逐队列提取表达矩阵
# -------------------------------

for (acc in datasets) {
  
  message("\n=== 处理 cohort: ", acc, " ===")
  
  # -------------- 载入表达矩阵 --------------
  expr_rds_file <- file.path(expr_rds_dir, paste0(acc, "_expr_matrix_numeric.rds"))
  if (!file.exists(expr_rds_file)) {
    stop("找不到表达矩阵 RDS: ", expr_rds_file)
  }
  expr_mat <- readRDS(expr_rds_file)
  
  # -------------- 载入 pheno --------------
  pheno_file <- file.path(pheno_raw_dir, paste0("P00_", acc, "_pheno_raw.csv"))
  if (!file.exists(pheno_file)) {
    stop("找不到 phenotype CSV: ", pheno_file)
  }
  ph_raw <- read_csv(pheno_file, show_col_types = FALSE)
  
  # -------------- 当前 cohort 样本 list --------------
  smap <- included_map %>% filter(dataset == acc)
  keep_samples <- smap$sample_id
  
  message("主分析样本数: ", nrow(smap))
  message("对照样本: ", sum(smap$clinical_group_main == "Control"))
  message("Sepsis 样本: ", sum(smap$clinical_group_main == "Sepsis"))
  
  # -------------------------------
  # 子集表达矩阵并输出
  # -------------------------------
  
  expr_sub <- expr_mat[, colnames(expr_mat) %in% keep_samples, drop = FALSE]
  ph_sub   <- ph_raw %>% filter(sample_id %in% keep_samples)
  
  # 检查顺序一致
  if (!all(colnames(expr_sub) == ph_sub$sample_id)) {
    # 按 pheno 顺序调整矩阵列
    expr_sub <- expr_sub[, ph_sub$sample_id, drop = FALSE]
  }
  
  # 输出文件
  expr_out_csv <- file.path(out_expr_dir, paste0(acc, "_main_expr.csv"))
  pheno_out_csv <- file.path(out_pheno_dir, paste0(acc, "_main_pheno.csv"))
  
  message("保存表达矩阵: ", expr_out_csv)
  message("保存 phenotype 表: ", pheno_out_csv)
  
  # 用 write_csv 保留列名
  write_csv(
    as.data.frame(expr_sub) %>% mutate(gene = rownames(expr_sub)),
    expr_out_csv
  )
  
  write_csv(ph_sub, pheno_out_csv)
}

message("\n主分析表达矩阵提取完成！")