# ============================================================
# 01b_parse_local_series_matrix_core.R
# 本地解析重点 GEO series_matrix.txt.gz
#
# 队列：
# GSE65682, GSE54514, GSE95233, GSE57065, GSE137340, GSE236713
#
# 目的：
# 1. 不联网；
# 2. 不使用 T01 数据集级筛选表；
# 3. 每个样本一行导出真实 phenotype；
# 4. 暂不合并、暂不 Combat、暂不差异分析；
# 5. 仅生成 ExpressionSet RDS、数值表达矩阵 RDS、phenotype 原始表和解析概览。
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(GEOquery)
  library(Biobase)
  library(openxlsx)
  library(dplyr)
  library(stringr)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

geo_dir   <- file.path(project_dir, "01_raw_data", "geo_series_matrix")
pheno_dir <- file.path(project_dir, "02_processed_data", "phenotype")
rds_dir   <- file.path(project_dir, "02_processed_data", "eset_rds")
table_dir <- file.path(project_dir, "04_results", "tables")

dir.create(pheno_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rds_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

core_gse <- c(
  "GSE65682",
  "GSE54514",
  "GSE95233",
  "GSE57065",
  "GSE137340",
  "GSE236713"
)

# ---------- 安全提取表达矩阵 ----------
get_expr_matrix <- function(eset, acc = "unknown") {
  
  mat <- tryCatch(
    Biobase::exprs(eset),
    error = function(e) NULL
  )
  
  if (is.null(mat) || length(mat) == 0) {
    mat <- tryCatch(
      Biobase::assayData(eset)$exprs,
      error = function(e) NULL
    )
  }
  
  if (is.null(mat) || length(mat) == 0) {
    stop(acc, "：无法从 ExpressionSet 中提取表达矩阵。")
  }
  
  if (is.list(mat) && !is.data.frame(mat)) {
    message(acc, "：表达矩阵为 list，尝试转换为普通矩阵。")
    mat <- do.call(cbind, mat)
  }
  
  mat <- as.matrix(mat)
  
  suppressWarnings({
    mat_numeric <- matrix(
      as.numeric(mat),
      nrow = nrow(mat),
      ncol = ncol(mat),
      dimnames = dimnames(mat)
    )
  })
  
  na_ratio <- sum(is.na(mat_numeric)) / length(mat_numeric)
  
  if (na_ratio > 0.5) {
    stop(
      acc,
      "：表达矩阵转数值后 NA 比例超过 50%。",
      "请检查该 series_matrix 是否为标准表达矩阵。"
    )
  }
  
  return(mat_numeric)
}

# ---------- 安全折叠文本 ----------
safe_collapse <- function(x, max_nchar = 30000) {
  y <- paste(unique(as.character(x)), collapse = " | ")
  y <- stringr::str_replace_all(y, "[\r\n\t]", " ")
  if (nchar(y) > max_nchar) {
    y <- paste0(substr(y, 1, max_nchar), " ...[truncated]")
  }
  y
}

# ---------- 安全数值摘要 ----------
safe_num_summary <- function(mat) {
  x <- as.numeric(mat)
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(list(
      expr_min = NA_real_,
      expr_median = NA_real_,
      expr_max = NA_real_
    ))
  }
  
  return(list(
    expr_min = min(x, na.rm = TRUE),
    expr_median = median(x, na.rm = TRUE),
    expr_max = max(x, na.rm = TRUE)
  ))
}

overview_list <- list()
keyword_list  <- list()

for (acc in core_gse) {
  
  message("\n==============================")
  message("正在解析：", acc)
  message("==============================")
  
  gz_file <- file.path(geo_dir, paste0(acc, "_series_matrix.txt.gz"))
  
  if (!file.exists(gz_file)) {
    stop(
      paste0(
        "缺少文件：\n", gz_file,
        "\n\n请确认文件名是否严格为：", acc, "_series_matrix.txt.gz",
        "\n并且已放入：", geo_dir
      )
    )
  }
  
  gobj <- GEOquery::getGEO(filename = gz_file, GSEMatrix = TRUE)
  
  if (is.list(gobj)) {
    message(acc, "：读取后为 list，默认使用第 1 个 ExpressionSet。")
    gobj <- gobj[[1]]
  }
  
  if (!inherits(gobj, "ExpressionSet")) {
    stop(acc, "：未能解析为 ExpressionSet。")
  }
  
  mat <- get_expr_matrix(gobj, acc = acc)
  ph  <- Biobase::pData(gobj)
  
  ph$sample_id <- rownames(ph)
  ph$dataset   <- acc
  ph$platform  <- Biobase::annotation(gobj)
  
  saveRDS(gobj, file.path(rds_dir, paste0(acc, "_series_matrix_eset.rds")))
  saveRDS(mat,  file.path(rds_dir, paste0(acc, "_expr_matrix_numeric.rds")))
  
  pheno_file <- file.path(pheno_dir, paste0("P00_", acc, "_pheno_raw.csv"))
  write.csv(ph, pheno_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  ph_text <- apply(ph, 1, function(x) {
    paste(as.character(x), collapse = " | ")
  })
  
  ph_text_lower <- stringr::str_to_lower(ph_text)
  
  keyword_df <- data.frame(
    dataset = acc,
    sample_id = ph$sample_id,
    hit_control   = stringr::str_detect(ph_text_lower, "control|healthy|volunteer|donor"),
    hit_sepsis    = stringr::str_detect(ph_text_lower, "sepsis|septic"),
    hit_shock     = stringr::str_detect(ph_text_lower, "shock"),
    hit_sirs      = stringr::str_detect(ph_text_lower, "sirs|systemic inflammatory"),
    hit_survival  = stringr::str_detect(ph_text_lower, "survivor|non-survivor|nonsurvivor|survival|mortality|death|dead|alive"),
    hit_timepoint = stringr::str_detect(ph_text_lower, "day|d0|d1|d2|d3|d4|d5|admission|baseline|time|onset|30 min|30min|24 h|48 h"),
    stringsAsFactors = FALSE
  )
  
  keyword_list[[acc]] <- keyword_df
  
  candidate_cols <- names(ph)[sapply(ph, function(x) {
    y <- stringr::str_to_lower(safe_collapse(x, max_nchar = 20000))
    stringr::str_detect(
      y,
      "control|healthy|volunteer|donor|sepsis|septic|shock|sirs|survivor|non-survivor|nonsurvivor|survival|mortality|death|dead|alive|day|admission|baseline|onset|30 min|30min|24 h|48 h"
    )
  })]
  
  s <- safe_num_summary(mat)
  
  overview_list[[acc]] <- data.frame(
    dataset = acc,
    n_samples = ncol(mat),
    n_features = nrow(mat),
    platform = Biobase::annotation(gobj),
    expr_min = s$expr_min,
    expr_median = s$expr_median,
    expr_max = s$expr_max,
    n_pheno_columns = ncol(ph),
    candidate_group_columns = paste(candidate_cols, collapse = "; "),
    keyword_control_healthy_n = sum(keyword_df$hit_control, na.rm = TRUE),
    keyword_sepsis_septic_n = sum(keyword_df$hit_sepsis, na.rm = TRUE),
    keyword_shock_n = sum(keyword_df$hit_shock, na.rm = TRUE),
    keyword_sirs_n = sum(keyword_df$hit_sirs, na.rm = TRUE),
    keyword_survival_n = sum(keyword_df$hit_survival, na.rm = TRUE),
    keyword_timepoint_n = sum(keyword_df$hit_timepoint, na.rm = TRUE),
    pheno_raw_file = pheno_file,
    eset_rds_file = file.path(rds_dir, paste0(acc, "_series_matrix_eset.rds")),
    expr_matrix_rds_file = file.path(rds_dir, paste0(acc, "_expr_matrix_numeric.rds")),
    stringsAsFactors = FALSE
  )
  
  message("完成：", acc)
  message("样本数：", ncol(mat))
  message("探针/特征数：", nrow(mat))
  message("平台：", Biobase::annotation(gobj))
  message("表达矩阵数值范围：", round(s$expr_min, 3), " / ", round(s$expr_median, 3), " / ", round(s$expr_max, 3))
}

overview_df <- dplyr::bind_rows(overview_list)
keyword_df_all <- dplyr::bind_rows(keyword_list)

out_xlsx <- file.path(table_dir, "T00_core_series_matrix_parse_overview.xlsx")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overview")
openxlsx::writeData(wb, "overview", overview_df)

openxlsx::addWorksheet(wb, "keyword_hits_by_sample")
openxlsx::writeData(wb, "keyword_hits_by_sample", keyword_df_all)

for (acc in core_gse) {
  
  pheno_file <- file.path(pheno_dir, paste0("P00_", acc, "_pheno_raw.csv"))
  ph <- read.csv(pheno_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  sheet_name <- paste0(acc, "_pheno_cols")
  sheet_name <- substr(sheet_name, 1, 31)
  
  openxlsx::addWorksheet(wb, sheet_name)
  
  col_summary <- data.frame(
    column_name = names(ph),
    example_values = sapply(ph, safe_collapse, max_nchar = 1000),
    stringsAsFactors = FALSE
  )
  
  openxlsx::writeData(wb, sheet_name, col_summary)
}

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("\n============================================================")
message("本地 series matrix 解析完成。")
message("输出文件：")
message("1) ", out_xlsx)
message("2) phenotype: ", pheno_dir)
message("3) ExpressionSet RDS / numeric matrix RDS: ", rds_dir)
message("============================================================")

message("\n解析概览：")
print(overview_df %>% dplyr::select(dataset, n_samples, n_features, platform), n = Inf)