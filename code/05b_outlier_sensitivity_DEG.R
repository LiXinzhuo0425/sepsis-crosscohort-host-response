# ============================================================
# 05b_outlier_sensitivity_DEG.R
# 排除 QC 可疑样本后的 DEG 敏感性分析
#
# 当前目标：
# 1. 读取全样本 DEG 输入矩阵和 phenotype
# 2. 排除 QC 标记样本 GSM7574026
# 3. 使用与主分析一致的 batch-aware limma 模型重跑 DEG
# 4. 比较 full DEG 与 outlier-excluded DEG 的一致性
# 5. 输出敏感性分析表格和图
#
# 输入：
# 04_results/differential_expression/
#   merged_gene_expression_common_genes_unadjusted.csv
#   merged_pheno_for_DEG.csv
#   limma_DEG_Sepsis_vs_Control.csv
#
# 04_results/tables/
#   T03_QC_possible_outlier_samples.csv
#
# 输出：
# 04_results/sensitivity/
#   limma_DEG_Sepsis_vs_Control_excluding_QC_outliers.csv
#   T04_outlier_sensitivity_summary.xlsx
#   T04_outlier_sensitivity_gene_comparison.csv
#
# 05_figures/sensitivity/
#   F06A_logFC_correlation_full_vs_excluding_outlier.png/pdf
#   F06B_adjP_correlation_full_vs_excluding_outlier.png/pdf
#   F06C_sensitivity_volcano_excluding_outlier.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(limma)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

de_dir <- file.path(project_dir, "04_results", "differential_expression")
table_dir <- file.path(project_dir, "04_results", "tables")
sens_dir <- file.path(project_dir, "04_results", "sensitivity")
fig_dir <- file.path(project_dir, "05_figures", "sensitivity")

dir.create(sens_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

expr_file <- file.path(de_dir, "merged_gene_expression_common_genes_unadjusted.csv")
pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")
deg_full_file <- file.path(de_dir, "limma_DEG_Sepsis_vs_Control.csv")
outlier_file <- file.path(table_dir, "T03_QC_possible_outlier_samples.csv")

needed <- c(expr_file, pheno_file, deg_full_file, outlier_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 工具函数
# ============================================================

read_expr_matrix <- function(file_path) {
  dt <- data.table::fread(file_path, data.table = FALSE, check.names = FALSE)
  
  gene_col <- colnames(dt)[1]
  gene_id <- as.character(dt[[gene_col]])
  dt[[gene_col]] <- NULL
  
  mat <- as.matrix(dt)
  mode(mat) <- "numeric"
  rownames(mat) <- gene_id
  
  mat
}

run_limma_deg <- function(expr_mat, pheno) {
  
  common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
  
  if (length(common_samples) < 10) {
    stop("可用于 limma 的样本数过少。")
  }
  
  expr_mat <- expr_mat[, common_samples, drop = FALSE]
  pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]
  
  if (!all(colnames(expr_mat) == pheno$sample_id)) {
    stop("表达矩阵与 phenotype 样本顺序不一致。")
  }
  
  batch <- factor(pheno$dataset)
  group <- factor(pheno$clinical_group_main, levels = c("Control", "Sepsis"))
  
  if (any(is.na(group))) {
    stop("clinical_group_main 存在 NA。")
  }
  
  design <- stats::model.matrix(~ 0 + group + batch)
  colnames(design) <- make.names(colnames(design))
  
  fit <- limma::lmFit(expr_mat, design)
  cm <- limma::makeContrasts(
    contrasts = "groupSepsis - groupControl",
    levels = design
  )
  fit2 <- limma::contrasts.fit(fit, cm)
  fit2 <- limma::eBayes(fit2)
  
  deg <- limma::topTable(fit2, coef = 1, n = Inf, sort.by = "P")
  deg$gene_symbol <- rownames(deg)
  deg <- deg[, c("gene_symbol", setdiff(colnames(deg), "gene_symbol"))]
  
  deg$significance <- ifelse(
    deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5,
    ifelse(deg$logFC > 0, "Up_in_Sepsis", "Down_in_Sepsis"),
    "Not_significant"
  )
  
  deg
}

make_deg_summary <- function(deg, label) {
  data.frame(
    analysis = label,
    n_total_genes = nrow(deg),
    n_adjP_lt_0.05 = sum(deg$adj.P.Val < 0.05, na.rm = TRUE),
    n_adjP_lt_0.05_abs_logFC_ge_0.5 = sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5, na.rm = TRUE),
    n_up_in_sepsis = sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC > 0, na.rm = TRUE),
    n_down_in_sepsis = sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC < 0, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

safe_cor <- function(x, y, method = "pearson") {
  idx <- is.finite(x) & is.finite(y)
  if (sum(idx) < 3) return(NA_real_)
  as.numeric(stats::cor(x[idx], y[idx], method = method))
}

save_scatter_logFC <- function(comp_df) {
  
  r <- safe_cor(comp_df$logFC_full, comp_df$logFC_sensitivity, method = "pearson")
  
  p <- ggplot2::ggplot(
    comp_df,
    ggplot2::aes(x = logFC_full, y = logFC_sensitivity)
  ) +
    ggplot2::geom_point(alpha = 0.45, size = 1.1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = paste0("logFC consistency after excluding QC outlier, r = ", round(r, 3)),
      x = "Full analysis logFC",
      y = "Outlier-excluded analysis logFC"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F06A_logFC_correlation_full_vs_excluding_outlier.png"),
    plot = p,
    width = 6.5,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F06A_logFC_correlation_full_vs_excluding_outlier.pdf"),
    plot = p,
    width = 6.5,
    height = 6
  )
  
  invisible(p)
}

save_scatter_adjP <- function(comp_df) {
  
  comp_df$neglog10_adjP_full <- -log10(comp_df$adj.P.Val_full + 1e-300)
  comp_df$neglog10_adjP_sensitivity <- -log10(comp_df$adj.P.Val_sensitivity + 1e-300)
  
  r <- safe_cor(comp_df$neglog10_adjP_full, comp_df$neglog10_adjP_sensitivity, method = "spearman")
  
  p <- ggplot2::ggplot(
    comp_df,
    ggplot2::aes(x = neglog10_adjP_full, y = neglog10_adjP_sensitivity)
  ) +
    ggplot2::geom_point(alpha = 0.45, size = 1.1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = paste0("Adjusted P-value rank consistency, Spearman r = ", round(r, 3)),
      x = "Full analysis -log10 adjusted P",
      y = "Outlier-excluded -log10 adjusted P"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F06B_adjP_correlation_full_vs_excluding_outlier.png"),
    plot = p,
    width = 6.5,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F06B_adjP_correlation_full_vs_excluding_outlier.pdf"),
    plot = p,
    width = 6.5,
    height = 6
  )
  
  invisible(p)
}

save_sensitivity_volcano <- function(deg_sens) {
  
  deg_sens$neg_log10_adjP <- -log10(deg_sens$adj.P.Val + 1e-300)
  
  top_label <- deg_sens[order(deg_sens$adj.P.Val, -abs(deg_sens$logFC)), , drop = FALSE]
  top_label <- head(top_label, 15)
  
  p <- ggplot2::ggplot(
    deg_sens,
    ggplot2::aes(x = logFC, y = neg_log10_adjP, shape = significance)
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 1.3) +
    ggplot2::geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_text(
      data = top_label,
      ggplot2::aes(label = gene_symbol),
      size = 2.6,
      vjust = -0.5,
      check_overlap = TRUE
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Outlier-excluded differential expression: Sepsis vs Control",
      x = "log2 fold change",
      y = "-log10 adjusted P value",
      shape = "Significance"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F06C_sensitivity_volcano_excluding_outlier.png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F06C_sensitivity_volcano_excluding_outlier.pdf"),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

# ============================================================
# 读取数据
# ============================================================

message("Reading DEG sensitivity inputs...")

expr_mat <- read_expr_matrix(expr_file)
pheno <- data.table::fread(pheno_file, data.table = FALSE)
deg_full <- data.table::fread(deg_full_file, data.table = FALSE)
outliers <- data.table::fread(outlier_file, data.table = FALSE)

required_pheno_cols <- c("sample_id", "dataset", "clinical_group_main")
if (!all(required_pheno_cols %in% colnames(pheno))) {
  stop("merged_pheno_for_DEG.csv 缺少 sample_id / dataset / clinical_group_main。")
}

if (!"gene_symbol" %in% colnames(deg_full)) {
  stop("full DEG 表缺少 gene_symbol 列。")
}

if (!"sample_id" %in% colnames(outliers)) {
  stop("outlier 表缺少 sample_id 列。")
}

# ============================================================
# 确定排除样本
# ============================================================

outlier_samples <- unique(as.character(outliers$sample_id))
outlier_samples <- outlier_samples[!is.na(outlier_samples) & outlier_samples != ""]

# 保险锁定：当前 QC 已识别 GSM7574026
if (!"GSM7574026" %in% outlier_samples) {
  message("提示：outlier 表中未包含 GSM7574026。当前不会强行加入，按 outlier 表执行。")
}

message("Outlier samples to exclude: ", paste(outlier_samples, collapse = ", "))

pheno_sens <- pheno[!pheno$sample_id %in% outlier_samples, , drop = FALSE]
expr_sens <- expr_mat[, colnames(expr_mat) %in% pheno_sens$sample_id, drop = FALSE]
pheno_sens <- pheno_sens[match(colnames(expr_sens), pheno_sens$sample_id), , drop = FALSE]

if (!all(colnames(expr_sens) == pheno_sens$sample_id)) {
  stop("排除离群样本后，表达矩阵与 phenotype 没有对齐。")
}

# ============================================================
# 重跑 DEG
# ============================================================

message("Running outlier-excluded limma DEG...")

deg_sens <- run_limma_deg(expr_sens, pheno_sens)

data.table::fwrite(
  deg_sens,
  file.path(sens_dir, "limma_DEG_Sepsis_vs_Control_excluding_QC_outliers.csv")
)

# ============================================================
# 比较 full vs sensitivity
# ============================================================

message("Comparing full and outlier-excluded DEG...")

deg_full_small <- deg_full[, c("gene_symbol", "logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B", "significance"), drop = FALSE]
deg_sens_small <- deg_sens[, c("gene_symbol", "logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B", "significance"), drop = FALSE]

colnames(deg_full_small)[-1] <- paste0(colnames(deg_full_small)[-1], "_full")
colnames(deg_sens_small)[-1] <- paste0(colnames(deg_sens_small)[-1], "_sensitivity")

comp_df <- merge(
  deg_full_small,
  deg_sens_small,
  by = "gene_symbol",
  all = FALSE,
  sort = FALSE
)

comp_df$direction_full <- ifelse(comp_df$logFC_full > 0, "Up", "Down")
comp_df$direction_sensitivity <- ifelse(comp_df$logFC_sensitivity > 0, "Up", "Down")
comp_df$direction_consistent <- comp_df$direction_full == comp_df$direction_sensitivity

comp_df$full_DEG_0.05_0.5 <- comp_df$adj.P.Val_full < 0.05 & abs(comp_df$logFC_full) >= 0.5
comp_df$sens_DEG_0.05_0.5 <- comp_df$adj.P.Val_sensitivity < 0.05 & abs(comp_df$logFC_sensitivity) >= 0.5

data.table::fwrite(
  comp_df,
  file.path(sens_dir, "T04_outlier_sensitivity_gene_comparison.csv")
)

# ============================================================
# 统计一致性
# ============================================================

full_sig_genes <- comp_df$gene_symbol[comp_df$full_DEG_0.05_0.5]
sens_sig_genes <- comp_df$gene_symbol[comp_df$sens_DEG_0.05_0.5]

top100_full <- head(deg_full$gene_symbol[order(deg_full$adj.P.Val, -abs(deg_full$logFC))], 100)
top100_sens <- head(deg_sens$gene_symbol[order(deg_sens$adj.P.Val, -abs(deg_sens$logFC))], 100)

summary_df <- rbind(
  make_deg_summary(deg_full, "Full analysis"),
  make_deg_summary(deg_sens, "Excluding QC outlier")
)

consistency_df <- data.frame(
  metric = c(
    "n_outlier_samples_excluded",
    "excluded_samples",
    "n_genes_compared",
    "pearson_logFC_correlation",
    "spearman_logFC_correlation",
    "spearman_adjP_rank_correlation",
    "direction_consistency_all_genes",
    "direction_consistency_full_DEG_genes",
    "n_full_DEG_0.05_0.5",
    "n_sensitivity_DEG_0.05_0.5",
    "n_overlap_DEG_0.05_0.5",
    "overlap_fraction_of_full_DEG",
    "overlap_fraction_of_sensitivity_DEG",
    "n_top100_overlap",
    "top100_overlap_fraction"
  ),
  value = c(
    length(outlier_samples),
    paste(outlier_samples, collapse = ";"),
    nrow(comp_df),
    safe_cor(comp_df$logFC_full, comp_df$logFC_sensitivity, "pearson"),
    safe_cor(comp_df$logFC_full, comp_df$logFC_sensitivity, "spearman"),
    safe_cor(-log10(comp_df$adj.P.Val_full + 1e-300), -log10(comp_df$adj.P.Val_sensitivity + 1e-300), "spearman"),
    mean(comp_df$direction_consistent, na.rm = TRUE),
    mean(comp_df$direction_consistent[comp_df$full_DEG_0.05_0.5], na.rm = TRUE),
    length(full_sig_genes),
    length(sens_sig_genes),
    length(intersect(full_sig_genes, sens_sig_genes)),
    length(intersect(full_sig_genes, sens_sig_genes)) / max(length(full_sig_genes), 1),
    length(intersect(full_sig_genes, sens_sig_genes)) / max(length(sens_sig_genes), 1),
    length(intersect(top100_full, top100_sens)),
    length(intersect(top100_full, top100_sens)) / 100
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 图
# ============================================================

message("Saving sensitivity plots...")

save_scatter_logFC(comp_df)
save_scatter_adjP(comp_df)
save_sensitivity_volcano(deg_sens)

# ============================================================
# Excel 汇总
# ============================================================

message("Writing sensitivity workbook...")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary_DEG_counts")
openxlsx::writeData(wb, "summary_DEG_counts", summary_df)

openxlsx::addWorksheet(wb, "consistency_metrics")
openxlsx::writeData(wb, "consistency_metrics", consistency_df)

openxlsx::addWorksheet(wb, "excluded_samples")
excluded_df <- unique(outliers[outliers$sample_id %in% outlier_samples, , drop = FALSE])
openxlsx::writeData(wb, "excluded_samples", excluded_df)

openxlsx::addWorksheet(wb, "top100_full")
openxlsx::writeData(wb, "top100_full", deg_full[deg_full$gene_symbol %in% top100_full, , drop = FALSE])

openxlsx::addWorksheet(wb, "top100_sensitivity")
openxlsx::writeData(wb, "top100_sensitivity", deg_sens[deg_sens$gene_symbol %in% top100_sens, , drop = FALSE])

openxlsx::addWorksheet(wb, "gene_comparison")
openxlsx::writeData(wb, "gene_comparison", comp_df)

openxlsx::saveWorkbook(
  wb,
  file.path(sens_dir, "T04_outlier_sensitivity_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 05b Outlier sensitivity DEG 完成 ============")
message("输出目录：", sens_dir)
message("图输出目录：", fig_dir)

message("\n排除样本：")
print(outlier_samples)

message("\nDEG 数量对比：")
print(summary_df)

message("\n一致性指标：")
print(consistency_df)

message("\n关键输出：")
message("1) ", file.path(sens_dir, "limma_DEG_Sepsis_vs_Control_excluding_QC_outliers.csv"))
message("2) ", file.path(sens_dir, "T04_outlier_sensitivity_summary.xlsx"))
message("3) ", file.path(sens_dir, "T04_outlier_sensitivity_gene_comparison.csv"))
message("4) ", fig_dir)