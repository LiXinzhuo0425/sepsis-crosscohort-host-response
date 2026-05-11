# ============================================================
# 05_DEG_QC_and_visualization.R
# DEG 后质量控制与可视化
#
# 输入：
# 04_results/differential_expression/
#   T02_annotation_qc_by_dataset.csv
#   merged_gene_expression_common_genes_unadjusted.csv
#   merged_gene_expression_common_genes_combat_for_visualization.csv
#   merged_pheno_for_DEG.csv
#   limma_DEG_Sepsis_vs_Control.csv
#
# 输出：
# 05_figures/qc_bulk/
#   PCA 图
#   密度图
#   样本表达分布图
#   DEG 火山图增强版
#   top DEG heatmap
#
# 04_results/tables/
#   T03_QC_sample_expression_summary.csv
#   T03_QC_PCA_coordinates.csv
#   T03_QC_possible_outlier_samples.csv
#   T03_QC_DEG_summary.xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

de_dir <- file.path(project_dir, "04_results", "differential_expression")
table_dir <- file.path(project_dir, "04_results", "tables")
fig_dir <- file.path(project_dir, "05_figures", "qc_bulk")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 文件路径
# ============================================================

annotation_qc_file <- file.path(de_dir, "T02_annotation_qc_by_dataset.csv")
expr_raw_file <- file.path(de_dir, "merged_gene_expression_common_genes_unadjusted.csv")
expr_combat_file <- file.path(de_dir, "merged_gene_expression_common_genes_combat_for_visualization.csv")
pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")
deg_file <- file.path(de_dir, "limma_DEG_Sepsis_vs_Control.csv")

needed_files <- c(
  annotation_qc_file,
  expr_raw_file,
  expr_combat_file,
  pheno_file,
  deg_file
)

missing_files <- needed_files[!file.exists(needed_files)]

if (length(missing_files) > 0) {
  stop("缺少必要输入文件：\n", paste(missing_files, collapse = "\n"))
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

safe_scale_rows <- function(mat) {
  mat <- as.matrix(mat)
  row_mean <- rowMeans(mat, na.rm = TRUE)
  row_sd <- apply(mat, 1, sd, na.rm = TRUE)
  row_sd[is.na(row_sd) | row_sd == 0] <- 1
  sweep(sweep(mat, 1, row_mean, "-"), 1, row_sd, "/")
}

make_pca <- function(expr_mat, pheno, label, top_n_genes = 5000) {
  
  common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
  
  if (length(common_samples) < 3) {
    stop(label, " 可用于 PCA 的样本数过少。")
  }
  
  expr_mat <- expr_mat[, common_samples, drop = FALSE]
  pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]
  
  gene_ok <- rowSums(is.na(expr_mat)) == 0
  expr_mat <- expr_mat[gene_ok, , drop = FALSE]
  
  gene_var <- apply(expr_mat, 1, var)
  gene_var <- gene_var[is.finite(gene_var)]
  
  keep_genes <- names(sort(gene_var, decreasing = TRUE))[seq_len(min(top_n_genes, length(gene_var)))]
  pca_mat <- expr_mat[keep_genes, , drop = FALSE]
  
  pca <- stats::prcomp(t(pca_mat), center = TRUE, scale. = FALSE)
  
  var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
  
  out <- data.frame(
    sample_id = rownames(pca$x),
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    PC3 = ifelse(ncol(pca$x) >= 3, pca$x[, 3], NA_real_),
    PC1_percent = round(var_exp[1] * 100, 2),
    PC2_percent = round(var_exp[2] * 100, 2),
    matrix_type = label,
    stringsAsFactors = FALSE
  )
  
  out <- merge(out, pheno, by = "sample_id", all.x = TRUE, sort = FALSE)
  
  out
}

save_pca_plot <- function(pca_df, color_var, file_prefix, title_text) {
  
  pc1_lab <- paste0("PC1 (", unique(pca_df$PC1_percent)[1], "%)")
  pc2_lab <- paste0("PC2 (", unique(pca_df$PC2_percent)[1], "%)")
  
  p <- ggplot2::ggplot(
    pca_df,
    ggplot2::aes(
      x = PC1,
      y = PC2,
      color = .data[[color_var]]
    )
  ) +
    ggplot2::geom_point(size = 2.2, alpha = 0.85) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = title_text,
      x = pc1_lab,
      y = pc2_lab,
      color = color_var
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".png")),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".pdf")),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

make_density_data <- function(expr_mat, pheno, label, max_genes = 2500) {
  
  set.seed(20260507)
  
  common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
  expr_mat <- expr_mat[, common_samples, drop = FALSE]
  pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]
  
  gene_pool <- rownames(expr_mat)
  
  if (length(gene_pool) > max_genes) {
    gene_pool <- sample(gene_pool, max_genes)
  }
  
  mat_sub <- expr_mat[gene_pool, , drop = FALSE]
  
  dt <- data.table::as.data.table(t(mat_sub), keep.rownames = "sample_id")
  long_dt <- data.table::melt(
    dt,
    id.vars = "sample_id",
    variable.name = "gene_symbol",
    value.name = "expression"
  )
  
  ph_small <- pheno[, c("sample_id", "dataset", "clinical_group_main"), drop = FALSE]
  
  long_dt <- merge(long_dt, ph_small, by = "sample_id", all.x = TRUE, sort = FALSE)
  long_dt$matrix_type <- label
  
  long_dt <- long_dt[is.finite(long_dt$expression), ]
  
  long_dt
}

save_density_plot <- function(density_dt, group_var, file_prefix, title_text) {
  
  p <- ggplot2::ggplot(
    density_dt,
    ggplot2::aes(
      x = expression,
      group = .data[[group_var]],
      color = .data[[group_var]]
    )
  ) +
    ggplot2::geom_density(alpha = 0.7, linewidth = 0.8) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = title_text,
      x = "Expression value",
      y = "Density",
      color = group_var
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".png")),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".pdf")),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

sample_expression_summary <- function(expr_mat, pheno, label) {
  
  common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
  expr_mat <- expr_mat[, common_samples, drop = FALSE]
  pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]
  
  out <- data.frame(
    sample_id = colnames(expr_mat),
    matrix_type = label,
    expr_mean = colMeans(expr_mat, na.rm = TRUE),
    expr_median = apply(expr_mat, 2, median, na.rm = TRUE),
    expr_sd = apply(expr_mat, 2, sd, na.rm = TRUE),
    expr_iqr = apply(expr_mat, 2, IQR, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  merge(out, pheno, by = "sample_id", all.x = TRUE, sort = FALSE)
}

empty_outlier_df <- function() {
  data.frame(
    sample_id = character(),
    matrix_type = character(),
    dataset = character(),
    clinical_group_main = character(),
    PC1 = numeric(),
    PC2 = numeric(),
    PC_distance = numeric(),
    expr_median = numeric(),
    expr_iqr = numeric(),
    median_z = numeric(),
    iqr_z = numeric(),
    outlier_reason = character(),
    stringsAsFactors = FALSE
  )
}

detect_possible_outliers <- function(pca_df, expr_summary_df) {
  
  out_all <- empty_outlier_df()
  
  if (nrow(pca_df) > 0) {
    pc1_z <- as.numeric(scale(pca_df$PC1))
    pc2_z <- as.numeric(scale(pca_df$PC2))
    pca_df$PC_distance <- sqrt(pc1_z^2 + pc2_z^2)
    
    pca_distance_cutoff <- as.numeric(
      stats::quantile(pca_df$PC_distance, 0.75, na.rm = TRUE) +
        3 * stats::IQR(pca_df$PC_distance, na.rm = TRUE)
    )
    
    pca_out <- pca_df[
      is.finite(pca_df$PC_distance) & pca_df$PC_distance > pca_distance_cutoff,
      c("sample_id", "matrix_type", "dataset", "clinical_group_main", "PC1", "PC2", "PC_distance"),
      drop = FALSE
    ]
    
    if (nrow(pca_out) > 0) {
      pca_out$expr_median <- NA_real_
      pca_out$expr_iqr <- NA_real_
      pca_out$median_z <- NA_real_
      pca_out$iqr_z <- NA_real_
      pca_out$outlier_reason <- "Extreme PCA distance"
      out_all <- rbind(out_all, pca_out[, colnames(out_all), drop = FALSE])
    }
  }
  
  if (nrow(expr_summary_df) > 0) {
    expr_summary_df$median_z <- as.numeric(scale(expr_summary_df$expr_median))
    expr_summary_df$iqr_z <- as.numeric(scale(expr_summary_df$expr_iqr))
    
    expr_out <- expr_summary_df[
      abs(expr_summary_df$median_z) > 4 | abs(expr_summary_df$iqr_z) > 4,
      c("sample_id", "matrix_type", "dataset", "clinical_group_main", "expr_median", "expr_iqr", "median_z", "iqr_z"),
      drop = FALSE
    ]
    
    if (nrow(expr_out) > 0) {
      expr_out$PC1 <- NA_real_
      expr_out$PC2 <- NA_real_
      expr_out$PC_distance <- NA_real_
      expr_out$outlier_reason <- "Extreme expression distribution"
      expr_out <- expr_out[, colnames(out_all), drop = FALSE]
      out_all <- rbind(out_all, expr_out)
    }
  }
  
  out_all <- unique(out_all)
  rownames(out_all) <- NULL
  out_all
}

save_sample_distribution_plot <- function(expr_summary_df, value_col, file_prefix, title_text) {
  
  p <- ggplot2::ggplot(
    expr_summary_df,
    ggplot2::aes(
      x = dataset,
      y = .data[[value_col]],
      fill = clinical_group_main
    )
  ) +
    ggplot2::geom_boxplot(outlier.size = 0.6, alpha = 0.75) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = title_text,
      x = "Dataset",
      y = value_col,
      fill = "Group"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".pdf")),
    plot = p,
    width = 8,
    height = 6
  )
  
  invisible(p)
}

save_annotation_qc_plots <- function(annotation_qc) {
  
  p1 <- ggplot2::ggplot(
    annotation_qc,
    ggplot2::aes(x = dataset, y = n_genes_after_collapse)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Gene-level annotation yield by dataset",
      x = "Dataset",
      y = "Genes after probe-to-gene collapse"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F05A_annotation_gene_yield_by_dataset.png"),
    plot = p1,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F05A_annotation_gene_yield_by_dataset.pdf"),
    plot = p1,
    width = 7,
    height = 5
  )
  
  p2 <- ggplot2::ggplot(
    annotation_qc,
    ggplot2::aes(x = dataset, y = n_samples)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Included sample count by dataset",
      x = "Dataset",
      y = "Samples"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F05B_included_samples_by_dataset.png"),
    plot = p2,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F05B_included_samples_by_dataset.pdf"),
    plot = p2,
    width = 7,
    height = 5
  )
  
  invisible(list(p1 = p1, p2 = p2))
}

save_enhanced_volcano <- function(deg) {
  
  if (!"significance" %in% colnames(deg)) {
    deg$significance <- ifelse(
      deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5,
      ifelse(deg$logFC > 0, "Up_in_Sepsis", "Down_in_Sepsis"),
      "Not_significant"
    )
  }
  
  deg$neg_log10_adjP <- -log10(deg$adj.P.Val + 1e-300)
  
  top_label <- deg[order(deg$adj.P.Val, -abs(deg$logFC)), , drop = FALSE]
  top_label <- head(top_label, 15)
  
  p <- ggplot2::ggplot(
    deg,
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
      title = "Differential expression: Sepsis vs Control",
      x = "log2 fold change",
      y = "-log10 adjusted P value",
      shape = "Significance"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F05C_enhanced_volcano_Sepsis_vs_Control.png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F05C_enhanced_volcano_Sepsis_vs_Control.pdf"),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

save_top_deg_heatmap <- function(expr_combat, pheno, deg, top_n = 50) {
  
  top_deg <- deg[
    is.finite(deg$adj.P.Val) & !is.na(deg$gene_symbol),
    ,
    drop = FALSE
  ]
  
  top_deg <- top_deg[order(top_deg$adj.P.Val, -abs(top_deg$logFC)), , drop = FALSE]
  top_genes <- unique(top_deg$gene_symbol)
  top_genes <- top_genes[top_genes %in% rownames(expr_combat)]
  top_genes <- head(top_genes, top_n)
  
  if (length(top_genes) < 5) {
    warning("top DEG 数量过少，跳过 heatmap。")
    return(NULL)
  }
  
  mat <- expr_combat[top_genes, pheno$sample_id, drop = FALSE]
  mat_z <- safe_scale_rows(mat)
  
  ann_col <- data.frame(
    dataset = pheno$dataset,
    group = pheno$clinical_group_main,
    row.names = pheno$sample_id,
    stringsAsFactors = FALSE
  )
  
  if (requireNamespace("pheatmap", quietly = TRUE)) {
    
    pheatmap::pheatmap(
      mat_z,
      annotation_col = ann_col,
      show_colnames = FALSE,
      show_rownames = TRUE,
      fontsize_row = 7,
      filename = file.path(fig_dir, "F05D_top50_DEG_heatmap.png"),
      width = 9,
      height = 8
    )
    
    grDevices::pdf(file.path(fig_dir, "F05D_top50_DEG_heatmap.pdf"), width = 9, height = 8)
    pheatmap::pheatmap(
      mat_z,
      annotation_col = ann_col,
      show_colnames = FALSE,
      show_rownames = TRUE,
      fontsize_row = 7
    )
    grDevices::dev.off()
    
  } else {
    
    grDevices::png(file.path(fig_dir, "F05D_top50_DEG_heatmap.png"), width = 2400, height = 1800, res = 300)
    stats::heatmap(
      mat_z,
      scale = "none",
      Colv = NA,
      labCol = FALSE,
      margins = c(4, 8),
      main = "Top 50 DEGs"
    )
    grDevices::dev.off()
    
    grDevices::pdf(file.path(fig_dir, "F05D_top50_DEG_heatmap.pdf"), width = 9, height = 8)
    stats::heatmap(
      mat_z,
      scale = "none",
      Colv = NA,
      labCol = FALSE,
      margins = c(4, 8),
      main = "Top 50 DEGs"
    )
    grDevices::dev.off()
  }
  
  data.frame(
    rank = seq_along(top_genes),
    gene_symbol = top_genes,
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 读取数据
# ============================================================

message("Reading input files...")

annotation_qc <- data.table::fread(annotation_qc_file, data.table = FALSE)
expr_raw <- read_expr_matrix(expr_raw_file)
expr_combat <- read_expr_matrix(expr_combat_file)
pheno <- data.table::fread(pheno_file, data.table = FALSE)
deg <- data.table::fread(deg_file, data.table = FALSE)

required_pheno_cols <- c("sample_id", "dataset", "clinical_group_main")
if (!all(required_pheno_cols %in% colnames(pheno))) {
  stop("merged_pheno_for_DEG.csv 缺少 sample_id / dataset / clinical_group_main。")
}

if (!"gene_symbol" %in% colnames(deg)) {
  stop("limma_DEG_Sepsis_vs_Control.csv 缺少 gene_symbol 列。")
}

# 对齐样本顺序
common_samples <- Reduce(
  intersect,
  list(colnames(expr_raw), colnames(expr_combat), pheno$sample_id)
)

if (length(common_samples) < 10) {
  stop("表达矩阵和 phenotype 可共同匹配的样本过少。")
}

expr_raw <- expr_raw[, common_samples, drop = FALSE]
expr_combat <- expr_combat[, common_samples, drop = FALSE]
pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]

if (!all(colnames(expr_raw) == pheno$sample_id)) {
  stop("raw expression 与 pheno 顺序不一致。")
}

if (!all(colnames(expr_combat) == pheno$sample_id)) {
  stop("combat expression 与 pheno 顺序不一致。")
}

# ============================================================
# 1. Annotation QC
# ============================================================

message("Generating annotation QC plots...")

save_annotation_qc_plots(annotation_qc)

# ============================================================
# 2. PCA
# ============================================================

message("Running PCA...")

pca_raw <- make_pca(expr_raw, pheno, label = "Unadjusted")
pca_combat <- make_pca(expr_combat, pheno, label = "ComBat")

pca_all <- rbind(pca_raw, pca_combat)

data.table::fwrite(
  pca_all,
  file.path(table_dir, "T03_QC_PCA_coordinates.csv")
)

save_pca_plot(
  pca_raw,
  color_var = "dataset",
  file_prefix = "F05E_PCA_unadjusted_by_dataset",
  title_text = "PCA of unadjusted expression by dataset"
)

save_pca_plot(
  pca_raw,
  color_var = "clinical_group_main",
  file_prefix = "F05F_PCA_unadjusted_by_group",
  title_text = "PCA of unadjusted expression by group"
)

save_pca_plot(
  pca_combat,
  color_var = "dataset",
  file_prefix = "F05G_PCA_combat_by_dataset",
  title_text = "PCA of ComBat-adjusted expression by dataset"
)

save_pca_plot(
  pca_combat,
  color_var = "clinical_group_main",
  file_prefix = "F05H_PCA_combat_by_group",
  title_text = "PCA of ComBat-adjusted expression by group"
)

# ============================================================
# 3. Expression density and distribution
# ============================================================

message("Generating expression density plots...")

density_raw <- make_density_data(expr_raw, pheno, label = "Unadjusted", max_genes = 2500)
density_combat <- make_density_data(expr_combat, pheno, label = "ComBat", max_genes = 2500)

save_density_plot(
  density_raw,
  group_var = "dataset",
  file_prefix = "F05I_density_unadjusted_by_dataset",
  title_text = "Expression density before batch visualization adjustment"
)

save_density_plot(
  density_combat,
  group_var = "dataset",
  file_prefix = "F05J_density_combat_by_dataset",
  title_text = "Expression density after ComBat adjustment"
)

save_density_plot(
  density_combat,
  group_var = "clinical_group_main",
  file_prefix = "F05K_density_combat_by_group",
  title_text = "ComBat-adjusted expression density by group"
)

expr_summary_raw <- sample_expression_summary(expr_raw, pheno, "Unadjusted")
expr_summary_combat <- sample_expression_summary(expr_combat, pheno, "ComBat")
expr_summary_all <- rbind(expr_summary_raw, expr_summary_combat)

data.table::fwrite(
  expr_summary_all,
  file.path(table_dir, "T03_QC_sample_expression_summary.csv")
)

save_sample_distribution_plot(
  expr_summary_combat,
  value_col = "expr_median",
  file_prefix = "F05L_sample_median_expression_combat_by_dataset",
  title_text = "Sample median expression after ComBat adjustment"
)

save_sample_distribution_plot(
  expr_summary_combat,
  value_col = "expr_iqr",
  file_prefix = "F05M_sample_IQR_expression_combat_by_dataset",
  title_text = "Sample expression IQR after ComBat adjustment"
)

# ============================================================
# 4. Possible outlier report
# ============================================================

message("Detecting possible outliers...")

outliers_raw <- detect_possible_outliers(pca_raw, expr_summary_raw)
outliers_combat <- detect_possible_outliers(pca_combat, expr_summary_combat)

possible_outliers <- rbind(outliers_raw, outliers_combat)
possible_outliers <- unique(possible_outliers)

data.table::fwrite(
  possible_outliers,
  file.path(table_dir, "T03_QC_possible_outlier_samples.csv")
)

# ============================================================
# 5. DEG summaries and plots
# ============================================================

message("Generating DEG summary plots...")

save_enhanced_volcano(deg)

top_deg_heatmap_genes <- save_top_deg_heatmap(expr_combat, pheno, deg, top_n = 50)

if (!is.null(top_deg_heatmap_genes)) {
  data.table::fwrite(
    top_deg_heatmap_genes,
    file.path(table_dir, "T03_QC_top50_DEG_heatmap_genes.csv")
  )
}

deg_summary <- data.frame(
  metric = c(
    "n_total_common_genes",
    "n_total_samples",
    "n_control",
    "n_sepsis",
    "n_adjP_lt_0.05",
    "n_adjP_lt_0.05_abs_logFC_ge_0.5",
    "n_up_in_sepsis_adjP_lt_0.05_abs_logFC_ge_0.5",
    "n_down_in_sepsis_adjP_lt_0.05_abs_logFC_ge_0.5"
  ),
  value = c(
    nrow(expr_raw),
    ncol(expr_raw),
    sum(pheno$clinical_group_main == "Control", na.rm = TRUE),
    sum(pheno$clinical_group_main == "Sepsis", na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05, na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5, na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC > 0, na.rm = TRUE),
    sum(deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5 & deg$logFC < 0, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

dataset_summary <- aggregate(
  sample_id ~ dataset + clinical_group_main,
  data = pheno,
  FUN = length
)
colnames(dataset_summary)[colnames(dataset_summary) == "sample_id"] <- "n"

top_deg_table <- deg[order(deg$adj.P.Val, -abs(deg$logFC)), , drop = FALSE]
top_deg_table <- head(top_deg_table, 100)

# ============================================================
# 6. Excel 汇总
# ============================================================

message("Writing QC summary workbook...")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "DEG_summary")
openxlsx::writeData(wb, "DEG_summary", deg_summary)

openxlsx::addWorksheet(wb, "dataset_summary")
openxlsx::writeData(wb, "dataset_summary", dataset_summary)

openxlsx::addWorksheet(wb, "annotation_qc")
openxlsx::writeData(wb, "annotation_qc", annotation_qc)

openxlsx::addWorksheet(wb, "top100_DEG")
openxlsx::writeData(wb, "top100_DEG", top_deg_table)

openxlsx::addWorksheet(wb, "possible_outliers")
openxlsx::writeData(wb, "possible_outliers", possible_outliers)

openxlsx::addWorksheet(wb, "PCA_coordinates")
openxlsx::writeData(wb, "PCA_coordinates", pca_all)

openxlsx::saveWorkbook(
  wb,
  file.path(table_dir, "T03_QC_DEG_visualization_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 7. 控制台输出
# ============================================================

message("\n============ 05 DEG QC and visualization 完成 ============")
message("QC 图输出目录：", fig_dir)
message("QC 表输出目录：", table_dir)
message("\n主分析样本：")
print(dataset_summary)

message("\nDEG summary：")
print(deg_summary)

message("\nPossible outlier samples：", nrow(possible_outliers))
if (nrow(possible_outliers) > 0) {
  print(possible_outliers)
}

message("\n生成的关键文件：")
message("1) ", file.path(table_dir, "T03_QC_DEG_visualization_summary.xlsx"))
message("2) ", file.path(table_dir, "T03_QC_PCA_coordinates.csv"))
message("3) ", file.path(table_dir, "T03_QC_sample_expression_summary.csv"))
message("4) ", file.path(table_dir, "T03_QC_possible_outlier_samples.csv"))
message("5) ", fig_dir)