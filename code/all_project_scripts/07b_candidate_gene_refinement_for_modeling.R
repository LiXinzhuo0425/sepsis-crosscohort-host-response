# ============================================================
# 07b_candidate_gene_refinement_for_modeling.R
# 候选基因进一步收缩，用于后续 LASSO/Elastic net 建模
#
# 目标：
# 1. 从 07 输出的候选基因表中选择更严格建模候选
# 2. 控制方向一致性、单基因 AUROC 和最差队列表现
# 3. 按表达相关性去冗余，避免高度同轴基因堆叠
# 4. 输出 refined candidate genes，供 08_train_diagnostic_model.R 使用
#
# 输入：
# 04_results/candidate_selection/
#   T06_candidate_gene_priority_table.csv
#   T06_candidate_gene_dataset_level_metrics.csv
#
# 04_results/differential_expression/
#   merged_gene_expression_common_genes_unadjusted.csv
#   merged_pheno_for_DEG.csv
#
# 输出：
# 04_results/candidate_selection/
#   T07_refined_modeling_candidate_genes.csv
#   T07_refined_candidate_correlation_matrix.csv
#   T07_refined_candidate_AUROC_matrix.csv
#   T07_refined_candidate_summary.xlsx
#
# 05_figures/candidate_selection/
#   F09A_refined_candidate_priority_score.png/pdf
#   F09B_refined_candidate_mean_AUROC.png/pdf
#   F09C_refined_candidate_correlation_heatmap.png/pdf
#   F09D_refined_candidate_AUROC_heatmap.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

cand_dir <- file.path(project_dir, "04_results", "candidate_selection")
de_dir <- file.path(project_dir, "04_results", "differential_expression")
fig_dir <- file.path(project_dir, "05_figures", "candidate_selection")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(cand_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

priority_file <- file.path(cand_dir, "T06_candidate_gene_priority_table.csv")
dataset_metric_file <- file.path(cand_dir, "T06_candidate_gene_dataset_level_metrics.csv")
expr_file <- file.path(de_dir, "merged_gene_expression_common_genes_unadjusted.csv")
pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")

needed <- c(priority_file, dataset_metric_file, expr_file, pheno_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 参数
# ============================================================

min_direction_consistency <- 1.00
min_mean_auc <- 0.80
min_auc_ge_070_datasets <- 5
min_min_auc <- 0.55

# 去冗余相关阈值
cor_cutoff <- 0.85

# 最终最多输出候选基因数
max_refined_genes <- 50

# 如果严格筛选后太少，启动宽松补充
min_refined_genes <- 25

# 绘图 top N
top_n_plot <- 40

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

scale_01 <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

make_auc_matrix <- function(dataset_metrics, genes) {
  dm <- dataset_metrics[dataset_metrics$gene_symbol %in% genes, , drop = FALSE]
  
  datasets <- sort(unique(dm$dataset))
  mat <- matrix(NA_real_, nrow = length(genes), ncol = length(datasets))
  rownames(mat) <- genes
  colnames(mat) <- datasets
  
  for (i in seq_len(nrow(dm))) {
    mat[dm$gene_symbol[i], dm$dataset[i]] <- dm$auc[i]
  }
  
  mat
}

greedy_correlation_prune <- function(expr_mat, candidate_table, cor_cutoff = 0.85, max_genes = 50) {
  
  genes <- candidate_table$gene_symbol
  genes <- genes[genes %in% rownames(expr_mat)]
  
  if (length(genes) <= 1) {
    return(genes)
  }
  
  expr_sub <- t(expr_mat[genes, , drop = FALSE])
  cor_mat <- stats::cor(expr_sub, use = "pairwise.complete.obs", method = "spearman")
  cor_mat[is.na(cor_mat)] <- 0
  
  selected <- character()
  
  for (g in genes) {
    if (length(selected) == 0) {
      selected <- c(selected, g)
    } else {
      max_abs_cor <- max(abs(cor_mat[g, selected]), na.rm = TRUE)
      if (!is.finite(max_abs_cor) || max_abs_cor < cor_cutoff) {
        selected <- c(selected, g)
      }
    }
    
    if (length(selected) >= max_genes) {
      break
    }
  }
  
  selected
}

save_barplot <- function(df, x_col, y_col, file_prefix, title_text, y_label) {
  
  plot_df <- df[order(df[[y_col]], decreasing = TRUE), , drop = FALSE]
  plot_df <- head(plot_df, top_n_plot)
  plot_df[[x_col]] <- factor(plot_df[[x_col]], levels = rev(plot_df[[x_col]]))
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(
      title = title_text,
      x = NULL,
      y = y_label
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".png")),
    plot = p,
    width = 8,
    height = 7,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".pdf")),
    plot = p,
    width = 8,
    height = 7
  )
  
  invisible(p)
}

save_matrix_heatmap <- function(mat, file_prefix, title_text, fill_label) {
  
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    message("跳过 heatmap：", file_prefix)
    return(NULL)
  }
  
  df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  colnames(df) <- c("row_var", "col_var", "value")
  
  df$row_var <- factor(df$row_var, levels = rev(rownames(mat)))
  df$col_var <- factor(df$col_var, levels = colnames(mat))
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = col_var, y = row_var, fill = value)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = title_text,
      x = NULL,
      y = NULL,
      fill = fill_label
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".png")),
    plot = p,
    width = 8,
    height = 8,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(file_prefix, ".pdf")),
    plot = p,
    width = 8,
    height = 8
  )
  
  invisible(p)
}

# ============================================================
# 读取数据
# ============================================================

message("Reading inputs...")

priority <- data.table::fread(priority_file, data.table = FALSE)
dataset_metrics <- data.table::fread(dataset_metric_file, data.table = FALSE)
expr_mat <- read_expr_matrix(expr_file)
pheno <- data.table::fread(pheno_file, data.table = FALSE)

required_priority_cols <- c(
  "gene_symbol",
  "passes_screening",
  "direction_consistency",
  "mean_auc",
  "min_auc",
  "n_auc_ge_070",
  "priority_score",
  "global_logFC",
  "global_adjP",
  "global_direction",
  "immune_GO_keyword_hit"
)

if (!all(required_priority_cols %in% colnames(priority))) {
  stop("priority 表缺少必要列。")
}

if (!all(c("sample_id", "dataset", "clinical_group_main") %in% colnames(pheno))) {
  stop("phenotype 表缺少 sample_id / dataset / clinical_group_main。")
}

common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
expr_mat <- expr_mat[, common_samples, drop = FALSE]
pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]

if (!all(colnames(expr_mat) == pheno$sample_id)) {
  stop("表达矩阵与 phenotype 没有对齐。")
}

priority$gene_symbol <- as.character(priority$gene_symbol)
priority$passes_screening <- as.logical(priority$passes_screening)
priority$immune_GO_keyword_hit <- as.logical(priority$immune_GO_keyword_hit)

priority <- priority[priority$gene_symbol %in% rownames(expr_mat), , drop = FALSE]

# ============================================================
# 严格筛选
# ============================================================

strict_pool <- priority[
  priority$passes_screening == TRUE &
    priority$direction_consistency >= min_direction_consistency &
    priority$mean_auc >= min_mean_auc &
    priority$n_auc_ge_070 >= min_auc_ge_070_datasets &
    priority$min_auc >= min_min_auc,
  ,
  drop = FALSE
]

strict_pool <- strict_pool[
  order(
    -strict_pool$immune_GO_keyword_hit,
    -strict_pool$priority_score,
    strict_pool$global_adjP
  ),
  ,
  drop = FALSE
]

message("Strict candidate pool: ", nrow(strict_pool))

# 如果严格筛选后太少，启动宽松补充
relaxed_pool <- priority[
  priority$passes_screening == TRUE &
    priority$direction_consistency >= 0.90 &
    priority$mean_auc >= 0.78 &
    priority$n_auc_ge_070 >= 4 &
    priority$min_auc >= 0.50,
  ,
  drop = FALSE
]

relaxed_pool <- relaxed_pool[
  order(
    -relaxed_pool$immune_GO_keyword_hit,
    -relaxed_pool$priority_score,
    relaxed_pool$global_adjP
  ),
  ,
  drop = FALSE
]

if (nrow(strict_pool) < min_refined_genes) {
  message("Strict pool < ", min_refined_genes, "，使用 relaxed pool 补充。")
  base_pool <- relaxed_pool
} else {
  base_pool <- strict_pool
}

base_pool <- base_pool[!duplicated(base_pool$gene_symbol), , drop = FALSE]

# ============================================================
# 相关性去冗余
# ============================================================

message("Running correlation pruning...")

selected_genes <- greedy_correlation_prune(
  expr_mat = expr_mat,
  candidate_table = base_pool,
  cor_cutoff = cor_cutoff,
  max_genes = max_refined_genes
)

refined_table <- base_pool[base_pool$gene_symbol %in% selected_genes, , drop = FALSE]
refined_table$selection_order <- match(refined_table$gene_symbol, selected_genes)
refined_table <- refined_table[order(refined_table$selection_order), , drop = FALSE]

# 如果去冗余后还是少于 min_refined_genes，允许从 base_pool 中继续补，但仍避免完全相同表达轴
if (nrow(refined_table) < min_refined_genes) {
  
  message("Correlation-pruned genes < ", min_refined_genes, "，尝试从 base_pool 补充到最低数量。")
  
  current <- refined_table$gene_symbol
  remaining <- setdiff(base_pool$gene_symbol, current)
  
  if (length(remaining) > 0) {
    expr_sub <- t(expr_mat[base_pool$gene_symbol, , drop = FALSE])
    cor_mat <- stats::cor(expr_sub, use = "pairwise.complete.obs", method = "spearman")
    cor_mat[is.na(cor_mat)] <- 0
    
    for (g in remaining) {
      max_abs_cor <- max(abs(cor_mat[g, current]), na.rm = TRUE)
      if (!is.finite(max_abs_cor) || max_abs_cor < 0.92) {
        current <- c(current, g)
      }
      if (length(current) >= min_refined_genes) break
    }
  }
  
  refined_table <- base_pool[base_pool$gene_symbol %in% current, , drop = FALSE]
  refined_table$selection_order <- match(refined_table$gene_symbol, current)
  refined_table <- refined_table[order(refined_table$selection_order), , drop = FALSE]
}

# ============================================================
# 相关矩阵和 AUROC 矩阵
# ============================================================

refined_genes <- refined_table$gene_symbol

expr_refined <- t(expr_mat[refined_genes, , drop = FALSE])
cor_mat <- stats::cor(expr_refined, use = "pairwise.complete.obs", method = "spearman")
cor_mat[is.na(cor_mat)] <- 0

auc_mat <- make_auc_matrix(dataset_metrics, refined_genes)

# ============================================================
# 输出表
# ============================================================

message("Writing outputs...")

data.table::fwrite(
  refined_table,
  file.path(cand_dir, "T07_refined_modeling_candidate_genes.csv")
)

data.table::fwrite(
  as.data.frame(cor_mat, check.names = FALSE),
  file.path(cand_dir, "T07_refined_candidate_correlation_matrix.csv")
)

data.table::fwrite(
  data.frame(gene_symbol = rownames(auc_mat), auc_mat, check.names = FALSE),
  file.path(cand_dir, "T07_refined_candidate_AUROC_matrix.csv")
)

selection_summary <- data.frame(
  metric = c(
    "n_priority_genes",
    "n_strict_pool",
    "n_relaxed_pool",
    "n_base_pool_used",
    "n_refined_genes",
    "cor_cutoff",
    "min_direction_consistency",
    "min_mean_auc",
    "min_auc_ge_070_datasets",
    "min_min_auc"
  ),
  value = c(
    nrow(priority),
    nrow(strict_pool),
    nrow(relaxed_pool),
    nrow(base_pool),
    nrow(refined_table),
    cor_cutoff,
    min_direction_consistency,
    min_mean_auc,
    min_auc_ge_070_datasets,
    min_min_auc
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "selection_summary")
openxlsx::writeData(wb, "selection_summary", selection_summary)

openxlsx::addWorksheet(wb, "strict_pool")
openxlsx::writeData(wb, "strict_pool", strict_pool)

openxlsx::addWorksheet(wb, "relaxed_pool")
openxlsx::writeData(wb, "relaxed_pool", relaxed_pool)

openxlsx::addWorksheet(wb, "refined_candidates")
openxlsx::writeData(wb, "refined_candidates", refined_table)

openxlsx::addWorksheet(wb, "dataset_level_metrics")
openxlsx::writeData(
  wb,
  "dataset_level_metrics",
  dataset_metrics[dataset_metrics$gene_symbol %in% refined_genes, , drop = FALSE]
)

openxlsx::addWorksheet(wb, "AUROC_matrix")
openxlsx::writeData(
  wb,
  "AUROC_matrix",
  data.frame(gene_symbol = rownames(auc_mat), auc_mat, check.names = FALSE)
)

openxlsx::addWorksheet(wb, "correlation_matrix")
openxlsx::writeData(
  wb,
  "correlation_matrix",
  data.frame(gene_symbol = rownames(cor_mat), cor_mat, check.names = FALSE)
)

openxlsx::saveWorkbook(
  wb,
  file.path(cand_dir, "T07_refined_candidate_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 绘图
# ============================================================

message("Saving figures...")

save_barplot(
  refined_table,
  x_col = "gene_symbol",
  y_col = "priority_score",
  file_prefix = "F09A_refined_candidate_priority_score",
  title_text = "Refined candidate genes ranked by priority score",
  y_label = "Priority score"
)

save_barplot(
  refined_table,
  x_col = "gene_symbol",
  y_col = "mean_auc",
  file_prefix = "F09B_refined_candidate_mean_AUROC",
  title_text = "Refined candidate genes ranked by mean single-gene AUROC",
  y_label = "Mean AUROC across datasets"
)

save_matrix_heatmap(
  cor_mat,
  file_prefix = "F09C_refined_candidate_correlation_heatmap",
  title_text = "Spearman correlation among refined candidate genes",
  fill_label = "Spearman r"
)

save_matrix_heatmap(
  auc_mat,
  file_prefix = "F09D_refined_candidate_AUROC_heatmap",
  title_text = "Dataset-level single-gene AUROC of refined candidates",
  fill_label = "AUROC"
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_07b_candidate_gene_refinement_for_modeling.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 07b Candidate gene refinement 完成 ============")
message("输出目录：", cand_dir)
message("图输出目录：", fig_dir)

message("\nSelection summary:")
print(selection_summary)

message("\nRefined candidate genes:")
print(
  refined_table[
    ,
    c(
      "selection_order",
      "gene_symbol",
      "global_logFC",
      "global_adjP",
      "global_direction",
      "direction_consistency",
      "mean_auc",
      "min_auc",
      "n_auc_ge_070",
      "immune_GO_keyword_hit",
      "priority_score"
    ),
    drop = FALSE
  ]
)

message("\n关键输出：")
message("1) ", file.path(cand_dir, "T07_refined_modeling_candidate_genes.csv"))
message("2) ", file.path(cand_dir, "T07_refined_candidate_correlation_matrix.csv"))
message("3) ", file.path(cand_dir, "T07_refined_candidate_AUROC_matrix.csv"))
message("4) ", file.path(cand_dir, "T07_refined_candidate_summary.xlsx"))
message("5) ", fig_dir)