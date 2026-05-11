# ============================================================
# 07_candidate_gene_selection_preparation.R
# 候选 signature 基因筛选准备
#
# 目标：
# 1. 从稳定 DEG 中建立候选池
# 2. 计算每个候选基因在每个 dataset 内的方向一致性
# 3. 计算每个候选基因在每个 dataset 内的单基因 AUROC
# 4. 汇总候选基因优先级
#
# 输入：
# 04_results/differential_expression/
#   limma_DEG_Sepsis_vs_Control.csv
#   merged_gene_expression_common_genes_unadjusted.csv
#   merged_pheno_for_DEG.csv
#
# 04_results/enrichment/
#   T05_GO_BP_enrichment.xlsx
#
# 输出：
# 04_results/candidate_selection/
#   T06_candidate_gene_screening_table.xlsx
#   T06_candidate_gene_dataset_level_metrics.csv
#   T06_candidate_gene_priority_table.csv
#
# 05_figures/candidate_selection/
#   F08A_candidate_gene_direction_consistency.png/pdf
#   F08B_candidate_gene_mean_AUROC.png/pdf
#   F08C_top_candidate_gene_AUROC_heatmap.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
  library(pROC)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

de_dir <- file.path(project_dir, "04_results", "differential_expression")
enrich_dir <- file.path(project_dir, "04_results", "enrichment")
out_dir <- file.path(project_dir, "04_results", "candidate_selection")
fig_dir <- file.path(project_dir, "05_figures", "candidate_selection")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

deg_file <- file.path(de_dir, "limma_DEG_Sepsis_vs_Control.csv")
expr_file <- file.path(de_dir, "merged_gene_expression_common_genes_unadjusted.csv")
pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")
go_file <- file.path(enrich_dir, "T05_GO_BP_enrichment.xlsx")

needed <- c(deg_file, expr_file, pheno_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 参数
# ============================================================

padj_cutoff <- 0.05
logfc_cutoff <- 0.5

# 进入单基因评估的候选池上限，避免过大导致脚本慢
max_candidate_genes <- 300

# 候选基因初筛要求
min_direction_consistency <- 0.80
min_mean_auc <- 0.70
min_auc_ge_070_datasets <- 3

# 绘图展示 top N
top_n_plot <- 50
top_n_heatmap <- 40

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

safe_wilcox_p <- function(x, y) {
  if (length(unique(c(x, y))) < 2) return(NA_real_)
  out <- try(stats::wilcox.test(x, y)$p.value, silent = TRUE)
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out)
}

safe_auc <- function(response, predictor) {
  response <- as.character(response)
  predictor <- as.numeric(predictor)
  
  idx <- !is.na(response) & is.finite(predictor)
  response <- response[idx]
  predictor <- predictor[idx]
  
  if (length(unique(response)) != 2) return(NA_real_)
  if (length(unique(predictor)) < 2) return(NA_real_)
  
  response_bin <- ifelse(response == "Sepsis", 1, 0)
  
  roc_obj <- try(
    pROC::roc(
      response = response_bin,
      predictor = predictor,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(roc_obj, "try-error")) return(NA_real_)
  as.numeric(pROC::auc(roc_obj))
}

get_direction <- function(logfc) {
  if (is.na(logfc)) return(NA_character_)
  if (logfc > 0) return("Up_in_Sepsis")
  if (logfc < 0) return("Down_in_Sepsis")
  "No_change"
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

extract_go_keyword_flag <- function(gene_symbols, go_file) {
  # 只作为弱标记：是否出现在与计划书主线相关的 GO term geneID 中
  if (!file.exists(go_file)) {
    return(data.frame(
      gene_symbol = gene_symbols,
      immune_GO_keyword_hit = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  sheets <- try(openxlsx::getSheetNames(go_file), silent = TRUE)
  if (inherits(sheets, "try-error")) {
    return(data.frame(
      gene_symbol = gene_symbols,
      immune_GO_keyword_hit = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  target_sheets <- intersect(
    c("GO_BP_up_in_sepsis", "GO_BP_down_in_sepsis", "GO_BP_all_DEG"),
    sheets
  )
  
  if (length(target_sheets) == 0) {
    return(data.frame(
      gene_symbol = gene_symbols,
      immune_GO_keyword_hit = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  keyword_pattern <- paste(
    c(
      "myeloid",
      "leukocyte",
      "neutrophil",
      "bacterium",
      "inflammatory",
      "inflammation",
      "cytokine",
      "T cell",
      "lymphocyte",
      "adaptive immune",
      "antigen receptor",
      "platelet",
      "coagulation"
    ),
    collapse = "|"
  )
  
  hit_genes <- character()
  
  for (sh in target_sheets) {
    df <- try(openxlsx::read.xlsx(go_file, sheet = sh), silent = TRUE)
    if (inherits(df, "try-error")) next
    if (!all(c("Description", "geneID") %in% colnames(df))) next
    
    df$Description <- as.character(df$Description)
    df$geneID <- as.character(df$geneID)
    
    df_hit <- df[grepl(keyword_pattern, df$Description, ignore.case = TRUE), , drop = FALSE]
    
    if (nrow(df_hit) > 0) {
      z <- unlist(strsplit(df_hit$geneID, "/", fixed = TRUE))
      z <- unique(z[!is.na(z) & z != ""])
      hit_genes <- unique(c(hit_genes, z))
    }
  }
  
  data.frame(
    gene_symbol = gene_symbols,
    immune_GO_keyword_hit = gene_symbols %in% hit_genes,
    stringsAsFactors = FALSE
  )
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

save_auc_heatmap <- function(dataset_metrics, priority_table) {
  
  top_genes <- head(priority_table$gene_symbol, top_n_heatmap)
  hm <- dataset_metrics[
    dataset_metrics$gene_symbol %in% top_genes,
    c("gene_symbol", "dataset", "auc"),
    drop = FALSE
  ]
  
  if (nrow(hm) == 0) return(NULL)
  
  hm$gene_symbol <- factor(hm$gene_symbol, levels = rev(top_genes))
  
  p <- ggplot2::ggplot(
    hm,
    ggplot2::aes(x = dataset, y = gene_symbol, fill = auc)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "Single-gene AUROC across datasets",
      x = "Dataset",
      y = "Candidate gene",
      fill = "AUROC"
    )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F08C_top_candidate_gene_AUROC_heatmap.png"),
    plot = p,
    width = 8,
    height = 8,
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, "F08C_top_candidate_gene_AUROC_heatmap.pdf"),
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

deg <- data.table::fread(deg_file, data.table = FALSE)
expr_mat <- read_expr_matrix(expr_file)
pheno <- data.table::fread(pheno_file, data.table = FALSE)

required_deg_cols <- c("gene_symbol", "logFC", "adj.P.Val")
if (!all(required_deg_cols %in% colnames(deg))) {
  stop("DEG 表缺少 gene_symbol / logFC / adj.P.Val。")
}

required_pheno_cols <- c("sample_id", "dataset", "clinical_group_main")
if (!all(required_pheno_cols %in% colnames(pheno))) {
  stop("phenotype 表缺少 sample_id / dataset / clinical_group_main。")
}

common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
expr_mat <- expr_mat[, common_samples, drop = FALSE]
pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]

if (!all(colnames(expr_mat) == pheno$sample_id)) {
  stop("表达矩阵与 phenotype 没有对齐。")
}

# ============================================================
# 构建候选池
# ============================================================

deg$gene_symbol <- as.character(deg$gene_symbol)
deg$global_direction <- ifelse(deg$logFC > 0, "Up_in_Sepsis", "Down_in_Sepsis")

candidate_deg <- deg[
  deg$adj.P.Val < padj_cutoff &
    abs(deg$logFC) >= logfc_cutoff &
    deg$gene_symbol %in% rownames(expr_mat),
  ,
  drop = FALSE
]

candidate_deg <- candidate_deg[order(candidate_deg$adj.P.Val, -abs(candidate_deg$logFC)), , drop = FALSE]

if (nrow(candidate_deg) > max_candidate_genes) {
  message("候选 DEG 数量较大，按 adj.P.Val 和 |logFC| 截取前 ", max_candidate_genes, " 个用于候选筛查。")
  candidate_deg <- head(candidate_deg, max_candidate_genes)
}

candidate_genes <- unique(candidate_deg$gene_symbol)

message("Candidate genes for dataset-level screening: ", length(candidate_genes))

# ============================================================
# 逐基因、逐队列计算方向和 AUROC
# ============================================================

message("Computing dataset-level metrics...")

dataset_list <- sort(unique(pheno$dataset))
metric_list <- list()
counter <- 1

for (g in candidate_genes) {
  
  x_all <- as.numeric(expr_mat[g, ])
  
  for (ds in dataset_list) {
    
    idx_ds <- pheno$dataset == ds
    ph_ds <- pheno[idx_ds, , drop = FALSE]
    x_ds <- x_all[idx_ds]
    
    if (!all(c("Control", "Sepsis") %in% unique(ph_ds$clinical_group_main))) {
      next
    }
    
    x_control <- x_ds[ph_ds$clinical_group_main == "Control"]
    x_sepsis <- x_ds[ph_ds$clinical_group_main == "Sepsis"]
    
    mean_control <- mean(x_control, na.rm = TRUE)
    mean_sepsis <- mean(x_sepsis, na.rm = TRUE)
    median_control <- median(x_control, na.rm = TRUE)
    median_sepsis <- median(x_sepsis, na.rm = TRUE)
    
    logfc_ds <- mean_sepsis - mean_control
    direction_ds <- get_direction(logfc_ds)
    
    auc_ds <- safe_auc(ph_ds$clinical_group_main, x_ds)
    p_wilcox <- safe_wilcox_p(x_control, x_sepsis)
    
    metric_list[[counter]] <- data.frame(
      gene_symbol = g,
      dataset = ds,
      n_control = sum(ph_ds$clinical_group_main == "Control"),
      n_sepsis = sum(ph_ds$clinical_group_main == "Sepsis"),
      mean_control = mean_control,
      mean_sepsis = mean_sepsis,
      median_control = median_control,
      median_sepsis = median_sepsis,
      dataset_logFC = logfc_ds,
      dataset_direction = direction_ds,
      auc = auc_ds,
      wilcox_p = p_wilcox,
      stringsAsFactors = FALSE
    )
    
    counter <- counter + 1
  }
}

dataset_metrics <- do.call(rbind, metric_list)

if (is.null(dataset_metrics) || nrow(dataset_metrics) == 0) {
  stop("没有生成任何 dataset-level gene metrics。")
}

data.table::fwrite(
  dataset_metrics,
  file.path(out_dir, "T06_candidate_gene_dataset_level_metrics.csv")
)

# ============================================================
# 汇总基因级优先级
# ============================================================

message("Summarising candidate priority scores...")

priority_list <- list()

for (g in candidate_genes) {
  
  m <- dataset_metrics[dataset_metrics$gene_symbol == g, , drop = FALSE]
  d <- candidate_deg[candidate_deg$gene_symbol == g, , drop = FALSE]
  d <- d[1, , drop = FALSE]
  
  global_dir <- d$global_direction[1]
  
  n_dataset_tested <- nrow(m)
  n_direction_consistent <- sum(m$dataset_direction == global_dir, na.rm = TRUE)
  direction_consistency <- n_direction_consistent / max(n_dataset_tested, 1)
  
  mean_auc <- mean(m$auc, na.rm = TRUE)
  median_auc <- median(m$auc, na.rm = TRUE)
  min_auc <- min(m$auc, na.rm = TRUE)
  max_auc <- max(m$auc, na.rm = TRUE)
  n_auc_ge_070 <- sum(m$auc >= 0.70, na.rm = TRUE)
  n_auc_ge_075 <- sum(m$auc >= 0.75, na.rm = TRUE)
  n_auc_ge_080 <- sum(m$auc >= 0.80, na.rm = TRUE)
  
  n_wilcox_p_lt_005 <- sum(m$wilcox_p < 0.05, na.rm = TRUE)
  
  priority_list[[g]] <- data.frame(
    gene_symbol = g,
    global_logFC = d$logFC[1],
    global_adjP = d$adj.P.Val[1],
    global_direction = global_dir,
    n_dataset_tested = n_dataset_tested,
    n_direction_consistent = n_direction_consistent,
    direction_consistency = direction_consistency,
    mean_auc = mean_auc,
    median_auc = median_auc,
    min_auc = min_auc,
    max_auc = max_auc,
    n_auc_ge_070 = n_auc_ge_070,
    n_auc_ge_075 = n_auc_ge_075,
    n_auc_ge_080 = n_auc_ge_080,
    n_wilcox_p_lt_005 = n_wilcox_p_lt_005,
    stringsAsFactors = FALSE
  )
}

priority_table <- do.call(rbind, priority_list)
rownames(priority_table) <- NULL

# GO keyword flag
go_flag <- extract_go_keyword_flag(priority_table$gene_symbol, go_file)
priority_table <- merge(priority_table, go_flag, by = "gene_symbol", all.x = TRUE, sort = FALSE)
priority_table$immune_GO_keyword_hit[is.na(priority_table$immune_GO_keyword_hit)] <- FALSE

# 简单综合评分：用于排序，不作为统计检验
priority_table$score_direction <- scale_01(priority_table$direction_consistency)
priority_table$score_auc <- scale_01(priority_table$mean_auc)
priority_table$score_min_auc <- scale_01(priority_table$min_auc)
priority_table$score_logfc <- scale_01(abs(priority_table$global_logFC))
priority_table$score_p <- scale_01(-log10(priority_table$global_adjP + 1e-300))
priority_table$score_go <- ifelse(priority_table$immune_GO_keyword_hit, 1, 0)

priority_table$priority_score <- with(
  priority_table,
  0.25 * score_direction +
    0.30 * score_auc +
    0.15 * score_min_auc +
    0.15 * score_logfc +
    0.10 * score_p +
    0.05 * score_go
)

priority_table$passes_screening <- with(
  priority_table,
  direction_consistency >= min_direction_consistency &
    mean_auc >= min_mean_auc &
    n_auc_ge_070 >= min_auc_ge_070_datasets
)

priority_table <- priority_table[
  order(
    -priority_table$passes_screening,
    -priority_table$priority_score,
    priority_table$global_adjP
  ),
  ,
  drop = FALSE
]

data.table::fwrite(
  priority_table,
  file.path(out_dir, "T06_candidate_gene_priority_table.csv")
)

# ============================================================
# 输出 Excel
# ============================================================

message("Writing candidate screening workbook...")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "screening_parameters")
param_df <- data.frame(
  parameter = c(
    "padj_cutoff",
    "logfc_cutoff",
    "max_candidate_genes",
    "min_direction_consistency",
    "min_mean_auc",
    "min_auc_ge_070_datasets"
  ),
  value = c(
    padj_cutoff,
    logfc_cutoff,
    max_candidate_genes,
    min_direction_consistency,
    min_mean_auc,
    min_auc_ge_070_datasets
  ),
  stringsAsFactors = FALSE
)
openxlsx::writeData(wb, "screening_parameters", param_df)

openxlsx::addWorksheet(wb, "candidate_DEG_pool")
openxlsx::writeData(wb, "candidate_DEG_pool", candidate_deg)

openxlsx::addWorksheet(wb, "dataset_level_metrics")
openxlsx::writeData(wb, "dataset_level_metrics", dataset_metrics)

openxlsx::addWorksheet(wb, "priority_table")
openxlsx::writeData(wb, "priority_table", priority_table)

openxlsx::addWorksheet(wb, "passed_screening")
openxlsx::writeData(
  wb,
  "passed_screening",
  priority_table[priority_table$passes_screening == TRUE, , drop = FALSE]
)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T06_candidate_gene_screening_table.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 绘图
# ============================================================

message("Saving candidate selection plots...")

save_barplot(
  priority_table,
  x_col = "gene_symbol",
  y_col = "direction_consistency",
  file_prefix = "F08A_candidate_gene_direction_consistency",
  title_text = "Candidate genes ranked by cross-dataset direction consistency",
  y_label = "Direction consistency"
)

save_barplot(
  priority_table,
  x_col = "gene_symbol",
  y_col = "mean_auc",
  file_prefix = "F08B_candidate_gene_mean_AUROC",
  title_text = "Candidate genes ranked by mean single-gene AUROC",
  y_label = "Mean AUROC across datasets"
)

save_auc_heatmap(dataset_metrics, priority_table)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_07_candidate_gene_selection_preparation.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 07 Candidate gene selection preparation 完成 ============")
message("输出目录：", out_dir)
message("图输出目录：", fig_dir)

message("\n候选池基因数：", length(candidate_genes))
message("通过初筛基因数：", sum(priority_table$passes_screening, na.rm = TRUE))

message("\nTop 30 candidate genes:")
print(
  priority_table[
    seq_len(min(30, nrow(priority_table))),
    c(
      "gene_symbol",
      "global_logFC",
      "global_adjP",
      "global_direction",
      "direction_consistency",
      "mean_auc",
      "min_auc",
      "n_auc_ge_070",
      "immune_GO_keyword_hit",
      "priority_score",
      "passes_screening"
    ),
    drop = FALSE
  ]
)

message("\n关键输出：")
message("1) ", file.path(out_dir, "T06_candidate_gene_screening_table.xlsx"))
message("2) ", file.path(out_dir, "T06_candidate_gene_dataset_level_metrics.csv"))
message("3) ", file.path(out_dir, "T06_candidate_gene_priority_table.csv"))
message("4) ", fig_dir)