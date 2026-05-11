# ============================================================
# 19_scRNA_signature_localization_module_score.R
# Memory-friendly signature localization and module score analysis
# Dataset: GSE167363
#
# 修正版 v2：
# 1. 保留 AddModuleScore
# 2. 所有分组汇总改用 data.table
# 3. 修复 Seurat metadata 写入错误：
#    cell_type_level1 使用 AddMetaData，且 row.names = colnames(seu)
# 4. 不运行 FindAllMarkers，先做 canonical marker module score 粗注释
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(Seurat)
  library(SeuratObject)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

sc_dir <- file.path(project_dir, "02_processed_data", "scRNA", "GSE167363")
table_dir <- file.path(project_dir, "04_results", "single_cell")
fig_dir <- file.path(project_dir, "05_figures", "single_cell")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

seurat_file <- file.path(sc_dir, "GSE167363_seurat_processed.rds")
gene_presence_file <- file.path(table_dir, "T18_GSE167363_gene_presence_signature.csv")

if (!file.exists(seurat_file)) {
  stop("缺少 Seurat 对象：", seurat_file)
}

if (!file.exists(gene_presence_file)) {
  stop("缺少 gene presence 表：", gene_presence_file)
}

# ============================================================
# 工具函数
# ============================================================

safe_save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    limitsize = FALSE
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    limitsize = FALSE
  )
}

present_genes <- function(genes, object) {
  unique(genes[genes %in% rownames(object)])
}

score_summary_dt <- function(dt, score_cols, group_cols) {
  out_list <- list()
  
  for (score_col in score_cols) {
    needed_cols <- unique(c(group_cols, score_col))
    sub_dt <- dt[, ..needed_cols]
    
    z <- sub_dt[
      ,
      .(
        n_cells = .N,
        mean_score = mean(get(score_col), na.rm = TRUE),
        median_score = median(get(score_col), na.rm = TRUE)
      ),
      by = group_cols
    ]
    
    z[, score_name := score_col]
    data.table::setcolorder(z, c(group_cols, "score_name", "n_cells", "mean_score", "median_score"))
    
    out_list[[score_col]] <- z
  }
  
  data.table::rbindlist(out_list, use.names = TRUE, fill = TRUE)
}

make_cluster_marker_wide <- function(cluster_marker_scores, marker_score_cols) {
  z <- cluster_marker_scores[
    ,
    .(seurat_cluster, score_name, mean_score, n_cells)
  ]
  
  wide <- data.table::dcast(
    z,
    seurat_cluster + n_cells ~ score_name,
    value.var = "mean_score"
  )
  
  for (cc in marker_score_cols) {
    if (!cc %in% colnames(wide)) {
      wide[[cc]] <- NA_real_
    }
  }
  
  wide
}

rank_cluster_annotation <- function(cluster_score_dt, score_cols) {
  out_list <- list()
  
  for (i in seq_len(nrow(cluster_score_dt))) {
    row <- cluster_score_dt[i]
    scores <- as.numeric(row[, ..score_cols])
    names(scores) <- score_cols
    
    if (all(is.na(scores))) {
      best <- NA_character_
      second <- NA_character_
      best_score <- NA_real_
      second_score <- NA_real_
      margin <- NA_real_
    } else {
      ord <- order(scores, decreasing = TRUE, na.last = TRUE)
      best <- names(scores)[ord[1]]
      second <- names(scores)[ord[2]]
      best_score <- scores[ord[1]]
      second_score <- scores[ord[2]]
      margin <- best_score - second_score
    }
    
    out_list[[i]] <- data.table(
      seurat_cluster = as.character(row$seurat_cluster),
      n_cells = as.integer(row$n_cells),
      predicted_cell_type = gsub("_score$", "", best),
      best_marker_score = best_score,
      second_best_cell_type = gsub("_score$", "", second),
      second_best_marker_score = second_score,
      score_margin = margin
    )
  }
  
  data.table::rbindlist(out_list)
}

make_level1_celltype <- function(predicted_cell_type) {
  x <- as.character(predicted_cell_type)
  out <- x
  
  out[grepl("Classical_monocyte|Nonclassical_monocyte|Monocyte", x)] <- "Monocyte/Myeloid"
  out[grepl("T_NK|CD4_T|CD8_T_NK", x)] <- "T/NK cell"
  out[grepl("B_cell|Plasma", x)] <- "B/Plasma cell"
  out[grepl("Dendritic_cell", x)] <- "Dendritic cell"
  out[grepl("Platelet", x)] <- "Platelet"
  out[grepl("Erythroid", x)] <- "Erythroid"
  
  out[is.na(out) | out == "NA" | out == ""] <- "Uncertain"
  out
}

# ============================================================
# 读取对象
# ============================================================

message("Reading Seurat object...")

seu <- readRDS(seurat_file)
gene_presence <- data.table::fread(gene_presence_file, data.table = FALSE)

DefaultAssay(seu) <- "RNA"

# ============================================================
# Gene sets
# ============================================================

final_10_genes <- c(
  "RAB31", "VNN1", "TCN1", "EMILIN2", "ZDHHC19",
  "ANKRD22", "PGLYRP1", "FAM20A", "RNASE3", "HK3"
)

nested_recurrent_genes <- c(
  "CD177", "TDRD9", "LILRA6", "RAB31", "VNN1",
  "ANKRD22", "ARG1", "C3AR1", "RNASE3", "WFDC1"
)

myeloid_innate_up_genes <- c(
  "CD177", "VNN1", "ARG1", "C3AR1", "RNASE3",
  "LILRA6", "PGLYRP1", "TCN1", "ANKRD22", "FAM20A"
)

adaptive_or_nonmyeloid_genes <- c(
  "RAB31", "EMILIN2", "ZDHHC19", "TDRD9", "WFDC1", "HK3"
)

gene_sets <- list(
  Final10 = present_genes(final_10_genes, seu),
  NestedRecurrent = present_genes(nested_recurrent_genes, seu),
  MyeloidInnateUp = present_genes(myeloid_innate_up_genes, seu),
  AdaptiveOrNonmyeloid = present_genes(adaptive_or_nonmyeloid_genes, seu)
)

gene_sets <- gene_sets[vapply(gene_sets, length, integer(1)) > 0]

gene_set_table <- data.frame(
  gene_set = rep(names(gene_sets), vapply(gene_sets, length, integer(1))),
  gene_symbol = unlist(gene_sets, use.names = FALSE),
  stringsAsFactors = FALSE
)

canonical_markers <- list(
  T_NK = c("CD3D", "CD3E", "TRAC", "NKG7", "GNLY", "KLRD1"),
  CD4_T = c("IL7R", "CCR7", "LTB", "CD4"),
  CD8_T_NK = c("CD8A", "CD8B", "NKG7", "GNLY", "GZMB", "PRF1"),
  B_cell = c("MS4A1", "CD79A", "CD79B", "BANK1", "CD74"),
  Plasma = c("MZB1", "XBP1", "JCHAIN", "IGHG1", "IGKC"),
  Monocyte = c("LYZ", "S100A8", "S100A9", "LST1", "FCN1", "CTSS"),
  Classical_monocyte = c("S100A8", "S100A9", "FCN1", "VCAN", "LYZ"),
  Nonclassical_monocyte = c("FCGR3A", "MS4A7", "LST1", "AIF1", "LILRB1"),
  Dendritic_cell = c("FCER1A", "CST3", "CLEC10A", "LILRA4", "GZMB"),
  Platelet = c("PPBP", "PF4", "NRGN", "GP9", "SDPR"),
  Erythroid = c("HBB", "HBA1", "HBA2", "ALAS2")
)

canonical_markers <- lapply(canonical_markers, present_genes, object = seu)
canonical_markers <- canonical_markers[vapply(canonical_markers, length, integer(1)) > 0]

canonical_marker_table <- data.frame(
  marker_set = rep(names(canonical_markers), vapply(canonical_markers, length, integer(1))),
  gene_symbol = unlist(canonical_markers, use.names = FALSE),
  stringsAsFactors = FALSE
)

# ============================================================
# AddModuleScore
# ============================================================

message("Adding signature module scores...")

set.seed(20260508)

seu <- Seurat::AddModuleScore(
  object = seu,
  features = gene_sets,
  name = names(gene_sets),
  seed = 20260508,
  ctrl = 50
)

score_cols_raw <- paste0(names(gene_sets), seq_along(gene_sets))
score_cols_clean <- paste0(names(gene_sets), "_score")

for (i in seq_along(score_cols_raw)) {
  if (score_cols_raw[i] %in% colnames(seu@meta.data)) {
    colnames(seu@meta.data)[colnames(seu@meta.data) == score_cols_raw[i]] <- score_cols_clean[i]
  }
}

message("Adding canonical marker module scores...")

seu <- Seurat::AddModuleScore(
  object = seu,
  features = canonical_markers,
  name = names(canonical_markers),
  seed = 20260508,
  ctrl = 50
)

marker_score_cols_raw <- paste0(names(canonical_markers), seq_along(canonical_markers))
marker_score_cols_clean <- paste0(names(canonical_markers), "_score")

for (i in seq_along(marker_score_cols_raw)) {
  if (marker_score_cols_raw[i] %in% colnames(seu@meta.data)) {
    colnames(seu@meta.data)[colnames(seu@meta.data) == marker_score_cols_raw[i]] <- marker_score_cols_clean[i]
  }
}

score_cols_clean <- score_cols_clean[score_cols_clean %in% colnames(seu@meta.data)]
marker_score_cols_clean <- marker_score_cols_clean[marker_score_cols_clean %in% colnames(seu@meta.data)]

# ============================================================
# 构建轻量 metadata data.table
# ============================================================

message("Building lightweight metadata table...")

seu$seurat_cluster <- as.character(Seurat::Idents(seu))

meta_cols <- unique(c(
  "sample_label",
  "gsm",
  "sample_title",
  "clinical_group_scRNA",
  "sepsis_status",
  "outcome_group",
  "timepoint",
  "patient_id",
  "seurat_cluster",
  score_cols_clean,
  marker_score_cols_clean
))

meta_cols <- meta_cols[meta_cols %in% colnames(seu@meta.data)]

cell_meta_dt <- data.table::as.data.table(seu@meta.data[, meta_cols, drop = FALSE])
cell_meta_dt[, cell_id := rownames(seu@meta.data)]

gc()

# ============================================================
# Cluster annotation
# ============================================================

message("Annotating clusters with data.table summaries...")

cluster_marker_scores <- score_summary_dt(
  dt = cell_meta_dt,
  score_cols = marker_score_cols_clean,
  group_cols = c("seurat_cluster")
)

cluster_marker_wide <- make_cluster_marker_wide(
  cluster_marker_scores = cluster_marker_scores,
  marker_score_cols = marker_score_cols_clean
)

cluster_annotation <- rank_cluster_annotation(
  cluster_score_dt = cluster_marker_wide,
  score_cols = marker_score_cols_clean
)

cluster_annotation[, manual_cell_type_level1 := make_level1_celltype(predicted_cell_type)]

anno_map <- cluster_annotation$manual_cell_type_level1
names(anno_map) <- cluster_annotation$seurat_cluster

# ============================================================
# 修复点：用 AddMetaData 写入 cell-level metadata
# ============================================================

cell_type_vec <- anno_map[as.character(seu$seurat_cluster)]
cell_type_vec <- as.character(cell_type_vec)
cell_type_vec[is.na(cell_type_vec) | cell_type_vec == ""] <- "Uncertain"

cell_type_df <- data.frame(
  cell_type_level1 = cell_type_vec,
  row.names = colnames(seu),
  stringsAsFactors = FALSE
)

if (!identical(rownames(cell_type_df), colnames(seu))) {
  stop("cell_type_df 行名与 Seurat cell names 不一致。")
}

seu <- SeuratObject::AddMetaData(
  object = seu,
  metadata = cell_type_df
)

cell_meta_dt[, cell_type_level1 := anno_map[as.character(seurat_cluster)]]
cell_meta_dt[is.na(cell_type_level1) | cell_type_level1 == "", cell_type_level1 := "Uncertain"]

gc()

# ============================================================
# Signature score summaries
# ============================================================

message("Summarizing signature scores with data.table...")

score_by_cluster <- score_summary_dt(
  dt = cell_meta_dt,
  score_cols = score_cols_clean,
  group_cols = c("seurat_cluster")
)

score_by_celltype <- score_summary_dt(
  dt = cell_meta_dt,
  score_cols = score_cols_clean,
  group_cols = c("cell_type_level1")
)

score_by_group <- score_summary_dt(
  dt = cell_meta_dt,
  score_cols = score_cols_clean,
  group_cols = c("clinical_group_scRNA")
)

score_by_outcome_timepoint <- score_summary_dt(
  dt = cell_meta_dt,
  score_cols = score_cols_clean,
  group_cols = c("clinical_group_scRNA", "outcome_group", "timepoint")
)

score_by_celltype_group <- score_summary_dt(
  dt = cell_meta_dt,
  score_cols = score_cols_clean,
  group_cols = c("cell_type_level1", "clinical_group_scRNA")
)

# ============================================================
# Average expression
# ============================================================

message("Calculating average expression for signature genes...")

signature_genes_present <- unique(unlist(gene_sets, use.names = FALSE))
signature_genes_present <- signature_genes_present[signature_genes_present %in% rownames(seu)]

avg_cluster <- Seurat::AverageExpression(
  seu,
  assays = "RNA",
  features = signature_genes_present,
  group.by = "seurat_cluster",
  slot = "data",
  verbose = FALSE
)$RNA

avg_celltype <- Seurat::AverageExpression(
  seu,
  assays = "RNA",
  features = signature_genes_present,
  group.by = "cell_type_level1",
  slot = "data",
  verbose = FALSE
)$RNA

avg_cluster_df <- as.data.frame(as.matrix(avg_cluster))
avg_cluster_df$gene_symbol <- rownames(avg_cluster_df)
avg_cluster_df <- avg_cluster_df[, c("gene_symbol", setdiff(colnames(avg_cluster_df), "gene_symbol"))]

avg_celltype_df <- as.data.frame(as.matrix(avg_celltype))
avg_celltype_df$gene_symbol <- rownames(avg_celltype_df)
avg_celltype_df <- avg_celltype_df[, c("gene_symbol", setdiff(colnames(avg_celltype_df), "gene_symbol"))]

# ============================================================
# Localization summary
# ============================================================

top_celltype_final10 <- as.data.frame(score_by_celltype[score_name == "Final10_score"])
top_celltype_final10 <- top_celltype_final10[order(-top_celltype_final10$mean_score), , drop = FALSE]

top_celltype_nested <- as.data.frame(score_by_celltype[score_name == "NestedRecurrent_score"])
top_celltype_nested <- top_celltype_nested[order(-top_celltype_nested$mean_score), , drop = FALSE]

top_final_celltype <- ifelse(nrow(top_celltype_final10) > 0, top_celltype_final10$cell_type_level1[1], NA)
top_nested_celltype <- ifelse(nrow(top_celltype_nested) > 0, top_celltype_nested$cell_type_level1[1], NA)

top_final_score <- ifelse(nrow(top_celltype_final10) > 0, top_celltype_final10$mean_score[1], NA)
top_nested_score <- ifelse(nrow(top_celltype_nested) > 0, top_celltype_nested$mean_score[1], NA)

localization_summary <- data.frame(
  metric = c(
    "n_cells",
    "n_clusters",
    "n_cell_type_level1",
    "n_final10_genes_present",
    "n_nested_recurrent_genes_present",
    "top_celltype_Final10_score",
    "top_celltype_NestedRecurrent_score",
    "top_celltype_Final10_mean_score",
    "top_celltype_NestedRecurrent_mean_score"
  ),
  value = c(
    ncol(seu),
    length(unique(cell_meta_dt$seurat_cluster)),
    length(unique(cell_meta_dt$cell_type_level1)),
    length(gene_sets$Final10),
    length(gene_sets$NestedRecurrent),
    top_final_celltype,
    top_nested_celltype,
    top_final_score,
    top_nested_score
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存对象和表格
# ============================================================

message("Saving scored and annotated object and tables...")

saveRDS(
  seu,
  file.path(sc_dir, "GSE167363_seurat_annotated_scored.rds")
)

data.table::fwrite(
  gene_set_table,
  file.path(table_dir, "T19_scRNA_signature_gene_sets.csv")
)

data.table::fwrite(
  canonical_marker_table,
  file.path(table_dir, "T19_scRNA_canonical_marker_gene_sets.csv")
)

data.table::fwrite(
  cluster_marker_scores,
  file.path(table_dir, "T19_scRNA_cluster_marker_scores.csv")
)

data.table::fwrite(
  cluster_annotation,
  file.path(table_dir, "T19_scRNA_cluster_annotation.csv")
)

data.table::fwrite(
  score_by_cluster,
  file.path(table_dir, "T19_scRNA_signature_score_by_cluster.csv")
)

data.table::fwrite(
  score_by_celltype,
  file.path(table_dir, "T19_scRNA_signature_score_by_celltype.csv")
)

data.table::fwrite(
  score_by_group,
  file.path(table_dir, "T19_scRNA_signature_score_by_group.csv")
)

data.table::fwrite(
  score_by_outcome_timepoint,
  file.path(table_dir, "T19_scRNA_signature_score_by_outcome_timepoint.csv")
)

data.table::fwrite(
  score_by_celltype_group,
  file.path(table_dir, "T19_scRNA_signature_score_by_celltype_group.csv")
)

data.table::fwrite(
  avg_cluster_df,
  file.path(table_dir, "T19_scRNA_signature_gene_average_expression_by_cluster.csv")
)

data.table::fwrite(
  avg_celltype_df,
  file.path(table_dir, "T19_scRNA_signature_gene_average_expression_by_celltype.csv")
)

data.table::fwrite(
  localization_summary,
  file.path(table_dir, "T19_scRNA_signature_localization_summary.csv")
)

cell_meta_out <- data.table::copy(cell_meta_dt)

data.table::fwrite(
  cell_meta_out,
  file.path(table_dir, "T19_scRNA_cell_metadata_with_scores.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "localization_summary")
openxlsx::writeData(wb, "localization_summary", localization_summary)

openxlsx::addWorksheet(wb, "signature_gene_sets")
openxlsx::writeData(wb, "signature_gene_sets", gene_set_table)

openxlsx::addWorksheet(wb, "canonical_marker_sets")
openxlsx::writeData(wb, "canonical_marker_sets", canonical_marker_table)

openxlsx::addWorksheet(wb, "cluster_annotation")
openxlsx::writeData(wb, "cluster_annotation", cluster_annotation)

openxlsx::addWorksheet(wb, "score_by_celltype")
openxlsx::writeData(wb, "score_by_celltype", score_by_celltype)

openxlsx::addWorksheet(wb, "score_by_group")
openxlsx::writeData(wb, "score_by_group", score_by_group)

openxlsx::addWorksheet(wb, "score_by_outcome_timepoint")
openxlsx::writeData(wb, "score_by_outcome_timepoint", score_by_outcome_timepoint)

openxlsx::addWorksheet(wb, "score_by_celltype_group")
openxlsx::writeData(wb, "score_by_celltype_group", score_by_celltype_group)

openxlsx::addWorksheet(wb, "avg_expr_by_cluster")
openxlsx::writeData(wb, "avg_expr_by_cluster", avg_cluster_df)

openxlsx::addWorksheet(wb, "avg_expr_by_celltype")
openxlsx::writeData(wb, "avg_expr_by_celltype", avg_celltype_df)

openxlsx::saveWorkbook(
  wb,
  file.path(table_dir, "T19_scRNA_signature_localization_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# Figures
# ============================================================

message("Saving figures...")

p_celltype <- Seurat::DimPlot(
  seu,
  reduction = "umap",
  group.by = "cell_type_level1",
  label = TRUE,
  repel = TRUE,
  raster = TRUE
) +
  ggplot2::ggtitle("GSE167363 coarse cell-type annotation")

safe_save_plot(
  p_celltype,
  "F19A_UMAP_celltype_annotation",
  width = 8,
  height = 6
)

if ("Final10_score" %in% colnames(seu@meta.data)) {
  p_final10 <- Seurat::FeaturePlot(
    seu,
    reduction = "umap",
    features = "Final10_score",
    raster = TRUE
  ) +
    ggplot2::ggtitle("Final 10-gene module score")
  
  safe_save_plot(
    p_final10,
    "F19B_UMAP_final10_score",
    width = 7,
    height = 6
  )
}

if ("NestedRecurrent_score" %in% colnames(seu@meta.data)) {
  p_nested <- Seurat::FeaturePlot(
    seu,
    reduction = "umap",
    features = "NestedRecurrent_score",
    raster = TRUE
  ) +
    ggplot2::ggtitle("Nested recurrent gene module score")
  
  safe_save_plot(
    p_nested,
    "F19C_UMAP_nested_recurrent_score",
    width = 7,
    height = 6
  )
}

vln_features_celltype <- intersect(
  c("Final10_score", "NestedRecurrent_score", "MyeloidInnateUp_score"),
  colnames(seu@meta.data)
)

if (length(vln_features_celltype) > 0) {
  p_vln_celltype <- Seurat::VlnPlot(
    seu,
    features = vln_features_celltype,
    group.by = "cell_type_level1",
    pt.size = 0,
    ncol = length(vln_features_celltype)
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  safe_save_plot(
    p_vln_celltype,
    "F19D_signature_scores_by_celltype",
    width = 14,
    height = 6
  )
}

if (length(vln_features_celltype) > 0) {
  p_vln_group <- Seurat::VlnPlot(
    seu,
    features = vln_features_celltype,
    group.by = "clinical_group_scRNA",
    pt.size = 0,
    ncol = length(vln_features_celltype)
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  safe_save_plot(
    p_vln_group,
    "F19E_signature_scores_by_group",
    width = 12,
    height = 5
  )
}

dot_features <- unique(c(final_10_genes, nested_recurrent_genes))
dot_features <- dot_features[dot_features %in% rownames(seu)]

if (length(dot_features) > 0) {
  p_dot_sig <- Seurat::DotPlot(
    seu,
    features = dot_features,
    group.by = "cell_type_level1"
  ) +
    ggplot2::coord_flip() +
    ggplot2::ggtitle("Signature gene expression by coarse cell type")
  
  safe_save_plot(
    p_dot_sig,
    "F19F_signature_gene_dotplot_by_celltype",
    width = 9,
    height = 7
  )
}

canonical_dot_features <- unique(unlist(canonical_markers, use.names = FALSE))
canonical_dot_features <- canonical_dot_features[canonical_dot_features %in% rownames(seu)]
canonical_dot_features <- unique(canonical_dot_features)

if (length(canonical_dot_features) > 50) {
  canonical_dot_features <- canonical_dot_features[1:50]
}

if (length(canonical_dot_features) > 0) {
  p_dot_marker <- Seurat::DotPlot(
    seu,
    features = canonical_dot_features,
    group.by = "seurat_cluster"
  ) +
    ggplot2::coord_flip() +
    ggplot2::ggtitle("Canonical marker expression by Seurat cluster")
  
  safe_save_plot(
    p_dot_marker,
    "F19G_canonical_marker_dotplot_by_cluster",
    width = 10,
    height = 9
  )
}

heat_df <- as.data.frame(score_by_cluster[
  score_name %in% c("Final10_score", "NestedRecurrent_score", "MyeloidInnateUp_score")
])

if (nrow(heat_df) > 0) {
  p_heat <- ggplot2::ggplot(
    heat_df,
    ggplot2::aes(x = score_name, y = seurat_cluster, fill = mean_score)
  ) +
    ggplot2::geom_tile() +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(
      title = "Signature module scores by Seurat cluster",
      x = "Signature score",
      y = "Seurat cluster",
      fill = "Mean score"
    )
  
  safe_save_plot(
    p_heat,
    "F19H_signature_score_heatmap_by_cluster",
    width = 7,
    height = 8
  )
}

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_19_scRNA_signature_localization_module_score.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 19 scRNA signature localization and module score 完成 ============")
message("Seurat object 输出：", file.path(sc_dir, "GSE167363_seurat_annotated_scored.rds"))
message("表格输出目录：", table_dir)
message("图输出目录：", fig_dir)

message("\nLocalization summary:")
print(localization_summary)

message("\nCluster annotation:")
print(cluster_annotation)

message("\nSignature score by cell type:")
print(score_by_celltype)

message("\nSignature score by clinical group:")
print(score_by_group)

message("\nSignature score by outcome/timepoint:")
print(score_by_outcome_timepoint)

message("\n关键输出：")
message("1) ", file.path(sc_dir, "GSE167363_seurat_annotated_scored.rds"))
message("2) ", file.path(table_dir, "T19_scRNA_signature_localization_summary.xlsx"))
message("3) ", file.path(table_dir, "T19_scRNA_cluster_annotation.csv"))
message("4) ", file.path(table_dir, "T19_scRNA_signature_score_by_celltype.csv"))
message("5) ", file.path(table_dir, "T19_scRNA_signature_score_by_group.csv"))
message("6) ", file.path(table_dir, "T19_scRNA_signature_gene_average_expression_by_celltype.csv"))
message("7) ", fig_dir)

message("\n下一步：")
message("把 Localization summary、Cluster annotation、Signature score by cell type、Signature score by clinical group 贴给我。")
message("我会判断 scRNA 结果能不能支撑 manuscript 中的细胞来源解释。")