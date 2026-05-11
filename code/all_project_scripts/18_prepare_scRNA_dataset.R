# ============================================================
# 18_prepare_scRNA_dataset.R
# Prepare GSE167363 scRNA-seq Seurat object
#
# 目的：
# 1. 读取 18c 整理好的 12 个 10x MEX 样本
# 2. 创建 Seurat object
# 3. 添加 sample-level metadata
# 4. 基础 QC
# 5. NormalizeData / FindVariableFeatures / ScaleData / PCA / UMAP / clustering
# 6. 输出 scRNA object、QC 表、初步图
#
# 输入：
# 04_results/single_cell/
#   T18c_GSE167363_per_sample_10x_dirs.csv
#
# 输出：
# 02_processed_data/scRNA/GSE167363/
#   GSE167363_seurat_qc_unfiltered.rds
#   GSE167363_seurat_processed.rds
#
# 04_results/single_cell/
#   T18_GSE167363_sample_level_QC.csv
#   T18_GSE167363_cell_metadata.csv
#   T18_GSE167363_cluster_composition.csv
#   T18_GSE167363_gene_presence_signature.csv
#   T18_GSE167363_prepare_summary.xlsx
#
# 05_figures/single_cell/
#   F18A_QC_violin_by_sample.png/pdf
#   F18B_QC_scatter_nCount_vs_percentMT.png/pdf
#   F18C_UMAP_by_sample.png/pdf
#   F18D_UMAP_by_group.png/pdf
#   F18E_UMAP_by_cluster.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(Seurat)
  library(ggplot2)
  library(Matrix)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

sc_out_dir <- file.path(project_dir, "02_processed_data", "scRNA", "GSE167363")
table_dir <- file.path(project_dir, "04_results", "single_cell")
fig_dir <- file.path(project_dir, "05_figures", "single_cell")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(sc_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 参数
# ============================================================

min_cells <- 3
min_features <- 200

qc_min_features <- 200
qc_max_features <- 6000
qc_max_percent_mt <- 20

n_pcs <- 30
cluster_resolution <- 0.5

# 如果过滤后细胞过少，可以后续放宽 qc_max_percent_mt
apply_qc_filter <- TRUE

# ============================================================
# 输入文件
# ============================================================

dirs_file <- file.path(table_dir, "T18c_GSE167363_per_sample_10x_dirs.csv")

if (!file.exists(dirs_file)) {
  stop("缺少输入文件：", dirs_file)
}

sample_dirs <- data.table::fread(dirs_file, data.table = FALSE)

required_cols <- c(
  "gsm",
  "sample_title",
  "sample_label",
  "data_dir",
  "complete_10x_mex",
  "clinical_group_scRNA",
  "sepsis_status",
  "outcome_group",
  "timepoint",
  "patient_id",
  "dataset"
)

if (!all(required_cols %in% colnames(sample_dirs))) {
  stop("T18c 文件缺少必要列：", paste(setdiff(required_cols, colnames(sample_dirs)), collapse = ", "))
}

sample_dirs <- sample_dirs[sample_dirs$complete_10x_mex == TRUE, , drop = FALSE]

if (nrow(sample_dirs) == 0) {
  stop("没有 complete_10x_mex == TRUE 的样本目录。")
}

for (d in sample_dirs$data_dir) {
  needed_files <- file.path(d, c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz"))
  if (!all(file.exists(needed_files))) {
    stop("10x 目录缺少三件套：", d)
  }
}

# ============================================================
# 目标基因
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

safe_save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
  ggplot2::ggsave(
    filename = file.path(fig_dir, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height
  )
}

make_sample_metadata <- function(sample_dirs) {
  md <- sample_dirs[, c(
    "gsm",
    "sample_title",
    "sample_label",
    "clinical_group_scRNA",
    "sepsis_status",
    "outcome_group",
    "timepoint",
    "patient_id",
    "dataset"
  ), drop = FALSE]
  
  md
}

# ============================================================
# 读取 10x 数据
# ============================================================

message("Reading 10x MEX data with Seurat::Read10X...")

data_dirs <- sample_dirs$data_dir
names(data_dirs) <- sample_dirs$sample_label

counts <- Seurat::Read10X(data.dir = data_dirs)

# 兼容 Read10X 返回 list 的情况
if (is.list(counts) && !inherits(counts, "dgCMatrix")) {
  if ("Gene Expression" %in% names(counts)) {
    counts <- counts[["Gene Expression"]]
  } else if ("RNA" %in% names(counts)) {
    counts <- counts[["RNA"]]
  } else {
    counts <- counts[[1]]
  }
}

message("Count matrix dimensions:")
print(dim(counts))

# ============================================================
# 创建 Seurat object
# ============================================================

message("Creating Seurat object...")

seu <- Seurat::CreateSeuratObject(
  counts = counts,
  project = "GSE167363",
  min.cells = min_cells,
  min.features = min_features
)

# Read10X named vector 会给 barcode 加 sample_label 前缀
seu$cell_barcode <- colnames(seu)

# 从 cell name 中解析 sample_label
cell_names <- colnames(seu)
sample_label_from_cell <- sub("_(.*)$", "", cell_names)

# 更稳的解析：逐个 sample_label 匹配开头
sample_label_exact <- rep(NA_character_, length(cell_names))

for (sl in sample_dirs$sample_label) {
  idx <- startsWith(cell_names, paste0(sl, "_"))
  sample_label_exact[idx] <- sl
}

if (any(is.na(sample_label_exact))) {
  # fallback
  sample_label_exact[is.na(sample_label_exact)] <- sample_label_from_cell[is.na(sample_label_exact)]
}

seu$sample_label <- sample_label_exact

sample_md <- make_sample_metadata(sample_dirs)
rownames(sample_md) <- sample_md$sample_label

# 添加 sample-level metadata
for (col in colnames(sample_md)) {
  seu[[col]] <- sample_md[seu$sample_label, col]
}

# mitochondrial percentage
seu[["percent.mt"]] <- Seurat::PercentageFeatureSet(seu, pattern = "^MT-")

# ============================================================
# 保存未过滤对象
# ============================================================

saveRDS(
  seu,
  file.path(sc_out_dir, "GSE167363_seurat_qc_unfiltered.rds")
)

# ============================================================
# QC summary before filtering
# ============================================================

cell_meta_before <- seu@meta.data
cell_meta_before$cell_id <- rownames(cell_meta_before)

sample_qc_before <- aggregate(
  cbind(nFeature_RNA, nCount_RNA, percent.mt) ~ sample_label + gsm + sample_title +
    clinical_group_scRNA + sepsis_status + outcome_group + timepoint + patient_id,
  data = cell_meta_before,
  FUN = median
)

cell_counts <- aggregate(
  cell_id ~ sample_label + gsm + sample_title +
    clinical_group_scRNA + sepsis_status + outcome_group + timepoint + patient_id,
  data = cell_meta_before,
  FUN = length
)

colnames(cell_counts)[colnames(cell_counts) == "cell_id"] <- "n_cells_before_QC"

sample_qc_before <- merge(
  sample_qc_before,
  cell_counts,
  by = c("sample_label", "gsm", "sample_title", "clinical_group_scRNA", "sepsis_status", "outcome_group", "timepoint", "patient_id"),
  all.x = TRUE,
  sort = FALSE
)

colnames(sample_qc_before)[colnames(sample_qc_before) == "nFeature_RNA"] <- "median_nFeature_RNA_before_QC"
colnames(sample_qc_before)[colnames(sample_qc_before) == "nCount_RNA"] <- "median_nCount_RNA_before_QC"
colnames(sample_qc_before)[colnames(sample_qc_before) == "percent.mt"] <- "median_percent_mt_before_QC"

# ============================================================
# QC filtering
# ============================================================

message("Applying QC filter...")

if (apply_qc_filter) {
  seu_f <- subset(
    seu,
    subset = nFeature_RNA >= qc_min_features &
      nFeature_RNA <= qc_max_features &
      percent.mt <= qc_max_percent_mt
  )
} else {
  seu_f <- seu
}

message("Cells before QC: ", ncol(seu))
message("Cells after QC: ", ncol(seu_f))

if (ncol(seu_f) < 500) {
  warning("QC 后细胞数较少，请检查过滤阈值。")
}

# ============================================================
# QC summary after filtering
# ============================================================

cell_meta_after <- seu_f@meta.data
cell_meta_after$cell_id <- rownames(cell_meta_after)

sample_qc_after <- aggregate(
  cbind(nFeature_RNA, nCount_RNA, percent.mt) ~ sample_label + gsm + sample_title +
    clinical_group_scRNA + sepsis_status + outcome_group + timepoint + patient_id,
  data = cell_meta_after,
  FUN = median
)

cell_counts_after <- aggregate(
  cell_id ~ sample_label + gsm + sample_title +
    clinical_group_scRNA + sepsis_status + outcome_group + timepoint + patient_id,
  data = cell_meta_after,
  FUN = length
)

colnames(cell_counts_after)[colnames(cell_counts_after) == "cell_id"] <- "n_cells_after_QC"

sample_qc_after <- merge(
  sample_qc_after,
  cell_counts_after,
  by = c("sample_label", "gsm", "sample_title", "clinical_group_scRNA", "sepsis_status", "outcome_group", "timepoint", "patient_id"),
  all.x = TRUE,
  sort = FALSE
)

colnames(sample_qc_after)[colnames(sample_qc_after) == "nFeature_RNA"] <- "median_nFeature_RNA_after_QC"
colnames(sample_qc_after)[colnames(sample_qc_after) == "nCount_RNA"] <- "median_nCount_RNA_after_QC"
colnames(sample_qc_after)[colnames(sample_qc_after) == "percent.mt"] <- "median_percent_mt_after_QC"

sample_qc <- merge(
  sample_qc_before,
  sample_qc_after,
  by = c("sample_label", "gsm", "sample_title", "clinical_group_scRNA", "sepsis_status", "outcome_group", "timepoint", "patient_id"),
  all = TRUE,
  sort = FALSE
)

sample_qc$n_cells_removed_by_QC <- sample_qc$n_cells_before_QC - sample_qc$n_cells_after_QC
sample_qc$fraction_cells_retained <- sample_qc$n_cells_after_QC / sample_qc$n_cells_before_QC

# ============================================================
# 标准 Seurat 流程
# ============================================================

message("Running standard Seurat workflow...")

seu_f <- Seurat::NormalizeData(
  seu_f,
  normalization.method = "LogNormalize",
  scale.factor = 10000,
  verbose = FALSE
)

seu_f <- Seurat::FindVariableFeatures(
  seu_f,
  selection.method = "vst",
  nfeatures = 2000,
  verbose = FALSE
)

all_features <- rownames(seu_f)

seu_f <- Seurat::ScaleData(
  seu_f,
  features = all_features,
  verbose = FALSE
)

seu_f <- Seurat::RunPCA(
  seu_f,
  features = Seurat::VariableFeatures(seu_f),
  npcs = n_pcs,
  verbose = FALSE
)

seu_f <- Seurat::FindNeighbors(
  seu_f,
  dims = 1:n_pcs,
  verbose = FALSE
)

seu_f <- Seurat::FindClusters(
  seu_f,
  resolution = cluster_resolution,
  verbose = FALSE
)

seu_f <- Seurat::RunUMAP(
  seu_f,
  dims = 1:n_pcs,
  verbose = FALSE
)

# ============================================================
# 基因存在性检查
# ============================================================

gene_presence <- data.frame(
  gene_symbol = target_genes,
  in_scRNA_features = target_genes %in% rownames(seu_f),
  gene_set = ifelse(
    target_genes %in% final_10_genes & target_genes %in% nested_recurrent_genes,
    "Final10_and_nested_recurrent",
    ifelse(
      target_genes %in% final_10_genes,
      "Final10_only",
      "Nested_recurrent_only"
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# cluster composition
# ============================================================

cell_meta_final <- seu_f@meta.data
cell_meta_final$cell_id <- rownames(cell_meta_final)
cell_meta_final$seurat_cluster <- as.character(Seurat::Idents(seu_f))

cluster_composition <- as.data.frame.matrix(
  table(cell_meta_final$seurat_cluster, cell_meta_final$clinical_group_scRNA)
)
cluster_composition$seurat_cluster <- rownames(cluster_composition)
rownames(cluster_composition) <- NULL

cluster_n <- aggregate(
  cell_id ~ seurat_cluster,
  data = cell_meta_final,
  FUN = length
)
colnames(cluster_n)[colnames(cluster_n) == "cell_id"] <- "n_cells"

cluster_composition <- merge(
  cluster_n,
  cluster_composition,
  by = "seurat_cluster",
  all.x = TRUE,
  sort = FALSE
)

# ============================================================
# 保存对象和表格
# ============================================================

message("Saving objects and tables...")

saveRDS(
  seu_f,
  file.path(sc_out_dir, "GSE167363_seurat_processed.rds")
)

data.table::fwrite(
  sample_qc,
  file.path(table_dir, "T18_GSE167363_sample_level_QC.csv")
)

data.table::fwrite(
  cell_meta_final,
  file.path(table_dir, "T18_GSE167363_cell_metadata.csv")
)

data.table::fwrite(
  cluster_composition,
  file.path(table_dir, "T18_GSE167363_cluster_composition.csv")
)

data.table::fwrite(
  gene_presence,
  file.path(table_dir, "T18_GSE167363_gene_presence_signature.csv")
)

summary_df <- data.frame(
  metric = c(
    "dataset",
    "n_samples",
    "n_cells_before_QC",
    "n_cells_after_QC",
    "fraction_cells_retained",
    "n_genes_after_CreateSeuratObject",
    "n_genes_after_QC",
    "qc_min_features",
    "qc_max_features",
    "qc_max_percent_mt",
    "n_pcs",
    "cluster_resolution",
    "n_clusters",
    "n_signature_genes_checked",
    "n_signature_genes_present"
  ),
  value = c(
    "GSE167363",
    length(unique(seu_f$sample_label)),
    ncol(seu),
    ncol(seu_f),
    ncol(seu_f) / ncol(seu),
    nrow(seu),
    nrow(seu_f),
    qc_min_features,
    qc_max_features,
    qc_max_percent_mt,
    n_pcs,
    cluster_resolution,
    length(unique(cell_meta_final$seurat_cluster)),
    nrow(gene_presence),
    sum(gene_presence$in_scRNA_features)
  ),
  stringsAsFactors = FALSE
)

data.table::fwrite(
  summary_df,
  file.path(table_dir, "T18_GSE167363_prepare_summary.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary")
openxlsx::writeData(wb, "summary", summary_df)

openxlsx::addWorksheet(wb, "sample_QC")
openxlsx::writeData(wb, "sample_QC", sample_qc)

openxlsx::addWorksheet(wb, "cluster_composition")
openxlsx::writeData(wb, "cluster_composition", cluster_composition)

openxlsx::addWorksheet(wb, "gene_presence")
openxlsx::writeData(wb, "gene_presence", gene_presence)

openxlsx::addWorksheet(wb, "cell_metadata_head")
openxlsx::writeData(wb, "cell_metadata_head", head(cell_meta_final, 5000))

openxlsx::saveWorkbook(
  wb,
  file.path(table_dir, "T18_GSE167363_prepare_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 作图
# ============================================================

message("Saving figures...")

p_qc_vln <- Seurat::VlnPlot(
  seu,
  features = c("nFeature_RNA", "nCount_RNA", "percent.mt"),
  group.by = "sample_label",
  pt.size = 0,
  ncol = 3
) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

safe_save_plot(
  p_qc_vln,
  "F18A_QC_violin_by_sample",
  width = 14,
  height = 6
)

p_qc_scatter <- ggplot2::ggplot(
  seu@meta.data,
  ggplot2::aes(x = nCount_RNA, y = percent.mt)
) +
  ggplot2::geom_point(alpha = 0.25, size = 0.4) +
  ggplot2::facet_wrap(~ sample_label, scales = "free_x") +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::labs(
    title = "QC scatter: nCount_RNA vs percent.mt",
    x = "nCount_RNA",
    y = "percent.mt"
  )

safe_save_plot(
  p_qc_scatter,
  "F18B_QC_scatter_nCount_vs_percentMT",
  width = 12,
  height = 8
)

p_umap_sample <- Seurat::DimPlot(
  seu_f,
  reduction = "umap",
  group.by = "sample_label",
  raster = TRUE
) +
  ggplot2::ggtitle("GSE167363 UMAP by sample")

safe_save_plot(
  p_umap_sample,
  "F18C_UMAP_by_sample",
  width = 8,
  height = 6
)

p_umap_group <- Seurat::DimPlot(
  seu_f,
  reduction = "umap",
  group.by = "clinical_group_scRNA",
  raster = TRUE
) +
  ggplot2::ggtitle("GSE167363 UMAP by clinical group")

safe_save_plot(
  p_umap_group,
  "F18D_UMAP_by_group",
  width = 8,
  height = 6
)

p_umap_cluster <- Seurat::DimPlot(
  seu_f,
  reduction = "umap",
  group.by = "seurat_clusters",
  label = TRUE,
  raster = TRUE
) +
  ggplot2::ggtitle("GSE167363 UMAP by Seurat cluster")

safe_save_plot(
  p_umap_cluster,
  "F18E_UMAP_by_cluster",
  width = 8,
  height = 6
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_18_prepare_scRNA_dataset.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 18 Prepare scRNA dataset 完成 ============")
message("Seurat object 输出目录：", sc_out_dir)
message("表格输出目录：", table_dir)
message("图输出目录：", fig_dir)

message("\nSummary:")
print(summary_df)

message("\nSample-level QC:")
print(sample_qc)

message("\nCluster composition:")
print(cluster_composition)

message("\nSignature gene presence:")
print(gene_presence)

message("\n关键输出：")
message("1) ", file.path(sc_out_dir, "GSE167363_seurat_processed.rds"))
message("2) ", file.path(table_dir, "T18_GSE167363_prepare_summary.xlsx"))
message("3) ", file.path(table_dir, "T18_GSE167363_sample_level_QC.csv"))
message("4) ", file.path(table_dir, "T18_GSE167363_cell_metadata.csv"))
message("5) ", file.path(table_dir, "T18_GSE167363_cluster_composition.csv"))
message("6) ", file.path(table_dir, "T18_GSE167363_gene_presence_signature.csv"))
message("7) ", fig_dir)

message("\n下一步：")
message("把 Summary、Sample-level QC、Cluster composition、Signature gene presence 贴给我。")
message("如果 QC 后细胞量和目标基因存在性正常，就继续 19_scRNA_signature_localization_module_score.R。")