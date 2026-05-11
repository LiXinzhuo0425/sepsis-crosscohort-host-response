# ============================================================
# 04_expression_annotation_merge_DEG.R
# 本地/注释包映射 + gene-level 合并 + batch-aware limma DEG
# Sepsis vs Control
#
# 平台注释策略：
# GPL13667  -> hgu219.db
# GPL570    -> hgu133plus2.db
# GPL10558  -> illuminaHumanv4.db
# GPL6947   -> illuminaHumanv3.db，若不可用则尝试本地 GPL6947.annot.gz
# GPL17077  -> 本地 GPL17077_full_table.txt
#
# 主 DEG：
# limma model: expression ~ group + dataset batch
#
# ComBat 矩阵：
# 仅输出用于 PCA/heatmap/QC，不作为主 DEG 输入
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(limma)
  library(sva)
  library(ggplot2)
  library(AnnotationDbi)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

expr_dir  <- file.path(project_dir, "02_processed_data/main_expr")
table_dir <- file.path(project_dir, "04_results/tables")
ann_dir   <- file.path(project_dir, "02_processed_data/platform_annotation")
out_dir   <- file.path(project_dir, "04_results/differential_expression")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ann_dir, recursive = TRUE, showWarnings = FALSE)

overview_file <- file.path(table_dir, "T00_core_series_matrix_parse_overview.xlsx")
sample_map_file <- file.path(table_dir, "T01_core_included_sample_map.csv")

if (!file.exists(overview_file)) {
  stop("缺少 overview 文件：", overview_file)
}

if (!file.exists(sample_map_file)) {
  stop("缺少纳入样本表：", sample_map_file)
}

ov <- openxlsx::read.xlsx(overview_file, sheet = "overview")

if (!all(c("dataset", "platform") %in% colnames(ov))) {
  stop("Overview 表缺少 dataset/platform 列：", overview_file)
}

platform_map <- data.frame(
  dataset = as.character(ov$dataset),
  platform = as.character(ov$platform),
  stringsAsFactors = FALSE
)

sample_map <- data.table::fread(sample_map_file, data.table = FALSE)
sample_map[] <- lapply(sample_map, as.character)

need_cols <- c("dataset", "sample_id", "clinical_group_main")
if (!all(need_cols %in% colnames(sample_map))) {
  stop("T01_core_included_sample_map.csv 缺少必要列：dataset, sample_id, clinical_group_main")
}

sample_map <- sample_map[
  sample_map$clinical_group_main %in% c("Control", "Sepsis"),
  ,
  drop = FALSE
]

# ============================================================
# 安全工具函数
# ============================================================

clean_symbol <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  
  # 常见多重分隔符清理
  x <- gsub("///", ";", x, fixed = TRUE)
  x <- gsub("\\s+//\\s+", ";", x)
  x <- gsub("\\s*;\\s*", ";", x)
  x <- gsub("\\s+", "", x)
  
  out <- sapply(strsplit(x, ";", fixed = TRUE), function(z) {
    z <- z[z != "" & z != "---" & z != "NA" & z != "na" & z != "NULL" & z != "null"]
    if (length(z) == 0) return(NA_character_)
    z[1]
  })
  
  out <- as.character(out)
  out[out == ""] <- NA_character_
  out
}

dedup_probe2gene <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  if (!all(c("probe_id", "gene_symbol") %in% colnames(df))) {
    stop("probe2gene 表必须包含 probe_id 和 gene_symbol 两列。")
  }
  
  df$probe_id <- as.character(df$probe_id)
  df$gene_symbol <- clean_symbol(df$gene_symbol)
  
  df <- df[
    !is.na(df$probe_id) & df$probe_id != "" &
      !is.na(df$gene_symbol) & df$gene_symbol != "",
    ,
    drop = FALSE
  ]
  
  df <- df[!duplicated(df$probe_id), , drop = FALSE]
  rownames(df) <- NULL
  df
}

find_first_col <- function(cn, patterns) {
  cn_lower <- tolower(cn)
  for (p in patterns) {
    hit <- grep(p, cn_lower, perl = TRUE)
    if (length(hit) > 0) return(cn[hit[1]])
  }
  NA_character_
}

read_local_annot_file_robust <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("找不到本地注释文件：", file_path)
  }
  
  # 第一轮：直接 fread
  df_try <- try(
    data.table::fread(
      file_path,
      sep = "\t",
      fill = TRUE,
      header = TRUE,
      quote = "",
      data.table = FALSE
    ),
    silent = TRUE
  )
  
  df <- NULL
  
  if (!inherits(df_try, "try-error") && ncol(df_try) >= 2) {
    df <- df_try
  } else {
    # 第二轮：逐行找真正表头
    lines <- readLines(file_path, warn = FALSE)
    tab_counts <- lengths(regmatches(lines, gregexpr("\t", lines, fixed = TRUE)))
    candidate_idx <- which(tab_counts >= 2)
    
    if (length(candidate_idx) == 0) {
      stop("无法在注释文件中找到制表符表格：", file_path)
    }
    
    header_idx <- NA_integer_
    for (i in candidate_idx) {
      z <- tolower(lines[i])
      if (grepl("id", z) && grepl("symbol|gene", z)) {
        header_idx <- i
        break
      }
    }
    
    if (is.na(header_idx)) {
      header_idx <- candidate_idx[1]
    }
    
    table_text <- paste(lines[header_idx:length(lines)], collapse = "\n")
    df <- data.table::fread(
      text = table_text,
      sep = "\t",
      fill = TRUE,
      header = TRUE,
      quote = "",
      data.table = FALSE
    )
  }
  
  cn <- colnames(df)
  
  id_col <- find_first_col(
    cn,
    c(
      "^id$",
      "^probe[_ ]?id$",
      "probe",
      "^spot[_ ]?id$",
      "^name$"
    )
  )
  
  symbol_col <- find_first_col(
    cn,
    c(
      "^gene[_ ]?symbol$",
      "^symbol$",
      "gene.*symbol",
      "genesymbol",
      "gene_assignment"
    )
  )
  
  if (is.na(id_col) || is.na(symbol_col)) {
    message("无法识别注释文件列名，文件：", file_path)
    message("列名如下：")
    print(cn)
    stop("无法识别 probe_id 或 gene_symbol 列。")
  }
  
  out <- data.frame(
    probe_id = df[[id_col]],
    gene_symbol = df[[symbol_col]],
    stringsAsFactors = FALSE
  )
  
  dedup_probe2gene(out)
}

get_db_probe2gene <- function(db_pkg_name, probe_ids) {
  if (!requireNamespace(db_pkg_name, quietly = TRUE)) {
    stop(
      "缺少注释包：", db_pkg_name,
      "\n请先安装，例如：BiocManager::install('", db_pkg_name, "', ask = FALSE, update = FALSE)"
    )
  }
  
  db_obj <- getExportedValue(db_pkg_name, db_pkg_name)
  
  mapped <- AnnotationDbi::select(
    db_obj,
    keys = unique(as.character(probe_ids)),
    columns = c("SYMBOL"),
    keytype = "PROBEID"
  )
  
  probe_col <- colnames(mapped)[1]
  
  out <- data.frame(
    probe_id = mapped[[probe_col]],
    gene_symbol = mapped$SYMBOL,
    stringsAsFactors = FALSE
  )
  
  dedup_probe2gene(out)
}

get_probe2gene <- function(platform_id, probe_ids) {
  
  platform_id <- as.character(platform_id)
  
  if (platform_id == "GPL13667") {
    message("  Annotation source: hgu219.db for GPL13667")
    return(get_db_probe2gene("hgu219.db", probe_ids))
  }
  
  if (platform_id == "GPL570") {
    message("  Annotation source: hgu133plus2.db for GPL570")
    return(get_db_probe2gene("hgu133plus2.db", probe_ids))
  }
  
  if (platform_id == "GPL10558") {
    message("  Annotation source: illuminaHumanv4.db for GPL10558")
    return(get_db_probe2gene("illuminaHumanv4.db", probe_ids))
  }
  
  if (platform_id == "GPL6947") {
    if (requireNamespace("illuminaHumanv3.db", quietly = TRUE)) {
      message("  Annotation source: illuminaHumanv3.db for GPL6947")
      return(get_db_probe2gene("illuminaHumanv3.db", probe_ids))
    }
    
    f <- file.path(ann_dir, "GPL6947.annot.gz")
    message("  Annotation source: local GPL6947.annot.gz")
    return(read_local_annot_file_robust(f))
  }
  
  if (platform_id == "GPL17077") {
    f <- file.path(ann_dir, "GPL17077_full_table.txt")
    message("  Annotation source: local GPL17077_full_table.txt")
    return(read_local_annot_file_robust(f))
  }
  
  stop("暂未定义平台注释规则：", platform_id)
}

read_expr_csv <- function(expr_file) {
  df <- data.table::fread(expr_file, data.table = FALSE)
  
  if (!"gene" %in% colnames(df)) {
    stop("表达矩阵缺少 gene 列：", expr_file)
  }
  
  probe_ids <- as.character(df$gene)
  df$gene <- NULL
  
  mat <- as.matrix(df)
  mode(mat) <- "numeric"
  rownames(mat) <- probe_ids
  
  list(mat = mat, probe_ids = probe_ids)
}

collapse_probe_to_gene <- function(expr_mat, probe2gene) {
  probe2gene <- dedup_probe2gene(probe2gene)
  
  idx <- match(rownames(expr_mat), probe2gene$probe_id)
  gene_symbol <- probe2gene$gene_symbol[idx]
  
  valid <- !is.na(gene_symbol) & gene_symbol != ""
  expr_mat <- expr_mat[valid, , drop = FALSE]
  gene_symbol <- gene_symbol[valid]
  
  if (nrow(expr_mat) == 0) {
    stop("probe 到 gene 映射后无有效表达行。")
  }
  
  df <- data.frame(gene_symbol = gene_symbol, expr_mat, check.names = FALSE)
  agg <- stats::aggregate(. ~ gene_symbol, data = df, FUN = mean)
  
  gene_names <- agg$gene_symbol
  agg$gene_symbol <- NULL
  
  mat2 <- as.matrix(agg)
  mode(mat2) <- "numeric"
  rownames(mat2) <- gene_names
  
  mat2
}

# ============================================================
# 检查必要注释包
# ============================================================

required_pkgs <- c("hgu219.db", "hgu133plus2.db", "illuminaHumanv4.db")

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "缺少必要注释包：", paste(missing_pkgs, collapse = ", "),
    "\n请先安装后再运行。"
  )
}

if (!requireNamespace("illuminaHumanv3.db", quietly = TRUE)) {
  message("提示：未检测到 illuminaHumanv3.db。GPL6947 将尝试使用本地 GPL6947.annot.gz。")
}

# ============================================================
# 逐队列读取表达矩阵、按 T01 样本表对齐、注释并折叠到 gene-level
# ============================================================

expr_files <- list.files(expr_dir, pattern = "_main_expr.csv$", full.names = TRUE)

if (length(expr_files) == 0) {
  stop("没有找到 main_expr 文件：", expr_dir)
}

merged_expr_list <- list()
merged_pheno_list <- list()
annotation_qc_list <- list()

for (expr_file in expr_files) {
  
  acc <- sub("_main_expr.csv$", "", basename(expr_file))
  message("\nProcessing cohort: ", acc)
  
  platform_id <- platform_map$platform[platform_map$dataset == acc]
  if (length(platform_id) != 1 || is.na(platform_id) || platform_id == "") {
    stop("找不到平台映射：", acc)
  }
  
  smap <- sample_map[sample_map$dataset == acc, , drop = FALSE]
  if (nrow(smap) == 0) {
    stop("T01_core_included_sample_map.csv 中找不到该队列样本：", acc)
  }
  
  expr_obj <- read_expr_csv(expr_file)
  expr_mat <- expr_obj$mat
  
  keep_samples <- intersect(colnames(expr_mat), as.character(smap$sample_id))
  
  if (length(keep_samples) == 0) {
    stop("表达矩阵列名与 T01 样本表完全无法匹配：", acc)
  }
  
  missing_in_expr <- setdiff(as.character(smap$sample_id), colnames(expr_mat))
  if (length(missing_in_expr) > 0) {
    message("  Warning: T01 中有样本不在表达矩阵中，数量：", length(missing_in_expr))
  }
  
  expr_mat <- expr_mat[, keep_samples, drop = FALSE]
  smap <- smap[match(keep_samples, as.character(smap$sample_id)), , drop = FALSE]
  
  if (!all(colnames(expr_mat) == as.character(smap$sample_id))) {
    stop("表达矩阵与 T01 样本表无法对齐：", acc)
  }
  
  probe2gene <- get_probe2gene(platform_id, rownames(expr_mat))
  gene_mat <- collapse_probe_to_gene(expr_mat, probe2gene)
  
  merged_expr_list[[acc]] <- gene_mat
  
  smap$dataset <- acc
  merged_pheno_list[[acc]] <- smap
  
  annotation_qc_list[[acc]] <- data.frame(
    dataset = acc,
    platform = platform_id,
    n_samples = ncol(expr_mat),
    n_probes_input = nrow(expr_mat),
    n_probe2gene_available = nrow(probe2gene),
    n_genes_after_collapse = nrow(gene_mat),
    n_control = sum(smap$clinical_group_main == "Control", na.rm = TRUE),
    n_sepsis = sum(smap$clinical_group_main == "Sepsis", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  message("  Samples: ", ncol(expr_mat))
  message("  Probes input: ", nrow(expr_mat))
  message("  Probe2gene available: ", nrow(probe2gene))
  message("  Genes after collapse: ", nrow(gene_mat))
}

annotation_qc <- do.call(rbind, annotation_qc_list)
write.csv(annotation_qc, file.path(out_dir, "T02_annotation_qc_by_dataset.csv"), row.names = FALSE)

# ============================================================
# 使用共同基因合并，避免跨平台大量 NA
# ============================================================

common_genes <- Reduce(intersect, lapply(merged_expr_list, rownames))
message("\nCommon genes across all cohorts: ", length(common_genes))

if (length(common_genes) < 3000) {
  stop("共同基因数过少，停止。请检查注释映射。")
}

merged_expr <- do.call(
  cbind,
  lapply(merged_expr_list, function(m) {
    m[common_genes, , drop = FALSE]
  })
)

merged_pheno <- do.call(rbind, merged_pheno_list)
merged_pheno <- as.data.frame(merged_pheno)
rownames(merged_pheno) <- as.character(merged_pheno$sample_id)

if (!all(colnames(merged_expr) == rownames(merged_pheno))) {
  stop("合并表达矩阵和 phenotype 没有对齐。")
}

na_gene <- rowSums(is.na(merged_expr)) > 0
if (any(na_gene)) {
  message("Removing genes with NA after merge: ", sum(na_gene))
  merged_expr <- merged_expr[!na_gene, , drop = FALSE]
}

batch <- factor(merged_pheno$dataset)
group <- factor(merged_pheno$clinical_group_main, levels = c("Control", "Sepsis"))

if (any(is.na(group))) {
  stop("clinical_group_main 存在 NA。")
}

# ============================================================
# DEG 主分析：limma 模型中纳入 batch
# ============================================================

design <- model.matrix(~ 0 + group + batch)
colnames(design) <- make.names(colnames(design))

fit <- limma::lmFit(merged_expr, design)
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

# ============================================================
# ComBat 矩阵，仅用于后续 PCA/heatmap/QC
# ============================================================

modcombat <- model.matrix(~ group)

combat_expr <- sva::ComBat(
  dat = merged_expr,
  batch = batch,
  mod = modcombat,
  par.prior = TRUE,
  prior.plots = FALSE
)

# ============================================================
# 输出
# ============================================================

write.csv(
  merged_expr,
  file.path(out_dir, "merged_gene_expression_common_genes_unadjusted.csv"),
  row.names = TRUE
)

write.csv(
  combat_expr,
  file.path(out_dir, "merged_gene_expression_common_genes_combat_for_visualization.csv"),
  row.names = TRUE
)

write.csv(
  merged_pheno,
  file.path(out_dir, "merged_pheno_for_DEG.csv"),
  row.names = FALSE
)

write.csv(
  deg,
  file.path(out_dir, "limma_DEG_Sepsis_vs_Control.csv"),
  row.names = FALSE
)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "annotation_qc")
openxlsx::writeData(wb, "annotation_qc", annotation_qc)
openxlsx::addWorksheet(wb, "DEG")
openxlsx::writeData(wb, "DEG", deg)
openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T02_DEG_outputs_summary.xlsx"),
  overwrite = TRUE
)

volcano_df <- deg
volcano_df$neg_log10_adjP <- -log10(volcano_df$adj.P.Val + 1e-300)

p <- ggplot2::ggplot(volcano_df, ggplot2::aes(x = logFC, y = neg_log10_adjP)) +
  ggplot2::geom_point(ggplot2::aes(shape = significance), alpha = 0.55, size = 1.3) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Sepsis vs Control",
    x = "log2 fold change",
    y = "-log10 adjusted P value"
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "Volcano_Sepsis_vs_Control.png"),
  plot = p,
  width = 7,
  height = 6,
  dpi = 300
)

message("\n============ DEG 分析完成 ============")
message("输出目录：", out_dir)
message("共同基因数：", nrow(merged_expr))
message("样本数：", ncol(merged_expr))
message("DEG adj.P.Val < 0.05 & |logFC| >= 0.5：")
print(table(deg$significance))