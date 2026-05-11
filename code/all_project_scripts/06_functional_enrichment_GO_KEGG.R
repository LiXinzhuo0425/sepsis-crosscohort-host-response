# ============================================================
# 06_functional_enrichment_GO_KEGG.R
# DEG 功能富集分析：GO BP / KEGG
#
# 输入：
# 04_results/differential_expression/limma_DEG_Sepsis_vs_Control.csv
#
# 输出：
# 04_results/enrichment/
#   T05_DEG_gene_lists.xlsx
#   T05_GO_BP_enrichment.xlsx
#   T05_KEGG_enrichment.xlsx
#   T05_enrichment_summary.xlsx
#
# 05_figures/enrichment/
#   F07A_GO_BP_all_DEG_dotplot.png/pdf
#   F07B_GO_BP_up_DEG_dotplot.png/pdf
#   F07C_GO_BP_down_DEG_dotplot.png/pdf
#   F07D_KEGG_all_DEG_dotplot.png/pdf
#   F07E_KEGG_up_DEG_dotplot.png/pdf
#   F07F_KEGG_down_DEG_dotplot.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
  library(clusterProfiler)
  library(org.Hs.eg.db)
  library(AnnotationDbi)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

de_dir <- file.path(project_dir, "04_results", "differential_expression")
enrich_dir <- file.path(project_dir, "04_results", "enrichment")
fig_dir <- file.path(project_dir, "05_figures", "enrichment")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(enrich_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

deg_file <- file.path(de_dir, "limma_DEG_Sepsis_vs_Control.csv")

if (!file.exists(deg_file)) {
  stop("缺少 DEG 文件：", deg_file)
}

# ============================================================
# 参数
# ============================================================

padj_cutoff <- 0.05
logfc_cutoff <- 0.5

go_pvalue_cutoff <- 0.05
go_qvalue_cutoff <- 0.20
kegg_pvalue_cutoff <- 0.05
kegg_qvalue_cutoff <- 0.20

min_gs_size <- 10
max_gs_size <- 500
show_n_terms <- 20

# ============================================================
# 工具函数
# ============================================================

clean_symbol_vec <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  x <- x[x != "" & x != "---" & x != "NA" & x != "na" & x != "NULL"]
  unique(x)
}

symbol_to_entrez <- function(symbols) {
  symbols <- clean_symbol_vec(symbols)
  
  if (length(symbols) == 0) {
    return(data.frame(
      SYMBOL = character(),
      ENTREZID = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  mapped <- AnnotationDbi::select(
    org.Hs.eg.db,
    keys = symbols,
    columns = c("ENTREZID", "SYMBOL"),
    keytype = "SYMBOL"
  )
  
  mapped <- as.data.frame(mapped, stringsAsFactors = FALSE)
  mapped <- mapped[!is.na(mapped$ENTREZID) & mapped$ENTREZID != "", , drop = FALSE]
  mapped <- mapped[!duplicated(mapped$SYMBOL), , drop = FALSE]
  rownames(mapped) <- NULL
  
  mapped
}

enrich_go_bp <- function(entrez_ids, universe_ids) {
  entrez_ids <- unique(as.character(entrez_ids))
  universe_ids <- unique(as.character(universe_ids))
  entrez_ids <- entrez_ids[!is.na(entrez_ids) & entrez_ids != ""]
  universe_ids <- universe_ids[!is.na(universe_ids) & universe_ids != ""]
  
  if (length(entrez_ids) < 5) {
    return(NULL)
  }
  
  res <- try(
    clusterProfiler::enrichGO(
      gene = entrez_ids,
      universe = universe_ids,
      OrgDb = org.Hs.eg.db,
      keyType = "ENTREZID",
      ont = "BP",
      pvalueCutoff = go_pvalue_cutoff,
      pAdjustMethod = "BH",
      qvalueCutoff = go_qvalue_cutoff,
      minGSSize = min_gs_size,
      maxGSSize = max_gs_size,
      readable = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(res, "try-error")) return(NULL)
  if (is.null(res)) return(NULL)
  if (nrow(as.data.frame(res)) == 0) return(NULL)
  
  res
}

enrich_kegg <- function(entrez_ids, universe_ids) {
  entrez_ids <- unique(as.character(entrez_ids))
  universe_ids <- unique(as.character(universe_ids))
  entrez_ids <- entrez_ids[!is.na(entrez_ids) & entrez_ids != ""]
  universe_ids <- universe_ids[!is.na(universe_ids) & universe_ids != ""]
  
  if (length(entrez_ids) < 5) {
    return(NULL)
  }
  
  res <- try(
    clusterProfiler::enrichKEGG(
      gene = entrez_ids,
      universe = universe_ids,
      organism = "hsa",
      keyType = "kegg",
      pvalueCutoff = kegg_pvalue_cutoff,
      pAdjustMethod = "BH",
      qvalueCutoff = kegg_qvalue_cutoff,
      minGSSize = min_gs_size,
      maxGSSize = max_gs_size
    ),
    silent = TRUE
  )
  
  if (inherits(res, "try-error")) return(NULL)
  if (is.null(res)) return(NULL)
  if (nrow(as.data.frame(res)) == 0) return(NULL)
  
  res <- clusterProfiler::setReadable(res, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")
  
  res
}

result_to_df <- function(enrich_obj, label, database) {
  if (is.null(enrich_obj)) {
    return(data.frame())
  }
  
  df <- as.data.frame(enrich_obj)
  
  if (nrow(df) == 0) {
    return(data.frame())
  }
  
  df$gene_set <- label
  df$database <- database
  
  df
}

save_dotplot <- function(enrich_obj, file_prefix, title_text, show_n = 20) {
  if (is.null(enrich_obj)) {
    message("跳过绘图，无富集结果：", file_prefix)
    return(NULL)
  }
  
  df <- as.data.frame(enrich_obj)
  
  if (nrow(df) == 0) {
    message("跳过绘图，富集结果为空：", file_prefix)
    return(NULL)
  }
  
  n_show <- min(show_n, nrow(df))
  
  p <- clusterProfiler::dotplot(enrich_obj, showCategory = n_show) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(title = title_text)
  
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

save_barplot_top <- function(enrich_df, file_prefix, title_text, top_n = 15) {
  if (is.null(enrich_df) || nrow(enrich_df) == 0) {
    message("跳过 barplot，无富集结果：", file_prefix)
    return(NULL)
  }
  
  df <- enrich_df[order(enrich_df$p.adjust, -enrich_df$Count), , drop = FALSE]
  df <- head(df, top_n)
  
  df$Description <- factor(df$Description, levels = rev(df$Description))
  df$neg_log10_padj <- -log10(df$p.adjust + 1e-300)
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = Description, y = neg_log10_padj)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(
      title = title_text,
      x = NULL,
      y = "-log10 adjusted P value"
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

make_enrichment_summary <- function(go_all, go_up, go_down, kegg_all, kegg_up, kegg_down) {
  data.frame(
    analysis = c(
      "GO_BP_all_DEG",
      "GO_BP_up_DEG",
      "GO_BP_down_DEG",
      "KEGG_all_DEG",
      "KEGG_up_DEG",
      "KEGG_down_DEG"
    ),
    n_terms = c(
      ifelse(is.null(go_all), 0, nrow(as.data.frame(go_all))),
      ifelse(is.null(go_up), 0, nrow(as.data.frame(go_up))),
      ifelse(is.null(go_down), 0, nrow(as.data.frame(go_down))),
      ifelse(is.null(kegg_all), 0, nrow(as.data.frame(kegg_all))),
      ifelse(is.null(kegg_up), 0, nrow(as.data.frame(kegg_up))),
      ifelse(is.null(kegg_down), 0, nrow(as.data.frame(kegg_down)))
    ),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 读取 DEG
# ============================================================

message("Reading DEG table...")

deg <- data.table::fread(deg_file, data.table = FALSE)

required_cols <- c("gene_symbol", "logFC", "adj.P.Val")
if (!all(required_cols %in% colnames(deg))) {
  stop("DEG 表缺少必要列：gene_symbol, logFC, adj.P.Val")
}

deg$gene_symbol <- as.character(deg$gene_symbol)

all_universe_symbols <- clean_symbol_vec(deg$gene_symbol)

deg_sig <- deg[
  deg$adj.P.Val < padj_cutoff & abs(deg$logFC) >= logfc_cutoff,
  ,
  drop = FALSE
]

deg_up <- deg_sig[deg_sig$logFC > 0, , drop = FALSE]
deg_down <- deg_sig[deg_sig$logFC < 0, , drop = FALSE]

all_sig_symbols <- clean_symbol_vec(deg_sig$gene_symbol)
up_symbols <- clean_symbol_vec(deg_up$gene_symbol)
down_symbols <- clean_symbol_vec(deg_down$gene_symbol)

message("Universe genes: ", length(all_universe_symbols))
message("Significant DEG: ", length(all_sig_symbols))
message("Up in sepsis: ", length(up_symbols))
message("Down in sepsis: ", length(down_symbols))

# ============================================================
# SYMBOL -> ENTREZID
# ============================================================

message("Mapping SYMBOL to ENTREZID...")

map_universe <- symbol_to_entrez(all_universe_symbols)
map_all <- symbol_to_entrez(all_sig_symbols)
map_up <- symbol_to_entrez(up_symbols)
map_down <- symbol_to_entrez(down_symbols)

universe_entrez <- unique(map_universe$ENTREZID)
all_entrez <- unique(map_all$ENTREZID)
up_entrez <- unique(map_up$ENTREZID)
down_entrez <- unique(map_down$ENTREZID)

mapping_summary <- data.frame(
  gene_set = c("universe", "all_DEG", "up_DEG", "down_DEG"),
  n_symbol_input = c(length(all_universe_symbols), length(all_sig_symbols), length(up_symbols), length(down_symbols)),
  n_entrez_mapped = c(length(universe_entrez), length(all_entrez), length(up_entrez), length(down_entrez)),
  mapping_rate = c(
    length(universe_entrez) / max(length(all_universe_symbols), 1),
    length(all_entrez) / max(length(all_sig_symbols), 1),
    length(up_entrez) / max(length(up_symbols), 1),
    length(down_entrez) / max(length(down_symbols), 1)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存 DEG gene lists
# ============================================================

message("Writing DEG gene lists...")

wb_genes <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_genes, "mapping_summary")
openxlsx::writeData(wb_genes, "mapping_summary", mapping_summary)

openxlsx::addWorksheet(wb_genes, "all_significant_DEG")
openxlsx::writeData(wb_genes, "all_significant_DEG", deg_sig)

openxlsx::addWorksheet(wb_genes, "up_in_sepsis")
openxlsx::writeData(wb_genes, "up_in_sepsis", deg_up)

openxlsx::addWorksheet(wb_genes, "down_in_sepsis")
openxlsx::writeData(wb_genes, "down_in_sepsis", deg_down)

openxlsx::addWorksheet(wb_genes, "map_universe")
openxlsx::writeData(wb_genes, "map_universe", map_universe)

openxlsx::addWorksheet(wb_genes, "map_all_DEG")
openxlsx::writeData(wb_genes, "map_all_DEG", map_all)

openxlsx::addWorksheet(wb_genes, "map_up_DEG")
openxlsx::writeData(wb_genes, "map_up_DEG", map_up)

openxlsx::addWorksheet(wb_genes, "map_down_DEG")
openxlsx::writeData(wb_genes, "map_down_DEG", map_down)

openxlsx::saveWorkbook(
  wb_genes,
  file.path(enrich_dir, "T05_DEG_gene_lists.xlsx"),
  overwrite = TRUE
)

# ============================================================
# GO BP enrichment
# ============================================================

message("Running GO BP enrichment...")

ego_all <- enrich_go_bp(all_entrez, universe_entrez)
ego_up <- enrich_go_bp(up_entrez, universe_entrez)
ego_down <- enrich_go_bp(down_entrez, universe_entrez)

go_all_df <- result_to_df(ego_all, "all_DEG", "GO_BP")
go_up_df <- result_to_df(ego_up, "up_in_sepsis", "GO_BP")
go_down_df <- result_to_df(ego_down, "down_in_sepsis", "GO_BP")

go_combined <- rbind(go_all_df, go_up_df, go_down_df)

wb_go <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_go, "GO_BP_all_DEG")
openxlsx::writeData(wb_go, "GO_BP_all_DEG", go_all_df)

openxlsx::addWorksheet(wb_go, "GO_BP_up_in_sepsis")
openxlsx::writeData(wb_go, "GO_BP_up_in_sepsis", go_up_df)

openxlsx::addWorksheet(wb_go, "GO_BP_down_in_sepsis")
openxlsx::writeData(wb_go, "GO_BP_down_in_sepsis", go_down_df)

openxlsx::addWorksheet(wb_go, "GO_BP_combined")
openxlsx::writeData(wb_go, "GO_BP_combined", go_combined)

openxlsx::saveWorkbook(
  wb_go,
  file.path(enrich_dir, "T05_GO_BP_enrichment.xlsx"),
  overwrite = TRUE
)

# ============================================================
# KEGG enrichment
# ============================================================

message("Running KEGG enrichment...")

ekegg_all <- enrich_kegg(all_entrez, universe_entrez)
ekegg_up <- enrich_kegg(up_entrez, universe_entrez)
ekegg_down <- enrich_kegg(down_entrez, universe_entrez)

kegg_all_df <- result_to_df(ekegg_all, "all_DEG", "KEGG")
kegg_up_df <- result_to_df(ekegg_up, "up_in_sepsis", "KEGG")
kegg_down_df <- result_to_df(ekegg_down, "down_in_sepsis", "KEGG")

kegg_combined <- rbind(kegg_all_df, kegg_up_df, kegg_down_df)

wb_kegg <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_kegg, "KEGG_all_DEG")
openxlsx::writeData(wb_kegg, "KEGG_all_DEG", kegg_all_df)

openxlsx::addWorksheet(wb_kegg, "KEGG_up_in_sepsis")
openxlsx::writeData(wb_kegg, "KEGG_up_in_sepsis", kegg_up_df)

openxlsx::addWorksheet(wb_kegg, "KEGG_down_in_sepsis")
openxlsx::writeData(wb_kegg, "KEGG_down_in_sepsis", kegg_down_df)

openxlsx::addWorksheet(wb_kegg, "KEGG_combined")
openxlsx::writeData(wb_kegg, "KEGG_combined", kegg_combined)

openxlsx::saveWorkbook(
  wb_kegg,
  file.path(enrich_dir, "T05_KEGG_enrichment.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 绘图
# ============================================================

message("Saving enrichment plots...")

save_dotplot(
  ego_all,
  "F07A_GO_BP_all_DEG_dotplot",
  "GO Biological Process enrichment: all significant DEGs",
  show_n_terms
)

save_dotplot(
  ego_up,
  "F07B_GO_BP_up_DEG_dotplot",
  "GO Biological Process enrichment: genes upregulated in sepsis",
  show_n_terms
)

save_dotplot(
  ego_down,
  "F07C_GO_BP_down_DEG_dotplot",
  "GO Biological Process enrichment: genes downregulated in sepsis",
  show_n_terms
)

save_dotplot(
  ekegg_all,
  "F07D_KEGG_all_DEG_dotplot",
  "KEGG enrichment: all significant DEGs",
  show_n_terms
)

save_dotplot(
  ekegg_up,
  "F07E_KEGG_up_DEG_dotplot",
  "KEGG enrichment: genes upregulated in sepsis",
  show_n_terms
)

save_dotplot(
  ekegg_down,
  "F07F_KEGG_down_DEG_dotplot",
  "KEGG enrichment: genes downregulated in sepsis",
  show_n_terms
)

save_barplot_top(
  go_up_df,
  "F07G_GO_BP_up_DEG_barplot",
  "Top GO BP terms: genes upregulated in sepsis",
  top_n = 15
)

save_barplot_top(
  go_down_df,
  "F07H_GO_BP_down_DEG_barplot",
  "Top GO BP terms: genes downregulated in sepsis",
  top_n = 15
)

save_barplot_top(
  kegg_up_df,
  "F07I_KEGG_up_DEG_barplot",
  "Top KEGG pathways: genes upregulated in sepsis",
  top_n = 15
)

save_barplot_top(
  kegg_down_df,
  "F07J_KEGG_down_DEG_barplot",
  "Top KEGG pathways: genes downregulated in sepsis",
  top_n = 15
)

# ============================================================
# Summary workbook
# ============================================================

enrich_summary <- make_enrichment_summary(
  ego_all, ego_up, ego_down,
  ekegg_all, ekegg_up, ekegg_down
)

top_terms <- data.frame()

if (nrow(go_up_df) > 0) {
  top_terms <- rbind(top_terms, head(go_up_df[order(go_up_df$p.adjust), ], 10))
}

if (nrow(go_down_df) > 0) {
  top_terms <- rbind(top_terms, head(go_down_df[order(go_down_df$p.adjust), ], 10))
}

if (nrow(kegg_up_df) > 0) {
  top_terms <- rbind(top_terms, head(kegg_up_df[order(kegg_up_df$p.adjust), ], 10))
}

if (nrow(kegg_down_df) > 0) {
  top_terms <- rbind(top_terms, head(kegg_down_df[order(kegg_down_df$p.adjust), ], 10))
}

wb_summary <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_summary, "mapping_summary")
openxlsx::writeData(wb_summary, "mapping_summary", mapping_summary)

openxlsx::addWorksheet(wb_summary, "enrichment_summary")
openxlsx::writeData(wb_summary, "enrichment_summary", enrich_summary)

openxlsx::addWorksheet(wb_summary, "top_terms")
openxlsx::writeData(wb_summary, "top_terms", top_terms)

openxlsx::addWorksheet(wb_summary, "GO_combined")
openxlsx::writeData(wb_summary, "GO_combined", go_combined)

openxlsx::addWorksheet(wb_summary, "KEGG_combined")
openxlsx::writeData(wb_summary, "KEGG_combined", kegg_combined)

openxlsx::saveWorkbook(
  wb_summary,
  file.path(enrich_dir, "T05_enrichment_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_06_functional_enrichment_GO_KEGG.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 06 Functional enrichment 完成 ============")
message("富集结果目录：", enrich_dir)
message("富集图目录：", fig_dir)

message("\nDEG gene counts:")
print(data.frame(
  category = c("all_significant_DEG", "up_in_sepsis", "down_in_sepsis"),
  n = c(length(all_sig_symbols), length(up_symbols), length(down_symbols))
))

message("\nMapping summary:")
print(mapping_summary)

message("\nEnrichment summary:")
print(enrich_summary)

message("\n关键输出：")
message("1) ", file.path(enrich_dir, "T05_DEG_gene_lists.xlsx"))
message("2) ", file.path(enrich_dir, "T05_GO_BP_enrichment.xlsx"))
message("3) ", file.path(enrich_dir, "T05_KEGG_enrichment.xlsx"))
message("4) ", file.path(enrich_dir, "T05_enrichment_summary.xlsx"))
message("5) ", fig_dir)