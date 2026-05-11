# ============================================================
# 37_BMC_Genomics_composite_main_figures_builder.R
# Build submission-grade composite main figures for BMC Genomics
#
# Purpose:
#   Rebuild Figure 2 to Figure 5 as multi-panel main figures.
#
# Inputs:
#   /Users/felix/Documents/Sepsis_CrossCohort_scRNA/04_results/final_freeze/tables
#   /Users/felix/Documents/Sepsis_CrossCohort_scRNA/04_results/final_freeze/figures
#   /Users/felix/Documents/Sepsis_CrossCohort_scRNA/07_manuscript/BMC_Genomics_final_submission_package/00_READY_TO_UPLOAD
#
# Outputs:
#   00_READY_TO_UPLOAD/figures/Figure_2.pdf/png
#   00_READY_TO_UPLOAD/figures/Figure_3.pdf/png
#   00_READY_TO_UPLOAD/figures/Figure_4.pdf/png
#   00_READY_TO_UPLOAD/figures/Figure_5.pdf/png
#   checks/BMC_Genomics_composite_main_figure_manifest_v1.2.csv
#   checks/BMC_Genomics_composite_main_figure_checks_v1.2.csv
#   BMC_Genomics_composite_main_figure_manifest_v1.2.xlsx
#
# Notes:
#   1. This script does not change any frozen numerical results.
#   2. It reads frozen tables and existing frozen figure outputs only.
#   3. It creates reviewer-friendly composite figures.
#   4. It keeps PDF and PNG outputs.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(openxlsx)
})

# ============================================================
# Paths
# ============================================================

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

freeze_dir <- file.path(project_dir, "04_results", "final_freeze")
freeze_table_dir <- file.path(freeze_dir, "tables")
freeze_figure_dir <- file.path(freeze_dir, "figures")

package_dir <- file.path(
  project_dir,
  "07_manuscript",
  "BMC_Genomics_final_submission_package"
)

ready_dir <- file.path(package_dir, "00_READY_TO_UPLOAD")
ready_fig_dir <- file.path(ready_dir, "figures")
ready_check_dir <- file.path(ready_dir, "checks")

log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(ready_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_check_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Utilities
# ============================================================

file_exists_nonzero <- function(path) {
  file.exists(path) && isTRUE(file.info(path)$size > 0)
}

file_size_mb <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    warning("Missing file: ", path)
    return(data.frame())
  }
  data.table::fread(path, data.table = FALSE)
}

write_csv_safe <- function(x, path) {
  data.table::fwrite(x, path)
}

find_freeze_table <- function(patterns) {
  files <- list.files(freeze_table_dir, recursive = TRUE, full.names = TRUE)
  files <- files[tolower(tools::file_ext(files)) %in% c("csv", "xlsx", "xls")]
  if (length(files) == 0) return(NA_character_)
  
  lower <- tolower(basename(files))
  hit <- rep(FALSE, length(files))
  
  for (p in patterns) {
    hit <- hit | grepl(tolower(p), lower, perl = TRUE)
  }
  
  candidates <- files[hit]
  if (length(candidates) == 0) return(NA_character_)
  candidates[order(nchar(basename(candidates)))][1]
}

find_freeze_figure <- function(patterns, ext_priority = c("pdf", "png")) {
  files <- list.files(freeze_figure_dir, recursive = TRUE, full.names = TRUE)
  files <- files[tolower(tools::file_ext(files)) %in% tolower(ext_priority)]
  if (length(files) == 0) return(NA_character_)
  
  lower <- tolower(basename(files))
  hit <- rep(FALSE, length(files))
  
  for (p in patterns) {
    hit <- hit | grepl(tolower(p), lower, perl = TRUE)
  }
  
  candidates <- files[hit]
  if (length(candidates) == 0) return(NA_character_)
  
  ext_rank <- match(tolower(tools::file_ext(candidates)), tolower(ext_priority))
  ext_rank[is.na(ext_rank)] <- 999
  size_rank <- vapply(candidates, file_size_mb, numeric(1))
  
  candidates[order(ext_rank, size_rank)][1]
}

read_table_any <- function(path) {
  if (is.na(path) || !file.exists(path)) return(data.frame())
  
  ext <- tolower(tools::file_ext(path))
  
  out <- tryCatch({
    if (ext == "csv") {
      data.table::fread(path, data.table = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      sheets <- openxlsx::getSheetNames(path)
      if (length(sheets) < 1) {
        data.frame()
      } else {
        openxlsx::read.xlsx(path, sheet = sheets[1])
      }
    } else {
      data.frame()
    }
  }, error = function(e) {
    warning("Read failed: ", path, " | ", conditionMessage(e))
    data.frame()
  })
  
  as.data.frame(out)
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

round3 <- function(x) {
  ifelse(is.na(x), NA, round(as.numeric(x), 3))
}

theme_bmc <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1),
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = grid::unit(c(5, 5, 5, 5), "pt")
    )
}

panel_label <- function(label) {
  grid::textGrob(
    label,
    x = grid::unit(0.01, "npc"),
    y = grid::unit(0.98, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontface = "bold", fontsize = 13)
  )
}

with_panel_label <- function(grob, label) {
  gridExtra::arrangeGrob(
    grob,
    top = panel_label(label)
  )
}

save_composite <- function(grob, pdf_path, png_path, width = 11, height = 8.5, dpi = 300) {
  grDevices::pdf(pdf_path, width = width, height = height, onefile = TRUE)
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()
  
  grDevices::png(
    png_path,
    width = width * dpi,
    height = height * dpi,
    res = dpi
  )
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()
}

make_placeholder_plot <- function(title, message) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = message,
      size = 3.2
    ) +
    ggplot2::labs(title = title) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    theme_bmc() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

# ============================================================
# Locate frozen source tables
# ============================================================

table2_path <- find_freeze_table(c("t20_table2.*nested_lodo_validation", "nested_lodo_validation"))
table3_path <- find_freeze_table(c("t20_table3.*calibration_threshold_transport", "calibration_threshold_transport"))
table4_path <- find_freeze_table(c("t20_table4.*scrna_localization", "scrna_localization"))
gene_freq_path <- find_freeze_table(c("t14_gene_frequency", "gene_selection_frequency"))
go_path <- find_freeze_table(c("t05_go_bp", "go_bp_enrichment"))
dca_path <- find_freeze_table(c("t17_dca_summary"))
pooled_perf_path <- find_freeze_table(c("t17_pooled_perf"))
threshold_path <- find_freeze_table(c("t17_threshold"))
calibration_path <- find_freeze_table(c("t17_calibration"))
sc_cluster_path <- find_freeze_table(c("t19_scrna_cluster"))
sc_celltype_path <- find_freeze_table(c("t19_scrna_celltype"))
sc_summary_path <- find_freeze_table(c("t19_scrna_summary"))

table2 <- read_table_any(table2_path)
table3 <- read_table_any(table3_path)
table4 <- read_table_any(table4_path)
gene_freq <- read_table_any(gene_freq_path)
go_tbl <- read_table_any(go_path)
dca_tbl <- read_table_any(dca_path)
pooled_perf <- read_table_any(pooled_perf_path)
threshold_tbl <- read_table_any(threshold_path)
calibration_tbl <- read_table_any(calibration_path)
sc_cluster <- read_table_any(sc_cluster_path)
sc_celltype <- read_table_any(sc_celltype_path)
sc_summary <- read_table_any(sc_summary_path)

source_table_inventory <- data.frame(
  source_name = c(
    "table2_nested_LODO",
    "table3_calibration_threshold",
    "table4_scRNA_localization",
    "gene_frequency",
    "GO_BP_enrichment",
    "DCA_summary",
    "pooled_performance",
    "threshold_table",
    "calibration_table",
    "sc_cluster_annotation",
    "sc_celltype_score",
    "sc_summary"
  ),
  path = c(
    table2_path,
    table3_path,
    table4_path,
    gene_freq_path,
    go_path,
    dca_path,
    pooled_perf_path,
    threshold_path,
    calibration_path,
    sc_cluster_path,
    sc_celltype_path,
    sc_summary_path
  ),
  exists = file.exists(c(
    table2_path,
    table3_path,
    table4_path,
    gene_freq_path,
    go_path,
    dca_path,
    pooled_perf_path,
    threshold_path,
    calibration_path,
    sc_cluster_path,
    sc_celltype_path,
    sc_summary_path
  )),
  stringsAsFactors = FALSE
)

# ============================================================
# Figure 2: Nested LODO validation performance
# ============================================================

make_figure2 <- function() {
  if (nrow(table2) == 0) {
    p1 <- make_placeholder_plot("AUROC by held-out dataset", "T20 Table 2 not found")
    p2 <- make_placeholder_plot("AUPRC by held-out dataset", "T20 Table 2 not found")
    p3 <- make_placeholder_plot("Gene recurrence", "T14 gene frequency not found")
  } else {
    t2 <- table2
    
    if (!"validation_dataset" %in% names(t2)) {
      t2$validation_dataset <- seq_len(nrow(t2))
    }
    
    t2$AUROC <- safe_num(t2$AUROC)
    t2$AUROC_low <- safe_num(t2$AUROC_low)
    t2$AUROC_high <- safe_num(t2$AUROC_high)
    t2$AUPRC <- safe_num(t2$AUPRC)
    t2$Brier <- safe_num(t2$Brier)
    
    t2$validation_dataset <- factor(
      t2$validation_dataset,
      levels = t2$validation_dataset[order(t2$AUROC)]
    )
    
    p1 <- ggplot2::ggplot(t2, ggplot2::aes(x = validation_dataset, y = AUROC)) +
      ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = AUROC_low, ymax = AUROC_high),
        width = 0.15,
        linewidth = 0.3
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      ggplot2::labs(
        title = "Held-out discrimination",
        x = NULL,
        y = "AUROC"
      ) +
      theme_bmc()
    
    p2 <- ggplot2::ggplot(t2, ggplot2::aes(x = validation_dataset, y = AUPRC)) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      ggplot2::labs(
        title = "Precision-recall performance",
        x = NULL,
        y = "AUPRC"
      ) +
      theme_bmc()
    
    p3_base <- gene_freq
    
    if (nrow(p3_base) > 0) {
      gene_col <- intersect(
        c("gene", "gene_symbol", "Gene", "feature", "selected_gene"),
        names(p3_base)
      )[1]
      freq_col <- intersect(
        c("n_selected", "selection_count", "frequency", "n_folds", "selected_in_n_folds"),
        names(p3_base)
      )[1]
      
      if (!is.na(gene_col) && !is.na(freq_col)) {
        p3_base$gene_plot <- as.character(p3_base[[gene_col]])
        p3_base$freq_plot <- safe_num(p3_base[[freq_col]])
        p3_base <- p3_base[order(-p3_base$freq_plot), , drop = FALSE]
        p3_base <- head(p3_base, 12)
        
        p3_base$gene_plot <- factor(
          p3_base$gene_plot,
          levels = rev(p3_base$gene_plot)
        )
        
        p3 <- ggplot2::ggplot(p3_base, ggplot2::aes(x = gene_plot, y = freq_plot)) +
          ggplot2::geom_col(width = 0.65) +
          ggplot2::coord_flip() +
          ggplot2::labs(
            title = "Selected-gene recurrence",
            x = NULL,
            y = "Number of nested LODO folds"
          ) +
          theme_bmc()
      } else {
        p3 <- make_placeholder_plot("Selected-gene recurrence", "Gene-frequency columns not detected")
      }
    } else {
      p3 <- make_placeholder_plot("Selected-gene recurrence", "T14 gene-frequency table not found")
    }
  }
  
  grob <- gridExtra::arrangeGrob(
    with_panel_label(ggplotGrob(p1), "A"),
    with_panel_label(ggplotGrob(p2), "B"),
    with_panel_label(ggplotGrob(p3), "C"),
    ncol = 2,
    layout_matrix = rbind(c(1, 2), c(3, 3)),
    top = grid::textGrob(
      "Figure 2. Strict nested leave-one-dataset-out validation performance",
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    )
  )
  
  grob
}

# ============================================================
# Figure 3: Calibration and threshold transportability
# ============================================================

make_figure3 <- function() {
  if (nrow(table3) == 0) {
    p1 <- make_placeholder_plot("Observed vs mean predicted", "T20 Table 3 not found")
    p2 <- make_placeholder_plot("Calibration-in-the-large", "T20 Table 3 not found")
    p3 <- make_placeholder_plot("Threshold drift", "T20 Table 3 not found")
    p4 <- make_placeholder_plot("Fixed-threshold operating profile", "T20 Table 3 not found")
  } else {
    t3 <- table3
    
    if (!"validation_dataset" %in% names(t3)) {
      t3$validation_dataset <- seq_len(nrow(t3))
    }
    
    numeric_cols <- c(
      "observed_rate",
      "mean_predicted",
      "calibration_in_the_large",
      "Brier",
      "AUROC",
      "AUPRC",
      "fixed_training_threshold",
      "local_youden_threshold",
      "threshold_shift",
      "abs_threshold_shift",
      "fixed_sensitivity",
      "fixed_specificity",
      "local_sensitivity",
      "local_specificity"
    )
    
    for (cc in intersect(numeric_cols, names(t3))) {
      t3[[cc]] <- safe_num(t3[[cc]])
    }
    
    t3$validation_dataset <- as.character(t3$validation_dataset)
    
    p1 <- ggplot2::ggplot(
      t3,
      ggplot2::aes(x = observed_rate, y = mean_predicted, label = validation_dataset)
    ) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_text(vjust = -0.7, size = 2.5) +
      ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      ggplot2::labs(
        title = "Observed versus mean predicted risk",
        x = "Observed sepsis rate",
        y = "Mean predicted probability"
      ) +
      theme_bmc()
    
    p2 <- ggplot2::ggplot(
      t3,
      ggplot2::aes(
        x = reorder(validation_dataset, calibration_in_the_large),
        y = calibration_in_the_large
      )
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Calibration-in-the-large",
        x = NULL,
        y = "Mean predicted minus observed rate"
      ) +
      theme_bmc()
    
    p3 <- ggplot2::ggplot(
      t3,
      ggplot2::aes(
        x = reorder(validation_dataset, abs_threshold_shift),
        y = abs_threshold_shift
      )
    ) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Absolute threshold shift",
        x = NULL,
        y = "|local Youden threshold - fixed threshold|"
      ) +
      theme_bmc()
    
    op <- data.frame(
      validation_dataset = rep(t3$validation_dataset, 2),
      metric = rep(c("Fixed sensitivity", "Fixed specificity"), each = nrow(t3)),
      value = c(t3$fixed_sensitivity, t3$fixed_specificity),
      stringsAsFactors = FALSE
    )
    
    p4 <- ggplot2::ggplot(
      op,
      ggplot2::aes(x = validation_dataset, y = value, group = metric, shape = metric)
    ) +
      ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_point(size = 2) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      ggplot2::labs(
        title = "Fixed-threshold operating profile",
        x = NULL,
        y = "Sensitivity or specificity",
        shape = NULL
      ) +
      theme_bmc() +
      ggplot2::theme(legend.position = "bottom")
  }
  
  gridExtra::arrangeGrob(
    with_panel_label(ggplotGrob(p1), "A"),
    with_panel_label(ggplotGrob(p2), "B"),
    with_panel_label(ggplotGrob(p3), "C"),
    with_panel_label(ggplotGrob(p4), "D"),
    ncol = 2,
    top = grid::textGrob(
      "Figure 3. Calibration and fixed-threshold transportability across held-out cohorts",
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    )
  )
}

# ============================================================
# Figure 4: Functional enrichment
# ============================================================

standardize_go <- function(go_tbl) {
  if (nrow(go_tbl) == 0) return(go_tbl)
  
  names_lower <- tolower(names(go_tbl))
  
  term_col <- names(go_tbl)[match(TRUE, names_lower %in% c("description", "term", "go_term", "ontology_term"))]
  if (is.na(term_col)) {
    term_col <- intersect(names(go_tbl), c("Description", "Term", "ID"))[1]
  }
  
  padj_col <- names(go_tbl)[match(TRUE, names_lower %in% c("p.adjust", "padj", "adj_p", "qvalue", "q_value", "fdr"))]
  count_col <- names(go_tbl)[match(TRUE, names_lower %in% c("count", "gene_count", "n_genes"))]
  direction_col <- names(go_tbl)[match(TRUE, names_lower %in% c("direction", "regulation", "contrast", "gene_direction"))]
  
  if (is.na(term_col)) {
    go_tbl$term_plot <- paste0("Term_", seq_len(nrow(go_tbl)))
  } else {
    go_tbl$term_plot <- as.character(go_tbl[[term_col]])
  }
  
  if (!is.na(padj_col)) {
    go_tbl$padj_plot <- safe_num(go_tbl[[padj_col]])
  } else {
    go_tbl$padj_plot <- NA_real_
  }
  
  if (!is.na(count_col)) {
    go_tbl$count_plot <- safe_num(go_tbl[[count_col]])
  } else {
    go_tbl$count_plot <- NA_real_
  }
  
  if (!is.na(direction_col)) {
    go_tbl$direction_plot <- as.character(go_tbl[[direction_col]])
  } else {
    go_tbl$direction_plot <- "All"
  }
  
  go_tbl$neglog10_padj <- -log10(pmax(go_tbl$padj_plot, 1e-300))
  go_tbl$term_short <- ifelse(
    nchar(go_tbl$term_plot) > 45,
    paste0(substr(go_tbl$term_plot, 1, 42), "..."),
    go_tbl$term_plot
  )
  
  go_tbl
}

make_go_panel <- function(df, title) {
  if (nrow(df) == 0) {
    return(make_placeholder_plot(title, "GO table or matching terms not available"))
  }
  
  df <- df[order(df$padj_plot), , drop = FALSE]
  df <- head(df, 10)
  df$term_short <- factor(df$term_short, levels = rev(df$term_short))
  
  ggplot2::ggplot(df, ggplot2::aes(x = term_short, y = neglog10_padj)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = expression(-log[10]("adjusted P"))
    ) +
    theme_bmc(base_size = 8)
}

make_figure4 <- function() {
  go_std <- standardize_go(go_tbl)
  
  if (nrow(go_std) == 0) {
    p1 <- make_placeholder_plot("Upregulated biological processes", "GO enrichment table not found")
    p2 <- make_placeholder_plot("Downregulated biological processes", "GO enrichment table not found")
  } else {
    dir_lower <- tolower(go_std$direction_plot)
    
    up_idx <- grepl("up|positive|sepsis_up|upregulated", dir_lower)
    down_idx <- grepl("down|negative|control_up|downregulated", dir_lower)
    
    if (!any(up_idx) && !any(down_idx)) {
      innate_terms <- grepl(
        "myeloid|neutrophil|bacter|innate|inflamm|cytokine|leukocyte|granulocyte|defense|immune",
        tolower(go_std$term_plot)
      )
      adaptive_terms <- grepl(
        "t cell|lymphocyte|adaptive|antigen|t-cell|b cell|activation",
        tolower(go_std$term_plot)
      )
      
      up_df <- go_std[innate_terms, , drop = FALSE]
      down_df <- go_std[adaptive_terms, , drop = FALSE]
      
      if (nrow(up_df) == 0) up_df <- head(go_std[order(go_std$padj_plot), , drop = FALSE], 10)
      if (nrow(down_df) == 0) down_df <- head(go_std[order(-go_std$padj_plot), , drop = FALSE], 10)
    } else {
      up_df <- go_std[up_idx, , drop = FALSE]
      down_df <- go_std[down_idx, , drop = FALSE]
    }
    
    p1 <- make_go_panel(up_df, "Upregulated or innate/myeloid-associated processes")
    p2 <- make_go_panel(down_df, "Downregulated or adaptive immune-associated processes")
  }
  
  p3 <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0.05, xmax = 0.45, ymin = 0.55, ymax = 0.85, fill = "grey92", color = "grey30") +
    ggplot2::annotate("rect", xmin = 0.55, xmax = 0.95, ymin = 0.55, ymax = 0.85, fill = "grey92", color = "grey30") +
    ggplot2::annotate("rect", xmin = 0.30, xmax = 0.70, ymin = 0.15, ymax = 0.40, fill = "grey92", color = "grey30") +
    ggplot2::annotate("text", x = 0.25, y = 0.70, label = "Innate/myeloid\nactivation", size = 3.2) +
    ggplot2::annotate("text", x = 0.75, y = 0.70, label = "Host response\nand coagulation", size = 3.2) +
    ggplot2::annotate("text", x = 0.50, y = 0.275, label = "Bulk blood\nsepsis signature", size = 3.2) +
    ggplot2::annotate("segment", x = 0.33, xend = 0.42, y = 0.55, yend = 0.40, arrow = ggplot2::arrow(length = grid::unit(0.12, "inches"))) +
    ggplot2::annotate("segment", x = 0.67, xend = 0.58, y = 0.55, yend = 0.40, arrow = ggplot2::arrow(length = grid::unit(0.12, "inches"))) +
    ggplot2::labs(title = "Biological interpretation boundary") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    theme_bmc() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  gridExtra::arrangeGrob(
    with_panel_label(ggplotGrob(p1), "A"),
    with_panel_label(ggplotGrob(p2), "B"),
    with_panel_label(ggplotGrob(p3), "C"),
    ncol = 2,
    layout_matrix = rbind(c(1, 2), c(3, 3)),
    top = grid::textGrob(
      "Figure 4. Functional enrichment of sepsis-associated transcriptomic alterations",
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    )
  )
}

# ============================================================
# Figure 5: scRNA localization
# ============================================================

make_figure5 <- function() {
  # Panel A: broad cell type cell counts from Table 4
  if (nrow(table4) == 0) {
    p1 <- make_placeholder_plot("Cell-type composition", "T20 Table 4 not found")
    p2 <- make_placeholder_plot("Final10 module score", "T20 Table 4 not found")
    p3 <- make_placeholder_plot("Nested recurrent module score", "T20 Table 4 not found")
  } else {
    t4 <- table4
    
    celltype_col <- intersect(c("cell_type_level1", "manual_cell_type_level1", "cell_type"), names(t4))[1]
    n_col <- intersect(c("n_cells", "N_cells", "cells"), names(t4))[1]
    final_col <- intersect(c("mean_score_Final10", "Final10_score", "mean_Final10_score"), names(t4))[1]
    nested_col <- intersect(c("mean_score_NestedRecurrent", "NestedRecurrent_score", "mean_NestedRecurrent_score"), names(t4))[1]
    
    if (is.na(celltype_col)) t4$cell_type_level1 <- paste0("Type_", seq_len(nrow(t4))) else t4$cell_type_level1 <- as.character(t4[[celltype_col]])
    if (is.na(n_col)) t4$n_cells <- NA_real_ else t4$n_cells <- safe_num(t4[[n_col]])
    if (is.na(final_col)) t4$mean_score_Final10 <- NA_real_ else t4$mean_score_Final10 <- safe_num(t4[[final_col]])
    if (is.na(nested_col)) t4$mean_score_NestedRecurrent <- NA_real_ else t4$mean_score_NestedRecurrent <- safe_num(t4[[nested_col]])
    
    t4$cell_type_level1 <- factor(
      t4$cell_type_level1,
      levels = t4$cell_type_level1[order(t4$mean_score_Final10)]
    )
    
    p1 <- ggplot2::ggplot(t4, ggplot2::aes(x = cell_type_level1, y = n_cells)) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Broad cell-type composition",
        x = NULL,
        y = "Number of cells"
      ) +
      theme_bmc()
    
    p2 <- ggplot2::ggplot(t4, ggplot2::aes(x = cell_type_level1, y = mean_score_Final10)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Final10 module score",
        x = NULL,
        y = "Mean module score"
      ) +
      theme_bmc()
    
    p3 <- ggplot2::ggplot(t4, ggplot2::aes(x = cell_type_level1, y = mean_score_NestedRecurrent)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Nested recurrent module score",
        x = NULL,
        y = "Mean module score"
      ) +
      theme_bmc()
  }
  
  # Panel D: try to use a source scRNA UMAP or dotplot if present
  source_umap <- find_freeze_figure(
    c("umap_celltype", "celltype_annotation", "f19a", "umap"),
    ext_priority = c("png", "pdf")
  )
  
  if (!is.na(source_umap) && file.exists(source_umap) && tolower(tools::file_ext(source_umap)) == "png") {
    img <- tryCatch({
      png::readPNG(source_umap)
    }, error = function(e) NULL)
    
    if (!is.null(img)) {
      raster_grob <- grid::rasterGrob(img, interpolate = TRUE)
      p4_grob <- gridExtra::arrangeGrob(
        raster_grob,
        top = grid::textGrob("Representative scRNA visualization", gp = grid::gpar(fontsize = 10, fontface = "bold"))
      )
    } else {
      p4 <- make_placeholder_plot("Representative scRNA visualization", "Source image could not be read")
      p4_grob <- ggplotGrob(p4)
    }
  } else {
    p4 <- make_placeholder_plot(
      "scRNA localization boundary",
      "Module scores localize the bulk signature\nand do not constitute diagnostic validation"
    )
    p4_grob <- ggplotGrob(p4)
  }
  
  gridExtra::arrangeGrob(
    with_panel_label(ggplotGrob(p1), "A"),
    with_panel_label(ggplotGrob(p2), "B"),
    with_panel_label(ggplotGrob(p3), "C"),
    with_panel_label(p4_grob, "D"),
    ncol = 2,
    top = grid::textGrob(
      "Figure 5. Single-cell localization of the host-response signature",
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    )
  )
}

# ============================================================
# Build figures
# ============================================================

message("Building composite main figures...")

figure_manifest <- data.frame()

build_one <- function(fig_id, grob_fun, width = 11, height = 8.5) {
  pdf_path <- file.path(ready_fig_dir, paste0(fig_id, ".pdf"))
  png_path <- file.path(ready_fig_dir, paste0(fig_id, ".png"))
  
  status <- "READY_NEEDS_VISUAL_CHECK"
  err <- NA_character_
  
  tryCatch({
    grob <- grob_fun()
    save_composite(grob, pdf_path, png_path, width = width, height = height, dpi = 300)
  }, error = function(e) {
    status <<- "BUILD_FAILED"
    err <<- conditionMessage(e)
  })
  
  data.frame(
    figure = fig_id,
    pdf_path = pdf_path,
    png_path = png_path,
    pdf_exists = file_exists_nonzero(pdf_path),
    png_exists = file_exists_nonzero(png_path),
    pdf_size_mb = file_size_mb(pdf_path),
    png_size_mb = file_size_mb(png_path),
    pdf_under_10MB = ifelse(file_exists_nonzero(pdf_path), file_size_mb(pdf_path) <= 10, FALSE),
    png_under_10MB = ifelse(file_exists_nonzero(png_path), file_size_mb(png_path) <= 10, FALSE),
    status = ifelse(
      status == "BUILD_FAILED",
      "BUILD_FAILED",
      ifelse(file_exists_nonzero(pdf_path) && file_exists_nonzero(png_path), "READY_NEEDS_VISUAL_CHECK", "CHECK")
    ),
    error_message = err,
    stringsAsFactors = FALSE
  )
}

figure_manifest <- rbind(
  figure_manifest,
  build_one("Figure_2", make_figure2, width = 11, height = 8.5),
  build_one("Figure_3", make_figure3, width = 11, height = 8.5),
  build_one("Figure_4", make_figure4, width = 11, height = 8.5),
  build_one("Figure_5", make_figure5, width = 11, height = 8.5)
)

# ============================================================
# Checks
# ============================================================

required_pdf <- file.path(ready_fig_dir, paste0("Figure_", 1:5, ".pdf"))
required_png <- file.path(ready_fig_dir, paste0("Figure_", 1:5, ".png"))

checks <- data.frame(
  check_id = c(
    "C01",
    "C02",
    "C03",
    "C04",
    "C05",
    "C06",
    "C07",
    "C08",
    "C09",
    "C10"
  ),
  check_item = c(
    "Figure 1 PDF exists",
    "Figure 2 PDF exists",
    "Figure 3 PDF exists",
    "Figure 4 PDF exists",
    "Figure 5 PDF exists",
    "All Figure 1-5 PDFs exist",
    "All Figure 1-5 PNGs exist",
    "All Figure 1-5 PDFs under 10 MB",
    "No composite figure build failure",
    "Manual visual review required"
  ),
  observed = c(
    file_exists_nonzero(required_pdf[1]),
    file_exists_nonzero(required_pdf[2]),
    file_exists_nonzero(required_pdf[3]),
    file_exists_nonzero(required_pdf[4]),
    file_exists_nonzero(required_pdf[5]),
    all(vapply(required_pdf, file_exists_nonzero, logical(1))),
    all(vapply(required_png, file_exists_nonzero, logical(1))),
    all(vapply(required_pdf, function(x) file_exists_nonzero(x) && file_size_mb(x) <= 10, logical(1))),
    !any(figure_manifest$status == "BUILD_FAILED"),
    TRUE
  ),
  status = c(
    ifelse(file_exists_nonzero(required_pdf[1]), "PASS", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[2]), "PASS", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[3]), "PASS", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[4]), "PASS", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[5]), "PASS", "CHECK"),
    ifelse(all(vapply(required_pdf, file_exists_nonzero, logical(1))), "PASS", "CHECK"),
    ifelse(all(vapply(required_png, file_exists_nonzero, logical(1))), "PASS", "CHECK"),
    ifelse(all(vapply(required_pdf, function(x) file_exists_nonzero(x) && file_size_mb(x) <= 10, logical(1))), "PASS", "CHECK"),
    ifelse(!any(figure_manifest$status == "BUILD_FAILED"), "PASS", "CHECK"),
    "MANUAL_REQUIRED"
  ),
  stringsAsFactors = FALSE
)

n_failed_checks <- sum(checks$status == "CHECK", na.rm = TRUE)

overall_status <- data.frame(
  metric = c(
    "n_composite_figures_rebuilt",
    "n_failed_checks",
    "figure_1_exists",
    "figure_2_exists",
    "figure_3_exists",
    "figure_4_exists",
    "figure_5_exists",
    "ready_for_submission_figure_upload",
    "recommended_next_step"
  ),
  value = c(
    nrow(figure_manifest),
    n_failed_checks,
    ifelse(file_exists_nonzero(required_pdf[1]), "YES", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[2]), "YES", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[3]), "YES", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[4]), "YES", "CHECK"),
    ifelse(file_exists_nonzero(required_pdf[5]), "YES", "CHECK"),
    ifelse(n_failed_checks == 0, "YES_AFTER_MANUAL_VISUAL_REVIEW", "NO_FIX_ITEMS_FIRST"),
    ifelse(
      n_failed_checks == 0,
      "Open Figure_1 to Figure_5 PDFs manually. If readable and consistent with captions, upload PDFs as main figures.",
      "Fix failed figure checks and rerun Step 37."
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Write outputs
# ============================================================

manifest_path <- file.path(ready_check_dir, "BMC_Genomics_composite_main_figure_manifest_v1.2.csv")
checks_path <- file.path(ready_check_dir, "BMC_Genomics_composite_main_figure_checks_v1.2.csv")
overall_path <- file.path(ready_check_dir, "BMC_Genomics_composite_main_figure_overall_status_v1.2.csv")
source_inventory_path <- file.path(ready_check_dir, "BMC_Genomics_composite_main_figure_source_table_inventory_v1.2.csv")
xlsx_path <- file.path(ready_dir, "BMC_Genomics_composite_main_figure_manifest_v1.2.xlsx")

write_csv_safe(figure_manifest, manifest_path)
write_csv_safe(checks, checks_path)
write_csv_safe(overall_status, overall_path)
write_csv_safe(source_table_inventory, source_inventory_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "figure_manifest")
openxlsx::writeData(wb, "figure_manifest", figure_manifest)

openxlsx::addWorksheet(wb, "checks")
openxlsx::writeData(wb, "checks", checks)

openxlsx::addWorksheet(wb, "source_tables")
openxlsx::writeData(wb, "source_tables", source_table_inventory)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

readme_path <- file.path(ready_dir, "README_STEP37_COMPOSITE_MAIN_FIGURES.md")

readme_lines <- c(
  "# Step 37 composite main figures",
  "",
  "This step rebuilds Figure 2 to Figure 5 as composite main figures.",
  "",
  "Primary upload files:",
  "",
  "- figures/Figure_1.pdf",
  "- figures/Figure_2.pdf",
  "- figures/Figure_3.pdf",
  "- figures/Figure_4.pdf",
  "- figures/Figure_5.pdf",
  "",
  "PNG versions are also exported for visual checking.",
  "",
  "Manual visual checks before submission:",
  "",
  "1. Confirm all panel labels are visible.",
  "2. Confirm text is readable at normal zoom.",
  "3. Confirm each figure matches the figure legend in the manuscript.",
  "4. Confirm Figure 2 shows nested LODO performance.",
  "5. Confirm Figure 3 shows calibration and threshold transportability.",
  "6. Confirm Figure 4 does not overstate mechanistic proof.",
  "7. Confirm Figure 5 states biological localization only."
)

writeLines(readme_lines, con = readme_path, useBytes = TRUE)

sink(file.path(log_dir, "sessionInfo_37_BMC_Genomics_composite_main_figures_builder.txt"))
print(sessionInfo())
sink()

# ============================================================
# Console output
# ============================================================

message("\n============ 37 BMC Genomics composite main figures builder 完成 ============")
message("READY_TO_UPLOAD 目录：", ready_dir)

message("\nOverall status:")
print(overall_status)

message("\nFigure manifest:")
print(figure_manifest)

message("\nFinal checks:")
print(checks)

message("\nSource table inventory:")
print(source_table_inventory)

message("\n关键输出：")
message("1) ", file.path(ready_fig_dir, "Figure_1.pdf"))
message("2) ", file.path(ready_fig_dir, "Figure_2.pdf"))
message("3) ", file.path(ready_fig_dir, "Figure_3.pdf"))
message("4) ", file.path(ready_fig_dir, "Figure_4.pdf"))
message("5) ", file.path(ready_fig_dir, "Figure_5.pdf"))
message("6) ", manifest_path)
message("7) ", checks_path)
message("8) ", overall_path)
message("9) ", xlsx_path)
message("10) ", readme_path)

message("\n下一步：")
message("把 Overall status、Figure manifest、Final checks、Source table inventory 贴给我。")
message("我会判断这 5 张主图是否可以直接上传，还是还需要针对某一张继续精修。")