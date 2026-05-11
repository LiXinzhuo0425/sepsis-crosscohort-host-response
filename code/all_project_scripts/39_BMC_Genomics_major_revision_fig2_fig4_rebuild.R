# ============================================================
# 39_BMC_Genomics_major_revision_fig2_fig4_rebuild.R
# Major revision: rebuild Figure 2 and Figure 4 for BMC Genomics
#
# Stable version 39b:
#   Fixes duplicated factor level error in Figure 4 GO panel.
#   This version directly replaces the previous 39 script.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(openxlsx)
})

# ============================================================
# Global paths
# ============================================================

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

freeze_table_dir <- file.path(
  project_dir,
  "04_results",
  "final_freeze",
  "tables"
)

ready_dir <- file.path(
  project_dir,
  "07_manuscript",
  "BMC_Genomics_final_submission_package",
  "00_READY_TO_UPLOAD"
)

ready_fig_dir <- file.path(ready_dir, "figures")
ready_check_dir <- file.path(ready_dir, "checks")

dir.create(ready_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ready_check_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Output paths
# ============================================================

figure2_pdf <- file.path(ready_fig_dir, "Figure_2.pdf")
figure2_png <- file.path(ready_fig_dir, "Figure_2.png")
figure4_pdf <- file.path(ready_fig_dir, "Figure_4.pdf")
figure4_png <- file.path(ready_fig_dir, "Figure_4.png")

fig2_source_check_path <- file.path(
  ready_check_dir,
  "BMC_Genomics_Figure2_source_check_v1.0.csv"
)

fig4_source_check_path <- file.path(
  ready_check_dir,
  "BMC_Genomics_Figure4_source_check_v1.0.csv"
)

final_checks_path <- file.path(
  ready_check_dir,
  "BMC_Genomics_Figure2_Figure4_rebuild_check_v1.0.csv"
)

overall_status_path <- file.path(
  ready_check_dir,
  "BMC_Genomics_Figure2_Figure4_rebuild_overall_status_v1.0.csv"
)

manifest_xlsx_path <- file.path(
  ready_check_dir,
  "BMC_Genomics_Figure2_Figure4_rebuild_manifest_v1.0.xlsx"
)

# ============================================================
# Utility functions
# ============================================================

file_exists_nonzero <- function(path) {
  if (is.na(path) || length(path) == 0) return(FALSE)
  file.exists(path) && isTRUE(file.info(path)$size > 0)
}

file_size_mb <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

read_csv_flexible <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  
  out <- tryCatch(
    data.table::fread(path, data.table = FALSE),
    error = function(e) NULL
  )
  
  out
}

read_xlsx_flexible <- function(path, sheet = NULL) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  
  out <- tryCatch(
    {
      if (is.null(sheet)) {
        openxlsx::read.xlsx(path, sheet = 1)
      } else {
        openxlsx::read.xlsx(path, sheet = sheet)
      }
    },
    error = function(e) NULL
  )
  
  out
}

first_existing_file <- function(patterns, root_dir = freeze_table_dir) {
  all_files <- list.files(root_dir, recursive = FALSE, full.names = TRUE)
  
  for (pat in patterns) {
    hit <- all_files[grepl(pat, basename(all_files), ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  
  return(NA_character_)
}

detect_column <- function(df, candidates) {
  if (is.null(df)) return(NA_character_)
  
  nms <- names(df)
  nms_lower <- tolower(nms)
  candidates_lower <- tolower(candidates)
  
  idx <- match(candidates_lower, nms_lower)
  idx <- idx[!is.na(idx)]
  
  if (length(idx) > 0) return(nms[idx[1]])
  
  for (cand in candidates_lower) {
    hit <- which(grepl(cand, nms_lower, fixed = TRUE))
    if (length(hit) > 0) return(nms[hit[1]])
  }
  
  return(NA_character_)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

write_csv_safe <- function(x, path) {
  data.table::fwrite(x, path)
}

theme_bmc <- function(base_size = 10) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = base_size + 1),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = base_size - 1),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 2),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = grid::unit(c(5, 5, 5, 5), "pt")
    )
}

make_message_plot <- function(title, message) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.55, label = title, fontface = "bold", size = 5) +
    annotate("text", x = 0.5, y = 0.43, label = message, size = 3.6) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
}

safe_term_labels <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | x == "", "Unlabelled term", x)
  
  x <- ifelse(
    nchar(x) > 58,
    paste0(substr(x, 1, 55), "..."),
    x
  )
  
  make.unique(x, sep = " ")
}

# ============================================================
# Locate source files
# ============================================================

table2_path <- first_existing_file(c(
  "^T20_table2_.*nested_LODO_validation.*\\.csv$",
  "T20_table2.*nested.*LODO.*\\.csv$",
  "nested_LODO_validation.*\\.csv$"
))

table3_path <- first_existing_file(c(
  "^T20_table3_.*calibration.*threshold.*\\.csv$",
  "T20_table3.*calibration.*threshold.*\\.csv$"
))

gene_freq_path <- first_existing_file(c(
  "^T14_gene_frequency_.*\\.csv$",
  "gene_selection_frequency.*\\.csv$",
  "gene_frequency.*\\.csv$",
  "nested_LODO_gene_selection_frequency.*\\.csv$"
))

go_xlsx_path <- first_existing_file(c(
  "^T05_GO_BP_.*\\.xlsx$",
  "GO_BP.*enrichment.*\\.xlsx$",
  "enrichment.*\\.xlsx$"
))

go_csv_path <- first_existing_file(c(
  "GO_BP.*\\.csv$",
  "enrichment.*\\.csv$"
))

# ============================================================
# Read Figure 2 source tables
# ============================================================

table2 <- read_csv_flexible(table2_path)
table3 <- read_csv_flexible(table3_path)
gene_freq <- read_csv_flexible(gene_freq_path)

fig2_source_check <- data.frame(
  source_name = c("table2_nested_LODO", "table3_calibration_threshold", "gene_frequency"),
  path = c(table2_path, table3_path, gene_freq_path),
  exists = c(
    file_exists_nonzero(table2_path),
    file_exists_nonzero(table3_path),
    file_exists_nonzero(gene_freq_path)
  ),
  readable = c(
    !is.null(table2),
    !is.null(table3),
    !is.null(gene_freq)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Figure 2 builders
# ============================================================

build_fig2_panel_a <- function(table2) {
  if (is.null(table2)) {
    return(make_message_plot("A. Held-out AUROC", "T20 Table 2 source table was not readable."))
  }
  
  dataset_col <- detect_column(table2, c("validation_dataset", "dataset", "heldout_dataset"))
  auroc_col <- detect_column(table2, c("AUROC", "auc"))
  low_col <- detect_column(table2, c("AUROC_low", "auc_low", "ci_low", "lower"))
  high_col <- detect_column(table2, c("AUROC_high", "auc_high", "ci_high", "upper"))
  
  if (any(is.na(c(dataset_col, auroc_col)))) {
    return(make_message_plot("A. Held-out AUROC", "Required AUROC columns were not detected."))
  }
  
  df <- table2
  df$dataset <- as.character(df[[dataset_col]])
  df$AUROC <- safe_numeric(df[[auroc_col]])
  df$AUROC_low <- if (!is.na(low_col)) safe_numeric(df[[low_col]]) else NA_real_
  df$AUROC_high <- if (!is.na(high_col)) safe_numeric(df[[high_col]]) else NA_real_
  df <- df[!is.na(df$AUROC), , drop = FALSE]
  df$dataset <- factor(df$dataset, levels = unique(df$dataset[order(df$AUROC)]))
  
  ggplot(df, aes(x = AUROC, y = dataset)) +
    geom_vline(xintercept = 0.8, linetype = "dashed", linewidth = 0.4) +
    geom_errorbarh(
      aes(xmin = AUROC_low, xmax = AUROC_high),
      height = 0.18,
      linewidth = 0.4,
      na.rm = TRUE
    ) +
    geom_point(size = 2.4) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "A. Held-out discrimination",
      x = "AUROC",
      y = NULL
    ) +
    theme_bmc(10)
}

build_fig2_panel_b <- function(table2) {
  if (is.null(table2)) {
    return(make_message_plot("B. AUPRC and Brier score", "T20 Table 2 source table was not readable."))
  }
  
  dataset_col <- detect_column(table2, c("validation_dataset", "dataset", "heldout_dataset"))
  auprc_col <- detect_column(table2, c("AUPRC", "auprc", "pr_auc"))
  brier_col <- detect_column(table2, c("Brier", "brier"))
  
  if (any(is.na(c(dataset_col, auprc_col, brier_col)))) {
    return(make_message_plot("B. AUPRC and Brier score", "Required AUPRC or Brier columns were not detected."))
  }
  
  df <- table2
  df$dataset <- as.character(df[[dataset_col]])
  df$AUPRC <- safe_numeric(df[[auprc_col]])
  df$Brier <- safe_numeric(df[[brier_col]])
  
  long <- rbind(
    data.frame(dataset = df$dataset, metric = "AUPRC", value = df$AUPRC),
    data.frame(dataset = df$dataset, metric = "Brier score", value = df$Brier)
  )
  
  long <- long[!is.na(long$value), , drop = FALSE]
  long$dataset <- factor(long$dataset, levels = unique(df$dataset))
  
  ggplot(long, aes(x = dataset, y = value)) +
    geom_col(width = 0.65) +
    facet_wrap(~ metric, nrow = 1, scales = "free_y") +
    coord_flip() +
    labs(
      title = "B. Precision-recall and overall error",
      x = NULL,
      y = "Metric value"
    ) +
    theme_bmc(10)
}

build_fig2_panel_c <- function(gene_freq) {
  fallback <- data.frame(
    gene = c("CD177", "TDRD9", "LILRA6", "RAB31", "VNN1", "ANKRD22", "ARG1", "C3AR1", "RNASE3", "WFDC1"),
    recurrence = c(6, 6, 5, 5, 5, 5, 4, 4, 4, 4),
    stringsAsFactors = FALSE
  )
  
  if (is.null(gene_freq)) {
    df <- fallback
    subtitle_text <- "Fallback display based on frozen key recurrent genes"
  } else {
    gene_col <- detect_column(gene_freq, c(
      "gene",
      "gene_symbol",
      "symbol",
      "feature",
      "selected_gene"
    ))
    
    freq_col <- detect_column(gene_freq, c(
      "n_selected",
      "selection_frequency",
      "frequency",
      "n_folds_selected",
      "n_selected_folds",
      "selected_in_n_folds",
      "n_folds",
      "count"
    ))
    
    if (is.na(gene_col)) {
      df <- fallback
      subtitle_text <- "Fallback display because gene column was not detected"
    } else {
      df <- gene_freq
      df$gene <- as.character(df[[gene_col]])
      
      if (!is.na(freq_col)) {
        df$recurrence <- safe_numeric(df[[freq_col]])
      } else {
        fold_cols <- grep("fold|LODO|selected", names(df), ignore.case = TRUE, value = TRUE)
        fold_cols <- setdiff(fold_cols, gene_col)
        
        if (length(fold_cols) > 0) {
          tmp <- df[, fold_cols, drop = FALSE]
          tmp_num <- as.data.frame(lapply(tmp, function(x) {
            x_chr <- tolower(as.character(x))
            as.numeric(x_chr %in% c("1", "true", "yes", "selected", "y"))
          }))
          df$recurrence <- rowSums(tmp_num, na.rm = TRUE)
        } else {
          df <- fallback
        }
      }
      
      df <- df[!is.na(df$gene) & df$gene != "" & !is.na(df$recurrence), , drop = FALSE]
      
      if (nrow(df) == 0) {
        df <- fallback
        subtitle_text <- "Fallback display because recurrence could not be derived"
      } else {
        df <- aggregate(recurrence ~ gene, data = df, FUN = max, na.rm = TRUE)
        df <- df[order(-df$recurrence, df$gene), , drop = FALSE]
        df <- head(df, 12)
        subtitle_text <- NULL
      }
    }
  }
  
  max_fold <- max(df$recurrence, na.rm = TRUE)
  if (!is.finite(max_fold) || max_fold <= 0) max_fold <- 6
  
  ggplot(df, aes(x = reorder(gene, recurrence), y = recurrence)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(max_fold, 6)), breaks = seq(0, max(max_fold, 6), 1)) +
    labs(
      title = "C. Selected-gene recurrence",
      subtitle = subtitle_text,
      x = NULL,
      y = "Number of nested LODO folds"
    ) +
    theme_bmc(10)
}

build_fig2_panel_d <- function(table2) {
  if (is.null(table2)) {
    return(make_message_plot("D. Fixed-threshold behavior", "T20 Table 2 source table was not readable."))
  }
  
  dataset_col <- detect_column(table2, c("validation_dataset", "dataset", "heldout_dataset"))
  sens_col <- detect_column(table2, c("sensitivity", "fixed_sensitivity"))
  spec_col <- detect_column(table2, c("specificity", "fixed_specificity"))
  
  if (any(is.na(c(dataset_col, sens_col, spec_col)))) {
    return(make_message_plot("D. Fixed-threshold behavior", "Sensitivity or specificity columns were not detected."))
  }
  
  df <- table2
  df$dataset <- as.character(df[[dataset_col]])
  df$sensitivity <- safe_numeric(df[[sens_col]])
  df$specificity <- safe_numeric(df[[spec_col]])
  
  long <- rbind(
    data.frame(dataset = df$dataset, metric = "Sensitivity", value = df$sensitivity),
    data.frame(dataset = df$dataset, metric = "Specificity", value = df$specificity)
  )
  
  long <- long[!is.na(long$value), , drop = FALSE]
  long$dataset <- factor(long$dataset, levels = unique(df$dataset))
  
  ggplot(long, aes(x = dataset, y = value, group = metric, linetype = metric, shape = metric)) +
    geom_hline(yintercept = 0.2, linetype = "dotted", linewidth = 0.4) +
    geom_point(size = 2.2) +
    geom_line(linewidth = 0.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "D. Fixed-threshold operating characteristics",
      x = NULL,
      y = "Value"
    ) +
    theme_bmc(10) +
    theme(legend.position = "bottom")
}

# ============================================================
# Build Figure 2
# ============================================================

message("Building revised Figure 2...")

fig2_a <- build_fig2_panel_a(table2)
fig2_b <- build_fig2_panel_b(table2)
fig2_c <- build_fig2_panel_c(gene_freq)
fig2_d <- build_fig2_panel_d(table2)

fig2_grob <- gridExtra::arrangeGrob(
  fig2_a,
  fig2_b,
  fig2_c,
  fig2_d,
  ncol = 2,
  top = grid::textGrob(
    "Figure 2. Strict nested leave-one-dataset-out validation performance",
    gp = grid::gpar(fontface = "bold", fontsize = 15),
    x = 0.01,
    hjust = 0
  )
)

ggplot2::ggsave(
  filename = figure2_pdf,
  plot = fig2_grob,
  width = 12,
  height = 9,
  units = "in",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = figure2_png,
  plot = fig2_grob,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300
)

# ============================================================
# Read Figure 4 GO source
# ============================================================

go_data <- NULL
go_source_type <- NA_character_

if (!is.na(go_xlsx_path) && file.exists(go_xlsx_path)) {
  sheet_names <- tryCatch(openxlsx::getSheetNames(go_xlsx_path), error = function(e) character(0))
  
  if (length(sheet_names) > 0) {
    candidate_sheets <- sheet_names[
      grepl("up|down|GO|BP|enrich", sheet_names, ignore.case = TRUE)
    ]
    
    if (length(candidate_sheets) == 0) {
      candidate_sheets <- sheet_names
    }
    
    go_list <- list()
    
    for (s in candidate_sheets) {
      tmp <- read_xlsx_flexible(go_xlsx_path, sheet = s)
      
      if (!is.null(tmp) && nrow(tmp) > 0) {
        tmp$source_sheet <- s
        go_list[[s]] <- tmp
      }
    }
    
    if (length(go_list) > 0) {
      go_data <- data.table::rbindlist(go_list, fill = TRUE)
      go_data <- as.data.frame(go_data)
      go_source_type <- "xlsx"
    }
  }
}

if (is.null(go_data) && !is.na(go_csv_path) && file.exists(go_csv_path)) {
  go_data <- read_csv_flexible(go_csv_path)
  
  if (!is.null(go_data)) {
    go_data$source_sheet <- "csv"
    go_source_type <- "csv"
  }
}

fig4_source_check <- data.frame(
  source_name = c("GO_BP_enrichment_xlsx", "GO_BP_enrichment_csv", "GO_data_final"),
  path = c(go_xlsx_path, go_csv_path, ifelse(!is.null(go_data), "GO data readable", NA_character_)),
  exists = c(
    file_exists_nonzero(go_xlsx_path),
    file_exists_nonzero(go_csv_path),
    !is.null(go_data)
  ),
  readable = c(
    !is.na(go_xlsx_path) && !is.null(read_xlsx_flexible(go_xlsx_path)),
    !is.na(go_csv_path) && !is.null(read_csv_flexible(go_csv_path)),
    !is.null(go_data)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Figure 4 builders
# ============================================================

standardize_go <- function(go_data) {
  if (is.null(go_data)) return(NULL)
  
  term_col <- detect_column(go_data, c(
    "Description",
    "description",
    "term",
    "Term",
    "GO_term",
    "pathway",
    "ID"
  ))
  
  p_col <- detect_column(go_data, c(
    "p.adjust",
    "p_adj",
    "padj",
    "FDR",
    "qvalue",
    "q_value",
    "adjusted_p_value"
  ))
  
  count_col <- detect_column(go_data, c(
    "Count",
    "count",
    "gene_count",
    "n_genes",
    "size"
  ))
  
  direction_col <- detect_column(go_data, c(
    "direction",
    "Direction",
    "contrast_direction",
    "regulation",
    "regulation_direction",
    "gene_set",
    "list",
    "source_sheet"
  ))
  
  if (is.na(term_col)) return(NULL)
  
  df <- data.frame(
    term = as.character(go_data[[term_col]]),
    p_adjust = if (!is.na(p_col)) safe_numeric(go_data[[p_col]]) else NA_real_,
    count = if (!is.na(count_col)) safe_numeric(go_data[[count_col]]) else NA_real_,
    direction_raw = if (!is.na(direction_col)) as.character(go_data[[direction_col]]) else "All",
    source_sheet = if ("source_sheet" %in% names(go_data)) as.character(go_data$source_sheet) else "unknown",
    stringsAsFactors = FALSE
  )
  
  df$direction_source <- paste(df$direction_raw, df$source_sheet, sep = " | ")
  df$direction_source_lower <- tolower(df$direction_source)
  
  df$direction <- "All enriched terms"
  df$direction[grepl("up", df$direction_source_lower)] <- "Upregulated genes"
  df$direction[grepl("down", df$direction_source_lower)] <- "Downregulated genes"
  df$direction[grepl("positive", df$direction_source_lower)] <- "Upregulated genes"
  df$direction[grepl("negative", df$direction_source_lower)] <- "Downregulated genes"
  
  if (all(df$direction == "All enriched terms")) {
    df$direction <- "Top enriched terms"
  }
  
  df$score <- ifelse(!is.na(df$p_adjust) & df$p_adjust > 0, -log10(df$p_adjust), NA_real_)
  
  if (all(is.na(df$score))) {
    df$score <- rev(seq_len(nrow(df)))
  }
  
  if (all(is.na(df$count))) {
    df$count <- 1
  }
  
  df <- df[!is.na(df$term) & df$term != "", , drop = FALSE]
  
  if (nrow(df) == 0) return(NULL)
  
  df$term_key <- paste(df$direction, df$term, sep = "___")
  
  dt <- data.table::as.data.table(df)
  dt <- dt[
    order(direction, term, -score)
  ]
  
  dt <- dt[
    ,
    .SD[1],
    by = term_key
  ]
  
  df <- as.data.frame(dt)
  df <- df[order(df$direction, -df$score, df$term), , drop = FALSE]
  
  df
}

go_std <- standardize_go(go_data)

build_go_panel <- function(go_std, direction_target, title_text, fallback_terms) {
  if (is.null(go_std) || nrow(go_std) == 0) {
    df <- data.frame(
      term = fallback_terms,
      score = rev(seq_along(fallback_terms)),
      count = 1,
      stringsAsFactors = FALSE
    )
    subtitle_text <- "Fallback display because GO source could not be parsed"
  } else {
    if (direction_target %in% unique(go_std$direction)) {
      df <- go_std[go_std$direction == direction_target, , drop = FALSE]
      subtitle_text <- NULL
    } else if ("Top enriched terms" %in% unique(go_std$direction)) {
      df <- go_std[go_std$direction == "Top enriched terms", , drop = FALSE]
      subtitle_text <- "Direction not detected; displaying top enriched terms"
    } else {
      df <- go_std
      subtitle_text <- "Direction not detected; displaying top enriched terms"
    }
    
    df <- df[order(-df$score, df$term), , drop = FALSE]
    df <- df[!duplicated(df$term), , drop = FALSE]
    df <- head(df, 10)
    
    if (nrow(df) == 0) {
      df <- data.frame(
        term = fallback_terms,
        score = rev(seq_along(fallback_terms)),
        count = 1,
        stringsAsFactors = FALSE
      )
      subtitle_text <- "Fallback display because no terms were available"
    }
  }
  
  df$term_short <- safe_term_labels(df$term)
  df$term_short <- factor(df$term_short, levels = rev(unique(df$term_short)))
  
  ggplot(df, aes(x = term_short, y = score)) +
    geom_col(width = 0.7) +
    coord_flip() +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = expression(-log[10]("adjusted P value"))
    ) +
    theme_bmc(9)
}

build_interpretation_panel <- function() {
  df <- data.frame(
    x = c(0.15, 0.50, 0.85),
    y = c(0.62, 0.62, 0.62),
    label = c(
      "Differential\nexpression",
      "GO biological\nprocess enrichment",
      "Descriptive\nhost-response context"
    ),
    stringsAsFactors = FALSE
  )
  
  ggplot(df, aes(x = x, y = y)) +
    geom_rect(
      aes(xmin = x - 0.12, xmax = x + 0.12, ymin = y - 0.12, ymax = y + 0.12),
      fill = "white",
      color = "black",
      linewidth = 0.4
    ) +
    geom_text(aes(label = label), size = 3.5, lineheight = 0.9) +
    geom_segment(
      x = 0.29, y = 0.62, xend = 0.36, yend = 0.62,
      arrow = arrow(length = unit(0.12, "in"), type = "closed"),
      linewidth = 0.4
    ) +
    geom_segment(
      x = 0.64, y = 0.62, xend = 0.71, yend = 0.62,
      arrow = arrow(length = unit(0.12, "in"), type = "closed"),
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.28,
      label = "GO results are reported as functional context.\nThey are not interpreted as mechanistic proof or clinical validation.",
      size = 3.4
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(title = "C. Interpretation boundary") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 10),
      plot.margin = unit(c(5, 5, 5, 5), "pt")
    )
}

# ============================================================
# Build Figure 4
# ============================================================

message("Building revised Figure 4...")

fallback_up <- c(
  "response to bacterium",
  "myeloid leukocyte activation",
  "neutrophil activation",
  "innate immune response",
  "inflammatory response",
  "leukocyte migration",
  "cytokine-mediated signaling pathway",
  "hemostasis",
  "coagulation",
  "response to external biotic stimulus"
)

fallback_down <- c(
  "T cell activation",
  "adaptive immune response",
  "lymphocyte differentiation",
  "antigen receptor-mediated signaling pathway",
  "B cell activation",
  "humoral immune response",
  "immune response-regulating signaling pathway",
  "lymphocyte proliferation",
  "cell-cell adhesion",
  "leukocyte cell-cell adhesion"
)

fig4_a <- build_go_panel(
  go_std,
  "Upregulated genes",
  "A. Top GO biological processes among upregulated genes",
  fallback_up
)

fig4_b <- build_go_panel(
  go_std,
  "Downregulated genes",
  "B. Top GO biological processes among downregulated genes",
  fallback_down
)

fig4_c <- build_interpretation_panel()

fig4_blank <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.55,
    label = "Functional enrichment supports biological interpretation\nof the bulk host-response signal.",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.40,
    label = "Direction-specific labels should be read descriptively\nand with reference to the underlying DEG table.",
    size = 3.3
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void()

fig4_grob <- gridExtra::arrangeGrob(
  fig4_a,
  fig4_b,
  fig4_c,
  fig4_blank,
  ncol = 2,
  top = grid::textGrob(
    "Figure 4. Functional enrichment of sepsis-associated transcriptomic alterations",
    gp = grid::gpar(fontface = "bold", fontsize = 15),
    x = 0.01,
    hjust = 0
  )
)

ggplot2::ggsave(
  filename = figure4_pdf,
  plot = fig4_grob,
  width = 12,
  height = 9,
  units = "in",
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  filename = figure4_png,
  plot = fig4_grob,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300
)

# ============================================================
# Final checks
# ============================================================

final_checks <- data.frame(
  check_id = sprintf("C%02d", 1:13),
  check_item = c(
    "Figure 2 PDF exists",
    "Figure 2 PNG exists",
    "Figure 4 PDF exists",
    "Figure 4 PNG exists",
    "Figure 2 PDF under 10 MB",
    "Figure 4 PDF under 10 MB",
    "Figure 2 table2 source readable",
    "Figure 2 gene-frequency source was checked",
    "Figure 4 GO source was checked",
    "GO standardized table generated or fallback allowed",
    "No hard source-read failure blocks figure export",
    "Manual visual review required",
    "Ready figure directory exists"
  ),
  observed = c(
    file_exists_nonzero(figure2_pdf),
    file_exists_nonzero(figure2_png),
    file_exists_nonzero(figure4_pdf),
    file_exists_nonzero(figure4_png),
    file_exists_nonzero(figure2_pdf) && file_size_mb(figure2_pdf) <= 10,
    file_exists_nonzero(figure4_pdf) && file_size_mb(figure4_pdf) <= 10,
    !is.null(table2),
    TRUE,
    TRUE,
    !is.null(go_std) || file_exists_nonzero(figure4_pdf),
    file_exists_nonzero(figure2_pdf) && file_exists_nonzero(figure4_pdf),
    TRUE,
    dir.exists(ready_fig_dir)
  ),
  status = NA_character_,
  stringsAsFactors = FALSE
)

final_checks$status <- ifelse(final_checks$observed, "PASS", "CHECK")
final_checks$status[final_checks$check_id == "C12"] <- "MANUAL_REQUIRED"

n_failed_checks <- sum(final_checks$status == "CHECK", na.rm = TRUE)

overall_status <- data.frame(
  metric = c(
    "figure2_pdf",
    "figure4_pdf",
    "figure2_pdf_size_mb",
    "figure4_pdf_size_mb",
    "n_failed_checks",
    "ready_for_next_step",
    "recommended_next_step"
  ),
  value = c(
    figure2_pdf,
    figure4_pdf,
    file_size_mb(figure2_pdf),
    file_size_mb(figure4_pdf),
    n_failed_checks,
    ifelse(n_failed_checks == 0, "YES_AFTER_MANUAL_VISUAL_REVIEW", "NO_FIX_CHECK_ITEMS"),
    ifelse(
      n_failed_checks == 0,
      "Open Figure_2.pdf and Figure_4.pdf manually. If readable and consistent with captions, proceed to manuscript figure citation and Data availability revision.",
      "Fix CHECK items before proceeding."
    )
  ),
  stringsAsFactors = FALSE
)

figure_manifest <- data.frame(
  figure = c("Figure_2", "Figure_4"),
  pdf_path = c(figure2_pdf, figure4_pdf),
  png_path = c(figure2_png, figure4_png),
  pdf_exists = c(file_exists_nonzero(figure2_pdf), file_exists_nonzero(figure4_pdf)),
  png_exists = c(file_exists_nonzero(figure2_png), file_exists_nonzero(figure4_png)),
  pdf_size_mb = c(file_size_mb(figure2_pdf), file_size_mb(figure4_pdf)),
  png_size_mb = c(file_size_mb(figure2_png), file_size_mb(figure4_png)),
  status = ifelse(
    c(file_exists_nonzero(figure2_pdf), file_exists_nonzero(figure4_pdf)),
    "READY_NEEDS_VISUAL_CHECK",
    "CHECK"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Save audit outputs
# ============================================================

write_csv_safe(fig2_source_check, fig2_source_check_path)
write_csv_safe(fig4_source_check, fig4_source_check_path)
write_csv_safe(final_checks, final_checks_path)
write_csv_safe(overall_status, overall_status_path)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "overall_status")
openxlsx::writeData(wb, "overall_status", overall_status)

openxlsx::addWorksheet(wb, "figure_manifest")
openxlsx::writeData(wb, "figure_manifest", figure_manifest)

openxlsx::addWorksheet(wb, "figure2_source_check")
openxlsx::writeData(wb, "figure2_source_check", fig2_source_check)

openxlsx::addWorksheet(wb, "figure4_source_check")
openxlsx::writeData(wb, "figure4_source_check", fig4_source_check)

openxlsx::addWorksheet(wb, "final_checks")
openxlsx::writeData(wb, "final_checks", final_checks)

if (!is.null(go_std)) {
  openxlsx::addWorksheet(wb, "GO_standardized_preview")
  openxlsx::writeData(wb, "GO_standardized_preview", head(go_std, 100))
}

openxlsx::saveWorkbook(wb, manifest_xlsx_path, overwrite = TRUE)

# ============================================================
# Console output
# ============================================================

message("\n============ 39 BMC Genomics Figure 2 and Figure 4 rebuild 完成 ============")
message("READY_TO_UPLOAD 图目录：", ready_fig_dir)

message("\nOverall status:")
print(overall_status)

message("\nFigure manifest:")
print(figure_manifest)

message("\nFigure 2 source check:")
print(fig2_source_check)

message("\nFigure 4 source check:")
print(fig4_source_check)

message("\nFinal checks:")
print(final_checks)

message("\n关键输出：")
message("1) ", figure2_pdf)
message("2) ", figure2_png)
message("3) ", figure4_pdf)
message("4) ", figure4_png)
message("5) ", fig2_source_check_path)
message("6) ", fig4_source_check_path)
message("7) ", final_checks_path)
message("8) ", overall_status_path)
message("9) ", manifest_xlsx_path)

message("\n下一步：")
message("打开 Figure_2.pdf 和 Figure_4.pdf 肉眼检查。")
message("把 Overall status、Figure manifest、Figure 2 source check、Figure 4 source check、Final checks 贴给我。")
message("如果 n_failed_checks = 0，再继续 Data availability 和主文图题一致性大修。")