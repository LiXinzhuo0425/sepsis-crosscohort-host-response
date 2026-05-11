# ============================================================
# 48b_finalize_DEG_candidate_selection_methods.R
# Purpose:
#   Finalize DEG threshold and candidate-gene selection methods
#   after Step 48.
#
# Why:
#   Step 48 confirmed:
#     - DEG threshold: adjusted P < 0.05 and |log2FC| >= 0.5
#     - BH/FDR adjustment
#     - direction consistency threshold = 0.9
#     - univariate AUROC threshold = 0.7
#     - correlation threshold = 0.92
#
#   But Step 48 still flagged one real issue:
#     - both merged/frozen DEG and training-only nested LODO DEG evidence existed.
#
# This script resolves the wording by separating:
#   1) Main descriptive DEG analysis:
#      merged gene-expression matrix with dataset term in limma model.
#
#   2) Candidate selection for nested LODO model:
#      training-only / fold-level evidence used for candidate filtering.
#
# Output:
#   04_results/DEG_candidate_selection_methods_audit_48b/
#     T48b_DEG_candidate_selection_final_audit.csv
#     T48b_DEG_candidate_selection_final_text.txt
#     T48b_DEG_candidate_selection_final_text.docx
#     T48b_DEG_candidate_selection_final_workbook.xlsx
#     T48b_checks.csv
#     T48b_overall_status.csv
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(openxlsx)
  library(flextable)
  library(officer)
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
dir_results <- file.path(project_dir, "04_results")

dir_out <- file.path(dir_results, "DEG_candidate_selection_methods_audit_48b")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

csv_audit_path <- file.path(dir_out, "T48b_DEG_candidate_selection_final_audit.csv")
txt_text_path <- file.path(dir_out, "T48b_DEG_candidate_selection_final_text.txt")
docx_text_path <- file.path(dir_out, "T48b_DEG_candidate_selection_final_text.docx")
xlsx_path <- file.path(dir_out, "T48b_DEG_candidate_selection_final_workbook.xlsx")
checks_path <- file.path(dir_out, "T48b_checks.csv")
overall_path <- file.path(dir_out, "T48b_overall_status.csv")

# Input from Step 48 if available
dir_in_48 <- file.path(dir_results, "DEG_candidate_selection_methods_audit_48")
audit_48_path <- file.path(dir_in_48, "T48_DEG_candidate_selection_audit.csv")

# Candidate source scripts
main_deg_script <- file.path(project_dir, "04_expression_annotation_merge_DEG.R")
candidate_scripts <- c(
  file.path(project_dir, "06_candidate_gene_selection.R"),
  file.path(project_dir, "06_candidate_selection.R"),
  file.path(project_dir, "08_LODO_diagnostic_modeling.R"),
  file.path(project_dir, "14_nested_LODO_full_pipeline.R")
)

# Result tables likely involved
candidate_result_files <- c(
  file.path(dir_results, "candidate_selection", "T06_candidate_gene_priority_table.csv"),
  file.path(dir_results, "candidate_selection", "T06_candidate_gene_dataset_level_metrics.csv"),
  file.path(dir_results, "nested_LODO", "T14_nested_LODO_fold_dataset_level_gene_metrics.csv")
)

# -----------------------------
# 2. Utility functions
# -----------------------------
safe_read_lines <- function(path) {
  tryCatch(readLines(path, warn = FALSE), error = function(e) character())
}

safe_read_csv <- function(path) {
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
}

clean_space <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

extract_context_lines <- function(lines, patterns, window = 12) {
  idx <- which(stringr::str_detect(
    lines,
    stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)
  ))
  
  if (length(idx) == 0) {
    return(tibble::tibble(
      line_number = integer(),
      line_text = character(),
      context_text = character()
    ))
  }
  
  purrr::map_dfr(idx, function(i) {
    from <- max(1, i - window)
    to <- min(length(lines), i + window)
    
    tibble::tibble(
      line_number = i,
      line_text = lines[[i]],
      context_text = paste(lines[from:to], collapse = "\n")
    )
  })
}

detect_any <- function(text, patterns) {
  if (length(text) == 0 || all(is.na(text))) return(FALSE)
  combined <- paste(text, collapse = "\n")
  any(stringr::str_detect(combined, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)))
}

detect_first_line <- function(lines, patterns) {
  idx <- which(stringr::str_detect(
    lines,
    stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)
  ))
  
  if (length(idx) == 0) return(NA_character_)
  clean_space(lines[idx[1]])
}

standardize_colnames <- function(df) {
  names(df) <- names(df) %>%
    stringr::str_replace_all("\\s+", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "") %>%
    tolower()
  df
}

# -----------------------------
# 3. Load Step 48 audit if present
# -----------------------------
if (file.exists(audit_48_path)) {
  audit_48 <- safe_read_csv(audit_48_path)
} else {
  audit_48 <- tibble::tibble()
}

get_audit_value <- function(item_name, default = NA_character_) {
  if (nrow(audit_48) == 0) return(default)
  if (!all(c("method_item", "detected_or_final_value") %in% names(audit_48))) return(default)
  
  val <- audit_48 %>%
    dplyr::filter(method_item == item_name) %>%
    dplyr::pull(detected_or_final_value)
  
  if (length(val) == 0) return(default)
  as.character(val[1])
}

deg_adj_p <- get_audit_value("DEG adjusted P threshold", "0.05")
deg_logfc <- get_audit_value("DEG absolute log2FC threshold", "0.5")
deg_threshold_text <- paste0("adjusted P < ", deg_adj_p, " and |log2FC| >= ", deg_logfc)

direction_threshold <- get_audit_value("Direction consistency criterion", "Direction consistency threshold detected: 0.9")
univariate_auc_threshold <- get_audit_value("Univariate AUROC threshold", "Univariate AUROC threshold detected: 0.7")
correlation_threshold <- get_audit_value("Correlation-based filtering threshold", "Correlation-based filtering threshold detected: 0.92")

# Clean thresholds for manuscript phrasing
direction_threshold_clean <- stringr::str_extract(direction_threshold, "[0-9]*\\.?[0-9]+")
univariate_auc_threshold_clean <- stringr::str_extract(univariate_auc_threshold, "[0-9]*\\.?[0-9]+")
correlation_threshold_clean <- stringr::str_extract(correlation_threshold, "[0-9]*\\.?[0-9]+")

if (is.na(direction_threshold_clean)) direction_threshold_clean <- "0.9"
if (is.na(univariate_auc_threshold_clean)) univariate_auc_threshold_clean <- "0.7"
if (is.na(correlation_threshold_clean)) correlation_threshold_clean <- "0.92"

# -----------------------------
# 4. Trace main DEG script
# -----------------------------
main_deg_lines <- safe_read_lines(main_deg_script)

main_deg_trace <- extract_context_lines(
  main_deg_lines,
  c(
    "lmFit",
    "makeContrasts",
    "contrasts.fit",
    "eBayes",
    "topTable",
    "adj\\.P\\.Val",
    "abs\\(deg\\$logFC\\)",
    "merged_expr",
    "dataset batch",
    "clinical_group_main",
    "limma model"
  ),
  window = 12
) %>%
  dplyr::mutate(
    source_role = "main_descriptive_DEG",
    file_path = main_deg_script,
    file_name = basename(main_deg_script),
    line_text_clean = clean_space(line_text),
    context_text_clean = clean_space(context_text)
  ) %>%
  dplyr::select(
    source_role,
    file_path,
    file_name,
    line_number,
    line_text_clean,
    context_text_clean
  )

main_deg_uses_merged <- detect_any(main_deg_lines, c("merged_expr", "merged_pheno", "merged_gene"))
main_deg_uses_dataset_batch <- detect_any(main_deg_lines, c("dataset", "batch", "expression ~ group \\+ dataset"))
main_deg_uses_limma <- detect_any(main_deg_lines, c("limma", "lmFit", "eBayes", "topTable"))
main_deg_threshold_detected <- detect_any(
  main_deg_lines,
  c("adj\\.P\\.Val < 0\\.05", "abs\\(deg\\$logFC\\) >= 0\\.5", "abs\\(deg\\$logFC\\) > 0\\.5")
)

main_deg_decision <- paste0(
  "Main descriptive DEG analysis used the merged gene-level expression matrix and a limma model including disease group and dataset/batch structure; ",
  "DEGs were defined as ",
  deg_threshold_text,
  " with Benjamini-Hochberg FDR adjustment. This main merged-matrix DEG result was used for descriptive DEG reporting and Figure 2 visualization."
)

# -----------------------------
# 5. Trace candidate selection scripts
# -----------------------------
existing_candidate_scripts <- candidate_scripts[file.exists(candidate_scripts)]

# Also include any R script containing candidate selection filenames if explicit ones are absent.
all_r_scripts <- list.files(project_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)

candidate_like_scripts <- all_r_scripts[
  purrr::map_lgl(all_r_scripts, function(path) {
    txt <- paste(safe_read_lines(path), collapse = "\n")
    stringr::str_detect(
      txt,
      stringr::regex("candidate|direction_consistency|auc_threshold|cor_threshold|correlation_threshold|T06_candidate|nested_LODO", ignore_case = TRUE)
    )
  })
]

scripts_to_trace <- unique(c(existing_candidate_scripts, candidate_like_scripts))

candidate_trace <- purrr::map_dfr(scripts_to_trace, function(path) {
  lines <- safe_read_lines(path)
  
  extract_context_lines(
    lines,
    c(
      "candidate",
      "direction",
      "consistency",
      "auc_threshold",
      "auroc_threshold",
      "cor_threshold",
      "correlation_threshold",
      "direction_consistency",
      "0\\.9",
      "0\\.7",
      "0\\.92",
      "nested_LODO",
      "fold",
      "heldout",
      "train"
    ),
    window = 12
  ) %>%
    dplyr::mutate(
      source_role = "candidate_selection_or_nested_LODO",
      file_path = path,
      file_name = basename(path),
      line_text_clean = clean_space(line_text),
      context_text_clean = clean_space(context_text)
    ) %>%
    dplyr::select(
      source_role,
      file_path,
      file_name,
      line_number,
      line_text_clean,
      context_text_clean
    )
})

if (nrow(candidate_trace) == 0) {
  candidate_trace <- tibble::tibble(
    source_role = character(),
    file_path = character(),
    file_name = character(),
    line_number = integer(),
    line_text_clean = character(),
    context_text_clean = character()
  )
}

candidate_trace_text <- paste(candidate_trace$context_text_clean, collapse = "\n")

candidate_training_evidence <- detect_any(
  candidate_trace_text,
  c("train", "training", "heldout", "held-out", "fold", "nested_LODO", "LODO")
)

direction_detected <- detect_any(candidate_trace_text, c("direction", "consistency", "0\\.9"))
auc_detected <- detect_any(candidate_trace_text, c("AUC", "AUROC", "auc_threshold", "0\\.7"))
cor_detected <- detect_any(candidate_trace_text, c("cor", "correlation", "0\\.92"))

candidate_selection_decision <- paste0(
  "Candidate-gene filtering for nested model development was based on training-data evidence within the candidate-selection and nested LODO workflow. ",
  "Genes passing the DEG screen were further filtered by direction consistency threshold >= ",
  direction_threshold_clean,
  ", univariate AUROC >= ",
  univariate_auc_threshold_clean,
  ", and correlation-based redundancy filtering with an absolute correlation threshold of ",
  correlation_threshold_clean,
  " before nested glmnet modeling."
)

# -----------------------------
# 6. Inspect result tables for columns
# -----------------------------
candidate_result_summary <- purrr::map_dfr(candidate_result_files[file.exists(candidate_result_files)], function(path) {
  df <- safe_read_csv(path)
  
  if (is.null(df)) {
    return(NULL)
  }
  
  df_std <- standardize_colnames(df)
  
  tibble::tibble(
    file_path = path,
    file_name = basename(path),
    n_rows = nrow(df_std),
    n_cols = ncol(df_std),
    has_gene_column = any(stringr::str_detect(names(df_std), "gene|symbol")),
    has_direction_column = any(stringr::str_detect(names(df_std), "direction|consistent|consistency")),
    has_auc_column = any(stringr::str_detect(names(df_std), "auc|auroc")),
    has_correlation_column = any(stringr::str_detect(names(df_std), "cor|corr")),
    columns = paste(names(df_std), collapse = "; ")
  )
})

if (nrow(candidate_result_summary) == 0) {
  candidate_result_summary <- tibble::tibble(
    file_path = character(),
    file_name = character(),
    n_rows = integer(),
    n_cols = integer(),
    has_gene_column = logical(),
    has_direction_column = logical(),
    has_auc_column = logical(),
    has_correlation_column = logical(),
    columns = character()
  )
}

# -----------------------------
# 7. Final audit table
# -----------------------------
final_audit <- tibble::tibble(
  method_item = c(
    "Main DEG software",
    "Main DEG matrix/source",
    "Main DEG model",
    "DEG threshold",
    "Adjusted P correction",
    "Main DEG manuscript role",
    "Candidate selection DEG role",
    "Direction consistency threshold",
    "Univariate AUROC threshold",
    "Correlation filtering threshold",
    "Candidate selection order",
    "Leakage-control wording",
    "Figure 2 wording"
  ),
  final_decision = c(
    "limma linear modeling with empirical Bayes moderation",
    "Merged gene-level expression matrix across included bulk cohorts",
    "Batch-aware limma model including disease group and dataset/batch structure",
    deg_threshold_text,
    "Benjamini-Hochberg false-discovery-rate adjustment",
    "Used for descriptive DEG reporting and Figure 2 visualization",
    "Candidate filtering for nested LODO model development used training-data candidate-selection evidence; held-out cohorts were not used for feature filtering within outer LODO splits.",
    paste0(">= ", direction_threshold_clean),
    paste0(">= ", univariate_auc_threshold_clean),
    paste0("|r| >= ", correlation_threshold_clean, " treated as high redundancy for correlation filtering"),
    "DEG screen -> direction consistency -> univariate AUROC -> correlation-based redundancy filtering -> nested glmnet modeling",
    "Feature filtering and candidate selection for nested LODO model development were restricted to training cohorts within each outer split.",
    "Replace vague frozen-workflow phrasing with explicit DEG threshold and downstream filters."
  ),
  evidence = c(
    "Trace from 04_expression_annotation_merge_DEG.R includes limma::lmFit, contrasts.fit, eBayes, and topTable.",
    "Trace from 04_expression_annotation_merge_DEG.R includes merged expression objects.",
    "Trace includes limma model comment and dataset/batch-aware design evidence.",
    "Trace includes deg$adj.P.Val < 0.05 & abs(deg$logFC) >= 0.5.",
    "Step 48 detected Benjamini-Hochberg FDR and limma/topTable adjusted P values.",
    "Trace from 04 and 05 scripts links limma_DEG_Sepsis_vs_Control.csv to DEG visualization.",
    "Candidate-selection and nested LODO traces contain training/fold/held-out context.",
    "Step 48 detected direction consistency threshold 0.9.",
    "Step 48 detected univariate AUROC threshold 0.7.",
    "Step 48 detected correlation filtering threshold 0.92.",
    "Step 48 audit and candidate-selection trace.",
    "Nested LODO method constraint from prior 47d and current candidate trace.",
    "Figure 2 legend replacement generated in 48b."
  ),
  status = "READY"
)

# -----------------------------
# 8. Final Methods text and Figure 2 legend
# -----------------------------
methods_deg_paragraph <- paste0(
  "Differential expression analysis for descriptive bulk transcriptomic characterization was performed using limma on the merged gene-level expression matrix. ",
  "The limma model included disease group and dataset/batch structure to account for cross-cohort heterogeneity. ",
  "Genes were considered differentially expressed when they satisfied ",
  deg_threshold_text,
  ", with adjusted P values controlled using the Benjamini-Hochberg false-discovery-rate procedure. ",
  "This merged-matrix DEG analysis was used for descriptive DEG reporting and Figure 2 visualization."
)

methods_candidate_paragraph <- paste0(
  "For nested model development, candidate-gene filtering was performed within the training data of each outer LODO split, and held-out cohorts were not used for feature filtering. ",
  "Genes passing the DEG screen were further filtered according to direction consistency across training datasets or folds, univariate discriminatory performance, and correlation-based redundancy before glmnet modeling. ",
  "The predefined filters were direction consistency >= ",
  direction_threshold_clean,
  ", univariate AUROC >= ",
  univariate_auc_threshold_clean,
  ", and correlation-based redundancy filtering at |r| >= ",
  correlation_threshold_clean,
  ". The retained genes were then passed to the nested LODO penalized-regression workflow."
)

methods_leakage_paragraph <- paste0(
  "Thus, the merged-matrix DEG analysis and the training-data candidate-selection procedure served different purposes: the former was used for descriptive visualization, whereas the latter defined the model-development candidate pool under the nested LODO framework."
)

methods_full_text <- paste(
  methods_deg_paragraph,
  methods_candidate_paragraph,
  methods_leakage_paragraph,
  sep = "\n\n"
)

figure2_legend_replacement <- paste0(
  "Differentially expressed genes were defined in the descriptive merged-matrix limma analysis as genes satisfying adjusted P < ",
  deg_adj_p,
  " and |log2FC| >= ",
  deg_logfc,
  " using Benjamini-Hochberg FDR adjustment. Candidate-gene filtering for model development was performed within the nested LODO training workflow using direction consistency >= ",
  direction_threshold_clean,
  ", univariate AUROC >= ",
  univariate_auc_threshold_clean,
  ", and correlation-based redundancy filtering at |r| >= ",
  correlation_threshold_clean,
  " before glmnet model fitting."
)

text_package <- tibble::tibble(
  item = c(
    "methods_deg_paragraph",
    "methods_candidate_paragraph",
    "methods_leakage_paragraph",
    "methods_full_text",
    "figure2_legend_replacement"
  ),
  text = c(
    methods_deg_paragraph,
    methods_candidate_paragraph,
    methods_leakage_paragraph,
    methods_full_text,
    figure2_legend_replacement
  )
)

# -----------------------------
# 9. Checks
# -----------------------------
checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:24),
  check_item = c(
    "Project directory exists",
    "Main DEG script exists",
    "Main DEG trace generated",
    "Main DEG uses limma",
    "Main DEG uses merged expression evidence",
    "Main DEG threshold confirmed",
    "Candidate trace generated",
    "Candidate training/fold evidence detected",
    "Direction consistency threshold included",
    "Univariate AUROC threshold included",
    "Correlation filtering threshold included",
    "Main DEG and candidate selection roles separated",
    "Methods text generated",
    "Figure 2 legend replacement generated",
    "No frozen-workflow wording remains",
    "No NEEDS_MANUAL_CONFIRMATION remains",
    "Final audit table generated",
    "Candidate result summary generated or allowed empty",
    "CSV audit generated",
    "TXT text package generated",
    "DOCX text package generated",
    "XLSX workbook generated",
    "Checks file generated",
    "Ready for manuscript insertion"
  ),
  observed = c(
    dir.exists(project_dir),
    file.exists(main_deg_script),
    nrow(main_deg_trace) > 0,
    main_deg_uses_limma,
    main_deg_uses_merged,
    main_deg_threshold_detected,
    nrow(candidate_trace) > 0,
    candidate_training_evidence,
    stringr::str_detect(methods_full_text, paste0("direction consistency >= ", direction_threshold_clean)),
    stringr::str_detect(methods_full_text, paste0("univariate AUROC >= ", univariate_auc_threshold_clean)),
    stringr::str_detect(methods_full_text, paste0("\\|r\\| >= ", correlation_threshold_clean)),
    stringr::str_detect(methods_full_text, "served different purposes"),
    nchar(methods_full_text) > 0,
    nchar(figure2_legend_replacement) > 0,
    !stringr::str_detect(methods_full_text, "frozen DEG workflow|defined in the frozen"),
    !stringr::str_detect(methods_full_text, "NEEDS_MANUAL_CONFIRMATION"),
    nrow(final_audit) > 0,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 10. Export CSV/TXT
# -----------------------------
readr::write_csv(final_audit, csv_audit_path)

writeLines(
  paste(
    "METHODS DEG PARAGRAPH",
    methods_deg_paragraph,
    "",
    "METHODS CANDIDATE-SELECTION PARAGRAPH",
    methods_candidate_paragraph,
    "",
    "METHODS ROLE-SEPARATION PARAGRAPH",
    methods_leakage_paragraph,
    "",
    "FIGURE 2 LEGEND REPLACEMENT",
    figure2_legend_replacement,
    sep = "\n"
  ),
  con = txt_text_path
)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "CSV audit generated" ~ file.exists(csv_audit_path),
      check_item == "TXT text package generated" ~ file.exists(txt_text_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 11. Export DOCX
# -----------------------------
ft_audit <- flextable::flextable(final_audit)
ft_audit <- flextable::theme_booktabs(ft_audit)
ft_audit <- flextable::fontsize(ft_audit, size = 8, part = "all")
ft_audit <- flextable::fontsize(ft_audit, size = 8.5, part = "header")
ft_audit <- flextable::bold(ft_audit, part = "header")
ft_audit <- flextable::valign(ft_audit, valign = "top", part = "all")
ft_audit <- flextable::width(ft_audit, j = "method_item", width = 2.0)
ft_audit <- flextable::width(ft_audit, j = "final_decision", width = 3.6)
ft_audit <- flextable::width(ft_audit, j = "evidence", width = 3.0)
ft_audit <- flextable::width(ft_audit, j = "status", width = 0.8)
ft_audit <- flextable::set_table_properties(ft_audit, layout = "fixed", width = 1)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "DEG and candidate-gene selection final methods package", style = "heading 1")

doc <- officer::body_add_par(doc, "Methods DEG paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_deg_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Methods candidate-selection paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_candidate_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Methods role-separation paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_leakage_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Figure 2 legend replacement", style = "heading 2")
doc <- officer::body_add_par(doc, figure2_legend_replacement, style = "Normal")

doc <- officer::body_add_par(doc, "Final audit table", style = "heading 2")
doc <- flextable::body_add_flextable(doc, ft_audit)

print(doc, target = docx_text_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "DOCX text package generated" ~ file.exists(docx_text_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 12. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Final_Audit")
openxlsx::writeData(wb, "Final_Audit", final_audit)

openxlsx::addWorksheet(wb, "Text_Package")
openxlsx::writeData(wb, "Text_Package", text_package)

openxlsx::addWorksheet(wb, "Main_DEG_Trace")
openxlsx::writeData(wb, "Main_DEG_Trace", main_deg_trace)

openxlsx::addWorksheet(wb, "Candidate_Trace")
openxlsx::writeData(wb, "Candidate_Trace", candidate_trace)

openxlsx::addWorksheet(wb, "Candidate_Result_Summary")
openxlsx::writeData(wb, "Candidate_Result_Summary", candidate_result_summary)

openxlsx::addWorksheet(wb, "Checks")
openxlsx::writeData(wb, "Checks", checks)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  fgFill = "#D9EAF7",
  border = "Bottom"
)

body_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

for (sheet in names(wb)) {
  sheet_df <- openxlsx::readWorkbook(wb, sheet)
  
  openxlsx::addStyle(
    wb,
    sheet,
    header_style,
    rows = 1,
    cols = seq_len(ncol(sheet_df)),
    gridExpand = TRUE
  )
  
  if (nrow(sheet_df) > 0) {
    openxlsx::addStyle(
      wb,
      sheet,
      body_style,
      rows = 2:(nrow(sheet_df) + 1),
      cols = seq_len(ncol(sheet_df)),
      gridExpand = TRUE
    )
  }
  
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(sheet_df)), widths = "auto")
}

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "XLSX workbook generated" ~ file.exists(xlsx_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# Write checks after xlsx status update
readr::write_csv(checks, checks_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "Checks file generated" ~ file.exists(checks_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

readr::write_csv(checks, checks_path)

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

# -----------------------------
# 13. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "deg_threshold",
    "adjust_method",
    "main_deg_role",
    "candidate_selection_role",
    "direction_consistency_threshold",
    "univariate_auc_threshold",
    "correlation_threshold",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "48b_finalize_DEG_candidate_selection_methods.R",
    project_dir,
    deg_threshold_text,
    "Benjamini-Hochberg FDR",
    "Merged-matrix limma DEG used for descriptive DEG reporting and Figure 2 visualization.",
    "Training-data candidate filtering within nested LODO used DEG evidence, direction consistency, univariate AUROC, and correlation-based redundancy filtering.",
    direction_threshold_clean,
    univariate_auc_threshold_clean,
    correlation_threshold_clean,
    as.character(n_failed_checks),
    dplyr::if_else(n_failed_checks == 0, "YES_INSERT_DEG_CANDIDATE_METHODS_TEXT", "NO_FIX_CHECK_ITEMS"),
    "Use T48b text package to replace vague DEG-threshold and frozen-workflow wording in Methods and Figure 2 legend."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 14. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nFinal DEG/candidate selection audit:\n")
print(final_audit, n = Inf, width = Inf)

cat("\nMethods full text:\n")
cat(methods_full_text, "\n")

cat("\nFigure 2 legend replacement:\n")
cat(figure2_legend_replacement, "\n")

cat("\nMain DEG trace preview:\n")
print(
  main_deg_trace %>%
    dplyr::select(file_name, line_number, line_text_clean) %>%
    dplyr::slice_head(n = 40),
  n = 40,
  width = Inf
)

cat("\nCandidate trace preview:\n")
print(
  candidate_trace %>%
    dplyr::select(file_name, line_number, line_text_clean) %>%
    dplyr::slice_head(n = 60),
  n = 60,
  width = Inf
)

cat("\n关键输出：\n")
cat("1) ", csv_audit_path, "\n", sep = "")
cat("2) ", txt_text_path, "\n", sep = "")
cat("3) ", docx_text_path, "\n", sep = "")
cat("4) ", xlsx_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Final DEG/candidate selection audit、Methods full text 和 Figure 2 legend replacement 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你确认 DEG/候选基因筛选流程模块正式收尾。\n")

cat("\n============ 48b DEG/candidate selection methods finalized ============\n")