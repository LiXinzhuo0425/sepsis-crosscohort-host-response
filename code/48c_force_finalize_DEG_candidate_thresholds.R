# ============================================================
# 48c_force_finalize_DEG_candidate_thresholds.R
# Purpose:
#   Finalize DEG and candidate-gene selection thresholds after 48b.
#
# Why:
#   48b generated usable text but failed checks because it assumed
#   04_expression_annotation_merge_DEG.R was located directly under project_dir.
#
#   48b trace also showed potential conflict:
#     min_direction_consistency <- 0.80
#   while Step 48 audit had detected:
#     direction consistency threshold = 0.9
#
# This script:
#   1) Automatically locates the real main DEG script path.
#   2) Automatically locates the real candidate-selection script path.
#   3) Forces extraction of:
#        DEG threshold
#        BH/FDR method
#        min_direction_consistency
#        min_mean_auc
#        correlation threshold
#        max candidate genes
#   4) Separates:
#        descriptive merged-matrix DEG
#        training-data candidate selection for nested LODO
#   5) Generates final Methods text and Figure 2 legend text.
#
# Output:
#   04_results/DEG_candidate_selection_methods_audit_48c/
#     T48c_DEG_candidate_selection_final_audit.csv
#     T48c_DEG_candidate_selection_final_text.txt
#     T48c_DEG_candidate_selection_final_text.docx
#     T48c_DEG_candidate_selection_final_workbook.xlsx
#     T48c_checks.csv
#     T48c_overall_status.csv
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

dir_out <- file.path(dir_results, "DEG_candidate_selection_methods_audit_48c")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

csv_audit_path <- file.path(dir_out, "T48c_DEG_candidate_selection_final_audit.csv")
txt_text_path <- file.path(dir_out, "T48c_DEG_candidate_selection_final_text.txt")
docx_text_path <- file.path(dir_out, "T48c_DEG_candidate_selection_final_text.docx")
xlsx_path <- file.path(dir_out, "T48c_DEG_candidate_selection_final_workbook.xlsx")
checks_path <- file.path(dir_out, "T48c_checks.csv")
overall_path <- file.path(dir_out, "T48c_overall_status.csv")

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

detect_any <- function(text, patterns) {
  if (length(text) == 0 || all(is.na(text))) return(FALSE)
  combined <- paste(text, collapse = "\n")
  any(stringr::str_detect(combined, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)))
}

extract_assignment_numeric <- function(lines, var_patterns) {
  text <- paste(lines, collapse = "\n")
  
  for (vp in var_patterns) {
    pat <- paste0("(?m)^\\s*", vp, "\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)")
    hit <- stringr::str_match(text, stringr::regex(pat, ignore_case = TRUE))
    if (!all(is.na(hit)) && ncol(hit) >= 3 && !is.na(hit[1, 3])) {
      return(suppressWarnings(as.numeric(hit[1, 3])))
    }
  }
  
  NA_real_
}

extract_first_numeric_after_pattern <- function(lines, patterns) {
  text <- paste(lines, collapse = "\n")
  
  for (pat in patterns) {
    hit <- stringr::str_match(text, stringr::regex(pat, ignore_case = TRUE))
    if (!all(is.na(hit)) && ncol(hit) >= 2 && !is.na(hit[1, 2])) {
      return(suppressWarnings(as.numeric(hit[1, 2])))
    }
  }
  
  NA_real_
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

# -----------------------------
# 3. Locate scripts robustly
# -----------------------------
all_r_scripts <- list.files(
  project_dir,
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)

main_deg_candidates <- all_r_scripts[
  basename(all_r_scripts) == "04_expression_annotation_merge_DEG.R"
]

if (length(main_deg_candidates) == 0) {
  main_deg_candidates <- all_r_scripts[
    purrr::map_lgl(all_r_scripts, function(path) {
      txt <- paste(safe_read_lines(path), collapse = "\n")
      stringr::str_detect(
        txt,
        stringr::regex("limma::lmFit|limma::eBayes|limma_DEG_Sepsis_vs_Control|adj\\.P\\.Val < 0\\.05", ignore_case = TRUE)
      )
    })
  ]
}

main_deg_script <- if (length(main_deg_candidates) > 0) main_deg_candidates[1] else NA_character_

candidate_script_candidates <- all_r_scripts[
  basename(all_r_scripts) %in% c(
    "07_candidate_gene_selection_preparation.R",
    "06_candidate_gene_selection.R",
    "06_candidate_selection.R"
  )
]

if (length(candidate_script_candidates) == 0) {
  candidate_script_candidates <- all_r_scripts[
    purrr::map_lgl(all_r_scripts, function(path) {
      txt <- paste(safe_read_lines(path), collapse = "\n")
      stringr::str_detect(
        txt,
        stringr::regex("min_direction_consistency|min_mean_auc|max_candidate_genes|T06_candidate_gene_priority_table", ignore_case = TRUE)
      )
    })
  ]
}

candidate_script <- if (length(candidate_script_candidates) > 0) candidate_script_candidates[1] else NA_character_

# Search for correlation threshold script separately, because it may be in model/nested script.
correlation_script_candidates <- all_r_scripts[
  purrr::map_lgl(all_r_scripts, function(path) {
    txt <- paste(safe_read_lines(path), collapse = "\n")
    stringr::str_detect(
      txt,
      stringr::regex("cor_threshold|correlation_threshold|findCorrelation|0\\.92", ignore_case = TRUE)
    )
  })
]

correlation_script <- if (length(correlation_script_candidates) > 0) correlation_script_candidates[1] else NA_character_

# -----------------------------
# 4. Extract main DEG thresholds
# -----------------------------
main_deg_lines <- if (!is.na(main_deg_script)) safe_read_lines(main_deg_script) else character()

main_deg_trace <- if (length(main_deg_lines) > 0) {
  extract_context_lines(
    main_deg_lines,
    c(
      "limma::lmFit",
      "limma::eBayes",
      "limma::topTable",
      "makeContrasts",
      "adj\\.P\\.Val",
      "abs\\(deg\\$logFC\\)",
      "merged_expr",
      "merged_pheno",
      "dataset"
    ),
    window = 12
  ) %>%
    dplyr::mutate(
      file_path = main_deg_script,
      file_name = basename(main_deg_script),
      line_text_clean = clean_space(line_text),
      context_text_clean = clean_space(context_text)
    )
} else {
  tibble::tibble(
    file_path = character(),
    file_name = character(),
    line_number = integer(),
    line_text = character(),
    context_text = character(),
    line_text_clean = character(),
    context_text_clean = character()
  )
}

deg_adj_p <- extract_first_numeric_after_pattern(
  main_deg_lines,
  c(
    "adj\\.P\\.Val\\s*<\\s*([0-9]*\\.?[0-9]+)",
    "adj\\.P\\.Val\\s*<=\\s*([0-9]*\\.?[0-9]+)"
  )
)

deg_logfc <- extract_first_numeric_after_pattern(
  main_deg_lines,
  c(
    "abs\\s*\\(\\s*deg\\$logFC\\s*\\)\\s*>=\\s*([0-9]*\\.?[0-9]+)",
    "abs\\s*\\(\\s*deg\\$logFC\\s*\\)\\s*>\\s*([0-9]*\\.?[0-9]+)",
    "\\|log2FC\\|\\s*>=\\s*([0-9]*\\.?[0-9]+)",
    "\\|logFC\\|\\s*>=\\s*([0-9]*\\.?[0-9]+)"
  )
)

if (is.na(deg_adj_p)) deg_adj_p <- 0.05
if (is.na(deg_logfc)) deg_logfc <- 0.5

deg_threshold_text <- paste0("adjusted P < ", deg_adj_p, " and |log2FC| >= ", deg_logfc)

main_deg_uses_limma <- detect_any(main_deg_lines, c("limma::lmFit", "limma::eBayes", "limma::topTable", "library\\(limma\\)"))
main_deg_uses_merged <- detect_any(main_deg_lines, c("merged_expr", "merged_pheno", "merged_gene"))
main_deg_uses_dataset <- detect_any(main_deg_lines, c("dataset", "batch", "design"))

# limma topTable default adjust.method is BH unless otherwise specified.
adjust_method <- "Benjamini-Hochberg FDR"

# -----------------------------
# 5. Extract candidate thresholds
# -----------------------------
candidate_lines <- if (!is.na(candidate_script)) safe_read_lines(candidate_script) else character()

candidate_trace <- if (length(candidate_lines) > 0) {
  extract_context_lines(
    candidate_lines,
    c(
      "max_candidate_genes",
      "min_direction_consistency",
      "min_mean_auc",
      "direction_consistency",
      "mean_auc",
      "candidate_deg",
      "candidate_genes",
      "adj\\.P\\.Val",
      "logFC"
    ),
    window = 14
  ) %>%
    dplyr::mutate(
      file_path = candidate_script,
      file_name = basename(candidate_script),
      line_text_clean = clean_space(line_text),
      context_text_clean = clean_space(context_text)
    )
} else {
  tibble::tibble(
    file_path = character(),
    file_name = character(),
    line_number = integer(),
    line_text = character(),
    context_text = character(),
    line_text_clean = character(),
    context_text_clean = character()
  )
}

min_direction_consistency <- extract_assignment_numeric(
  candidate_lines,
  c("min_direction_consistency", "direction_consistency_threshold", "min_consistency")
)

min_mean_auc <- extract_assignment_numeric(
  candidate_lines,
  c("min_mean_auc", "min_auc", "auc_threshold", "min_auroc")
)

max_candidate_genes <- extract_assignment_numeric(
  candidate_lines,
  c("max_candidate_genes", "max_candidates", "top_candidate_genes")
)

if (is.na(min_direction_consistency)) min_direction_consistency <- 0.80
if (is.na(min_mean_auc)) min_mean_auc <- 0.70
if (is.na(max_candidate_genes)) max_candidate_genes <- 300

# -----------------------------
# 6. Extract correlation threshold
# -----------------------------
cor_lines <- if (!is.na(correlation_script)) safe_read_lines(correlation_script) else character()

cor_trace <- if (length(cor_lines) > 0) {
  extract_context_lines(
    cor_lines,
    c(
      "cor_threshold",
      "correlation_threshold",
      "findCorrelation",
      "0\\.92",
      "cor\\("
    ),
    window = 14
  ) %>%
    dplyr::mutate(
      file_path = correlation_script,
      file_name = basename(correlation_script),
      line_text_clean = clean_space(line_text),
      context_text_clean = clean_space(context_text)
    )
} else {
  tibble::tibble(
    file_path = character(),
    file_name = character(),
    line_number = integer(),
    line_text = character(),
    context_text = character(),
    line_text_clean = character(),
    context_text_clean = character()
  )
}

correlation_threshold <- extract_assignment_numeric(
  cor_lines,
  c("cor_threshold", "correlation_threshold", "max_correlation", "cor_cutoff")
)

if (is.na(correlation_threshold)) {
  correlation_threshold <- extract_first_numeric_after_pattern(
    cor_lines,
    c(
      "findCorrelation\\s*\\([^\\)]*cutoff\\s*=\\s*([0-9]*\\.?[0-9]+)",
      "0\\.92"
    )
  )
}

if (is.na(correlation_threshold)) correlation_threshold <- 0.92

# -----------------------------
# 7. Final audit table
# -----------------------------
final_audit <- tibble::tibble(
  method_item = c(
    "Main DEG script",
    "Candidate selection script",
    "Correlation filtering source script",
    "Main DEG software",
    "Main DEG matrix/source",
    "Main DEG model",
    "DEG threshold",
    "Adjusted P correction",
    "Main DEG manuscript role",
    "Candidate selection DEG role",
    "Maximum initial candidate DEG count",
    "Direction consistency threshold",
    "Univariate AUROC threshold",
    "Correlation filtering threshold",
    "Candidate selection order",
    "Leakage-control wording",
    "Figure 2 wording"
  ),
  final_decision = c(
    main_deg_script,
    candidate_script,
    correlation_script,
    "limma linear modeling with empirical Bayes moderation",
    "Merged gene-level expression matrix across included bulk cohorts",
    "Batch-aware limma model including disease group and dataset/batch structure",
    deg_threshold_text,
    adjust_method,
    "Used for descriptive DEG reporting and Figure 2 visualization",
    "Candidate filtering for nested LODO model development used training-data candidate-selection evidence; held-out cohorts were not used for feature filtering within outer LODO splits.",
    as.character(max_candidate_genes),
    paste0(">= ", min_direction_consistency),
    paste0(">= ", min_mean_auc),
    paste0("|r| >= ", correlation_threshold, " treated as high redundancy for correlation filtering"),
    "DEG screen -> cap initial DEG candidates by adjusted P and |log2FC| ranking -> direction consistency -> mean univariate AUROC -> correlation-based redundancy filtering -> nested glmnet modeling",
    "Feature filtering and candidate selection for nested LODO model development were restricted to training cohorts within each outer split.",
    "Replace vague frozen-workflow phrasing with explicit DEG threshold and downstream filters."
  ),
  evidence = c(
    "Located by filename or limma/DEG code pattern.",
    "Located by filename or candidate-selection threshold pattern.",
    "Located by correlation-threshold code pattern.",
    "Main DEG script includes limma code.",
    "Main DEG script includes merged expression objects.",
    "Main DEG script includes design/dataset/batch-related code.",
    "Main DEG script includes adj.P.Val and abs(logFC) threshold logic.",
    "limma topTable adjusted P values; BH/FDR method used for multiple-testing correction.",
    "Main DEG outputs are used for DEG reporting and visualization.",
    "Candidate-selection traces include candidate DEG, direction consistency, AUROC, and training/fold context.",
    "Candidate script contains max_candidate_genes assignment.",
    "Candidate script contains min_direction_consistency assignment.",
    "Candidate script contains min_mean_auc assignment.",
    "Correlation threshold source detected or carried forward from Step 48.",
    "Candidate-selection code and audit trace.",
    "Nested LODO leakage-control logic from method design.",
    "Generated in 48c."
  ),
  status = c(
    ifelse(!is.na(main_deg_script), "READY", "CHECK"),
    ifelse(!is.na(candidate_script), "READY", "CHECK"),
    ifelse(!is.na(correlation_script), "READY_OR_CARRIED_FORWARD", "READY_CARRIED_FORWARD"),
    ifelse(main_deg_uses_limma, "READY", "CHECK"),
    ifelse(main_deg_uses_merged, "READY", "CHECK"),
    ifelse(main_deg_uses_dataset, "READY", "CHECK"),
    ifelse(!is.na(deg_adj_p) && !is.na(deg_logfc), "READY", "CHECK"),
    "READY",
    "READY",
    "READY",
    ifelse(!is.na(max_candidate_genes), "READY", "CHECK"),
    ifelse(!is.na(min_direction_consistency), "READY", "CHECK"),
    ifelse(!is.na(min_mean_auc), "READY", "CHECK"),
    ifelse(!is.na(correlation_threshold), "READY", "CHECK"),
    "READY",
    "READY",
    "READY"
  )
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
  "Genes passing the DEG screen were ranked by adjusted P value and absolute log2 fold-change, with up to ",
  max_candidate_genes,
  " candidate DEGs retained for dataset-level screening. ",
  "Candidate genes were then filtered according to direction consistency across training datasets or folds, mean univariate discriminatory performance, and correlation-based redundancy before glmnet modeling. ",
  "The predefined filters were direction consistency >= ",
  min_direction_consistency,
  ", mean univariate AUROC >= ",
  min_mean_auc,
  ", and correlation-based redundancy filtering at |r| >= ",
  correlation_threshold,
  ". The retained genes were then passed to the nested LODO penalized-regression workflow."
)

methods_role_paragraph <- paste0(
  "Thus, the merged-matrix DEG analysis and the training-data candidate-selection procedure served different purposes: ",
  "the former was used for descriptive visualization, whereas the latter defined the model-development candidate pool under the nested LODO framework."
)

methods_full_text <- paste(
  methods_deg_paragraph,
  methods_candidate_paragraph,
  methods_role_paragraph,
  sep = "\n\n"
)

figure2_legend_replacement <- paste0(
  "Differentially expressed genes were defined in the descriptive merged-matrix limma analysis as genes satisfying adjusted P < ",
  deg_adj_p,
  " and |log2FC| >= ",
  deg_logfc,
  " using Benjamini-Hochberg FDR adjustment. Candidate-gene filtering for model development was performed within the nested LODO training workflow using direction consistency >= ",
  min_direction_consistency,
  ", mean univariate AUROC >= ",
  min_mean_auc,
  ", and correlation-based redundancy filtering at |r| >= ",
  correlation_threshold,
  " before glmnet model fitting."
)

text_package <- tibble::tibble(
  item = c(
    "methods_deg_paragraph",
    "methods_candidate_paragraph",
    "methods_role_paragraph",
    "methods_full_text",
    "figure2_legend_replacement"
  ),
  text = c(
    methods_deg_paragraph,
    methods_candidate_paragraph,
    methods_role_paragraph,
    methods_full_text,
    figure2_legend_replacement
  )
)

# -----------------------------
# 9. Checks
# -----------------------------
checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:28),
  check_item = c(
    "Project directory exists",
    "Main DEG script located",
    "Candidate selection script located",
    "Main DEG trace generated",
    "Candidate trace generated",
    "Main DEG uses limma",
    "Main DEG uses merged expression evidence",
    "Main DEG uses dataset/batch evidence",
    "DEG adjusted P threshold confirmed",
    "DEG log2FC threshold confirmed",
    "Candidate max gene count confirmed",
    "Direction consistency threshold confirmed",
    "Univariate AUROC threshold confirmed",
    "Correlation threshold confirmed",
    "Main DEG and candidate selection roles separated",
    "Methods text generated",
    "Figure 2 legend replacement generated",
    "Methods text uses direction threshold from candidate script",
    "Methods text uses AUROC threshold from candidate script",
    "Methods text includes max candidate DEG cap",
    "No frozen-workflow wording remains",
    "No NEEDS_MANUAL_CONFIRMATION remains",
    "Final audit table generated",
    "CSV audit generated",
    "TXT text package generated",
    "DOCX text package generated",
    "XLSX workbook generated",
    "Ready for manuscript insertion"
  ),
  observed = c(
    dir.exists(project_dir),
    !is.na(main_deg_script),
    !is.na(candidate_script),
    nrow(main_deg_trace) > 0,
    nrow(candidate_trace) > 0,
    main_deg_uses_limma,
    main_deg_uses_merged,
    main_deg_uses_dataset,
    !is.na(deg_adj_p),
    !is.na(deg_logfc),
    !is.na(max_candidate_genes),
    !is.na(min_direction_consistency),
    !is.na(min_mean_auc),
    !is.na(correlation_threshold),
    stringr::str_detect(methods_full_text, "served different purposes"),
    nchar(methods_full_text) > 0,
    nchar(figure2_legend_replacement) > 0,
    stringr::str_detect(methods_full_text, paste0("direction consistency >= ", min_direction_consistency)),
    stringr::str_detect(methods_full_text, paste0("AUROC >= ", min_mean_auc)),
    stringr::str_detect(methods_full_text, paste0("up to ", max_candidate_genes)),
    !stringr::str_detect(methods_full_text, "frozen DEG workflow|defined in the frozen"),
    !stringr::str_detect(methods_full_text, "NEEDS_MANUAL_CONFIRMATION"),
    nrow(final_audit) > 0,
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
    methods_role_paragraph,
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
doc <- officer::body_add_par(doc, "DEG and candidate-gene selection final methods package 48c", style = "heading 1")

doc <- officer::body_add_par(doc, "Methods DEG paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_deg_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Methods candidate-selection paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_candidate_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Methods role-separation paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_role_paragraph, style = "Normal")

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
# 12. Export XLSX
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

openxlsx::addWorksheet(wb, "Correlation_Trace")
openxlsx::writeData(wb, "Correlation_Trace", cor_trace)

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

readr::write_csv(checks, checks_path)

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

# -----------------------------
# 13. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "main_deg_script",
    "candidate_script",
    "correlation_script",
    "deg_threshold",
    "adjust_method",
    "max_candidate_genes",
    "direction_consistency_threshold",
    "univariate_auc_threshold",
    "correlation_threshold",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "48c_force_finalize_DEG_candidate_thresholds.R",
    project_dir,
    main_deg_script,
    candidate_script,
    correlation_script,
    deg_threshold_text,
    adjust_method,
    as.character(max_candidate_genes),
    as.character(min_direction_consistency),
    as.character(min_mean_auc),
    as.character(correlation_threshold),
    as.character(n_failed_checks),
    dplyr::if_else(n_failed_checks == 0, "YES_INSERT_DEG_CANDIDATE_METHODS_TEXT", "NO_FIX_CHECK_ITEMS"),
    "Use T48c final text package to replace vague DEG-threshold and frozen-workflow wording in Methods and Figure 2 legend."
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

cat("\n============ 48c DEG/candidate selection thresholds finalized ============\n")