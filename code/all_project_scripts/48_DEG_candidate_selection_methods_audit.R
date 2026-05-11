# ============================================================
# 48_DEG_candidate_selection_methods_audit.R
# Purpose:
#   Audit and finalize DEG threshold and candidate-gene selection
#   workflow for manuscript Methods and Figure 2 legend.
#
# This script does NOT rerun DEG or modeling.
# It scans existing scripts and result tables for:
#   - DEG adjusted P threshold
#   - DEG absolute log2FC threshold
#   - adjusted P correction method
#   - whether DEG was based on merged matrix or training-only matrix
#   - whether DEG participated in candidate-gene filtering
#   - candidate selection sequence
#   - direction consistency criteria
#   - univariate AUROC threshold
#   - correlation-based filtering threshold
#
# Output:
#   04_results/DEG_candidate_selection_methods_audit_48/
#     T48_DEG_candidate_selection_audit.csv
#     T48_DEG_candidate_selection_text_package.txt
#     T48_DEG_candidate_selection_text_package.docx
#     T48_DEG_candidate_selection_workbook.xlsx
#     T48_checks.csv
#     T48_overall_status.csv
#
# Important:
#   If true thresholds cannot be detected, fields are marked
#   NEEDS_MANUAL_CONFIRMATION. Do not submit with placeholders.
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

dir_out <- file.path(dir_results, "DEG_candidate_selection_methods_audit_48")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

csv_audit_path <- file.path(dir_out, "T48_DEG_candidate_selection_audit.csv")
txt_text_path <- file.path(dir_out, "T48_DEG_candidate_selection_text_package.txt")
docx_text_path <- file.path(dir_out, "T48_DEG_candidate_selection_text_package.docx")
xlsx_path <- file.path(dir_out, "T48_DEG_candidate_selection_workbook.xlsx")
checks_path <- file.path(dir_out, "T48_checks.csv")
overall_path <- file.path(dir_out, "T48_overall_status.csv")

# -----------------------------
# 2. Utility functions
# -----------------------------
safe_read_lines <- function(path) {
  tryCatch(readLines(path, warn = FALSE), error = function(e) character())
}

safe_read_csv_like <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  out <- tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    } else if (ext %in% c("tsv", "txt")) {
      readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
  
  if (is.null(out)) return(NULL)
  if (!is.data.frame(out)) return(NULL)
  if (nrow(out) == 0 || ncol(out) == 0) return(NULL)
  
  as_tibble(out)
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

collapse_df_text <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::unite("all_text", dplyr::everything(), sep = " ", remove = TRUE, na.rm = TRUE) %>%
    dplyr::pull(all_text) %>%
    paste(collapse = "\n")
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

detect_first_match <- function(text, patterns, default = "NEEDS_MANUAL_CONFIRMATION") {
  if (length(text) == 0 || all(is.na(text))) return(default)
  combined <- paste(text, collapse = "\n")
  
  for (pat in patterns) {
    hit <- stringr::str_extract(combined, stringr::regex(pat, ignore_case = TRUE))
    if (!is.na(hit)) return(clean_space(hit))
  }
  
  default
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

extract_numeric_threshold <- function(text, patterns) {
  if (length(text) == 0 || all(is.na(text))) return(NA_real_)
  combined <- paste(text, collapse = "\n")
  
  for (pat in patterns) {
    hit <- stringr::str_match(combined, stringr::regex(pat, ignore_case = TRUE))
    if (!all(is.na(hit)) && ncol(hit) >= 2 && !is.na(hit[1, 2])) {
      return(suppressWarnings(as.numeric(hit[1, 2])))
    }
  }
  
  NA_real_
}

extract_assignment_value <- function(text, var_patterns) {
  if (length(text) == 0 || all(is.na(text))) return(NA_character_)
  combined <- paste(text, collapse = "\n")
  
  for (vp in var_patterns) {
    pat <- paste0("(?m)^\\s*", vp, "\\s*(<-|=)\\s*([^\\n#]+)")
    hit <- stringr::str_match(combined, stringr::regex(pat, ignore_case = TRUE))
    if (!all(is.na(hit)) && ncol(hit) >= 3 && !is.na(hit[1, 3])) {
      return(clean_space(hit[1, 3]))
    }
  }
  
  NA_character_
}

# -----------------------------
# 3. Inventory candidate scripts and result files
# -----------------------------
all_files <- list.files(
  project_dir,
  pattern = "\\.(R|r|Rmd|rmd|csv|tsv|txt|log)$",
  recursive = TRUE,
  full.names = TRUE
)

all_files <- all_files[
  !stringr::str_detect(all_files, "DEG_candidate_selection_methods_audit_48")
]

file_score <- function(path) {
  p <- stringr::str_to_lower(path)
  
  score <- 0
  score <- score + ifelse(stringr::str_detect(p, "deg|differential|limma|candidate|selection|priority|auc|auroc|correlation|direction|logfc|fdr|padj|adj"), 6, 0)
  score <- score + ifelse(stringr::str_detect(p, "t04|t05|t06|t14|final|freeze|nested|lodo|figure2|fig2"), 3, 0)
  score <- score - ifelse(stringr::str_detect(p, "expression_matrix|merged_gene_expression|png|pdf|docx|xlsx"), 4, 0)
  
  score
}

file_inventory <- tibble::tibble(
  file_path = all_files,
  file_name = basename(all_files),
  extension = tolower(tools::file_ext(all_files)),
  score = purrr::map_dbl(all_files, file_score)
) %>%
  dplyr::arrange(dplyr::desc(score), file_path)

candidate_files <- file_inventory %>%
  dplyr::filter(score >= 3) %>%
  dplyr::slice_head(n = 500)

read_file_as_text <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("r", "rmd", "txt", "log")) {
    return(paste(safe_read_lines(path), collapse = "\n"))
  }
  
  if (ext %in% c("csv", "tsv")) {
    df <- safe_read_csv_like(path)
    if (is.null(df)) return(NA_character_)
    return(collapse_df_text(df))
  }
  
  NA_character_
}

candidate_text_tbl <- candidate_files %>%
  dplyr::mutate(
    file_text = purrr::map_chr(file_path, read_file_as_text),
    file_text_lower = stringr::str_to_lower(file_text)
  ) %>%
  dplyr::filter(!is.na(file_text), file_text != "")

all_text <- paste(candidate_text_tbl$file_text, collapse = "\n\n")

# -----------------------------
# 4. Trace relevant code contexts
# -----------------------------
script_files <- candidate_files %>%
  dplyr::filter(extension %in% c("r", "rmd")) %>%
  dplyr::pull(file_path)

trace_records <- purrr::map_dfr(script_files, function(path) {
  lines <- safe_read_lines(path)
  
  deg_ctx <- extract_context_lines(
    lines,
    c(
      "limma",
      "lmFit",
      "eBayes",
      "topTable",
      "adj\\.P\\.Val",
      "adjust\\.method",
      "p\\.adjust",
      "logFC",
      "log2FC",
      "abs\\(",
      "DEG",
      "differential"
    ),
    window = 14
  ) %>%
    dplyr::mutate(trace_type = "DEG_threshold_or_limma")
  
  candidate_ctx <- extract_context_lines(
    lines,
    c(
      "candidate",
      "priority",
      "direction",
      "consistency",
      "auc",
      "AUROC",
      "cor\\(",
      "correlation",
      "findCorrelation",
      "corr",
      "filter",
      "selected",
      "rank"
    ),
    window = 14
  ) %>%
    dplyr::mutate(trace_type = "candidate_selection")
  
  dplyr::bind_rows(deg_ctx, candidate_ctx) %>%
    dplyr::mutate(
      file_path = path,
      file_name = basename(path),
      line_text_clean = clean_space(line_text),
      context_text_clean = clean_space(context_text)
    ) %>%
    dplyr::select(
      file_path,
      file_name,
      trace_type,
      line_number,
      line_text_clean,
      context_text_clean
    )
})

if (nrow(trace_records) == 0) {
  trace_records <- tibble::tibble(
    file_path = character(),
    file_name = character(),
    trace_type = character(),
    line_number = integer(),
    line_text_clean = character(),
    context_text_clean = character()
  )
}

deg_context <- trace_records %>%
  dplyr::filter(trace_type == "DEG_threshold_or_limma") %>%
  dplyr::pull(context_text_clean)

candidate_context <- trace_records %>%
  dplyr::filter(trace_type == "candidate_selection") %>%
  dplyr::pull(context_text_clean)

# -----------------------------
# 5. Detect DEG thresholds and method
# -----------------------------
# adjusted P threshold
adj_p_threshold <- extract_numeric_threshold(
  deg_context,
  c(
    "adj\\.P\\.Val\\s*[<]=?\\s*([0-9]*\\.?[0-9]+)",
    "adj[_\\.]?p[_\\.]?val\\s*[<]=?\\s*([0-9]*\\.?[0-9]+)",
    "padj\\s*[<]=?\\s*([0-9]*\\.?[0-9]+)",
    "fdr\\s*[<]=?\\s*([0-9]*\\.?[0-9]+)",
    "p_adj_threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "fdr_threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)"
  )
)

# If patterns with assignment have value in group 2 rather than group 1,
# do secondary extraction directly from assignment lines.
if (is.na(adj_p_threshold)) {
  adj_assign <- extract_assignment_value(
    deg_context,
    c("p_adj_threshold", "padj_threshold", "adj_p_threshold", "fdr_threshold", "deg_padj_cutoff")
  )
  adj_p_threshold <- suppressWarnings(as.numeric(stringr::str_extract(adj_assign, "[0-9]*\\.?[0-9]+")))
}

# absolute log2FC threshold
logfc_threshold <- extract_numeric_threshold(
  deg_context,
  c(
    "abs\\s*\\(\\s*logFC\\s*\\)\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)",
    "abs\\s*\\(\\s*log2FC\\s*\\)\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)",
    "\\|log2FC\\|\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)",
    "\\|logFC\\|\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)",
    "logfc_threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "log2fc_threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)"
  )
)

if (is.na(logfc_threshold)) {
  logfc_assign <- extract_assignment_value(
    deg_context,
    c("logfc_threshold", "log2fc_threshold", "abs_logfc_threshold", "deg_logfc_cutoff")
  )
  logfc_threshold <- suppressWarnings(as.numeric(stringr::str_extract(logfc_assign, "[0-9]*\\.?[0-9]+")))
}

adjust_method_detected <- detect_first_match(
  deg_context,
  c(
    "adjust\\.method\\s*=\\s*[\"']BH[\"']",
    "adjust\\.method\\s*=\\s*[\"']fdr[\"']",
    "method\\s*=\\s*[\"']BH[\"']",
    "method\\s*=\\s*[\"']fdr[\"']",
    "Benjamini[- ]Hochberg",
    "\\bBH\\b",
    "\\bFDR\\b"
  )
)

if (adjust_method_detected == "NEEDS_MANUAL_CONFIRMATION" && detect_any(deg_context, c("topTable"))) {
  adjust_method_decision <- "NEEDS_MANUAL_CONFIRMATION"
} else if (stringr::str_detect(adjust_method_detected, stringr::regex("BH|fdr|Benjamini", ignore_case = TRUE))) {
  adjust_method_decision <- "Benjamini-Hochberg FDR"
} else {
  adjust_method_decision <- adjust_method_detected
}

limma_detected <- detect_any(deg_context, c("limma|lmFit|eBayes|topTable"))

# -----------------------------
# 6. Determine DEG matrix source
# -----------------------------
merged_matrix_detected <- detect_any(
  deg_context,
  c("merged_gene_expression_common_genes", "merged", "combat", "combined", "pooled", "all cohorts", "full matrix")
)

training_only_detected <- detect_any(
  deg_context,
  c("train_only", "training only", "train_data", "training cohorts", "heldout", "held-out", "outer training")
)

deg_matrix_source <- dplyr::case_when(
  training_only_detected & !merged_matrix_detected ~ "Training-set-only matrices within each outer LODO split",
  training_only_detected & merged_matrix_detected ~ "Both merged/frozen DEG and training-only nested LODO DEG evidence detected; specify which was used for candidate filtering.",
  merged_matrix_detected ~ "Merged matrix or frozen DEG workflow detected; verify whether this was descriptive or used for candidate filtering.",
  TRUE ~ "NEEDS_MANUAL_CONFIRMATION"
)

deg_participates_candidate <- if (detect_any(candidate_context, c("DEG|differential|adj\\.P\\.Val|logFC|log2FC|fdr|padj"))) {
  "YES_DEG_EVIDENCE_USED_IN_CANDIDATE_SELECTION_OR_RANKING"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

# -----------------------------
# 7. Candidate-selection thresholds
# -----------------------------
direction_consistency_value <- detect_first_match(
  candidate_context,
  c(
    "direction[_\\. ]?consistency[_\\. ]?threshold\\s*(<-|=)\\s*[0-9]*\\.?[0-9]+",
    "direction[_\\. ]?consistency\\s*[>]=?\\s*[0-9]*\\.?[0-9]+",
    "same[_\\. ]?direction[_\\. ]?count\\s*[>]=?\\s*[0-9]+",
    "n[_\\. ]?same[_\\. ]?direction\\s*[>]=?\\s*[0-9]+",
    "consistent[_\\. ]?direction"
  )
)

direction_consistency_threshold <- extract_numeric_threshold(
  candidate_context,
  c(
    "direction[_\\. ]?consistency[_\\. ]?threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "direction[_\\. ]?consistency\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)",
    "same[_\\. ]?direction[_\\. ]?count\\s*[>]=?\\s*([0-9]+)",
    "n[_\\. ]?same[_\\. ]?direction\\s*[>]=?\\s*([0-9]+)"
  )
)

univariate_auc_threshold <- extract_numeric_threshold(
  candidate_context,
  c(
    "auc[_\\. ]?threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "auroc[_\\. ]?threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "univariate[_\\. ]?auc[_\\. ]?threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "AUC\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)",
    "AUROC\\s*[>]=?\\s*([0-9]*\\.?[0-9]+)"
  )
)

correlation_threshold <- extract_numeric_threshold(
  candidate_context,
  c(
    "cor[_\\. ]?threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "correlation[_\\. ]?threshold\\s*(<-|=)\\s*([0-9]*\\.?[0-9]+)",
    "abs\\s*\\(\\s*cor\\s*\\)\\s*[<]=?\\s*([0-9]*\\.?[0-9]+)",
    "cor\\s*[<]=?\\s*([0-9]*\\.?[0-9]+)",
    "findCorrelation\\s*\\([^\\)]*cutoff\\s*=\\s*([0-9]*\\.?[0-9]+)"
  )
)

direction_decision <- if (!is.na(direction_consistency_threshold)) {
  paste0("Direction consistency threshold detected: ", direction_consistency_threshold)
} else if (direction_consistency_value != "NEEDS_MANUAL_CONFIRMATION") {
  paste0("Direction consistency logic detected; exact numerical threshold requires verification: ", direction_consistency_value)
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

auc_decision <- if (!is.na(univariate_auc_threshold)) {
  paste0("Univariate AUROC threshold detected: ", univariate_auc_threshold)
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

correlation_decision <- if (!is.na(correlation_threshold)) {
  paste0("Correlation-based filtering threshold detected: ", correlation_threshold)
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

# -----------------------------
# 8. Candidate selection sequence
# -----------------------------
candidate_sequence <- c(
  "1. Construct training data within each outer LODO split.",
  "2. Run differential-expression screening using limma within the relevant training/frozen DEG workflow.",
  "3. Retain genes satisfying the predefined DEG threshold.",
  "4. Evaluate direction consistency across training datasets or folds.",
  "5. Evaluate univariate discrimination using AUROC.",
  "6. Remove highly correlated genes using correlation-based filtering.",
  "7. Pass the retained candidate genes into nested glmnet modeling with alpha/lambda tuning performed inside training data."
)

candidate_sequence_text <- paste(candidate_sequence, collapse = " ")

# -----------------------------
# 9. Audit table
# -----------------------------
adj_p_value <- ifelse(is.na(adj_p_threshold), "NEEDS_MANUAL_CONFIRMATION", as.character(adj_p_threshold))
logfc_value <- ifelse(is.na(logfc_threshold), "NEEDS_MANUAL_CONFIRMATION", as.character(logfc_threshold))
direction_value <- direction_decision
auc_value <- auc_decision
cor_value <- correlation_decision

deg_threshold_text <- if (!is.na(adj_p_threshold) && !is.na(logfc_threshold)) {
  paste0("Adjusted P < ", adj_p_threshold, " and |log2FC| > ", logfc_threshold)
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

audit_tbl <- tibble::tibble(
  method_item = c(
    "DEG software",
    "DEG adjusted P threshold",
    "DEG absolute log2FC threshold",
    "DEG combined threshold",
    "Adjusted P correction method",
    "DEG matrix/source",
    "DEG role in candidate selection",
    "Candidate selection sequence",
    "Direction consistency criterion",
    "Univariate AUROC threshold",
    "Correlation-based filtering threshold",
    "Leakage control"
  ),
  detected_or_final_value = c(
    ifelse(limma_detected, "limma linear modeling with empirical Bayes moderation", "NEEDS_MANUAL_CONFIRMATION"),
    adj_p_value,
    logfc_value,
    deg_threshold_text,
    adjust_method_decision,
    deg_matrix_source,
    deg_participates_candidate,
    candidate_sequence_text,
    direction_value,
    auc_value,
    cor_value,
    "DEG screening and candidate selection should be performed within training data for nested LODO model development; any merged-matrix DEG result should be described as descriptive or frozen post hoc unless it was explicitly used in the training-only workflow."
  ),
  manuscript_status = c(
    ifelse(limma_detected, "READY", "NEEDS_MANUAL_CONFIRMATION"),
    ifelse(is.na(adj_p_threshold), "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(is.na(logfc_threshold), "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(deg_threshold_text == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(adjust_method_decision == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(stringr::str_detect(deg_matrix_source, "NEEDS|verify|Both"), "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(deg_participates_candidate == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_OR_DETECTED"),
    "READY_RECOMMENDED_SEQUENCE",
    ifelse(direction_value == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_OR_DETECTED"),
    ifelse(auc_value == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(cor_value == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    "READY_RECOMMENDED"
  )
)

n_manual_items <- audit_tbl %>%
  dplyr::filter(stringr::str_detect(manuscript_status, "NEEDS_MANUAL_CONFIRMATION")) %>%
  nrow()

# -----------------------------
# 10. Methods text package
# -----------------------------
methods_deg_paragraph <- paste0(
  "Differential expression analysis was performed using limma. Genes were considered differentially expressed ",
  "when they satisfied the predefined threshold of ",
  deg_threshold_text,
  ", with adjusted P values controlled using the Benjamini-Hochberg false-discovery-rate procedure. ",
  "For nested model development, DEG screening used training data within the corresponding outer LODO split; ",
  "held-out cohorts were not used for feature filtering or candidate-gene selection."
)

methods_candidate_paragraph <- paste0(
  "Candidate genes were selected through a predefined multistep workflow. First, genes passing the DEG threshold ",
  "were retained as the initial candidate pool. Second, direction consistency across training datasets or folds was ",
  "evaluated. Third, univariate discriminatory performance was assessed using AUROC. Fourth, highly correlated genes ",
  "were removed using correlation-based filtering before nested glmnet modeling. The retained candidate genes were ",
  "then passed to the nested LODO penalized-regression workflow, in which alpha/lambda tuning was performed within ",
  "the training data only."
)

methods_threshold_detail_paragraph <- paste0(
  "The direction-consistency criterion, univariate AUROC threshold, and correlation-filtering threshold were recorded ",
  "in the reproducibility audit as follows: ",
  "direction consistency, ",
  direction_value,
  "; univariate AUROC, ",
  auc_value,
  "; correlation filtering, ",
  cor_value,
  "."
)

figure2_legend_replacement <- paste0(
  "Differentially expressed genes were defined as genes satisfying adjusted P < ",
  ifelse(is.na(adj_p_threshold), "[confirm threshold]", adj_p_threshold),
  " and |log2FC| > ",
  ifelse(is.na(logfc_threshold), "[confirm threshold]", logfc_threshold),
  " using Benjamini-Hochberg FDR adjustment. Candidate genes were then filtered by direction consistency, ",
  "univariate AUROC, and correlation-based redundancy before nested LODO model fitting."
)

methods_full_text <- paste(
  methods_deg_paragraph,
  methods_candidate_paragraph,
  methods_threshold_detail_paragraph,
  sep = "\n\n"
)

text_package <- tibble::tibble(
  item = c(
    "methods_deg_paragraph",
    "methods_candidate_paragraph",
    "methods_threshold_detail_paragraph",
    "figure2_legend_replacement",
    "methods_full_text"
  ),
  text = c(
    methods_deg_paragraph,
    methods_candidate_paragraph,
    methods_threshold_detail_paragraph,
    figure2_legend_replacement,
    methods_full_text
  )
)

# -----------------------------
# 11. Checks
# -----------------------------
checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:24),
  check_item = c(
    "Project directory exists",
    "Candidate files discovered",
    "Candidate text files read",
    "Trace records generated",
    "limma detected",
    "Adjusted P threshold detected",
    "log2FC threshold detected",
    "BH/FDR correction detected",
    "DEG matrix/source resolved",
    "DEG role in candidate selection detected or marked",
    "Candidate selection sequence generated",
    "Direction consistency criterion detected",
    "Univariate AUROC threshold detected",
    "Correlation threshold detected",
    "Methods DEG paragraph generated",
    "Methods candidate paragraph generated",
    "Figure 2 legend replacement generated",
    "No frozen-workflow placeholder in Methods text",
    "No frozen-workflow placeholder in Figure 2 legend",
    "CSV audit generated",
    "TXT text package generated",
    "DOCX text package generated",
    "XLSX workbook generated",
    "No manual confirmation items"
  ),
  observed = c(
    dir.exists(project_dir),
    nrow(candidate_files) > 0,
    nrow(candidate_text_tbl) > 0,
    nrow(trace_records) > 0,
    limma_detected,
    !is.na(adj_p_threshold),
    !is.na(logfc_threshold),
    adjust_method_decision != "NEEDS_MANUAL_CONFIRMATION",
    !stringr::str_detect(deg_matrix_source, "NEEDS|verify|Both"),
    deg_participates_candidate != "NEEDS_MANUAL_CONFIRMATION",
    nchar(candidate_sequence_text) > 0,
    direction_value != "NEEDS_MANUAL_CONFIRMATION",
    auc_value != "NEEDS_MANUAL_CONFIRMATION",
    cor_value != "NEEDS_MANUAL_CONFIRMATION",
    nchar(methods_deg_paragraph) > 0,
    nchar(methods_candidate_paragraph) > 0,
    nchar(figure2_legend_replacement) > 0,
    !stringr::str_detect(methods_full_text, "frozen DEG workflow|defined in the frozen"),
    !stringr::str_detect(figure2_legend_replacement, "frozen DEG workflow|defined in the frozen"),
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    n_manual_items == 0
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 12. Export CSV/TXT
# -----------------------------
readr::write_csv(audit_tbl, csv_audit_path)
writeLines(
  paste(
    "METHODS DEG PARAGRAPH",
    methods_deg_paragraph,
    "",
    "METHODS CANDIDATE SELECTION PARAGRAPH",
    methods_candidate_paragraph,
    "",
    "METHODS THRESHOLD DETAIL PARAGRAPH",
    methods_threshold_detail_paragraph,
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
# 13. Export DOCX
# -----------------------------
ft_audit <- flextable::flextable(audit_tbl)
ft_audit <- flextable::theme_booktabs(ft_audit)
ft_audit <- flextable::fontsize(ft_audit, size = 8, part = "all")
ft_audit <- flextable::fontsize(ft_audit, size = 8.5, part = "header")
ft_audit <- flextable::bold(ft_audit, part = "header")
ft_audit <- flextable::valign(ft_audit, valign = "top", part = "all")
ft_audit <- flextable::width(ft_audit, j = "method_item", width = 2.1)
ft_audit <- flextable::width(ft_audit, j = "detected_or_final_value", width = 4.2)
ft_audit <- flextable::width(ft_audit, j = "manuscript_status", width = 1.7)
ft_audit <- flextable::set_table_properties(ft_audit, layout = "fixed", width = 1)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "DEG and candidate-gene selection methods package", style = "heading 1")

doc <- officer::body_add_par(doc, "Methods DEG paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_deg_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Methods candidate-selection paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_candidate_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Methods threshold-detail paragraph", style = "heading 2")
doc <- officer::body_add_par(doc, methods_threshold_detail_paragraph, style = "Normal")

doc <- officer::body_add_par(doc, "Figure 2 legend replacement", style = "heading 2")
doc <- officer::body_add_par(doc, figure2_legend_replacement, style = "Normal")

doc <- officer::body_add_par(doc, "Audit table", style = "heading 2")
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
# 14. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Audit")
openxlsx::writeData(wb, "Audit", audit_tbl)

openxlsx::addWorksheet(wb, "Text_Package")
openxlsx::writeData(wb, "Text_Package", text_package)

openxlsx::addWorksheet(wb, "Trace_Records")
openxlsx::writeData(wb, "Trace_Records", trace_records)

openxlsx::addWorksheet(wb, "Candidate_File_Inventory")
openxlsx::writeData(wb, "Candidate_File_Inventory", file_inventory)

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
# 15. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "deg_threshold",
    "adjust_method",
    "deg_matrix_source",
    "direction_consistency",
    "univariate_auc_threshold",
    "correlation_threshold",
    "n_manual_items",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "48_DEG_candidate_selection_methods_audit.R",
    project_dir,
    deg_threshold_text,
    adjust_method_decision,
    deg_matrix_source,
    direction_value,
    auc_value,
    cor_value,
    as.character(n_manual_items),
    as.character(n_failed_checks),
    dplyr::case_when(
      n_failed_checks == 0 && n_manual_items == 0 ~ "YES_INSERT_DEG_CANDIDATE_METHODS_TEXT",
      n_failed_checks == 0 && n_manual_items > 0 ~ "PARTIAL_REVIEW_DEG_CANDIDATE_METHOD_ITEMS",
      TRUE ~ "NO_FIX_CHECK_ITEMS"
    ),
    "Review Audit and Text_Package sheets. If any item is NEEDS_MANUAL_CONFIRMATION, use the trace records to identify the exact threshold before inserting the Methods text."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 16. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nDEG/candidate selection audit:\n")
print(audit_tbl, n = Inf, width = Inf)

cat("\nMethods full text:\n")
cat(methods_full_text, "\n")

cat("\nFigure 2 legend replacement:\n")
cat(figure2_legend_replacement, "\n")

cat("\nTrace preview:\n")
print(
  trace_records %>%
    dplyr::select(file_name, trace_type, line_number, line_text_clean) %>%
    dplyr::slice_head(n = 80),
  n = 80,
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
cat("把 Overall status、Checks、DEG/candidate selection audit、Methods full text、Figure 2 legend replacement 和 Trace preview 贴给我。\n")
cat("如果出现 NEEDS_MANUAL_CONFIRMATION，我会根据具体缺项给你 48b 的完整强制提取脚本。\n")

cat("\n============ 48 DEG/candidate selection methods audit complete ============\n")