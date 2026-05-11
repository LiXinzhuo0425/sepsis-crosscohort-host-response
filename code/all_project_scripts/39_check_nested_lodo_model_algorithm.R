# ============================================================
# 39_check_nested_lodo_model_algorithm.R
# Purpose:
#   Audit actual project scripts and frozen outputs to verify
#   whether Nested LODO used Lasso logistic regression:
#   glmnet/cv.glmnet, family = "binomial", alpha = 1, lambda.1se.
#
# Save as:
#   /Users/felix/Documents/Sepsis_CrossCohort_scRNA/00_scripts/39_check_nested_lodo_model_algorithm.R
# ============================================================

options(stringsAsFactors = FALSE)
options(warn = 1)

cat("\n============ 39 Nested LODO model algorithm audit ============\n")

# -----------------------------
# 0. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

script_dir <- file.path(project_dir, "00_scripts")
results_dir <- file.path(project_dir, "04_results")
freeze_dir <- file.path(results_dir, "final_freeze")
audit_dir <- file.path(project_dir, "04_results", "model_algorithm_audit")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# -----------------------------
# 1. Utility functions
# -----------------------------
safe_list_files <- function(path, pattern = NULL, recursive = TRUE, full.names = TRUE) {
  if (!dir.exists(path)) return(character(0))
  out <- tryCatch(
    list.files(path, pattern = pattern, recursive = recursive, full.names = full.names),
    error = function(e) character(0)
  )
  out
}

safe_read_lines <- function(path, max_lines = 20000) {
  if (!file.exists(path)) return(character(0))
  x <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0))
    }
  )
  if (length(x) > max_lines) x <- x[seq_len(max_lines)]
  x
}

detect_pattern <- function(lines, pattern, ignore.case = TRUE) {
  if (length(lines) == 0) return(integer(0))
  grep(pattern, lines, ignore.case = ignore.case, perl = TRUE)
}

extract_hits <- function(files, patterns, context_n = 2) {
  hit_list <- list()
  idx <- 1
  
  for (f in files) {
    lines <- safe_read_lines(f)
    if (length(lines) == 0) next
    
    for (pname in names(patterns)) {
      pat <- patterns[[pname]]
      hit_idx <- detect_pattern(lines, pat, ignore.case = TRUE)
      
      if (length(hit_idx) > 0) {
        for (i in hit_idx) {
          lo <- max(1, i - context_n)
          hi <- min(length(lines), i + context_n)
          context <- paste(lines[lo:hi], collapse = "\n")
          
          hit_list[[idx]] <- data.frame(
            file = f,
            file_name = basename(f),
            pattern_name = pname,
            pattern = pat,
            line_number = i,
            line_text = lines[i],
            context = context,
            stringsAsFactors = FALSE
          )
          idx <- idx + 1
        }
      }
    }
  }
  
  if (length(hit_list) == 0) {
    return(data.frame(
      file = character(0),
      file_name = character(0),
      pattern_name = character(0),
      pattern = character(0),
      line_number = integer(0),
      line_text = character(0),
      context = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, hit_list)
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

read_any_table_safe <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") return(read_csv_safe(path))
  
  if (ext %in% c("tsv", "txt")) {
    return(tryCatch(
      read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    ))
  }
  
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) return(NULL)
    return(tryCatch(
      readxl::read_excel(path),
      error = function(e) NULL
    ))
  }
  
  NULL
}

contains_any <- function(x, patterns) {
  if (length(x) == 0 || all(is.na(x))) return(FALSE)
  txt <- paste(as.character(x), collapse = " ")
  any(vapply(patterns, function(p) grepl(p, txt, ignore.case = TRUE, perl = TRUE), logical(1)))
}

# -----------------------------
# 2. Search candidate files
# -----------------------------
all_script_files <- unique(c(
  safe_list_files(project_dir, pattern = "\\.(R|r|Rmd|rmd|qmd|md|txt|csv)$", recursive = TRUE),
  safe_list_files(script_dir, pattern = "\\.(R|r|Rmd|rmd|qmd)$", recursive = TRUE)
))

# Exclude very large raw expression matrices to avoid slow scanning
exclude_dirs <- c(
  "01_raw_data",
  "02_processed_data/bulk_gene_matrix",
  "02_processed_data/main_expr"
)

if (length(all_script_files) > 0) {
  keep <- rep(TRUE, length(all_script_files))
  for (ed in exclude_dirs) {
    keep <- keep & !grepl(ed, all_script_files, fixed = TRUE)
  }
  all_script_files <- all_script_files[keep]
}

candidate_script_files <- all_script_files[
  grepl("LODO|nested|T14|model|glmnet|lasso|elastic|signature|validation|train|threshold",
        basename(all_script_files),
        ignore.case = TRUE)
]

# If filename filtering is too narrow, still search all scripts.
files_to_search <- unique(c(candidate_script_files, all_script_files))

cat("\nFiles scanned for algorithm keywords: ", length(files_to_search), "\n", sep = "")

# -----------------------------
# 3. Keyword audit in scripts/text outputs
# -----------------------------
patterns <- c(
  glmnet_call = "\\bglmnet\\s*\\(",
  cv_glmnet_call = "\\bcv\\.glmnet\\s*\\(",
  family_binomial = "family\\s*=\\s*['\"]binomial['\"]|family\\s*=\\s*binomial",
  alpha_1_exact = "alpha\\s*=\\s*1\\b|alpha\\s*=\\s*1\\.0\\b|alpha\\s*=\\s*1\\.00\\b",
  alpha_grid = "alpha.*(1\\.00|1\\.0|1).*0\\.75.*0\\.50|alpha_grid|alpha\\.grid",
  lambda_1se = "lambda\\.1se|s\\s*=\\s*['\"]lambda\\.1se['\"]",
  lambda_min = "lambda\\.min|s\\s*=\\s*['\"]lambda\\.min['\"]",
  lasso_text = "\\bLASSO\\b|\\blasso\\b",
  elastic_net_text = "elastic net|Elastic Net|elastic_net",
  nested_lodo_text = "nested.*LODO|LODO|leave-one-dataset|held-out",
  threshold_youden = "Youden|optimal threshold|local threshold|fixed threshold",
  scale_training_only = "training.*scal|held-out.*scal|standardiz|center|scale",
  leakage_guard = "leakage|data leakage|held-out.*excluded|excluded.*held-out"
)

keyword_hits <- extract_hits(files_to_search, patterns, context_n = 2)

keyword_hits_path <- file.path(audit_dir, "T39_script_keyword_hits.csv")
write.csv(keyword_hits, keyword_hits_path, row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------
# 4. Per-file evidence summary
# -----------------------------
if (nrow(keyword_hits) > 0) {
  per_file <- aggregate(
    pattern_name ~ file + file_name,
    data = keyword_hits,
    FUN = function(x) paste(sort(unique(x)), collapse = ";")
  )
  names(per_file)[names(per_file) == "pattern_name"] <- "detected_patterns"
  
  per_file$n_hits <- vapply(
    seq_len(nrow(per_file)),
    function(i) sum(keyword_hits$file == per_file$file[i]),
    integer(1)
  )
  
  per_file$likely_model_script <- grepl(
    "cv_glmnet_call|glmnet_call|alpha_1_exact|lambda_1se|family_binomial",
    per_file$detected_patterns
  ) & grepl(
    "nested_lodo_text|threshold_youden|scale_training_only|leakage_guard",
    per_file$detected_patterns
  )
} else {
  per_file <- data.frame(
    file = character(0),
    file_name = character(0),
    detected_patterns = character(0),
    n_hits = integer(0),
    likely_model_script = logical(0)
  )
}

per_file_path <- file.path(audit_dir, "T39_per_file_algorithm_evidence.csv")
write.csv(per_file, per_file_path, row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------
# 5. Search frozen result files for model metadata
# -----------------------------
frozen_files <- unique(c(
  safe_list_files(freeze_dir, pattern = "\\.(csv|txt|md|xlsx)$", recursive = TRUE),
  safe_list_files(results_dir, pattern = "\\.(csv|txt|md|xlsx)$", recursive = TRUE)
))

frozen_files <- frozen_files[
  grepl("T14|nested|LODO|model|gene|selection|frequency|lambda|alpha|glmnet|validation|key|freeze",
        basename(frozen_files),
        ignore.case = TRUE)
]

cat("Frozen/result files scanned: ", length(frozen_files), "\n", sep = "")

frozen_patterns <- c(
  alpha_column_or_text = "\\balpha\\b",
  lambda_column_or_text = "\\blambda\\b|lambda\\.1se|lambda\\.min",
  glmnet_text = "glmnet|cv\\.glmnet",
  lasso_text = "\\blasso\\b|\\bLASSO\\b",
  binomial_text = "binomial|logistic",
  selected_gene_text = "selected_gene|selected genes|gene_selection|gene frequency|genes_selected",
  final10_text = "Final10|Final 10|10-gene|10 gene",
  nested_recurrent_text = "NestedRecurrent|nested recurrent|recurrent"
)

frozen_text_hits <- extract_hits(frozen_files, frozen_patterns, context_n = 2)
frozen_text_hits_path <- file.path(audit_dir, "T39_frozen_text_keyword_hits.csv")
write.csv(frozen_text_hits, frozen_text_hits_path, row.names = FALSE, fileEncoding = "UTF-8")

# Table-level column scan
table_scan_list <- list()
ti <- 1

for (f in frozen_files) {
  tbl <- read_any_table_safe(f)
  if (is.null(tbl)) next
  
  cn <- names(tbl)
  tbl_text_sample <- NULL
  
  # Sample first 100 rows to avoid heavy memory
  if (nrow(tbl) > 0) {
    tbl_small <- tbl[seq_len(min(100, nrow(tbl))), , drop = FALSE]
    tbl_text_sample <- unlist(tbl_small, use.names = FALSE)
  }
  
  table_scan_list[[ti]] <- data.frame(
    file = f,
    file_name = basename(f),
    n_rows = nrow(tbl),
    n_cols = ncol(tbl),
    columns = paste(cn, collapse = ";"),
    has_alpha_column = any(grepl("\\balpha\\b", cn, ignore.case = TRUE)),
    has_lambda_column = any(grepl("\\blambda\\b", cn, ignore.case = TRUE)),
    has_model_column = any(grepl("model|method|algorithm|classifier", cn, ignore.case = TRUE)),
    has_selected_gene_column = any(grepl("gene|selected", cn, ignore.case = TRUE)),
    text_mentions_alpha1 = contains_any(tbl_text_sample, c("alpha\\s*=\\s*1", "alpha.*1\\.00")),
    text_mentions_lambda1se = contains_any(tbl_text_sample, c("lambda\\.1se")),
    text_mentions_lasso = contains_any(tbl_text_sample, c("\\blasso\\b", "\\bLASSO\\b")),
    text_mentions_binomial_or_logistic = contains_any(tbl_text_sample, c("binomial", "logistic")),
    stringsAsFactors = FALSE
  )
  ti <- ti + 1
}

if (length(table_scan_list) > 0) {
  table_scan <- do.call(rbind, table_scan_list)
} else {
  table_scan <- data.frame(
    file = character(0),
    file_name = character(0),
    n_rows = integer(0),
    n_cols = integer(0),
    columns = character(0),
    has_alpha_column = logical(0),
    has_lambda_column = logical(0),
    has_model_column = logical(0),
    has_selected_gene_column = logical(0),
    text_mentions_alpha1 = logical(0),
    text_mentions_lambda1se = logical(0),
    text_mentions_lasso = logical(0),
    text_mentions_binomial_or_logistic = logical(0),
    stringsAsFactors = FALSE
  )
}

table_scan_path <- file.path(audit_dir, "T39_frozen_table_column_scan.csv")
write.csv(table_scan, table_scan_path, row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------
# 6. Strong-evidence extraction
# -----------------------------
strong_script_hits <- keyword_hits[
  keyword_hits$pattern_name %in% c(
    "cv_glmnet_call",
    "glmnet_call",
    "family_binomial",
    "alpha_1_exact",
    "lambda_1se",
    "lambda_min",
    "alpha_grid"
  ),
  ,
  drop = FALSE
]

strong_script_hits_path <- file.path(audit_dir, "T39_strong_script_evidence_hits.csv")
write.csv(strong_script_hits, strong_script_hits_path, row.names = FALSE, fileEncoding = "UTF-8")

# Per-file high-confidence flags
if (nrow(keyword_hits) > 0) {
  evidence_by_file <- unique(keyword_hits[, c("file", "file_name")])
  
  evidence_by_file$has_cv_glmnet <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name == "cv_glmnet_call"),
    logical(1)
  )
  evidence_by_file$has_glmnet <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name == "glmnet_call"),
    logical(1)
  )
  evidence_by_file$has_family_binomial <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name == "family_binomial"),
    logical(1)
  )
  evidence_by_file$has_alpha_1 <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name == "alpha_1_exact"),
    logical(1)
  )
  evidence_by_file$has_lambda_1se <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name == "lambda_1se"),
    logical(1)
  )
  evidence_by_file$has_nested_lodo <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name == "nested_lodo_text"),
    logical(1)
  )
  evidence_by_file$has_leakage_guard <- vapply(
    evidence_by_file$file,
    function(f) any(keyword_hits$file == f & keyword_hits$pattern_name %in% c("leakage_guard", "scale_training_only")),
    logical(1)
  )
  
  evidence_by_file$strong_support_lasso_logistic_lambda1se <- with(
    evidence_by_file,
    (has_cv_glmnet | has_glmnet) &
      has_family_binomial &
      has_alpha_1 &
      has_lambda_1se
  )
  
  evidence_by_file$strong_support_nested_training_only <- with(
    evidence_by_file,
    has_nested_lodo & has_leakage_guard
  )
  
  evidence_by_file$overall_support_level <- ifelse(
    evidence_by_file$strong_support_lasso_logistic_lambda1se &
      evidence_by_file$strong_support_nested_training_only,
    "STRONG_IN_SAME_OR_LINKED_SCRIPT",
    ifelse(
      evidence_by_file$strong_support_lasso_logistic_lambda1se,
      "STRONG_ALGORITHM_ONLY",
      ifelse(
        evidence_by_file$strong_support_nested_training_only,
        "STRONG_NESTED_DESIGN_ONLY",
        "PARTIAL_OR_CONTEXT"
      )
    )
  )
} else {
  evidence_by_file <- data.frame()
}

evidence_by_file_path <- file.path(audit_dir, "T39_evidence_by_file_summary.csv")
write.csv(evidence_by_file, evidence_by_file_path, row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------
# 7. Overall decision rule
# -----------------------------
n_algorithm_strong <- if (nrow(evidence_by_file) > 0) {
  sum(evidence_by_file$strong_support_lasso_logistic_lambda1se, na.rm = TRUE)
} else 0

n_nested_strong <- if (nrow(evidence_by_file) > 0) {
  sum(evidence_by_file$strong_support_nested_training_only, na.rm = TRUE)
} else 0

n_full_strong <- if (nrow(evidence_by_file) > 0) {
  sum(evidence_by_file$overall_support_level == "STRONG_IN_SAME_OR_LINKED_SCRIPT", na.rm = TRUE)
} else 0

frozen_has_alpha <- if (nrow(table_scan) > 0) {
  any(table_scan$has_alpha_column | table_scan$text_mentions_alpha1, na.rm = TRUE)
} else FALSE

frozen_has_lambda <- if (nrow(table_scan) > 0) {
  any(table_scan$has_lambda_column | table_scan$text_mentions_lambda1se, na.rm = TRUE)
} else FALSE

frozen_has_lasso_logistic <- if (nrow(table_scan) > 0) {
  any(table_scan$text_mentions_lasso | table_scan$text_mentions_binomial_or_logistic, na.rm = TRUE)
} else FALSE

decision <- "UNRESOLVED_NEED_MANUAL_SCRIPT_OPENING"

if (n_full_strong >= 1) {
  decision <- "SUPPORTED_LASSO_LOGISTIC_ALPHA1_LAMBDA1SE_WITH_NESTED_TRAINING_ONLY_EVIDENCE"
} else if (n_algorithm_strong >= 1 && n_nested_strong >= 1) {
  decision <- "SUPPORTED_BUT_EVIDENCE_SPLIT_ACROSS_FILES"
} else if (n_algorithm_strong >= 1) {
  decision <- "ALGORITHM_SUPPORTED_BUT_NESTED_TRAINING_ONLY_NOT_CONFIRMED"
} else if (n_nested_strong >= 1) {
  decision <- "NESTED_TRAINING_ONLY_SUPPORTED_BUT_ALGORITHM_NOT_CONFIRMED"
}

overall_status <- data.frame(
  metric = c(
    "audit_timestamp",
    "project_dir",
    "n_files_scanned",
    "n_keyword_hits",
    "n_files_with_strong_algorithm_evidence",
    "n_files_with_strong_nested_training_only_evidence",
    "n_files_with_full_strong_evidence",
    "frozen_outputs_mention_alpha_or_have_alpha_column",
    "frozen_outputs_mention_lambda_or_have_lambda_column",
    "frozen_outputs_mention_lasso_or_logistic",
    "decision",
    "recommended_methods_wording"
  ),
  value = c(
    timestamp,
    project_dir,
    length(files_to_search),
    nrow(keyword_hits),
    n_algorithm_strong,
    n_nested_strong,
    n_full_strong,
    frozen_has_alpha,
    frozen_has_lambda,
    frozen_has_lasso_logistic,
    decision,
    ifelse(
      decision %in% c(
        "SUPPORTED_LASSO_LOGISTIC_ALPHA1_LAMBDA1SE_WITH_NESTED_TRAINING_ONLY_EVIDENCE",
        "SUPPORTED_BUT_EVIDENCE_SPLIT_ACROSS_FILES"
      ),
      "Use: Lasso-penalized logistic regression, alpha = 1, lambda selected by inner CV using lambda.1se; all tuning performed within training folds.",
      "Do not finalize algorithm wording yet; open the strong evidence hit files and verify actual cv.glmnet call manually."
    )
  ),
  stringsAsFactors = FALSE
)

overall_status_path <- file.path(audit_dir, "T39_overall_algorithm_audit_status.csv")
write.csv(overall_status, overall_status_path, row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------
# 8. Human-readable report
# -----------------------------
report_path <- file.path(audit_dir, "T39_nested_lodo_model_algorithm_audit_report.md")

top_files <- evidence_by_file
if (nrow(top_files) > 0) {
  top_files <- top_files[order(
    top_files$overall_support_level,
    top_files$file_name,
    decreasing = FALSE
  ), , drop = FALSE]
}

report_lines <- c(
  "# T39 Nested LODO model algorithm audit",
  "",
  paste0("Audit timestamp: ", timestamp),
  "",
  "## Overall decision",
  "",
  paste0("Decision: **", decision, "**"),
  "",
  paste0("Recommended wording: ", overall_status$value[overall_status$metric == "recommended_methods_wording"]),
  "",
  "## What was checked",
  "",
  "- glmnet/cv.glmnet calls",
  "- family = binomial",
  "- alpha = 1 / 1.0 / 1.00",
  "- lambda.1se and lambda.min",
  "- nested LODO / held-out language",
  "- training-only scaling or leakage-guard language",
  "- frozen outputs for alpha/lambda/model/gene-selection metadata",
  "",
  "## Key output files",
  "",
  paste0("- Script keyword hits: `", keyword_hits_path, "`"),
  paste0("- Strong script evidence: `", strong_script_hits_path, "`"),
  paste0("- Per-file evidence summary: `", evidence_by_file_path, "`"),
  paste0("- Frozen table scan: `", table_scan_path, "`"),
  paste0("- Overall status: `", overall_status_path, "`"),
  "",
  "## Interpretation guide",
  "",
  "- If decision is `SUPPORTED_LASSO_LOGISTIC_ALPHA1_LAMBDA1SE_WITH_NESTED_TRAINING_ONLY_EVIDENCE`, the Methods can explicitly state Lasso-penalized logistic regression with alpha = 1 and lambda.1se selected by inner CV.",
  "- If decision is `SUPPORTED_BUT_EVIDENCE_SPLIT_ACROSS_FILES`, open the files listed in the evidence summary and verify that the algorithm and nested training-only design belong to the same T14/Nested LODO workflow.",
  "- If decision is unresolved, do not write alpha = 1 or lambda.1se in the manuscript yet.",
  ""
)

writeLines(report_lines, report_path, useBytes = TRUE)

# -----------------------------
# 9. Print concise console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nTop evidence files:\n")
if (nrow(evidence_by_file) > 0) {
  print(
    evidence_by_file[
      order(evidence_by_file$overall_support_level, evidence_by_file$file_name),
      c(
        "file_name",
        "has_cv_glmnet",
        "has_family_binomial",
        "has_alpha_1",
        "has_lambda_1se",
        "has_nested_lodo",
        "has_leakage_guard",
        "overall_support_level",
        "file"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
} else {
  message("No evidence files detected.")
}

cat("\nStrong script evidence hits, first 30 rows:\n")
if (nrow(strong_script_hits) > 0) {
  print(
    head(
      strong_script_hits[
        ,
        c("file_name", "pattern_name", "line_number", "line_text", "file"),
        drop = FALSE
      ],
      30
    ),
    row.names = FALSE
  )
} else {
  message("No strong script evidence hits detected.")
}

cat("\nFrozen table scan, first 30 rows:\n")
if (nrow(table_scan) > 0) {
  print(
    head(
      table_scan[
        ,
        c(
          "file_name",
          "n_rows",
          "n_cols",
          "has_alpha_column",
          "has_lambda_column",
          "has_model_column",
          "has_selected_gene_column",
          "text_mentions_alpha1",
          "text_mentions_lambda1se",
          "text_mentions_lasso",
          "text_mentions_binomial_or_logistic",
          "file"
        ),
        drop = FALSE
      ],
      30
    ),
    row.names = FALSE
  )
} else {
  message("No frozen tables could be scanned.")
}

cat("\n关键输出：\n")
cat("1) ", overall_status_path, "\n", sep = "")
cat("2) ", evidence_by_file_path, "\n", sep = "")
cat("3) ", strong_script_hits_path, "\n", sep = "")
cat("4) ", keyword_hits_path, "\n", sep = "")
cat("5) ", table_scan_path, "\n", sep = "")
cat("6) ", report_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Top evidence files、Strong script evidence hits 贴给我。\n")
cat("如果 decision 支持 alpha=1/lambda.1se，我会帮你把 Methods 第5节定稿。\n")

cat("\n============ 39 audit complete ============\n")