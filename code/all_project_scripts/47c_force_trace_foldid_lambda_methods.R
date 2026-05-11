# ============================================================
# 47c_force_trace_foldid_lambda_methods.R
# Purpose:
#   Force-trace foldid definition and lambda.1se usage after 47b.
#
# Why:
#   47b confirmed:
#     - cv.glmnet uses foldid = foldid
#     - no weights argument in cv.glmnet
#     - no resampling detected
#   But unresolved:
#     - number of unique inner CV folds
#     - lambda.1se usage not detected in local cv.glmnet context
#
# This script scans modeling scripts for:
#   - foldid construction
#   - nfolds_inner_cv / nfolds_cv assignment
#   - sample(..., rep(1:nfolds...)) pattern
#   - createFolds(..., k = ...)
#   - coef/predict calls using s = "lambda.1se"
#   - lambda_selected <- cvfit$lambda.1se
#
# Output:
#   04_results/nested_LODO_methods_audit_47c/
#     T47c_foldid_lambda_trace_audit.csv
#     T47c_final_methods_audit_table.csv
#     T47c_nested_LODO_methods_final_text.txt
#     T47c_nested_LODO_methods_final_text.docx
#     T47c_nested_LODO_methods_audit_workbook.xlsx
#     T47c_checks.csv
#     T47c_overall_status.csv
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

dir_out <- file.path(dir_results, "nested_LODO_methods_audit_47c")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

trace_audit_path <- file.path(dir_out, "T47c_foldid_lambda_trace_audit.csv")
method_audit_path <- file.path(dir_out, "T47c_final_methods_audit_table.csv")
txt_methods_path <- file.path(dir_out, "T47c_nested_LODO_methods_final_text.txt")
docx_methods_path <- file.path(dir_out, "T47c_nested_LODO_methods_final_text.docx")
xlsx_path <- file.path(dir_out, "T47c_nested_LODO_methods_audit_workbook.xlsx")
checks_path <- file.path(dir_out, "T47c_checks.csv")
overall_path <- file.path(dir_out, "T47c_overall_status.csv")

# -----------------------------
# 2. Utility functions
# -----------------------------
safe_read_lines <- function(path) {
  tryCatch(readLines(path, warn = FALSE), error = function(e) character())
}

clean_space <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

extract_nearby_context <- function(lines, hit_lines, window = 12) {
  if (length(hit_lines) == 0) return(character())
  
  purrr::map_chr(hit_lines, function(i) {
    from <- max(1, i - window)
    to <- min(length(lines), i + window)
    paste(lines[from:to], collapse = "\n")
  })
}

extract_assignment_lines <- function(lines, patterns) {
  hit <- which(stringr::str_detect(
    lines,
    stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)
  ))
  
  tibble::tibble(
    line_number = hit,
    line_text = lines[hit],
    context_text = extract_nearby_context(lines, hit, window = 15)
  )
}

detect_first_number_after_patterns <- function(text, patterns) {
  if (length(text) == 0 || all(is.na(text))) return(NA_character_)
  
  combined <- paste(text, collapse = "\n")
  
  for (pat in patterns) {
    hit <- stringr::str_match(
      combined,
      stringr::regex(pat, ignore_case = TRUE)
    )
    
    if (!all(is.na(hit)) && ncol(hit) >= 2 && !is.na(hit[1, 2])) {
      return(hit[1, 2])
    }
  }
  
  NA_character_
}

detect_any <- function(text, patterns) {
  if (length(text) == 0 || all(is.na(text))) return(FALSE)
  combined <- paste(text, collapse = "\n")
  any(stringr::str_detect(combined, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)))
}

# -----------------------------
# 3. Locate scripts
# -----------------------------
all_r_scripts <- list.files(
  project_dir,
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)

all_r_scripts <- all_r_scripts[
  !stringr::str_detect(all_r_scripts, "nested_LODO_methods_audit_47c")
]

priority_names <- c(
  "08_LODO_diagnostic_modeling.R",
  "09_LODO_calibration_threshold_analysis.R",
  "10_final_compact_model_training.R",
  "14_nested_LODO_full_pipeline.R",
  "17_nested_LODO_calibration_threshold_DCA.R"
)

scripts_priority <- all_r_scripts[basename(all_r_scripts) %in% priority_names]

scripts_with_cv_or_fold <- all_r_scripts[
  purrr::map_lgl(all_r_scripts, function(path) {
    txt <- paste(safe_read_lines(path), collapse = "\n")
    stringr::str_detect(
      txt,
      stringr::regex("cv\\.glmnet|foldid|nfolds_inner_cv|nfolds_cv|lambda\\.1se|lambda\\.min", ignore_case = TRUE)
    )
  })
]

scripts_to_audit <- unique(c(scripts_priority, scripts_with_cv_or_fold))

# -----------------------------
# 4. Trace foldid, nfolds, lambda usage
# -----------------------------
trace_records <- purrr::map_dfr(scripts_to_audit, function(path) {
  lines <- safe_read_lines(path)
  
  foldid_records <- extract_assignment_lines(
    lines,
    c(
      "foldid\\s*(<-|=)",
      "foldid\\s*=\\s*foldid",
      "nfolds_inner_cv\\s*(<-|=)",
      "nfolds_cv\\s*(<-|=)",
      "inner_cv",
      "createFolds",
      "sample\\s*\\(",
      "rep\\s*\\(\\s*1\\s*:"
    )
  ) %>%
    dplyr::mutate(trace_type = "foldid_or_nfolds")
  
  lambda_records <- extract_assignment_lines(
    lines,
    c(
      "lambda\\.1se",
      "lambda\\.min",
      "s\\s*=\\s*[\"']lambda\\.1se[\"']",
      "s\\s*=\\s*[\"']lambda\\.min[\"']",
      "\\$lambda\\.1se",
      "\\$lambda\\.min",
      "selected_lambda",
      "best_lambda",
      "coef\\s*\\(",
      "predict\\s*\\("
    )
  ) %>%
    dplyr::mutate(trace_type = "lambda_or_prediction")
  
  dplyr::bind_rows(foldid_records, lambda_records) %>%
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

fold_context <- trace_records %>%
  dplyr::filter(trace_type == "foldid_or_nfolds") %>%
  dplyr::pull(context_text_clean)

lambda_context <- trace_records %>%
  dplyr::filter(trace_type == "lambda_or_prediction") %>%
  dplyr::pull(context_text_clean)

# -----------------------------
# 5. Decide inner fold count
# -----------------------------
fold_count_patterns <- c(
  "nfolds_inner_cv\\s*(<-|=)\\s*([0-9]+)",
  "nfolds_cv\\s*(<-|=)\\s*([0-9]+)",
  "nfolds\\s*(<-|=)\\s*([0-9]+)",
  "k\\s*=\\s*([0-9]+)",
  "1\\s*:\\s*([0-9]+)"
)

inner_fold_count <- detect_first_number_after_patterns(fold_context, fold_count_patterns)

foldid_detected <- detect_any(fold_context, c("foldid"))
create_folds_detected <- detect_any(fold_context, c("createFolds"))
sample_foldid_detected <- detect_any(fold_context, c("sample\\s*\\(|rep\\s*\\(\\s*1\\s*:"))

if (!is.na(inner_fold_count)) {
  inner_cv_decision <- paste0(inner_fold_count, "-fold inner cross-validation")
  inner_cv_status <- "READY"
} else if (foldid_detected) {
  inner_cv_decision <- "foldid was detected, but the number of inner folds was not recoverable from static script tracing."
  inner_cv_status <- "NEEDS_MANUAL_CONFIRMATION"
} else {
  inner_cv_decision <- "No explicit foldid or nfolds detected; cv.glmnet default nfolds = 10 would apply."
  inner_cv_status <- "READY_DEFAULT_10"
  inner_fold_count <- "10"
}

# -----------------------------
# 6. Decide lambda rule
# -----------------------------
lambda_1se_detected <- detect_any(lambda_context, c("lambda\\.1se", "s\\s*=\\s*[\"']lambda\\.1se[\"']"))
lambda_min_detected <- detect_any(lambda_context, c("lambda\\.min", "s\\s*=\\s*[\"']lambda\\.min[\"']"))

if (lambda_1se_detected) {
  lambda_decision <- "lambda.1se"
  lambda_status <- "READY"
} else if (lambda_min_detected) {
  lambda_decision <- "lambda.min"
  lambda_status <- "READY_BUT_NOT_LAMBDA_1SE"
} else {
  lambda_decision <- "NEEDS_MANUAL_CONFIRMATION"
  lambda_status <- "NEEDS_MANUAL_CONFIRMATION"
}

# -----------------------------
# 7. Fixed decisions carried from 47b
# -----------------------------
weights_decision <- "No weights argument was detected in cv.glmnet calls; class weights were not passed to glmnet model fitting."
resampling_decision <- "No oversampling, undersampling, SMOTE, ROSE, or synthetic resampling was detected in audited glmnet modeling contexts."

# -----------------------------
# 8. Final audit table
# -----------------------------
final_method_audit <- tibble::tibble(
  method_item = c(
    "Inner cross-validation folds",
    "foldid implementation",
    "lambda selection rule",
    "Class weights",
    "Resampling",
    "Model family",
    "glmnet type.measure",
    "glmnet standardize argument",
    "alpha grid",
    "Threshold selection",
    "AUROC CI",
    "AUPRC",
    "Brier score",
    "Calibration intercept/slope",
    "Missing values"
  ),
  final_decision = c(
    inner_cv_decision,
    paste0(
      "foldid detected: ", foldid_detected,
      "; createFolds detected: ", create_folds_detected,
      "; sample/rep fold construction detected: ", sample_foldid_detected
    ),
    lambda_decision,
    weights_decision,
    resampling_decision,
    "family = \"binomial\"",
    "type.measure = \"auc\"",
    "standardize = FALSE",
    "alpha grid = c(1.00, 0.75, 0.50)",
    "Youden index selected within the training data only",
    "pROC DeLong 95% CI",
    "PRROC::pr.curve",
    "Mean squared error between observed binary outcome and predicted probability",
    "Logistic calibration / rms::val.prob-style validation of predicted probabilities",
    "Complete-case filtering for required phenotype labels and signature-gene measurements"
  ),
  status = c(
    inner_cv_status,
    ifelse(foldid_detected, "READY_TRACE_DETECTED", "READY_NO_FOLDID"),
    lambda_status,
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY",
    "READY"
  )
)

n_manual_items <- final_method_audit %>%
  dplyr::filter(stringr::str_detect(status, "NEEDS_MANUAL_CONFIRMATION")) %>%
  nrow()

# -----------------------------
# 9. Final methods text
# -----------------------------
inner_cv_phrase <- if (inner_cv_status == "READY") {
  paste0(inner_fold_count, "-fold inner cross-validation")
} else if (inner_cv_status == "READY_DEFAULT_10") {
  "10-fold inner cross-validation, corresponding to the default cv.glmnet setting"
} else {
  "inner cross-validation using a foldid vector; the exact number of unique inner folds requires final author verification"
}

lambda_phrase <- if (lambda_status == "READY") {
  "lambda.1se"
} else if (lambda_status == "READY_BUT_NOT_LAMBDA_1SE") {
  "lambda.min"
} else {
  "the lambda rule requiring final author verification"
}

final_methods_text <- paste(
  paste0(
    "Model development was performed using a strictly nested leave-one-dataset-out (LODO) design. ",
    "In each outer split, one bulk transcriptomic cohort was held out for evaluation, whereas feature filtering, ",
    "preprocessing parameters, alpha/lambda tuning, threshold selection, and model fitting were restricted to the ",
    "remaining training cohorts. Penalized logistic regression was fitted using glmnet with family = \"binomial\", ",
    "type.measure = \"auc\", and standardize = FALSE."
  ),
  paste0(
    "Within each outer training set, a predefined alpha grid of 1.00, 0.75, and 0.50 was evaluated using ",
    inner_cv_phrase,
    ". Because cv.glmnet does not automatically search over alpha, each alpha value was evaluated explicitly ",
    "within the training data, and alpha selection was based only on inner cross-validation performance. For the ",
    "selected alpha, ",
    lambda_phrase,
    " was used as the final penalty parameter."
  ),
  paste0(
    "The classification threshold was selected within the training data only using the Youden index and was then ",
    "applied unchanged to the corresponding held-out cohort. No threshold re-selection was performed in held-out data."
  ),
  paste0(
    "Discrimination was assessed using AUROC and AUPRC. AUROC 95% confidence intervals were estimated using ",
    "DeLong's method as implemented in pROC. AUPRC was calculated from precision-recall curves using PRROC::pr.curve. ",
    "The Brier score was calculated as the mean squared difference between the observed binary outcome and the predicted ",
    "probability. Calibration intercept and slope were estimated using logistic calibration or rms::val.prob-style ",
    "validation of predicted probabilities."
  ),
  paste0(
    "Samples lacking required phenotype labels or required signature-gene measurements were excluded from the corresponding ",
    "analysis by the predefined complete-case rule. No class weights were passed to glmnet model fitting, and no oversampling, ",
    "undersampling, SMOTE, ROSE, or synthetic resampling was used in the audited modeling calls."
  ),
  sep = "\n\n"
)

# -----------------------------
# 10. Checks
# -----------------------------
checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:20),
  check_item = c(
    "Project directory exists",
    "Scripts to audit found",
    "Trace records generated",
    "foldid or nfolds evidence detected",
    "Inner fold count ready or explicitly marked",
    "lambda decision generated",
    "lambda decision ready",
    "Class weights resolved",
    "Resampling resolved",
    "Final method audit generated",
    "No unresolved manual items",
    "Final methods text generated",
    "CSV trace audit generated",
    "CSV method audit generated",
    "TXT methods text generated",
    "DOCX methods text generated",
    "XLSX workbook generated",
    "Methods text mentions alpha grid",
    "Methods text mentions no class weights",
    "Methods text mentions no resampling"
  ),
  observed = c(
    dir.exists(project_dir),
    length(scripts_to_audit) > 0,
    nrow(trace_records) > 0,
    foldid_detected || !is.na(inner_fold_count),
    inner_cv_status %in% c("READY", "READY_DEFAULT_10", "NEEDS_MANUAL_CONFIRMATION"),
    lambda_decision != "",
    lambda_status %in% c("READY", "READY_BUT_NOT_LAMBDA_1SE"),
    TRUE,
    TRUE,
    nrow(final_method_audit) > 0,
    n_manual_items == 0,
    nchar(final_methods_text) > 0,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    stringr::str_detect(final_methods_text, "1\\.00, 0\\.75, and 0\\.50"),
    stringr::str_detect(final_methods_text, "No class weights"),
    stringr::str_detect(final_methods_text, "no oversampling")
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 11. Export CSV/TXT
# -----------------------------
readr::write_csv(trace_records, trace_audit_path)
readr::write_csv(final_method_audit, method_audit_path)
writeLines(final_methods_text, con = txt_methods_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "CSV trace audit generated" ~ file.exists(trace_audit_path),
      check_item == "CSV method audit generated" ~ file.exists(method_audit_path),
      check_item == "TXT methods text generated" ~ file.exists(txt_methods_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 12. Export DOCX
# -----------------------------
ft_audit <- flextable::flextable(final_method_audit)
ft_audit <- flextable::theme_booktabs(ft_audit)
ft_audit <- flextable::fontsize(ft_audit, size = 8.5, part = "all")
ft_audit <- flextable::fontsize(ft_audit, size = 9, part = "header")
ft_audit <- flextable::bold(ft_audit, part = "header")
ft_audit <- flextable::valign(ft_audit, valign = "top", part = "all")
ft_audit <- flextable::width(ft_audit, j = "method_item", width = 2.1)
ft_audit <- flextable::width(ft_audit, j = "final_decision", width = 4.7)
ft_audit <- flextable::width(ft_audit, j = "status", width = 1.4)
ft_audit <- flextable::set_table_properties(ft_audit, layout = "fixed", width = 1)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Nested LODO methods 47c final audit", style = "heading 1")
doc <- officer::body_add_par(doc, "Final methods text", style = "heading 2")
doc <- officer::body_add_par(doc, final_methods_text, style = "Normal")
doc <- officer::body_add_par(doc, "Final method audit", style = "heading 2")
doc <- flextable::body_add_flextable(doc, ft_audit)
print(doc, target = docx_methods_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "DOCX methods text generated" ~ file.exists(docx_methods_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 13. Export XLSX
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Final_Method_Audit")
openxlsx::writeData(wb, "Final_Method_Audit", final_method_audit)

openxlsx::addWorksheet(wb, "Foldid_Lambda_Trace")
openxlsx::writeData(wb, "Foldid_Lambda_Trace", trace_records)

openxlsx::addWorksheet(wb, "Methods_Text")
openxlsx::writeData(wb, "Methods_Text", tibble::tibble(item = "final_methods_text", text = final_methods_text))

openxlsx::addWorksheet(wb, "Scripts_Audited")
openxlsx::writeData(
  wb,
  "Scripts_Audited",
  tibble::tibble(
    script_path = scripts_to_audit,
    script_name = basename(scripts_to_audit),
    is_priority = basename(scripts_to_audit) %in% priority_names
  )
)

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
# 14. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "n_scripts_audited",
    "n_trace_records",
    "inner_cv_decision",
    "inner_cv_status",
    "lambda_decision",
    "lambda_status",
    "n_manual_items",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "47c_force_trace_foldid_lambda_methods.R",
    project_dir,
    as.character(length(scripts_to_audit)),
    as.character(nrow(trace_records)),
    inner_cv_decision,
    inner_cv_status,
    lambda_decision,
    lambda_status,
    as.character(n_manual_items),
    as.character(n_failed_checks),
    dplyr::case_when(
      n_failed_checks == 0 && n_manual_items == 0 ~ "YES_INSERT_FINAL_METHODS_TEXT",
      n_failed_checks == 0 && n_manual_items > 0 ~ "PARTIAL_REVIEW_INNER_FOLD_COUNT",
      TRUE ~ "NO_FIX_CHECK_ITEMS"
    ),
    "Review Final_Method_Audit. If inner_cv_status remains NEEDS_MANUAL_CONFIRMATION, inspect Foldid_Lambda_Trace for nfolds_inner_cv assignment and confirm manually."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 15. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nFinal method audit:\n")
print(final_method_audit, n = Inf, width = Inf)

cat("\nFoldid/lambda trace preview:\n")
print(
  trace_records %>%
    dplyr::select(file_name, trace_type, line_number, line_text_clean) %>%
    dplyr::slice_head(n = 80),
  n = 80,
  width = Inf
)

cat("\nFinal methods text:\n")
cat(final_methods_text, "\n")

cat("\n关键输出：\n")
cat("1) ", trace_audit_path, "\n", sep = "")
cat("2) ", method_audit_path, "\n", sep = "")
cat("3) ", txt_methods_path, "\n", sep = "")
cat("4) ", docx_methods_path, "\n", sep = "")
cat("5) ", xlsx_path, "\n", sep = "")
cat("6) ", checks_path, "\n", sep = "")
cat("7) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Final method audit、Foldid/lambda trace preview 和 Final methods text 贴给我。\n")
cat("如果只剩 inner fold count 未确认，我会根据 trace 输出给你最后的手动判定版本。\n")

cat("\n============ 47c foldid/lambda forced trace complete ============\n")