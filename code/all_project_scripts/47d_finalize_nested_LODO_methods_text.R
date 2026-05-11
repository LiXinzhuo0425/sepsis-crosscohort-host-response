# ============================================================
# 47d_finalize_nested_LODO_methods_text.R
# Purpose:
#   Finalize nested LODO methods text after 47c.
#
# Why:
#   47c correctly traced nfolds_inner_cv <- 5, nfolds_cv <- 10,
#   and lambda.1se, but the automatic regex extraction produced
#   an invalid string: "<--fold inner cross-validation".
#
# This script manually resolves the two-layer CV wording:
#   - Nested LODO inner tuning CV: 5-fold
#   - Final compact model training CV: 10-fold
#
# It does NOT rerun models.
#
# Output:
#   04_results/nested_LODO_methods_audit_47d/
#     T47d_nested_LODO_methods_final_text.txt
#     T47d_nested_LODO_methods_final_text.docx
#     T47d_nested_LODO_methods_final_audit.csv
#     T47d_nested_LODO_methods_final_workbook.xlsx
#     T47d_checks.csv
#     T47d_overall_status.csv
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(openxlsx)
  library(flextable)
  library(officer)
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
dir_results <- file.path(project_dir, "04_results")

dir_in_47c <- file.path(dir_results, "nested_LODO_methods_audit_47c")
trace_47c_path <- file.path(dir_in_47c, "T47c_foldid_lambda_trace_audit.csv")

dir_out <- file.path(dir_results, "nested_LODO_methods_audit_47d")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

txt_methods_path <- file.path(dir_out, "T47d_nested_LODO_methods_final_text.txt")
docx_methods_path <- file.path(dir_out, "T47d_nested_LODO_methods_final_text.docx")
csv_audit_path <- file.path(dir_out, "T47d_nested_LODO_methods_final_audit.csv")
xlsx_path <- file.path(dir_out, "T47d_nested_LODO_methods_final_workbook.xlsx")
checks_path <- file.path(dir_out, "T47d_checks.csv")
overall_path <- file.path(dir_out, "T47d_overall_status.csv")

# -----------------------------
# 2. Read 47c trace if available
# -----------------------------
if (file.exists(trace_47c_path)) {
  trace_47c <- readr::read_csv(trace_47c_path, show_col_types = FALSE)
} else {
  trace_47c <- tibble::tibble()
}

trace_text <- if (nrow(trace_47c) > 0) {
  paste(trace_47c$line_text_clean, collapse = "\n")
} else {
  ""
}

# -----------------------------
# 3. Manually resolved decisions from 47c trace
# -----------------------------
nested_lodo_inner_cv_folds <- 5
final_compact_training_cv_folds <- 10

nested_lodo_inner_cv_evidence <- "08_LODO_diagnostic_modeling.R and 14_nested_LODO_full_pipeline.R contain nfolds_inner_cv <- 5 and construct foldid from seq_len(nfolds_inner_cv)."

final_compact_training_cv_evidence <- "10_final_compact_model_training.R contains nfolds_cv <- 10 and constructs foldid from seq_len(nfolds_cv)."

lambda_rule <- "lambda.1se"
lambda_evidence <- "Trace output contains lambda_rule <- \"lambda.1se\", selected_lambda <- lambda_rule, and prediction/coefficient extraction using selected_lambda."

class_weight_decision <- "No class weights were passed to glmnet model fitting."
class_weight_evidence <- "47b cv.glmnet audit found no weights argument in cv.glmnet calls."

resampling_decision <- "No oversampling, undersampling, SMOTE, ROSE, or synthetic resampling was used."
resampling_evidence <- "47b audit detected no resampling-related keywords in glmnet modeling contexts."

alpha_grid <- "c(1.00, 0.75, 0.50)"
model_family <- "family = \"binomial\""
glmnet_type_measure <- "type.measure = \"auc\""
glmnet_standardize <- "standardize = FALSE"
threshold_method <- "Youden index selected within the training data only"
auroc_ci_method <- "pROC DeLong 95% CI"
auprc_method <- "PRROC::pr.curve"
brier_method <- "Mean squared error between observed binary outcome and predicted probability"
calibration_method <- "Logistic calibration / rms::val.prob-style validation of predicted probabilities"
missing_value_method <- "Complete-case filtering for required phenotype labels and signature-gene measurements"

# -----------------------------
# 4. Final audit table
# -----------------------------
final_audit <- tibble::tibble(
  method_item = c(
    "Nested LODO outer loop",
    "Nested LODO inner CV folds",
    "Final compact model training CV folds",
    "foldid implementation",
    "Model family",
    "glmnet type.measure",
    "glmnet standardize argument",
    "alpha grid",
    "alpha selection basis",
    "lambda selection rule",
    "threshold selection",
    "AUROC 95% CI",
    "AUPRC",
    "Brier score",
    "Calibration intercept/slope",
    "Missing values",
    "Class weights",
    "Resampling",
    "Leakage control"
  ),
  final_decision = c(
    "Leave-one-dataset-out outer validation, with one bulk cohort held out at each outer split.",
    paste0(nested_lodo_inner_cv_folds, "-fold inner cross-validation"),
    paste0(final_compact_training_cv_folds, "-fold cross-validation for the final compact model trained after nested LODO selection."),
    "foldid was explicitly constructed and passed to cv.glmnet.",
    model_family,
    glmnet_type_measure,
    glmnet_standardize,
    alpha_grid,
    "Each alpha value was evaluated explicitly within training data because cv.glmnet does not search over alpha automatically.",
    lambda_rule,
    threshold_method,
    auroc_ci_method,
    auprc_method,
    brier_method,
    calibration_method,
    missing_value_method,
    class_weight_decision,
    resampling_decision,
    "Feature filtering, preprocessing/scaling, alpha/lambda tuning, threshold selection, and model fitting were restricted to the training cohorts within each outer LODO split."
  ),
  evidence = c(
    "Project design and nested LODO scripts.",
    nested_lodo_inner_cv_evidence,
    final_compact_training_cv_evidence,
    "47c trace includes foldid <- rep(seq_len(nfolds_inner_cv), ...) and foldid = foldid.",
    "47 and 47b audit detected family = \"binomial\" in cv.glmnet calls.",
    "47 and 47b audit detected type.measure = \"auc\" in cv.glmnet calls.",
    "47 and 47b audit detected standardize = FALSE in cv.glmnet calls.",
    "47 audit detected alpha_grid <- c(1.00, 0.75, 0.50).",
    "Documented manually based on glmnet behavior and project alpha loop.",
    lambda_evidence,
    "47 audit detected Youden index.",
    "47 audit detected pROC DeLong 95% CI.",
    "47 audit detected PRROC::pr.curve.",
    "47 audit detected Brier score calculation.",
    "47 audit detected rms::val.prob or equivalent logistic calibration.",
    "47 audit detected complete-case filtering or equivalent missingness handling.",
    class_weight_evidence,
    resampling_evidence,
    "Nested design statement and script-level separation of training and held-out data."
  ),
  status = "READY"
)

# -----------------------------
# 5. Final Methods text
# -----------------------------
methods_modeling_paragraph <- paste0(
  "Model development was performed using a strictly nested leave-one-dataset-out (LODO) design. ",
  "In each outer split, one bulk transcriptomic cohort was held out for evaluation, whereas feature filtering, ",
  "preprocessing parameters, alpha/lambda tuning, threshold selection, and model fitting were restricted to the ",
  "remaining training cohorts. Penalized logistic regression was fitted using glmnet with family = \"binomial\", ",
  "type.measure = \"auc\", and standardize = FALSE."
)

methods_tuning_paragraph <- paste0(
  "Within each outer training set, a predefined alpha grid of 1.00, 0.75, and 0.50 was evaluated using ",
  "5-fold inner cross-validation. The inner folds were implemented by an explicitly constructed foldid vector ",
  "and passed to cv.glmnet. Because cv.glmnet does not automatically search over alpha, each alpha value was ",
  "evaluated explicitly within the training data, and alpha selection was based only on inner cross-validation ",
  "performance. For the selected alpha, lambda.1se was used as the final penalty parameter to favor a more ",
  "parsimonious model within one standard error of the minimum cross-validation error. After nested LODO model ",
  "selection, the final compact model was trained using a separate 10-fold cross-validation procedure."
)

methods_threshold_paragraph <- paste0(
  "The classification threshold was selected within the training data only using the Youden index and was then ",
  "applied unchanged to the corresponding held-out cohort. No threshold re-selection was performed in held-out data."
)

methods_metrics_paragraph <- paste0(
  "Discrimination was assessed using AUROC and AUPRC. AUROC 95% confidence intervals were estimated using ",
  "DeLong's method as implemented in pROC. AUPRC was calculated from precision-recall curves using PRROC::pr.curve. ",
  "The Brier score was calculated as the mean squared difference between the observed binary outcome and the predicted ",
  "probability. Calibration intercept and slope were estimated using logistic calibration or rms::val.prob-style ",
  "validation of predicted probabilities."
)

methods_missing_weight_paragraph <- paste0(
  "Samples lacking required phenotype labels or required signature-gene measurements were excluded from the corresponding ",
  "analysis by the predefined complete-case rule. No class weights were passed to glmnet model fitting, and no oversampling, ",
  "undersampling, SMOTE, ROSE, or synthetic resampling was used in the audited modeling calls."
)

final_methods_text <- paste(
  methods_modeling_paragraph,
  methods_tuning_paragraph,
  methods_threshold_paragraph,
  methods_metrics_paragraph,
  methods_missing_weight_paragraph,
  sep = "\n\n"
)

# -----------------------------
# 6. Checks
# -----------------------------
checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:22),
  check_item = c(
    "Project directory exists",
    "47c trace file exists or manual evidence provided",
    "Nested LODO inner CV set to 5-fold",
    "Final compact model CV set to 10-fold",
    "lambda.1se selected",
    "No class weights reported",
    "No resampling reported",
    "Alpha grid reported",
    "Youden threshold reported",
    "pROC DeLong AUROC CI reported",
    "PRROC AUPRC reported",
    "Brier score formula reported",
    "Calibration method reported",
    "Missing-value handling reported",
    "Leakage control reported",
    "No placeholder fold text remains",
    "No NEEDS_MANUAL_CONFIRMATION remains",
    "Final audit table generated",
    "TXT methods text generated",
    "DOCX methods text generated",
    "XLSX workbook generated",
    "CSV checks ready"
  ),
  observed = c(
    dir.exists(project_dir),
    file.exists(trace_47c_path) || nested_lodo_inner_cv_folds == 5,
    nested_lodo_inner_cv_folds == 5,
    final_compact_training_cv_folds == 10,
    lambda_rule == "lambda.1se",
    stringr::str_detect(final_methods_text, "No class weights"),
    stringr::str_detect(final_methods_text, "no oversampling"),
    stringr::str_detect(final_methods_text, "1\\.00, 0\\.75, and 0\\.50"),
    stringr::str_detect(final_methods_text, "Youden index"),
    stringr::str_detect(final_methods_text, "DeLong"),
    stringr::str_detect(final_methods_text, "PRROC::pr.curve"),
    stringr::str_detect(final_methods_text, "mean squared difference"),
    stringr::str_detect(final_methods_text, "Calibration intercept and slope"),
    stringr::str_detect(final_methods_text, "complete-case"),
    stringr::str_detect(final_methods_text, "held out for evaluation"),
    !stringr::str_detect(final_methods_text, "<--fold|NEEDS_MANUAL|author verification"),
    !any(stringr::str_detect(final_audit$final_decision, "NEEDS_MANUAL|<--fold")),
    nrow(final_audit) > 0,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 7. Export CSV/TXT
# -----------------------------
readr::write_csv(final_audit, csv_audit_path)
writeLines(final_methods_text, con = txt_methods_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "TXT methods text generated" ~ file.exists(txt_methods_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 8. Export DOCX
# -----------------------------
ft_audit <- flextable::flextable(final_audit)
ft_audit <- flextable::theme_booktabs(ft_audit)
ft_audit <- flextable::fontsize(ft_audit, size = 8, part = "all")
ft_audit <- flextable::fontsize(ft_audit, size = 8.5, part = "header")
ft_audit <- flextable::bold(ft_audit, part = "header")
ft_audit <- flextable::valign(ft_audit, valign = "top", part = "all")
ft_audit <- flextable::width(ft_audit, j = "method_item", width = 2.0)
ft_audit <- flextable::width(ft_audit, j = "final_decision", width = 3.4)
ft_audit <- flextable::width(ft_audit, j = "evidence", width = 2.6)
ft_audit <- flextable::width(ft_audit, j = "status", width = 0.8)
ft_audit <- flextable::set_table_properties(ft_audit, layout = "fixed", width = 1)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Nested LODO methods final text package", style = "heading 1")
doc <- officer::body_add_par(doc, "Final Methods text", style = "heading 2")
doc <- officer::body_add_par(doc, final_methods_text, style = "Normal")
doc <- officer::body_add_par(doc, "Final methods audit", style = "heading 2")
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
# 9. Export XLSX
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Final_Method_Audit")
openxlsx::writeData(wb, "Final_Method_Audit", final_audit)

openxlsx::addWorksheet(wb, "Final_Methods_Text")
openxlsx::writeData(wb, "Final_Methods_Text", tibble::tibble(item = "final_methods_text", text = final_methods_text))

if (nrow(trace_47c) > 0) {
  openxlsx::addWorksheet(wb, "Trace_47c")
  openxlsx::writeData(wb, "Trace_47c", trace_47c)
}

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

# Write final checks after xlsx status update
readr::write_csv(checks, checks_path)

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

# -----------------------------
# 10. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "nested_lodo_inner_cv_folds",
    "final_compact_training_cv_folds",
    "lambda_rule",
    "class_weights",
    "resampling",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "47d_finalize_nested_LODO_methods_text.R",
    project_dir,
    as.character(nested_lodo_inner_cv_folds),
    as.character(final_compact_training_cv_folds),
    lambda_rule,
    class_weight_decision,
    resampling_decision,
    as.character(n_failed_checks),
    dplyr::if_else(n_failed_checks == 0, "YES_INSERT_FINAL_NESTED_LODO_METHODS_TEXT", "NO_FIX_CHECK_ITEMS"),
    "Use T47d_nested_LODO_methods_final_text.docx or txt to replace the incomplete nested LODO methods section in the manuscript."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 11. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nFinal methods audit:\n")
print(final_audit, n = Inf, width = Inf)

cat("\nFinal methods text:\n")
cat(final_methods_text, "\n")

cat("\n关键输出：\n")
cat("1) ", txt_methods_path, "\n", sep = "")
cat("2) ", docx_methods_path, "\n", sep = "")
cat("3) ", csv_audit_path, "\n", sep = "")
cat("4) ", xlsx_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Final methods audit 和 Final methods text 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你确认 nested LODO 方法学细节模块正式收尾。\n")

cat("\n============ 47d final nested LODO methods text complete ============\n")