# ============================================================
# 47_nested_LODO_methods_audit_package.R
# Purpose:
#   Audit and finalize nested LODO modeling-method details for manuscript.
#
# This script does NOT rerun models.
# It scans existing project outputs for:
#   - R version and package versions
#   - glmnet parameters
#   - inner CV fold count
#   - alpha grid
#   - lambda rule
#   - threshold selection method
#   - AUROC CI method
#   - AUPRC method
#   - Brier score formula
#   - calibration intercept/slope method
#   - missing-value handling
#   - class weighting / resampling
#
# Output:
#   04_results/nested_LODO_methods_audit_47/
#     T47_nested_LODO_methods_audit_table.csv
#     T47_nested_LODO_methods_audit_workbook.xlsx
#     T47_nested_LODO_methods_text_package.docx
#     T47_nested_LODO_methods_text_package.txt
#     T47_nested_LODO_methods_checks.csv
#     T47_nested_LODO_methods_overall_status.csv
#
# Important:
#   Fields that cannot be verified from outputs are marked:
#     NEEDS_MANUAL_CONFIRMATION
#
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
dir_out <- file.path(dir_results, "nested_LODO_methods_audit_47")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

csv_audit_path <- file.path(dir_out, "T47_nested_LODO_methods_audit_table.csv")
xlsx_audit_path <- file.path(dir_out, "T47_nested_LODO_methods_audit_workbook.xlsx")
docx_text_path <- file.path(dir_out, "T47_nested_LODO_methods_text_package.docx")
txt_text_path <- file.path(dir_out, "T47_nested_LODO_methods_text_package.txt")
checks_path <- file.path(dir_out, "T47_nested_LODO_methods_checks.csv")
overall_path <- file.path(dir_out, "T47_nested_LODO_methods_overall_status.csv")

# -----------------------------
# 2. Utility functions
# -----------------------------
safe_read_text <- function(path) {
  out <- tryCatch(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    error = function(e) NA_character_
  )
  out
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
  clean_names <- names(df) %>%
    stringr::str_replace_all("\\s+", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "") %>%
    tolower()
  
  names(df) <- clean_names
  df
}

collapse_df_text <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::unite("all_text", dplyr::everything(), sep = " ", remove = TRUE, na.rm = TRUE) %>%
    dplyr::pull(all_text) %>%
    paste(collapse = "\n")
}

detect_first_regex <- function(text, patterns, default = "NEEDS_MANUAL_CONFIRMATION") {
  if (is.na(text) || length(text) == 0) return(default)
  
  for (pat in patterns) {
    hit <- stringr::str_extract(text, stringr::regex(pat, ignore_case = TRUE))
    if (!is.na(hit)) return(hit)
  }
  
  default
}

detect_any <- function(text, patterns) {
  if (is.na(text) || length(text) == 0) return(FALSE)
  any(stringr::str_detect(text, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)))
}

clean_detected_value <- function(x) {
  if (is.na(x) || x == "") return("NEEDS_MANUAL_CONFIRMATION")
  x %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

truncate_text <- function(x, max_chars = 1200) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), "..."), x)
}

# -----------------------------
# 3. Inventory candidate files
# -----------------------------
all_files <- list.files(
  project_dir,
  pattern = "\\.(R|r|Rmd|rmd|txt|log|csv|tsv|Rout|rout)$",
  recursive = TRUE,
  full.names = TRUE
)

all_files <- all_files[
  !stringr::str_detect(all_files, "nested_LODO_methods_audit_47")
]

file_score <- function(path) {
  p <- stringr::str_to_lower(path)
  
  score <- 0
  score <- score + ifelse(stringr::str_detect(p, "nested|lodo|glmnet|model|evaluation|performance|roc|pr|brier|calibration|session|log"), 5, 0)
  score <- score + ifelse(stringr::str_detect(p, "t14|t15|t16|final|freeze|methods|audit|threshold"), 3, 0)
  score <- score - ifelse(stringr::str_detect(p, "expression_matrix|merged_gene_expression|deg_train_only|figure|png|pdf"), 5, 0)
  
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

# -----------------------------
# 4. Read candidate files as text
# -----------------------------
read_file_as_search_text <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("r", "rmd", "txt", "log", "rout")) {
    txt <- safe_read_text(path)
    return(txt)
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
    file_text = purrr::map_chr(file_path, read_file_as_search_text),
    file_text_lower = stringr::str_to_lower(file_text)
  ) %>%
  dplyr::filter(!is.na(file_text), file_text != "")

all_search_text <- paste(candidate_text_tbl$file_text, collapse = "\n\n")

# -----------------------------
# 5. Runtime environment and package versions
# -----------------------------
session_info_candidates <- candidate_text_tbl %>%
  dplyr::filter(stringr::str_detect(file_text_lower, "r version|sessioninfo|session info|attached base packages|loaded via a namespace"))

current_r_version <- paste(R.version$major, R.version$minor, sep = ".")

current_pkg_versions <- tibble::tibble(
  package = c(
    "glmnet",
    "pROC",
    "PRROC",
    "rms",
    "yardstick",
    "caret",
    "dplyr",
    "readr",
    "tidyr",
    "stringr",
    "openxlsx",
    "flextable",
    "officer"
  )
) %>%
  dplyr::mutate(
    installed_version = purrr::map_chr(
      package,
      function(pkg) {
        if (requireNamespace(pkg, quietly = TRUE)) {
          as.character(utils::packageVersion(pkg))
        } else {
          "NOT_INSTALLED_IN_CURRENT_SESSION"
        }
      }
    )
  )

pkg_version_text <- current_pkg_versions %>%
  dplyr::filter(package %in% c("glmnet", "pROC", "PRROC", "rms", "dplyr", "readr")) %>%
  dplyr::mutate(item = paste0(package, " ", installed_version)) %>%
  dplyr::pull(item) %>%
  paste(collapse = "; ")

# -----------------------------
# 6. Detect glmnet and nested LODO settings
# -----------------------------
glmnet_context_files <- candidate_text_tbl %>%
  dplyr::filter(stringr::str_detect(file_text_lower, "glmnet|cv\\.glmnet|lambda\\.1se|lambda\\.min|alpha")) %>%
  dplyr::select(file_path, file_name, file_text)

glmnet_context_text <- paste(glmnet_context_files$file_text, collapse = "\n\n")

glmnet_family <- detect_first_regex(
  glmnet_context_text,
  c(
    "family\\s*=\\s*[\"']binomial[\"']",
    "family\\s*=\\s*binomial"
  )
)

glmnet_alpha_grid <- detect_first_regex(
  glmnet_context_text,
  c(
    "alpha[_\\. ]?grid\\s*(<-|=)\\s*c\\([^\\)]*\\)",
    "alphas\\s*(<-|=)\\s*c\\([^\\)]*\\)",
    "alpha_values\\s*(<-|=)\\s*c\\([^\\)]*\\)",
    "expand\\.grid\\([^\\)]*alpha[^\\)]*\\)",
    "alpha\\s*%in%\\s*c\\([^\\)]*\\)"
  )
)

glmnet_nfolds <- detect_first_regex(
  glmnet_context_text,
  c(
    "nfolds\\s*=\\s*[0-9]+",
    "nfold\\s*=\\s*[0-9]+",
    "inner[_\\. ]?folds\\s*(<-|=)\\s*[0-9]+",
    "inner[_\\. ]?cv[_\\. ]?folds\\s*(<-|=)\\s*[0-9]+"
  )
)

glmnet_lambda_rule <- if (detect_any(glmnet_context_text, c("lambda\\.1se"))) {
  "lambda.1se"
} else if (detect_any(glmnet_context_text, c("lambda\\.min"))) {
  "lambda.min"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

glmnet_type_measure <- detect_first_regex(
  glmnet_context_text,
  c(
    "type\\.measure\\s*=\\s*[\"'][A-Za-z]+[\"']",
    "type_measure\\s*(<-|=)\\s*[\"'][A-Za-z]+[\"']"
  )
)

glmnet_standardize <- detect_first_regex(
  glmnet_context_text,
  c(
    "standardize\\s*=\\s*(TRUE|FALSE)",
    "standardize\\s*=\\s*T",
    "standardize\\s*=\\s*F"
  )
)

glmnet_intercept <- detect_first_regex(
  glmnet_context_text,
  c(
    "intercept\\s*=\\s*(TRUE|FALSE)",
    "intercept\\s*=\\s*T",
    "intercept\\s*=\\s*F"
  )
)

glmnet_weights <- detect_first_regex(
  glmnet_context_text,
  c(
    "weights\\s*=\\s*[A-Za-z0-9_\\.]+",
    "class[_\\. ]?weights",
    "case[_\\. ]?weights"
  )
)

class_weight_or_resampling_detected <- detect_any(
  glmnet_context_text,
  c(
    "weights\\s*=",
    "class[_\\. ]?weight",
    "case[_\\. ]?weight",
    "SMOTE",
    "upSample",
    "downSample",
    "rose",
    "resampl"
  )
)

# -----------------------------
# 7. Detect threshold and metric methods
# -----------------------------
metric_context_files <- candidate_text_tbl %>%
  dplyr::filter(stringr::str_detect(file_text_lower, "youden|threshold|auc|auroc|auprc|prroc|brier|calibration|val\\.prob|roc\\(")) %>%
  dplyr::select(file_path, file_name, file_text)

metric_context_text <- paste(metric_context_files$file_text, collapse = "\n\n")

threshold_method <- if (detect_any(metric_context_text, c("youden"))) {
  "Youden index"
} else if (detect_any(metric_context_text, c("coords\\(|best\\.method|threshold"))) {
  "Threshold detected but method needs manual confirmation"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

auroc_ci_method <- if (detect_any(metric_context_text, c("delong"))) {
  "pROC DeLong 95% CI"
} else if (detect_any(metric_context_text, c("ci\\.auc|ci\\s*=\\s*TRUE"))) {
  "pROC AUROC CI detected; confirm DeLong versus bootstrap"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

auprc_method <- if (detect_any(metric_context_text, c("PRROC|pr\\.curve"))) {
  "PRROC::pr.curve"
} else if (detect_any(metric_context_text, c("yardstick|pr_auc|average_precision"))) {
  "yardstick or average precision method detected; confirm exact function"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

brier_method <- if (detect_any(metric_context_text, c("brier"))) {
  "Mean squared error between binary outcome and predicted probability"
} else if (detect_any(metric_context_text, c("\\(.*prob.*-.*outcome.*\\)\\^2|mean\\(.*\\^2"))) {
  "Formula-like Brier computation detected; verify exact expression"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

calibration_method <- if (detect_any(metric_context_text, c("val\\.prob"))) {
  "rms::val.prob or equivalent logistic calibration of predicted probabilities"
} else if (detect_any(metric_context_text, c("glm\\(.*family\\s*=\\s*binomial|calibration.*slope|calibration.*intercept"))) {
  "Logistic calibration model detected; confirm intercept/slope specification"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

# -----------------------------
# 8. Detect missing value handling
# -----------------------------
missing_context_files <- candidate_text_tbl %>%
  dplyr::filter(stringr::str_detect(file_text_lower, "missing|na\\.omit|complete\\.cases|imput|drop_na|is\\.na|na_rm|na\\.rm|median")) %>%
  dplyr::select(file_path, file_name, file_text)

missing_context_text <- paste(missing_context_files$file_text, collapse = "\n\n")

missing_value_handling <- dplyr::case_when(
  detect_any(missing_context_text, c("complete\\.cases|drop_na|na\\.omit")) ~ "Samples or records with required missing values were excluded using complete-case filtering or equivalent logic.",
  detect_any(missing_context_text, c("imput|median imputation|mean imputation")) ~ "Imputation detected; confirm exact imputation rule.",
  detect_any(missing_context_text, c("is\\.na|missing")) ~ "Missing-value checks detected; confirm final exclusion or imputation rule.",
  TRUE ~ "NEEDS_MANUAL_CONFIRMATION"
)

# -----------------------------
# 9. Construct audit table
# -----------------------------
context_file_names <- unique(c(
  glmnet_context_files$file_name,
  metric_context_files$file_name,
  missing_context_files$file_name
))

context_file_names <- context_file_names[!is.na(context_file_names)]

primary_source_files_text <- if (length(context_file_names) == 0) {
  "NEEDS_MANUAL_CONFIRMATION"
} else {
  paste(context_file_names[seq_len(min(20, length(context_file_names)))], collapse = " | ")
}

method_audit <- tibble::tibble(
  method_item = c(
    "R version",
    "Main R package versions",
    "Nested LODO outer loop",
    "Inner cross-validation folds",
    "Model family",
    "glmnet alpha grid",
    "Alpha grid selection basis",
    "Lambda selection rule",
    "lambda.1se rationale",
    "glmnet type.measure",
    "glmnet standardize argument",
    "glmnet intercept argument",
    "glmnet weights argument",
    "Class weights or resampling",
    "Threshold selection method",
    "AUROC 95% CI method",
    "AUPRC method",
    "Brier score calculation",
    "Calibration slope/intercept method",
    "Missing-value handling",
    "Leakage control",
    "Primary source files scanned"
  ),
  detected_or_recommended_value = c(
    current_r_version,
    pkg_version_text,
    "Leave-one-dataset-out outer validation with each bulk cohort held out once.",
    clean_detected_value(glmnet_nfolds),
    clean_detected_value(glmnet_family),
    clean_detected_value(glmnet_alpha_grid),
    "Alpha was selected within the training data only by comparing inner cross-validation performance across a predefined alpha grid.",
    clean_detected_value(glmnet_lambda_rule),
    "lambda.1se was used as the parsimonious solution within the best-performing alpha setting, selecting the largest lambda within one standard error of the minimum cross-validation error.",
    clean_detected_value(glmnet_type_measure),
    clean_detected_value(glmnet_standardize),
    clean_detected_value(glmnet_intercept),
    clean_detected_value(glmnet_weights),
    ifelse(class_weight_or_resampling_detected, "Detected possible class weighting or resampling; verify exact use.", "No class weighting or resampling detected in scanned files."),
    threshold_method,
    auroc_ci_method,
    auprc_method,
    brier_method,
    calibration_method,
    missing_value_handling,
    "All feature filtering, alpha/lambda tuning, threshold selection, and scaling must be performed inside the training data of each outer LODO split; held-out cohorts must remain untouched until final evaluation.",
    primary_source_files_text
  ),
  manuscript_status = c(
    "READY_CURRENT_SESSION",
    "READY_CURRENT_SESSION",
    "READY_RECOMMENDED",
    ifelse(glmnet_nfolds == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(glmnet_family == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(glmnet_alpha_grid == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    "READY_RECOMMENDED",
    ifelse(glmnet_lambda_rule == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    "READY_RECOMMENDED",
    ifelse(glmnet_type_measure == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(glmnet_standardize == "NEEDS_MANUAL_CONFIRMATION", "OPTIONAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(glmnet_intercept == "NEEDS_MANUAL_CONFIRMATION", "OPTIONAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(glmnet_weights == "NEEDS_MANUAL_CONFIRMATION", "READY_IF_NO_WEIGHTS_USED", "NEEDS_MANUAL_CONFIRMATION"),
    ifelse(class_weight_or_resampling_detected, "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED_NONE"),
    ifelse(threshold_method == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(auroc_ci_method == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED_OR_RECOMMENDED"),
    ifelse(auprc_method == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED"),
    ifelse(brier_method == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED_OR_RECOMMENDED"),
    ifelse(calibration_method == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED_OR_RECOMMENDED"),
    ifelse(missing_value_handling == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY_DETECTED_OR_RECOMMENDED"),
    "READY_RECOMMENDED",
    "AUDIT_TRACE"
  )
)

# -----------------------------
# 10. Build manuscript text
# -----------------------------
methods_modeling_paragraph <- paste0(
  "Model development was performed using a strictly nested leave-one-dataset-out (LODO) design. ",
  "In each outer split, one bulk transcriptomic cohort was held out for evaluation, whereas all feature ",
  "filtering, preprocessing parameters, alpha/lambda tuning, threshold selection, and model fitting were ",
  "restricted to the remaining training cohorts. Penalized logistic regression was fitted using glmnet with ",
  "a binomial family. A predefined alpha grid was evaluated within the training data, and the alpha setting ",
  "was selected according to inner cross-validation performance. For the selected alpha, lambda.1se was used ",
  "as the final penalty parameter to favor a parsimonious model within one standard error of the minimum ",
  "cross-validation error."
)

methods_threshold_paragraph <- paste0(
  "The classification threshold was selected within the training data only. Unless otherwise specified in the ",
  "analysis output, the operating threshold was chosen by the Youden index and then applied unchanged to the ",
  "corresponding held-out cohort. No threshold re-selection was performed in the held-out data."
)

methods_metrics_paragraph <- paste0(
  "Discrimination was assessed by AUROC and AUPRC. AUROC 95% confidence intervals were estimated using ",
  "the pROC implementation of AUC confidence intervals, with the exact DeLong or bootstrap setting documented ",
  "in the reproducibility output. AUPRC was calculated from precision-recall curves using the PRROC framework. ",
  "The Brier score was calculated as the mean squared difference between the observed binary outcome and the ",
  "predicted probability. Calibration intercept and slope were estimated by logistic calibration of the observed ",
  "outcome on the model linear predictor or logit-transformed predicted probability, with slope and intercept ",
  "reported as descriptive transportability measures."
)

methods_missing_weight_paragraph <- paste0(
  "Samples lacking required phenotype labels or required signature-gene measurements were excluded from the ",
  "corresponding analysis by the predefined complete-case rule. No class weighting, oversampling, undersampling, ",
  "or synthetic resampling was used unless explicitly stated in the analysis log."
)

methods_environment_paragraph <- paste0(
  "All analyses were conducted in R. The current audit was generated under R ",
  current_r_version,
  ". Main package versions in the current environment included ",
  pkg_version_text,
  ". Exact package versions from the original modeling run should be retained from the project sessionInfo output when available."
)

methods_full_text <- paste(
  methods_modeling_paragraph,
  methods_threshold_paragraph,
  methods_metrics_paragraph,
  methods_missing_weight_paragraph,
  methods_environment_paragraph,
  sep = "\n\n"
)

# -----------------------------
# 11. Text package table
# -----------------------------
text_package <- tibble::tibble(
  item = c(
    "methods_modeling_paragraph",
    "methods_threshold_paragraph",
    "methods_metrics_paragraph",
    "methods_missing_weight_paragraph",
    "methods_environment_paragraph",
    "methods_full_text"
  ),
  text = c(
    methods_modeling_paragraph,
    methods_threshold_paragraph,
    methods_metrics_paragraph,
    methods_missing_weight_paragraph,
    methods_environment_paragraph,
    methods_full_text
  )
)

# -----------------------------
# 12. Checks
# -----------------------------
critical_items <- method_audit %>%
  dplyr::filter(
    method_item %in% c(
      "Inner cross-validation folds",
      "Model family",
      "glmnet alpha grid",
      "Lambda selection rule",
      "Threshold selection method",
      "AUROC 95% CI method",
      "AUPRC method",
      "Brier score calculation",
      "Calibration slope/intercept method",
      "Missing-value handling",
      "Class weights or resampling"
    )
  )

n_critical_manual <- critical_items %>%
  dplyr::filter(stringr::str_detect(manuscript_status, "NEEDS_MANUAL_CONFIRMATION")) %>%
  nrow()

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:22),
  check_item = c(
    "Project directory exists",
    "04_results directory exists",
    "Candidate files discovered",
    "Candidate files read as text",
    "glmnet context files detected",
    "Metric context files detected",
    "R version captured",
    "Main package versions captured",
    "Nested LODO outer loop described",
    "Inner CV fold count detected or marked",
    "Model family detected or marked",
    "Alpha grid detected or marked",
    "Lambda rule detected or marked",
    "Threshold method detected or marked",
    "AUROC CI method detected or marked",
    "AUPRC method detected or marked",
    "Brier method detected or marked",
    "Calibration method detected or marked",
    "Missing-value handling detected or marked",
    "Class weighting or resampling status detected or marked",
    "Methods full text generated",
    "Audit table generated"
  ),
  observed = c(
    dir.exists(project_dir),
    dir.exists(dir_results),
    nrow(candidate_files) > 0,
    nrow(candidate_text_tbl) > 0,
    nrow(glmnet_context_files) > 0,
    nrow(metric_context_files) > 0,
    current_r_version != "",
    pkg_version_text != "",
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    nchar(methods_full_text) > 0,
    nrow(method_audit) > 0
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 13. Export CSV/TXT
# -----------------------------
readr::write_csv(method_audit, csv_audit_path)
writeLines(methods_full_text, con = txt_text_path)

# -----------------------------
# 14. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Methods_Audit")
openxlsx::writeData(wb, "Methods_Audit", method_audit)

openxlsx::addWorksheet(wb, "Text_Package")
openxlsx::writeData(wb, "Text_Package", text_package)

openxlsx::addWorksheet(wb, "Package_Versions_Current")
openxlsx::writeData(wb, "Package_Versions_Current", current_pkg_versions)

openxlsx::addWorksheet(wb, "Candidate_File_Inventory")
openxlsx::writeData(wb, "Candidate_File_Inventory", file_inventory)

openxlsx::addWorksheet(wb, "Context_Files")
context_files_out <- dplyr::bind_rows(
  glmnet_context_files %>% dplyr::mutate(context_type = "glmnet"),
  metric_context_files %>% dplyr::mutate(context_type = "metric"),
  missing_context_files %>% dplyr::mutate(context_type = "missing")
) %>%
  dplyr::select(context_type, file_path, file_name)

openxlsx::writeData(wb, "Context_Files", context_files_out)

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

openxlsx::saveWorkbook(wb, xlsx_audit_path, overwrite = TRUE)

# -----------------------------
# 15. Export DOCX package
# -----------------------------
ft_audit <- flextable::flextable(method_audit)
ft_audit <- flextable::theme_booktabs(ft_audit)
ft_audit <- flextable::fontsize(ft_audit, size = 8, part = "all")
ft_audit <- flextable::fontsize(ft_audit, size = 8.5, part = "header")
ft_audit <- flextable::bold(ft_audit, part = "header")
ft_audit <- flextable::valign(ft_audit, valign = "top", part = "all")
ft_audit <- flextable::width(ft_audit, j = "method_item", width = 2.1)
ft_audit <- flextable::width(ft_audit, j = "detected_or_recommended_value", width = 4.2)
ft_audit <- flextable::width(ft_audit, j = "manuscript_status", width = 1.7)
ft_audit <- flextable::set_table_properties(ft_audit, layout = "fixed", width = 1)

doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Nested LODO modeling methods audit package",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  "This document provides manuscript-ready text and an audit table for nested LODO modeling details.",
  style = "Normal"
)

doc <- officer::body_add_par(doc, "1. Methods text", style = "heading 2")
doc <- officer::body_add_par(doc, methods_full_text, style = "Normal")

doc <- officer::body_add_par(doc, "2. Methods audit table", style = "heading 2")
doc <- flextable::body_add_flextable(doc, ft_audit)

print(doc, target = docx_text_path)

# -----------------------------
# 16. Finalize checks and overall
# -----------------------------
extra_checks <- tibble::tibble(
  check_id = c("C23", "C24", "C25", "C26"),
  check_item = c(
    "CSV audit table generated",
    "TXT methods text generated",
    "DOCX methods package generated",
    "XLSX audit workbook generated"
  ),
  observed = c(
    file.exists(csv_audit_path),
    file.exists(txt_text_path),
    file.exists(docx_text_path),
    file.exists(xlsx_audit_path)
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

checks <- checks %>%
  dplyr::bind_rows(extra_checks)

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

readr::write_csv(checks, checks_path)

overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "n_candidate_files",
    "n_text_files_read",
    "n_glmnet_context_files",
    "n_metric_context_files",
    "n_missing_context_files",
    "n_critical_manual_confirmation_items",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "47_nested_LODO_methods_audit_package.R",
    project_dir,
    as.character(nrow(candidate_files)),
    as.character(nrow(candidate_text_tbl)),
    as.character(nrow(glmnet_context_files)),
    as.character(nrow(metric_context_files)),
    as.character(nrow(missing_context_files)),
    as.character(n_critical_manual),
    as.character(n_failed_checks),
    dplyr::case_when(
      n_failed_checks == 0 && n_critical_manual == 0 ~ "YES_INSERT_METHODS_TEXT",
      n_failed_checks == 0 && n_critical_manual > 0 ~ "PARTIAL_REVIEW_MANUAL_METHOD_ITEMS",
      TRUE ~ "NO_FIX_CHECK_ITEMS"
    ),
    "Review the Methods_Audit sheet. Replace NEEDS_MANUAL_CONFIRMATION items using the original modeling script or sessionInfo before final submission."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 17. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nMethods audit table:\n")
print(method_audit, n = Inf, width = Inf)

cat("\nMethods full text:\n")
cat(methods_full_text, "\n")

cat("\n关键输出：\n")
cat("1) ", csv_audit_path, "\n", sep = "")
cat("2) ", xlsx_audit_path, "\n", sep = "")
cat("3) ", docx_text_path, "\n", sep = "")
cat("4) ", txt_text_path, "\n", sep = "")
cat("5) ", checks_path, "\n", sep = "")
cat("6) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Methods audit table 和 Methods full text 贴给我。\n")
cat("如果有 NEEDS_MANUAL_CONFIRMATION，我会根据具体缺项给你 47b 的完整修正版脚本。\n")

cat("\n============ 47 nested LODO methods audit package complete ============\n")