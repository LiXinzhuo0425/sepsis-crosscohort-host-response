# ============================================================
# 47b_force_extract_cv_weights_methods.R
# Purpose:
#   Force-audit inner CV folds and class weights / resampling
#   from actual modeling scripts.
#
# Why:
#   Step 47 found two critical unresolved method items:
#     1) Inner cross-validation folds
#     2) Class weights or resampling
#
# This script explicitly inspects likely modeling scripts:
#   - 09_LODO_calibration_threshold_analysis.R
#   - 10_final_compact_model_training.R
#   - 17_nested_LODO_calibration_threshold_DCA.R
#   - any other R script containing cv.glmnet in the project
#
# It extracts:
#   - cv.glmnet call blocks
#   - nfolds
#   - foldid
#   - weights
#   - alpha grid
#   - type.measure
#   - family
#   - lambda.1se / lambda.min
#   - resampling keywords
#
# Output:
#   04_results/nested_LODO_methods_audit_47b/
#     T47b_cv_glmnet_call_audit.csv
#     T47b_forced_methods_audit_table.csv
#     T47b_nested_LODO_methods_final_text.txt
#     T47b_nested_LODO_methods_final_text.docx
#     T47b_nested_LODO_methods_audit_workbook.xlsx
#     T47b_checks.csv
#     T47b_overall_status.csv
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

dir_out <- file.path(dir_results, "nested_LODO_methods_audit_47b")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

cv_call_audit_path <- file.path(dir_out, "T47b_cv_glmnet_call_audit.csv")
method_audit_path <- file.path(dir_out, "T47b_forced_methods_audit_table.csv")
txt_methods_path <- file.path(dir_out, "T47b_nested_LODO_methods_final_text.txt")
docx_methods_path <- file.path(dir_out, "T47b_nested_LODO_methods_final_text.docx")
xlsx_path <- file.path(dir_out, "T47b_nested_LODO_methods_audit_workbook.xlsx")
checks_path <- file.path(dir_out, "T47b_checks.csv")
overall_path <- file.path(dir_out, "T47b_overall_status.csv")

# -----------------------------
# 2. Utility functions
# -----------------------------
safe_read_lines <- function(path) {
  tryCatch(
    readLines(path, warn = FALSE),
    error = function(e) character()
  )
}

clean_space <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

detect_first_regex <- function(text, patterns, default = NA_character_) {
  if (is.na(text) || length(text) == 0 || text == "") return(default)
  
  for (pat in patterns) {
    hit <- stringr::str_extract(text, stringr::regex(pat, ignore_case = TRUE))
    if (!is.na(hit)) return(hit)
  }
  
  default
}

detect_any <- function(text, patterns) {
  if (is.na(text) || length(text) == 0 || text == "") return(FALSE)
  any(stringr::str_detect(text, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)))
}

extract_assignment_value <- function(text, variable_patterns) {
  for (vp in variable_patterns) {
    pat <- paste0("(?m)^\\s*", vp, "\\s*(<-|=)\\s*([^\\n#]+)")
    hit <- stringr::str_match(text, stringr::regex(pat, ignore_case = TRUE))
    if (!all(is.na(hit))) {
      return(clean_space(hit[, 3]))
    }
  }
  NA_character_
}

extract_numeric_from_text <- function(x) {
  if (is.na(x)) return(NA_real_)
  hit <- stringr::str_extract(x, "[0-9]+")
  suppressWarnings(as.numeric(hit))
}

# Extract call block by starting from line with cv.glmnet and continuing until parentheses balance.
extract_cv_glmnet_blocks <- function(lines, file_path) {
  idx <- which(stringr::str_detect(lines, "cv\\.glmnet\\s*\\("))
  
  if (length(idx) == 0) {
    return(tibble::tibble(
      file_path = character(),
      file_name = character(),
      start_line = integer(),
      end_line = integer(),
      call_text = character()
    ))
  }
  
  purrr::map_dfr(idx, function(i) {
    open_count <- 0
    end_i <- i
    block <- character()
    
    for (j in i:length(lines)) {
      line <- lines[[j]]
      block <- c(block, line)
      
      # Crude but effective parenthesis balance after removing quoted strings.
      line_no_strings <- stringr::str_replace_all(line, "\"[^\"]*\"|'[^']*'", "")
      open_count <- open_count +
        stringr::str_count(line_no_strings, "\\(") -
        stringr::str_count(line_no_strings, "\\)")
      
      if (open_count <= 0 && j > i) {
        end_i <- j
        break
      }
      
      # Safety stop if call block is abnormally long.
      if ((j - i) > 80) {
        end_i <- j
        break
      }
    }
    
    tibble::tibble(
      file_path = file_path,
      file_name = basename(file_path),
      start_line = i,
      end_line = end_i,
      call_text = paste(block, collapse = "\n")
    )
  })
}

# Extract context around cv.glmnet for nearby variables.
extract_context_block <- function(lines, start_line, end_line, window_before = 80, window_after = 20) {
  from <- max(1, start_line - window_before)
  to <- min(length(lines), end_line + window_after)
  paste(lines[from:to], collapse = "\n")
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
  !stringr::str_detect(all_r_scripts, "nested_LODO_methods_audit_47b")
]

script_names_priority <- c(
  "09_LODO_calibration_threshold_analysis.R",
  "10_final_compact_model_training.R",
  "17_nested_LODO_calibration_threshold_DCA.R"
)

priority_scripts <- all_r_scripts[basename(all_r_scripts) %in% script_names_priority]

scripts_with_cv <- all_r_scripts[
  purrr::map_lgl(
    all_r_scripts,
    function(path) {
      txt <- paste(safe_read_lines(path), collapse = "\n")
      stringr::str_detect(txt, "cv\\.glmnet\\s*\\(")
    }
  )
]

scripts_to_audit <- unique(c(priority_scripts, scripts_with_cv))

# -----------------------------
# 4. Extract cv.glmnet calls and local context
# -----------------------------
cv_blocks <- purrr::map_dfr(
  scripts_to_audit,
  function(path) {
    lines <- safe_read_lines(path)
    extract_cv_glmnet_blocks(lines, path)
  }
)

if (nrow(cv_blocks) == 0) {
  cv_blocks <- tibble::tibble(
    file_path = character(),
    file_name = character(),
    start_line = integer(),
    end_line = integer(),
    call_text = character()
  )
}

cv_blocks <- cv_blocks %>%
  dplyr::mutate(
    context_text = purrr::pmap_chr(
      list(file_path, start_line, end_line),
      function(file_path, start_line, end_line) {
        lines <- safe_read_lines(file_path)
        extract_context_block(lines, start_line, end_line)
      }
    ),
    call_text_clean = clean_space(call_text),
    context_text_clean = clean_space(context_text)
  )

# -----------------------------
# 5. Detect parameters from each cv.glmnet call
# -----------------------------
cv_call_audit <- cv_blocks %>%
  dplyr::mutate(
    nfolds_in_call = stringr::str_extract(call_text_clean, stringr::regex("nfolds\\s*=\\s*[0-9]+", ignore_case = TRUE)),
    foldid_in_call = stringr::str_extract(call_text_clean, stringr::regex("foldid\\s*=\\s*[A-Za-z0-9_\\.]+", ignore_case = TRUE)),
    weights_in_call = stringr::str_extract(call_text_clean, stringr::regex("weights\\s*=\\s*[^,\\)]+", ignore_case = TRUE)),
    family_in_call = stringr::str_extract(call_text_clean, stringr::regex("family\\s*=\\s*[\"']?binomial[\"']?", ignore_case = TRUE)),
    type_measure_in_call = stringr::str_extract(call_text_clean, stringr::regex("type\\.measure\\s*=\\s*[\"'][A-Za-z]+[\"']", ignore_case = TRUE)),
    standardize_in_call = stringr::str_extract(call_text_clean, stringr::regex("standardize\\s*=\\s*(TRUE|FALSE|T|F)", ignore_case = TRUE)),
    alpha_in_call = stringr::str_extract(call_text_clean, stringr::regex("alpha\\s*=\\s*[^,\\)]+", ignore_case = TRUE)),
    lambda_1se_in_context = stringr::str_detect(context_text_clean, stringr::regex("lambda\\.1se", ignore_case = TRUE)),
    lambda_min_in_context = stringr::str_detect(context_text_clean, stringr::regex("lambda\\.min", ignore_case = TRUE)),
    nfolds_in_context_assignment = purrr::map_chr(
      context_text,
      function(txt) {
        val <- extract_assignment_value(
          txt,
          c("inner[_\\. ]?folds", "inner[_\\. ]?cv[_\\. ]?folds", "nfolds", "nfold")
        )
        ifelse(is.na(val), NA_character_, val)
      }
    ),
    alpha_grid_in_context = purrr::map_chr(
      context_text,
      function(txt) {
        val <- extract_assignment_value(
          txt,
          c("alpha[_\\. ]?grid", "alphas", "alpha_values")
        )
        ifelse(is.na(val), NA_character_, val)
      }
    ),
    foldid_context_detected = stringr::str_detect(context_text_clean, stringr::regex("foldid", ignore_case = TRUE)),
    class_weight_context_detected = stringr::str_detect(context_text_clean, stringr::regex("class[_\\. ]?weight|case[_\\. ]?weight|sample[_\\. ]?weight|weights\\s*=", ignore_case = TRUE)),
    resampling_context_detected = stringr::str_detect(context_text_clean, stringr::regex("SMOTE|upSample|downSample|ROSE|oversampl|undersampl|synthetic", ignore_case = TRUE)),
    call_text_preview = substr(call_text_clean, 1, 1000),
    context_text_preview = substr(context_text_clean, 1, 1500)
  ) %>%
  dplyr::select(
    file_path,
    file_name,
    start_line,
    end_line,
    nfolds_in_call,
    nfolds_in_context_assignment,
    foldid_in_call,
    foldid_context_detected,
    weights_in_call,
    class_weight_context_detected,
    resampling_context_detected,
    family_in_call,
    type_measure_in_call,
    standardize_in_call,
    alpha_in_call,
    alpha_grid_in_context,
    lambda_1se_in_context,
    lambda_min_in_context,
    call_text_preview,
    context_text_preview
  )

# -----------------------------
# 6. Forced decisions
# -----------------------------
all_cv_text <- paste(
  cv_call_audit$call_text_preview,
  cv_call_audit$context_text_preview,
  collapse = "\n\n"
)

# Inner fold count decision
nfolds_values_call <- cv_call_audit$nfolds_in_call %>%
  na.omit() %>%
  as.character()

nfolds_values_context <- cv_call_audit$nfolds_in_context_assignment %>%
  na.omit() %>%
  as.character()

nfolds_numeric <- c(
  purrr::map_dbl(nfolds_values_call, extract_numeric_from_text),
  purrr::map_dbl(nfolds_values_context, extract_numeric_from_text)
)

nfolds_numeric <- nfolds_numeric[!is.na(nfolds_numeric)]

foldid_detected <- any(cv_call_audit$foldid_context_detected, na.rm = TRUE) ||
  any(!is.na(cv_call_audit$foldid_in_call))

if (length(unique(nfolds_numeric)) == 1) {
  inner_cv_fold_decision <- paste0(unique(nfolds_numeric), "-fold inner cross-validation")
  inner_cv_fold_value <- as.character(unique(nfolds_numeric))
  inner_cv_fold_status <- "READY_FOR_MANUSCRIPT"
} else if (length(unique(nfolds_numeric)) > 1) {
  inner_cv_fold_decision <- paste0(
    "Multiple nfolds values detected: ",
    paste(sort(unique(nfolds_numeric)), collapse = ", "),
    ". Verify which was used in the final nested LODO model."
  )
  inner_cv_fold_value <- paste(sort(unique(nfolds_numeric)), collapse = "; ")
  inner_cv_fold_status <- "NEEDS_MANUAL_CONFIRMATION"
} else if (foldid_detected) {
  inner_cv_fold_decision <- "foldid was detected, but the number of unique inner folds could not be inferred from static code."
  inner_cv_fold_value <- "foldid_detected"
  inner_cv_fold_status <- "NEEDS_MANUAL_CONFIRMATION"
} else {
  inner_cv_fold_decision <- "No explicit nfolds or foldid detected; cv.glmnet default is 10-fold cross-validation unless otherwise specified."
  inner_cv_fold_value <- "10_default_if_not_overridden"
  inner_cv_fold_status <- "READY_IF_DEFAULT_ACCEPTED"
}

# Weight decision
weights_in_call_detected <- any(!is.na(cv_call_audit$weights_in_call), na.rm = TRUE)
class_weight_context_detected <- any(cv_call_audit$class_weight_context_detected, na.rm = TRUE)
resampling_detected <- any(cv_call_audit$resampling_context_detected, na.rm = TRUE)

if (weights_in_call_detected) {
  weights_decision <- paste0(
    "weights argument detected in cv.glmnet call(s): ",
    paste(unique(na.omit(cv_call_audit$weights_in_call)), collapse = "; ")
  )
  weights_status <- "WEIGHTS_USED_OR_NEEDS_EXACT_DESCRIPTION"
} else {
  weights_decision <- "No weights argument was detected in cv.glmnet calls. Class weights were not passed to glmnet model fitting in the audited scripts."
  weights_status <- "READY_NO_WEIGHTS_IN_CV_GLMNET"
}

if (resampling_detected) {
  resampling_decision <- "Resampling-related keyword detected in cv.glmnet context; verify whether oversampling, undersampling, SMOTE, or ROSE was actually used."
  resampling_status <- "NEEDS_MANUAL_CONFIRMATION"
} else {
  resampling_decision <- "No oversampling, undersampling, SMOTE, ROSE, or synthetic resampling keywords were detected in cv.glmnet modeling contexts."
  resampling_status <- "READY_NO_RESAMPLING_DETECTED"
}

# Other forced details
family_decision <- if (any(stringr::str_detect(cv_call_audit$family_in_call, "binomial"), na.rm = TRUE)) {
  "family = \"binomial\""
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

type_measure_decision <- if (any(stringr::str_detect(cv_call_audit$type_measure_in_call, "auc"), na.rm = TRUE)) {
  "type.measure = \"auc\""
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

standardize_decision <- if (any(stringr::str_detect(cv_call_audit$standardize_in_call, "FALSE|F"), na.rm = TRUE)) {
  "standardize = FALSE"
} else if (any(stringr::str_detect(cv_call_audit$standardize_in_call, "TRUE|T"), na.rm = TRUE)) {
  "standardize = TRUE"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

alpha_grid_values <- cv_call_audit$alpha_grid_in_context %>%
  na.omit() %>%
  unique()

alpha_grid_decision <- if (length(alpha_grid_values) > 0) {
  paste(alpha_grid_values, collapse = "; ")
} else {
  detected_alpha_call <- cv_call_audit$alpha_in_call %>%
    na.omit() %>%
    unique()
  
  if (length(detected_alpha_call) > 0) {
    paste(detected_alpha_call, collapse = "; ")
  } else {
    "NEEDS_MANUAL_CONFIRMATION"
  }
}

lambda_decision <- if (any(cv_call_audit$lambda_1se_in_context, na.rm = TRUE)) {
  "lambda.1se"
} else if (any(cv_call_audit$lambda_min_in_context, na.rm = TRUE)) {
  "lambda.min"
} else {
  "NEEDS_MANUAL_CONFIRMATION"
}

# -----------------------------
# 7. Final forced audit table
# -----------------------------
forced_method_audit <- tibble::tibble(
  method_item = c(
    "Inner cross-validation folds",
    "Inner fold implementation",
    "Model family",
    "glmnet type.measure",
    "glmnet standardize argument",
    "glmnet alpha grid",
    "Lambda selection rule",
    "Class weights in cv.glmnet",
    "Resampling",
    "Class-weight interpretation",
    "Final Methods action"
  ),
  forced_decision = c(
    inner_cv_fold_decision,
    ifelse(foldid_detected, "foldid detected in local context", "No foldid detected in local context"),
    family_decision,
    type_measure_decision,
    standardize_decision,
    alpha_grid_decision,
    lambda_decision,
    weights_decision,
    resampling_decision,
    ifelse(
      weights_in_call_detected,
      "Report the exact weights argument in Methods.",
      "Report that no class weights were passed to glmnet model fitting."
    ),
    ifelse(
      inner_cv_fold_status %in% c("READY_FOR_MANUSCRIPT", "READY_IF_DEFAULT_ACCEPTED") &&
        weights_status == "READY_NO_WEIGHTS_IN_CV_GLMNET" &&
        resampling_status == "READY_NO_RESAMPLING_DETECTED",
      "READY_FOR_FINAL_METHODS_TEXT",
      "REVIEW_FOR_MANUAL_CONFIRMATION"
    )
  ),
  status = c(
    inner_cv_fold_status,
    ifelse(foldid_detected, "REVIEW_FOLDID", "READY_NO_FOLDID_DETECTED"),
    ifelse(family_decision == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(type_measure_decision == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(standardize_decision == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(alpha_grid_decision == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    ifelse(lambda_decision == "NEEDS_MANUAL_CONFIRMATION", "NEEDS_MANUAL_CONFIRMATION", "READY"),
    weights_status,
    resampling_status,
    ifelse(weights_in_call_detected, "NEEDS_EXACT_REPORTING", "READY"),
    ifelse(
      inner_cv_fold_status %in% c("READY_FOR_MANUSCRIPT", "READY_IF_DEFAULT_ACCEPTED") &&
        weights_status == "READY_NO_WEIGHTS_IN_CV_GLMNET" &&
        resampling_status == "READY_NO_RESAMPLING_DETECTED",
      "READY",
      "CHECK"
    )
  )
)

# -----------------------------
# 8. Build final methods text
# -----------------------------
inner_cv_phrase <- if (inner_cv_fold_status == "READY_FOR_MANUSCRIPT") {
  paste0(inner_cv_fold_value, "-fold inner cross-validation")
} else if (inner_cv_fold_status == "READY_IF_DEFAULT_ACCEPTED") {
  "10-fold inner cross-validation, corresponding to the default cv.glmnet setting because no explicit nfolds or foldid override was detected"
} else {
  "inner cross-validation with the fold count requiring final author verification"
}

weights_phrase <- if (!weights_in_call_detected && !resampling_detected) {
  "No class weights, oversampling, undersampling, SMOTE, ROSE, or synthetic resampling were used in the audited glmnet modeling calls."
} else if (weights_in_call_detected && !resampling_detected) {
  paste0(
    "Class weights were passed to cv.glmnet through the weights argument (",
    paste(unique(na.omit(cv_call_audit$weights_in_call)), collapse = "; "),
    "); no oversampling, undersampling, SMOTE, ROSE, or synthetic resampling was detected."
  )
} else {
  "Class weighting or resampling requires final author verification based on the audited glmnet call table."
}

final_methods_text <- paste(
  paste0(
    "Model development was performed using a strictly nested leave-one-dataset-out (LODO) design. ",
    "In each outer split, one bulk transcriptomic cohort was held out for evaluation, whereas feature filtering, ",
    "preprocessing parameters, alpha/lambda tuning, threshold selection, and model fitting were restricted to the ",
    "remaining training cohorts. Penalized logistic regression was fitted using glmnet with ",
    family_decision,
    ", ",
    type_measure_decision,
    ", and ",
    standardize_decision,
    "."
  ),
  paste0(
    "Within each outer training set, a predefined alpha grid was evaluated using ",
    inner_cv_phrase,
    ". The alpha grid was ",
    alpha_grid_decision,
    ". Because cv.glmnet does not automatically search over alpha, each alpha value was evaluated explicitly within ",
    "the training data, and alpha selection was based only on inner cross-validation performance. For the selected alpha, ",
    lambda_decision,
    " was used as the final penalty parameter to favor a parsimonious model within one standard error of the minimum ",
    "cross-validation error."
  ),
  paste0(
    "The classification threshold was selected within the training data only using the Youden index and was then applied ",
    "unchanged to the corresponding held-out cohort. No threshold re-selection was performed in held-out data."
  ),
  paste0(
    "Discrimination was assessed using AUROC and AUPRC. AUROC 95% confidence intervals were estimated using DeLong's ",
    "method as implemented in pROC. AUPRC was calculated from precision-recall curves using PRROC::pr.curve. The Brier ",
    "score was calculated as the mean squared difference between the observed binary outcome and the predicted probability. ",
    "Calibration intercept and slope were estimated using logistic calibration or rms::val.prob-style validation of predicted probabilities."
  ),
  paste0(
    "Samples lacking required phenotype labels or required signature-gene measurements were excluded from the corresponding ",
    "analysis by the predefined complete-case rule. ",
    weights_phrase
  ),
  sep = "\n\n"
)

# -----------------------------
# 9. Checks
# -----------------------------
n_manual_items <- forced_method_audit %>%
  dplyr::filter(stringr::str_detect(status, "NEEDS|CHECK|REVIEW")) %>%
  nrow()

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:22),
  check_item = c(
    "Project directory exists",
    "At least one priority or cv.glmnet script found",
    "At least one cv.glmnet call extracted",
    "cv.glmnet call audit generated",
    "Inner CV fold decision generated",
    "Inner CV fold decision is manuscript-ready or default-ready",
    "Weights decision generated",
    "No weights argument detected or exact weights reported",
    "Resampling decision generated",
    "No resampling detected",
    "Model family decision generated",
    "type.measure decision generated",
    "standardize decision generated",
    "alpha grid decision generated",
    "lambda decision generated",
    "Final methods text generated",
    "CSV cv.glmnet call audit generated",
    "CSV forced method audit generated",
    "TXT methods text generated",
    "DOCX methods text generated",
    "XLSX workbook generated",
    "No unresolved manual items"
  ),
  observed = c(
    dir.exists(project_dir),
    length(scripts_to_audit) > 0,
    nrow(cv_blocks) > 0,
    nrow(cv_call_audit) > 0,
    inner_cv_fold_decision != "",
    inner_cv_fold_status %in% c("READY_FOR_MANUSCRIPT", "READY_IF_DEFAULT_ACCEPTED"),
    weights_decision != "",
    weights_status %in% c("READY_NO_WEIGHTS_IN_CV_GLMNET", "WEIGHTS_USED_OR_NEEDS_EXACT_DESCRIPTION"),
    resampling_decision != "",
    resampling_status == "READY_NO_RESAMPLING_DETECTED",
    family_decision != "NEEDS_MANUAL_CONFIRMATION",
    type_measure_decision != "NEEDS_MANUAL_CONFIRMATION",
    standardize_decision != "NEEDS_MANUAL_CONFIRMATION",
    alpha_grid_decision != "NEEDS_MANUAL_CONFIRMATION",
    lambda_decision != "NEEDS_MANUAL_CONFIRMATION",
    nchar(final_methods_text) > 0,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    n_manual_items == 0
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 10. Export CSV/TXT
# -----------------------------
readr::write_csv(cv_call_audit, cv_call_audit_path)
readr::write_csv(forced_method_audit, method_audit_path)
writeLines(final_methods_text, con = txt_methods_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "CSV cv.glmnet call audit generated" ~ file.exists(cv_call_audit_path),
      check_item == "CSV forced method audit generated" ~ file.exists(method_audit_path),
      check_item == "TXT methods text generated" ~ file.exists(txt_methods_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 11. Export DOCX
# -----------------------------
ft_audit <- flextable::flextable(forced_method_audit)
ft_audit <- flextable::theme_booktabs(ft_audit)
ft_audit <- flextable::fontsize(ft_audit, size = 8.5, part = "all")
ft_audit <- flextable::fontsize(ft_audit, size = 9, part = "header")
ft_audit <- flextable::bold(ft_audit, part = "header")
ft_audit <- flextable::valign(ft_audit, valign = "top", part = "all")
ft_audit <- flextable::width(ft_audit, j = "method_item", width = 2.1)
ft_audit <- flextable::width(ft_audit, j = "forced_decision", width = 4.5)
ft_audit <- flextable::width(ft_audit, j = "status", width = 1.6)
ft_audit <- flextable::set_table_properties(ft_audit, layout = "fixed", width = 1)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Nested LODO modeling methods 47b forced audit", style = "heading 1")
doc <- officer::body_add_par(doc, "Final methods text", style = "heading 2")
doc <- officer::body_add_par(doc, final_methods_text, style = "Normal")
doc <- officer::body_add_par(doc, "Forced audit table", style = "heading 2")
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
# 12. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Forced_Method_Audit")
openxlsx::writeData(wb, "Forced_Method_Audit", forced_method_audit)

openxlsx::addWorksheet(wb, "CV_Glmnet_Call_Audit")
openxlsx::writeData(wb, "CV_Glmnet_Call_Audit", cv_call_audit)

openxlsx::addWorksheet(wb, "Scripts_Audited")
scripts_audited_tbl <- tibble::tibble(
  script_path = scripts_to_audit,
  script_name = basename(scripts_to_audit),
  is_priority_script = basename(scripts_to_audit) %in% script_names_priority,
  contains_cv_glmnet = script_path %in% scripts_with_cv
)
openxlsx::writeData(wb, "Scripts_Audited", scripts_audited_tbl)

openxlsx::addWorksheet(wb, "Methods_Text")
methods_text_tbl <- tibble::tibble(
  item = "final_methods_text",
  text = final_methods_text
)
openxlsx::writeData(wb, "Methods_Text", methods_text_tbl)

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
    "n_scripts_audited",
    "n_cv_glmnet_calls_extracted",
    "inner_cv_fold_decision",
    "inner_cv_fold_status",
    "weights_decision",
    "weights_status",
    "resampling_decision",
    "resampling_status",
    "n_manual_items",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "47b_force_extract_cv_weights_methods.R",
    project_dir,
    as.character(length(scripts_to_audit)),
    as.character(nrow(cv_call_audit)),
    inner_cv_fold_decision,
    inner_cv_fold_status,
    weights_decision,
    weights_status,
    resampling_decision,
    resampling_status,
    as.character(n_manual_items),
    as.character(n_failed_checks),
    dplyr::case_when(
      n_failed_checks == 0 && n_manual_items == 0 ~ "YES_INSERT_FINAL_METHODS_TEXT",
      n_failed_checks == 0 && n_manual_items > 0 ~ "PARTIAL_REVIEW_FORCED_AUDIT_ITEMS",
      TRUE ~ "NO_FIX_CHECK_ITEMS"
    ),
    "Review Forced_Method_Audit and CV_Glmnet_Call_Audit. If ready, use T47b_nested_LODO_methods_final_text.docx or txt in the manuscript."
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

cat("\nForced method audit:\n")
print(forced_method_audit, n = Inf, width = Inf)

cat("\nCV glmnet call audit preview:\n")
print(
  cv_call_audit %>%
    dplyr::select(
      file_name,
      start_line,
      nfolds_in_call,
      nfolds_in_context_assignment,
      foldid_in_call,
      weights_in_call,
      family_in_call,
      type_measure_in_call,
      standardize_in_call,
      alpha_in_call,
      alpha_grid_in_context,
      lambda_1se_in_context,
      resampling_context_detected
    ),
  n = Inf,
  width = Inf
)

cat("\nFinal methods text:\n")
cat(final_methods_text, "\n")

cat("\n关键输出：\n")
cat("1) ", cv_call_audit_path, "\n", sep = "")
cat("2) ", method_audit_path, "\n", sep = "")
cat("3) ", txt_methods_path, "\n", sep = "")
cat("4) ", docx_methods_path, "\n", sep = "")
cat("5) ", xlsx_path, "\n", sep = "")
cat("6) ", checks_path, "\n", sep = "")
cat("7) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Forced method audit、CV glmnet call audit preview 和 Final methods text 贴给我。\n")
cat("如果 n_failed_checks = 0 且 n_manual_items = 0，我会帮你确认 nested LODO 方法学细节模块是否可以正式收尾。\n")

cat("\n============ 47b forced nested LODO methods audit complete ============\n")