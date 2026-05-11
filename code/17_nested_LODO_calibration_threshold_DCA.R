# ============================================================
# 17_nested_LODO_calibration_threshold_DCA.R
# Calibration, threshold drift and DCA for strict nested LODO
#
# 目的：
# 1. 读取 14_nested_LODO_full_pipeline.R 输出的 predictions
# 2. 重新计算 pooled nested LODO performance
# 3. 重新整理 per-dataset calibration metrics
# 4. 重新整理 fixed threshold vs local threshold drift
# 5. 执行 exploratory decision curve analysis
# 6. 输出最终 calibration / threshold / DCA 表格和图
#
# 输入：
# 04_results/nested_LODO/
#   T14_nested_LODO_predictions.csv
#   T14_nested_LODO_validation_metrics.csv
#   T14_nested_LODO_calibration_metrics.csv
#   T14_nested_LODO_threshold_drift.csv
#
# 04_results/reporting/
#   T15b_cohort_control_type_eligibility_table.csv
#
# 输出：
# 04_results/dca/
#   T17_pooled_nested_LODO_performance.csv
#   T17_nested_LODO_calibration_metrics_final.csv
#   T17_nested_LODO_threshold_drift_final.csv
#   T17_nested_LODO_calibration_bins.csv
#   T17_nested_LODO_DCA_net_benefit.csv
#   T17_nested_LODO_DCA_summary.csv
#   T17_nested_LODO_calibration_threshold_DCA_summary.xlsx
#
# 05_figures/dca/
#   F17A_pooled_nested_LODO_ROC.png/pdf
#   F17B_pooled_nested_LODO_PR_curve.png/pdf
#   F17C_calibration_by_dataset.png/pdf
#   F17D_observed_vs_mean_predicted.png/pdf
#   F17E_threshold_drift_by_dataset.png/pdf
#   F17F_fixed_threshold_sensitivity_specificity.png/pdf
#   F17G_DCA_by_dataset.png/pdf
#   F17H_DCA_pooled.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(pROC)
  library(PRROC)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

nested_dir <- file.path(project_dir, "04_results", "nested_LODO")
report_dir <- file.path(project_dir, "04_results", "reporting")
out_dir <- file.path(project_dir, "04_results", "dca")
fig_dir <- file.path(project_dir, "05_figures", "dca")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

pred_file <- file.path(nested_dir, "T14_nested_LODO_predictions.csv")
metrics_file <- file.path(nested_dir, "T14_nested_LODO_validation_metrics.csv")
cal_file <- file.path(nested_dir, "T14_nested_LODO_calibration_metrics.csv")
threshold_file <- file.path(nested_dir, "T14_nested_LODO_threshold_drift.csv")
cohort_file <- file.path(report_dir, "T15b_cohort_control_type_eligibility_table.csv")

needed <- c(pred_file, metrics_file, cal_file, threshold_file)

missing <- needed[!file.exists(needed)]
if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 参数
# ============================================================

eps <- 1e-6
dca_thresholds <- seq(0.05, 0.80, by = 0.01)
n_calibration_bins <- 5

# ============================================================
# 工具函数
# ============================================================

clip_prob <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  p[p < eps] <- eps
  p[p > 1 - eps] <- 1 - eps
  p
}

logit <- function(p) {
  p <- clip_prob(p, eps = eps)
  log(p / (1 - p))
}

safe_auc_ci <- function(y_true, pred) {
  idx <- !is.na(y_true) & is.finite(pred)
  y_true <- y_true[idx]
  pred <- pred[idx]
  
  if (length(unique(y_true)) != 2) {
    return(c(AUROC = NA_real_, AUROC_low = NA_real_, AUROC_high = NA_real_))
  }
  
  roc_obj <- try(
    pROC::roc(
      response = y_true,
      predictor = pred,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(roc_obj, "try-error")) {
    return(c(AUROC = NA_real_, AUROC_low = NA_real_, AUROC_high = NA_real_))
  }
  
  ci <- try(pROC::ci.auc(roc_obj), silent = TRUE)
  
  if (inherits(ci, "try-error")) {
    return(c(
      AUROC = as.numeric(pROC::auc(roc_obj)),
      AUROC_low = NA_real_,
      AUROC_high = NA_real_
    ))
  }
  
  c(
    AUROC = as.numeric(pROC::auc(roc_obj)),
    AUROC_low = as.numeric(ci[1]),
    AUROC_high = as.numeric(ci[3])
  )
}

safe_auprc <- function(y_true, pred) {
  idx <- !is.na(y_true) & is.finite(pred)
  y_true <- y_true[idx]
  pred <- pred[idx]
  
  if (length(unique(y_true)) != 2) return(NA_real_)
  
  scores_pos <- pred[y_true == 1]
  scores_neg <- pred[y_true == 0]
  
  pr <- try(
    PRROC::pr.curve(
      scores.class0 = scores_pos,
      scores.class1 = scores_neg,
      curve = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(pr, "try-error")) return(NA_real_)
  as.numeric(pr$auc.integral)
}

make_roc_curve_df <- function(y_true, pred, label) {
  idx <- !is.na(y_true) & is.finite(pred)
  y_true <- y_true[idx]
  pred <- pred[idx]
  
  if (length(unique(y_true)) != 2) return(data.frame())
  
  roc_obj <- pROC::roc(
    response = y_true,
    predictor = pred,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  data.frame(
    curve = label,
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    threshold = roc_obj$thresholds,
    stringsAsFactors = FALSE
  )
}

make_pr_curve_df <- function(y_true, pred, label) {
  idx <- !is.na(y_true) & is.finite(pred)
  y_true <- y_true[idx]
  pred <- pred[idx]
  
  if (length(unique(y_true)) != 2) return(data.frame())
  
  ord <- order(pred, decreasing = TRUE)
  y_ord <- y_true[ord]
  pred_ord <- pred[ord]
  
  tp <- cumsum(y_ord == 1)
  fp <- cumsum(y_ord == 0)
  fn <- sum(y_ord == 1) - tp
  
  precision <- tp / pmax(tp + fp, 1)
  recall <- tp / pmax(tp + fn, 1)
  
  data.frame(
    curve = label,
    recall = recall,
    precision = precision,
    threshold = pred_ord,
    stringsAsFactors = FALSE
  )
}

calc_classification_metrics <- function(y_true, pred_prob, threshold) {
  pred_class <- ifelse(pred_prob >= threshold, 1, 0)
  
  tp <- sum(pred_class == 1 & y_true == 1, na.rm = TRUE)
  tn <- sum(pred_class == 0 & y_true == 0, na.rm = TRUE)
  fp <- sum(pred_class == 1 & y_true == 0, na.rm = TRUE)
  fn <- sum(pred_class == 0 & y_true == 1, na.rm = TRUE)
  
  data.frame(
    threshold = threshold,
    sensitivity = tp / max(tp + fn, 1),
    specificity = tn / max(tn + fp, 1),
    PPV = tp / max(tp + fp, 1),
    NPV = tn / max(tn + fn, 1),
    accuracy = (tp + tn) / max(tp + tn + fp + fn, 1),
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn,
    stringsAsFactors = FALSE
  )
}

choose_youden_threshold <- function(y_true, pred_prob) {
  if (length(unique(y_true)) != 2) return(NA_real_)
  
  roc_obj <- try(
    pROC::roc(
      response = y_true,
      predictor = pred_prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(roc_obj, "try-error")) return(NA_real_)
  
  coords <- try(
    pROC::coords(
      roc_obj,
      x = "best",
      best.method = "youden",
      ret = c("threshold", "sensitivity", "specificity"),
      transpose = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(coords, "try-error")) return(NA_real_)
  as.numeric(coords$threshold[1])
}

calc_calibration_metrics <- function(y_true, pred_prob) {
  pred_prob <- clip_prob(pred_prob, eps = eps)
  y_true <- as.numeric(y_true)
  
  observed_rate <- mean(y_true == 1, na.rm = TRUE)
  mean_predicted <- mean(pred_prob, na.rm = TRUE)
  brier <- mean((pred_prob - y_true)^2, na.rm = TRUE)
  cal_in_large <- mean_predicted - observed_rate
  
  cal_intercept <- NA_real_
  cal_slope <- NA_real_
  cal_intercept_se <- NA_real_
  cal_slope_se <- NA_real_
  
  if (length(unique(y_true)) == 2 && length(unique(pred_prob)) >= 3) {
    df <- data.frame(
      y = y_true,
      lp = logit(pred_prob)
    )
    
    fit <- try(
      stats::glm(y ~ lp, data = df, family = stats::binomial()),
      silent = TRUE
    )
    
    if (!inherits(fit, "try-error")) {
      co <- try(summary(fit)$coefficients, silent = TRUE)
      if (!inherits(co, "try-error") && all(c("(Intercept)", "lp") %in% rownames(co))) {
        cal_intercept <- as.numeric(co["(Intercept)", "Estimate"])
        cal_slope <- as.numeric(co["lp", "Estimate"])
        cal_intercept_se <- as.numeric(co["(Intercept)", "Std. Error"])
        cal_slope_se <- as.numeric(co["lp", "Std. Error"])
      }
    }
  }
  
  data.frame(
    observed_rate = observed_rate,
    mean_predicted = mean_predicted,
    calibration_in_the_large = cal_in_large,
    calibration_intercept = cal_intercept,
    calibration_slope = cal_slope,
    calibration_intercept_se = cal_intercept_se,
    calibration_slope_se = cal_slope_se,
    Brier = brier,
    stringsAsFactors = FALSE
  )
}

make_calibration_bins_by_dataset <- function(pred_df, n_bins = 5) {
  out_list <- list()
  
  for (ds in unique(pred_df$validation_dataset)) {
    df <- pred_df[pred_df$validation_dataset == ds, , drop = FALSE]
    df <- df[is.finite(df$pred_prob), , drop = FALSE]
    
    if (nrow(df) < 5) next
    
    df$rank_pred <- rank(df$pred_prob, ties.method = "first")
    
    bins <- min(n_bins, nrow(df))
    breaks <- unique(stats::quantile(
      df$rank_pred,
      probs = seq(0, 1, length.out = bins + 1),
      na.rm = TRUE
    ))
    
    if (length(breaks) < 3) next
    
    df$bin <- cut(
      df$rank_pred,
      breaks = breaks,
      include.lowest = TRUE,
      labels = FALSE
    )
    
    bin_summary <- aggregate(
      cbind(pred_prob, y_true) ~ bin,
      data = df,
      FUN = mean
    )
    
    bin_n <- aggregate(
      sample_id ~ bin,
      data = df,
      FUN = length
    )
    
    names(bin_summary)[names(bin_summary) == "pred_prob"] <- "mean_predicted"
    names(bin_summary)[names(bin_summary) == "y_true"] <- "observed_rate"
    names(bin_n)[names(bin_n) == "sample_id"] <- "n_bin"
    
    z <- merge(bin_summary, bin_n, by = "bin", all.x = TRUE)
    z$validation_dataset <- ds
    z <- z[, c("validation_dataset", "bin", "n_bin", "mean_predicted", "observed_rate")]
    
    out_list[[ds]] <- z
  }
  
  if (length(out_list) == 0) return(data.frame())
  do.call(rbind, out_list)
}

net_benefit_at_threshold <- function(y_true, pred_prob, threshold) {
  y_true <- as.numeric(y_true)
  pred_class <- ifelse(pred_prob >= threshold, 1, 0)
  
  n <- length(y_true)
  tp <- sum(pred_class == 1 & y_true == 1, na.rm = TRUE)
  fp <- sum(pred_class == 1 & y_true == 0, na.rm = TRUE)
  
  nb <- (tp / n) - (fp / n) * (threshold / (1 - threshold))
  
  nb
}

make_dca <- function(y_true, pred_prob, thresholds, dataset_label) {
  y_true <- as.numeric(y_true)
  pred_prob <- clip_prob(pred_prob, eps = eps)
  
  prevalence <- mean(y_true == 1, na.rm = TRUE)
  
  out <- data.frame(
    validation_dataset = dataset_label,
    threshold = thresholds,
    net_benefit_model = NA_real_,
    net_benefit_treat_all = NA_real_,
    net_benefit_treat_none = 0,
    prevalence = prevalence,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(thresholds)) {
    pt <- thresholds[i]
    out$net_benefit_model[i] <- net_benefit_at_threshold(y_true, pred_prob, pt)
    out$net_benefit_treat_all[i] <- prevalence - (1 - prevalence) * (pt / (1 - pt))
  }
  
  out
}

make_dca_summary <- function(dca_df) {
  out_list <- list()
  
  for (ds in unique(dca_df$validation_dataset)) {
    df <- dca_df[dca_df$validation_dataset == ds, , drop = FALSE]
    
    df$better_than_all <- df$net_benefit_model > df$net_benefit_treat_all
    df$better_than_none <- df$net_benefit_model > df$net_benefit_treat_none
    df$clinically_preferable <- df$better_than_all & df$better_than_none
    
    out_list[[ds]] <- data.frame(
      validation_dataset = ds,
      n_thresholds = nrow(df),
      n_thresholds_model_better_than_all = sum(df$better_than_all, na.rm = TRUE),
      n_thresholds_model_better_than_none = sum(df$better_than_none, na.rm = TRUE),
      n_thresholds_model_clinically_preferable = sum(df$clinically_preferable, na.rm = TRUE),
      fraction_thresholds_clinically_preferable = mean(df$clinically_preferable, na.rm = TRUE),
      max_net_benefit_model = max(df$net_benefit_model, na.rm = TRUE),
      threshold_at_max_net_benefit = df$threshold[which.max(df$net_benefit_model)[1]],
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, out_list)
}

# ============================================================
# 读取输入
# ============================================================

message("Reading nested LODO predictions and metrics...")

pred <- data.table::fread(pred_file, data.table = FALSE)
metrics_14 <- data.table::fread(metrics_file, data.table = FALSE)
cal_14 <- data.table::fread(cal_file, data.table = FALSE)
threshold_14 <- data.table::fread(threshold_file, data.table = FALSE)

if (file.exists(cohort_file)) {
  cohort_table <- data.table::fread(cohort_file, data.table = FALSE)
} else {
  cohort_table <- data.frame()
}

required_pred_cols <- c(
  "sample_id", "validation_dataset", "clinical_group_main",
  "y_true", "pred_prob", "fixed_training_threshold"
)

if (!all(required_pred_cols %in% colnames(pred))) {
  stop("T14_nested_LODO_predictions.csv 缺少必要列：", paste(setdiff(required_pred_cols, colnames(pred)), collapse = ", "))
}

pred$y_true <- as.numeric(pred$y_true)
pred$pred_prob <- clip_prob(pred$pred_prob, eps = eps)

# ============================================================
# Pooled performance
# ============================================================

message("Calculating pooled nested LODO performance...")

auc_ci <- safe_auc_ci(pred$y_true, pred$pred_prob)
pooled_local_threshold <- choose_youden_threshold(pred$y_true, pred$pred_prob)
pooled_cls_local <- calc_classification_metrics(pred$y_true, pred$pred_prob, pooled_local_threshold)

# pooled fixed threshold 不唯一，因为每折有自己的 training threshold
# 这里使用每个样本对应 fold 的 fixed_training_threshold
pred$pred_class_fold_fixed <- ifelse(pred$pred_prob >= pred$fixed_training_threshold, 1, 0)

tp <- sum(pred$pred_class_fold_fixed == 1 & pred$y_true == 1, na.rm = TRUE)
tn <- sum(pred$pred_class_fold_fixed == 0 & pred$y_true == 0, na.rm = TRUE)
fp <- sum(pred$pred_class_fold_fixed == 1 & pred$y_true == 0, na.rm = TRUE)
fn <- sum(pred$pred_class_fold_fixed == 0 & pred$y_true == 1, na.rm = TRUE)

pooled_fixed_summary <- data.frame(
  threshold_rule = "fold-specific fixed training threshold",
  sensitivity = tp / max(tp + fn, 1),
  specificity = tn / max(tn + fp, 1),
  PPV = tp / max(tp + fp, 1),
  NPV = tn / max(tn + fn, 1),
  accuracy = (tp + tn) / max(tp + tn + fp + fn, 1),
  TP = tp,
  TN = tn,
  FP = fp,
  FN = fn,
  stringsAsFactors = FALSE
)

pooled_cal <- calc_calibration_metrics(pred$y_true, pred$pred_prob)

pooled_performance <- data.frame(
  n = nrow(pred),
  n_case = sum(pred$y_true == 1),
  n_control = sum(pred$y_true == 0),
  prevalence = mean(pred$y_true == 1),
  AUROC = auc_ci[["AUROC"]],
  AUROC_low = auc_ci[["AUROC_low"]],
  AUROC_high = auc_ci[["AUROC_high"]],
  AUPRC = safe_auprc(pred$y_true, pred$pred_prob),
  Brier = mean((pred$pred_prob - pred$y_true)^2),
  mean_predicted = pooled_cal$mean_predicted,
  observed_rate = pooled_cal$observed_rate,
  calibration_in_the_large = pooled_cal$calibration_in_the_large,
  calibration_intercept = pooled_cal$calibration_intercept,
  calibration_slope = pooled_cal$calibration_slope,
  pooled_local_youden_threshold = pooled_local_threshold,
  local_threshold_sensitivity = pooled_cls_local$sensitivity,
  local_threshold_specificity = pooled_cls_local$specificity,
  fold_fixed_sensitivity = pooled_fixed_summary$sensitivity,
  fold_fixed_specificity = pooled_fixed_summary$specificity,
  fold_fixed_accuracy = pooled_fixed_summary$accuracy,
  stringsAsFactors = FALSE
)

# ============================================================
# Final per-dataset calibration and threshold
# ============================================================

message("Recalculating per-dataset calibration and threshold drift...")

cal_list <- list()
thr_list <- list()

for (ds in unique(pred$validation_dataset)) {
  df <- pred[pred$validation_dataset == ds, , drop = FALSE]
  
  cal <- calc_calibration_metrics(df$y_true, df$pred_prob)
  auc_ds <- safe_auc_ci(df$y_true, df$pred_prob)
  
  cal$validation_dataset <- ds
  cal$n <- nrow(df)
  cal$n_case <- sum(df$y_true == 1)
  cal$n_control <- sum(df$y_true == 0)
  cal$AUROC <- auc_ds[["AUROC"]]
  cal$AUROC_low <- auc_ds[["AUROC_low"]]
  cal$AUROC_high <- auc_ds[["AUROC_high"]]
  cal$AUPRC <- safe_auprc(df$y_true, df$pred_prob)
  
  cal_list[[ds]] <- cal
  
  fixed_threshold <- unique(df$fixed_training_threshold)
  fixed_threshold <- fixed_threshold[is.finite(fixed_threshold)][1]
  
  local_threshold <- choose_youden_threshold(df$y_true, df$pred_prob)
  
  fixed_cls <- calc_classification_metrics(df$y_true, df$pred_prob, fixed_threshold)
  local_cls <- calc_classification_metrics(df$y_true, df$pred_prob, local_threshold)
  
  thr <- data.frame(
    validation_dataset = ds,
    n = nrow(df),
    n_case = sum(df$y_true == 1),
    n_control = sum(df$y_true == 0),
    fixed_training_threshold = fixed_threshold,
    local_youden_threshold = local_threshold,
    threshold_shift = local_threshold - fixed_threshold,
    abs_threshold_shift = abs(local_threshold - fixed_threshold),
    fixed_sensitivity = fixed_cls$sensitivity,
    fixed_specificity = fixed_cls$specificity,
    fixed_accuracy = fixed_cls$accuracy,
    fixed_PPV = fixed_cls$PPV,
    fixed_NPV = fixed_cls$NPV,
    local_sensitivity = local_cls$sensitivity,
    local_specificity = local_cls$specificity,
    local_accuracy = local_cls$accuracy,
    local_PPV = local_cls$PPV,
    local_NPV = local_cls$NPV,
    stringsAsFactors = FALSE
  )
  
  thr_list[[ds]] <- thr
}

calibration_final <- do.call(rbind, cal_list)
threshold_final <- do.call(rbind, thr_list)

calibration_final <- calibration_final[, c(
  "validation_dataset", "n", "n_case", "n_control",
  "observed_rate", "mean_predicted", "calibration_in_the_large",
  "calibration_intercept", "calibration_slope",
  "calibration_intercept_se", "calibration_slope_se",
  "Brier", "AUROC", "AUROC_low", "AUROC_high", "AUPRC"
)]

threshold_final <- threshold_final[order(threshold_final$validation_dataset), , drop = FALSE]

calibration_bins <- make_calibration_bins_by_dataset(pred, n_bins = n_calibration_bins)

# ============================================================
# DCA
# ============================================================

message("Running decision curve analysis...")

dca_list <- list()

for (ds in unique(pred$validation_dataset)) {
  df <- pred[pred$validation_dataset == ds, , drop = FALSE]
  dca_list[[ds]] <- make_dca(
    y_true = df$y_true,
    pred_prob = df$pred_prob,
    thresholds = dca_thresholds,
    dataset_label = ds
  )
}

dca_pooled <- make_dca(
  y_true = pred$y_true,
  pred_prob = pred$pred_prob,
  thresholds = dca_thresholds,
  dataset_label = "Pooled_nested_LODO"
)

dca_all <- do.call(rbind, c(dca_list, list(Pooled_nested_LODO = dca_pooled)))
dca_summary <- make_dca_summary(dca_all)

# ============================================================
# 合并 control context
# ============================================================

if (nrow(cohort_table) > 0) {
  context_cols <- intersect(
    c(
      "dataset",
      "control_type_standardized",
      "clinical_control_strength",
      "primary_contrast_used",
      "recommended_manuscript_role",
      "main_limitation"
    ),
    colnames(cohort_table)
  )
  
  context <- cohort_table[, context_cols, drop = FALSE]
  names(context)[names(context) == "dataset"] <- "validation_dataset"
  
  calibration_final <- merge(calibration_final, context, by = "validation_dataset", all.x = TRUE, sort = FALSE)
  threshold_final <- merge(threshold_final, context, by = "validation_dataset", all.x = TRUE, sort = FALSE)
  metrics_context <- merge(metrics_14, context, by = "validation_dataset", all.x = TRUE, sort = FALSE)
} else {
  metrics_context <- metrics_14
}

# ============================================================
# Summary metrics
# ============================================================

summary_final <- data.frame(
  metric = c(
    "n_total_samples_pooled",
    "n_case_pooled",
    "n_control_pooled",
    "pooled_AUROC",
    "pooled_AUROC_low",
    "pooled_AUROC_high",
    "pooled_AUPRC",
    "pooled_Brier",
    "pooled_calibration_in_the_large",
    "pooled_fold_fixed_sensitivity",
    "pooled_fold_fixed_specificity",
    "median_dataset_AUROC",
    "min_dataset_AUROC",
    "max_dataset_AUROC",
    "n_dataset_AUROC_ge_0.80",
    "median_abs_calibration_in_the_large",
    "median_abs_threshold_shift",
    "max_abs_threshold_shift",
    "n_fixed_sensitivity_lt_0.20",
    "n_fixed_specificity_lt_0.20"
  ),
  value = c(
    pooled_performance$n,
    pooled_performance$n_case,
    pooled_performance$n_control,
    pooled_performance$AUROC,
    pooled_performance$AUROC_low,
    pooled_performance$AUROC_high,
    pooled_performance$AUPRC,
    pooled_performance$Brier,
    pooled_performance$calibration_in_the_large,
    pooled_performance$fold_fixed_sensitivity,
    pooled_performance$fold_fixed_specificity,
    median(calibration_final$AUROC, na.rm = TRUE),
    min(calibration_final$AUROC, na.rm = TRUE),
    max(calibration_final$AUROC, na.rm = TRUE),
    sum(calibration_final$AUROC >= 0.80, na.rm = TRUE),
    median(abs(calibration_final$calibration_in_the_large), na.rm = TRUE),
    median(abs(threshold_final$threshold_shift), na.rm = TRUE),
    max(abs(threshold_final$threshold_shift), na.rm = TRUE),
    sum(threshold_final$fixed_sensitivity < 0.20, na.rm = TRUE),
    sum(threshold_final$fixed_specificity < 0.20, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存表格
# ============================================================

message("Writing output tables...")

data.table::fwrite(
  pooled_performance,
  file.path(out_dir, "T17_pooled_nested_LODO_performance.csv")
)

data.table::fwrite(
  pooled_fixed_summary,
  file.path(out_dir, "T17_pooled_fold_fixed_threshold_classification.csv")
)

data.table::fwrite(
  calibration_final,
  file.path(out_dir, "T17_nested_LODO_calibration_metrics_final.csv")
)

data.table::fwrite(
  threshold_final,
  file.path(out_dir, "T17_nested_LODO_threshold_drift_final.csv")
)

data.table::fwrite(
  calibration_bins,
  file.path(out_dir, "T17_nested_LODO_calibration_bins.csv")
)

data.table::fwrite(
  dca_all,
  file.path(out_dir, "T17_nested_LODO_DCA_net_benefit.csv")
)

data.table::fwrite(
  dca_summary,
  file.path(out_dir, "T17_nested_LODO_DCA_summary.csv")
)

data.table::fwrite(
  summary_final,
  file.path(out_dir, "T17_calibration_threshold_DCA_summary_metrics.csv")
)

if (exists("metrics_context")) {
  data.table::fwrite(
    metrics_context,
    file.path(out_dir, "T17_nested_LODO_validation_metrics_with_context.csv")
  )
}

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary")
openxlsx::writeData(wb, "summary", summary_final)

openxlsx::addWorksheet(wb, "pooled_performance")
openxlsx::writeData(wb, "pooled_performance", pooled_performance)

openxlsx::addWorksheet(wb, "validation_metrics")
openxlsx::writeData(wb, "validation_metrics", metrics_context)

openxlsx::addWorksheet(wb, "calibration_final")
openxlsx::writeData(wb, "calibration_final", calibration_final)

openxlsx::addWorksheet(wb, "threshold_drift")
openxlsx::writeData(wb, "threshold_drift", threshold_final)

openxlsx::addWorksheet(wb, "calibration_bins")
openxlsx::writeData(wb, "calibration_bins", calibration_bins)

openxlsx::addWorksheet(wb, "DCA_summary")
openxlsx::writeData(wb, "DCA_summary", dca_summary)

openxlsx::addWorksheet(wb, "DCA_net_benefit")
openxlsx::writeData(wb, "DCA_net_benefit", dca_all)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T17_nested_LODO_calibration_threshold_DCA_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 作图
# ============================================================

message("Saving figures...")

# ROC pooled
roc_df <- make_roc_curve_df(pred$y_true, pred$pred_prob, "Pooled nested LODO")

p_roc <- ggplot2::ggplot(
  roc_df,
  ggplot2::aes(x = FPR, y = TPR)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = paste0("Pooled nested LODO ROC, AUROC = ", round(pooled_performance$AUROC, 3)),
    x = "1 - specificity",
    y = "Sensitivity"
  )

ggplot2::ggsave(file.path(fig_dir, "F17A_pooled_nested_LODO_ROC.png"), p_roc, width = 6.5, height = 6, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17A_pooled_nested_LODO_ROC.pdf"), p_roc, width = 6.5, height = 6)

# PR curve pooled
pr_df <- make_pr_curve_df(pred$y_true, pred$pred_prob, "Pooled nested LODO")

p_pr <- ggplot2::ggplot(
  pr_df,
  ggplot2::aes(x = recall, y = precision)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = mean(pred$y_true == 1), linetype = "dashed", linewidth = 0.35) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = paste0("Pooled nested LODO PR curve, AUPRC = ", round(pooled_performance$AUPRC, 3)),
    x = "Recall",
    y = "Precision"
  )

ggplot2::ggsave(file.path(fig_dir, "F17B_pooled_nested_LODO_PR_curve.png"), p_pr, width = 6.5, height = 6, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17B_pooled_nested_LODO_PR_curve.pdf"), p_pr, width = 6.5, height = 6)

# calibration by dataset
p_cal <- ggplot2::ggplot(
  calibration_bins,
  ggplot2::aes(x = mean_predicted, y = observed_rate)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
  ggplot2::geom_point(size = 2.2, alpha = 0.85) +
  ggplot2::geom_line(alpha = 0.65) +
  ggplot2::facet_wrap(~ validation_dataset) +
  ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::labs(
    title = "Nested LODO calibration by held-out dataset",
    x = "Mean predicted probability",
    y = "Observed sepsis proportion"
  )

ggplot2::ggsave(file.path(fig_dir, "F17C_calibration_by_dataset.png"), p_cal, width = 10, height = 7, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17C_calibration_by_dataset.pdf"), p_cal, width = 10, height = 7)

# observed vs predicted
p_obs <- ggplot2::ggplot(
  calibration_final,
  ggplot2::aes(x = mean_predicted, y = observed_rate, label = validation_dataset)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
  ggplot2::geom_point(size = 3, alpha = 0.85) +
  ggplot2::geom_text(vjust = -0.7, size = 3) +
  ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Observed rate vs mean predicted probability",
    x = "Mean predicted probability",
    y = "Observed sepsis proportion"
  )

ggplot2::ggsave(file.path(fig_dir, "F17D_observed_vs_mean_predicted.png"), p_obs, width = 7, height = 6, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17D_observed_vs_mean_predicted.pdf"), p_obs, width = 7, height = 6)

# threshold drift
threshold_final$validation_dataset <- factor(
  threshold_final$validation_dataset,
  levels = threshold_final$validation_dataset[order(threshold_final$threshold_shift)]
)

p_thr <- ggplot2::ggplot(
  threshold_final,
  ggplot2::aes(x = validation_dataset)
) +
  ggplot2::geom_point(ggplot2::aes(y = fixed_training_threshold), size = 3, alpha = 0.85) +
  ggplot2::geom_point(ggplot2::aes(y = local_youden_threshold), size = 3, shape = 17, alpha = 0.85) +
  ggplot2::geom_segment(
    ggplot2::aes(
      xend = validation_dataset,
      y = fixed_training_threshold,
      yend = local_youden_threshold
    ),
    linewidth = 0.6,
    alpha = 0.75
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Fixed training threshold vs local Youden threshold",
    x = "Held-out dataset",
    y = "Probability threshold"
  )

ggplot2::ggsave(file.path(fig_dir, "F17E_threshold_drift_by_dataset.png"), p_thr, width = 7, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17E_threshold_drift_by_dataset.pdf"), p_thr, width = 7, height = 5)

# fixed sensitivity / specificity
thr_long <- rbind(
  data.frame(
    validation_dataset = as.character(threshold_final$validation_dataset),
    metric = "Fixed sensitivity",
    value = threshold_final$fixed_sensitivity
  ),
  data.frame(
    validation_dataset = as.character(threshold_final$validation_dataset),
    metric = "Fixed specificity",
    value = threshold_final$fixed_specificity
  )
)

p_fixed <- ggplot2::ggplot(
  thr_long,
  ggplot2::aes(x = validation_dataset, y = value, fill = metric)
) +
  ggplot2::geom_col(position = "dodge", alpha = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Fixed-threshold sensitivity and specificity",
    x = "Held-out dataset",
    y = "Value",
    fill = "Metric"
  )

ggplot2::ggsave(file.path(fig_dir, "F17F_fixed_threshold_sensitivity_specificity.png"), p_fixed, width = 7, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17F_fixed_threshold_sensitivity_specificity.pdf"), p_fixed, width = 7, height = 5)

# DCA by dataset
dca_dataset_only <- dca_all[dca_all$validation_dataset != "Pooled_nested_LODO", , drop = FALSE]

p_dca_ds <- ggplot2::ggplot(
  dca_dataset_only,
  ggplot2::aes(x = threshold)
) +
  ggplot2::geom_line(ggplot2::aes(y = net_benefit_model), linewidth = 0.8) +
  ggplot2::geom_line(ggplot2::aes(y = net_benefit_treat_all), linetype = "dashed", linewidth = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = net_benefit_treat_none), linetype = "dotted", linewidth = 0.5) +
  ggplot2::facet_wrap(~ validation_dataset, scales = "free_y") +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::labs(
    title = "Exploratory DCA by held-out dataset",
    x = "Threshold probability",
    y = "Net benefit"
  )

ggplot2::ggsave(file.path(fig_dir, "F17G_DCA_by_dataset.png"), p_dca_ds, width = 10, height = 7, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17G_DCA_by_dataset.pdf"), p_dca_ds, width = 10, height = 7)

# DCA pooled
p_dca_pool <- ggplot2::ggplot(
  dca_pooled,
  ggplot2::aes(x = threshold)
) +
  ggplot2::geom_line(ggplot2::aes(y = net_benefit_model), linewidth = 0.9) +
  ggplot2::geom_line(ggplot2::aes(y = net_benefit_treat_all), linetype = "dashed", linewidth = 0.6) +
  ggplot2::geom_line(ggplot2::aes(y = net_benefit_treat_none), linetype = "dotted", linewidth = 0.6) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::labs(
    title = "Exploratory DCA for pooled nested LODO predictions",
    x = "Threshold probability",
    y = "Net benefit"
  )

ggplot2::ggsave(file.path(fig_dir, "F17H_DCA_pooled.png"), p_dca_pool, width = 7, height = 5, dpi = 300)
ggplot2::ggsave(file.path(fig_dir, "F17H_DCA_pooled.pdf"), p_dca_pool, width = 7, height = 5)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_17_nested_LODO_calibration_threshold_DCA.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 17 Nested LODO calibration, threshold and DCA 完成 ============")
message("输出目录：", out_dir)
message("图输出目录：", fig_dir)

message("\nPooled nested LODO performance:")
print(pooled_performance)

message("\nFinal calibration metrics:")
print(calibration_final)

message("\nFinal threshold drift:")
print(threshold_final)

message("\nDCA summary:")
print(dca_summary)

message("\nSummary metrics:")
print(summary_final)

message("\n关键输出：")
message("1) ", file.path(out_dir, "T17_pooled_nested_LODO_performance.csv"))
message("2) ", file.path(out_dir, "T17_nested_LODO_calibration_metrics_final.csv"))
message("3) ", file.path(out_dir, "T17_nested_LODO_threshold_drift_final.csv"))
message("4) ", file.path(out_dir, "T17_nested_LODO_DCA_net_benefit.csv"))
message("5) ", file.path(out_dir, "T17_nested_LODO_DCA_summary.csv"))
message("6) ", file.path(out_dir, "T17_nested_LODO_calibration_threshold_DCA_summary.xlsx"))
message("7) ", fig_dir)

message("\n下一步建议：")
message("如果 pooled AUROC 与 per-dataset 结果一致，且 DCA 没有明显异常，就继续 18_prepare_scRNA_dataset.R。")