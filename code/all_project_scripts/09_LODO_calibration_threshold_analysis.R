# ============================================================
# 09_LODO_calibration_threshold_analysis.R
# LODO 诊断模型的校准、Brier、阈值漂移分析
#
# 目的：
# 1. 基于 LODO 预测结果评估每个验证队列的校准
# 2. 计算 calibration-in-the-large, calibration slope, Brier
# 3. 比较训练集固定阈值与验证集本地 Youden 阈值
# 4. 输出阈值漂移和固定阈值操作性能
# 5. 形成后续 Figure 4 / Supplementary Figure 的图表
#
# 输入：
# 04_results/model_validation/
#   T08_LODO_predictions.csv
#   T08_LODO_validation_metrics.csv
#
# 输出：
# 04_results/model_validation/
#   T09_LODO_calibration_metrics.csv
#   T09_LODO_threshold_drift_metrics.csv
#   T09_LODO_calibration_threshold_summary.xlsx
#
# 05_figures/model_validation/
#   F11A_LODO_calibration_by_dataset.png/pdf
#   F11B_LODO_observed_vs_mean_predicted.png/pdf
#   F11C_LODO_Brier_by_dataset.png/pdf
#   F11D_LODO_threshold_drift.png/pdf
#   F11E_LODO_fixed_threshold_sensitivity_specificity.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(ggplot2)
  library(pROC)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

val_dir <- file.path(project_dir, "04_results", "model_validation")
fig_dir <- file.path(project_dir, "05_figures", "model_validation")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(val_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

pred_file <- file.path(val_dir, "T08_LODO_predictions.csv")
metric_file <- file.path(val_dir, "T08_LODO_validation_metrics.csv")

needed <- c(pred_file, metric_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 参数
# ============================================================

n_calibration_bins <- 5
eps <- 1e-6

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
  p <- clip_prob(p)
  log(p / (1 - p))
}

safe_auc <- function(y_true, pred_prob) {
  idx <- !is.na(y_true) & is.finite(pred_prob)
  y_true <- y_true[idx]
  pred_prob <- pred_prob[idx]
  
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
  as.numeric(pROC::auc(roc_obj))
}

choose_local_youden_threshold <- function(y_true, pred_prob) {
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

calc_calibration_metrics <- function(y_true, pred_prob) {
  
  pred_prob <- clip_prob(pred_prob, eps)
  y_true <- as.numeric(y_true)
  
  n <- length(y_true)
  observed_rate <- mean(y_true == 1, na.rm = TRUE)
  mean_predicted <- mean(pred_prob, na.rm = TRUE)
  brier <- mean((pred_prob - y_true)^2, na.rm = TRUE)
  
  calibration_in_the_large <- mean_predicted - observed_rate
  
  # Logistic calibration model:
  # outcome ~ logit(predicted probability)
  # intercept ideally 0, slope ideally 1
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
      co <- summary(fit)$coefficients
      cal_intercept <- as.numeric(co["(Intercept)", "Estimate"])
      cal_slope <- as.numeric(co["lp", "Estimate"])
      cal_intercept_se <- as.numeric(co["(Intercept)", "Std. Error"])
      cal_slope_se <- as.numeric(co["lp", "Std. Error"])
    }
  }
  
  data.frame(
    n = n,
    observed_rate = observed_rate,
    mean_predicted = mean_predicted,
    calibration_in_the_large = calibration_in_the_large,
    calibration_intercept = cal_intercept,
    calibration_slope = cal_slope,
    calibration_intercept_se = cal_intercept_se,
    calibration_slope_se = cal_slope_se,
    Brier = brier,
    AUROC = safe_auc(y_true, pred_prob),
    stringsAsFactors = FALSE
  )
}

make_calibration_bins <- function(df, n_bins = 5) {
  
  df <- df[is.finite(df$pred_prob), , drop = FALSE]
  df$pred_prob <- clip_prob(df$pred_prob, eps)
  
  if (nrow(df) < n_bins) {
    n_bins <- max(2, nrow(df))
  }
  
  # 用 rank 避免 cut 在重复概率下失败
  df$rank_pred <- rank(df$pred_prob, ties.method = "first")
  df$bin <- cut(
    df$rank_pred,
    breaks = unique(quantile(df$rank_pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)),
    include.lowest = TRUE,
    labels = FALSE
  )
  
  bin_df <- aggregate(
    cbind(pred_prob, y_true) ~ validation_dataset + bin,
    data = df,
    FUN = mean
  )
  
  names(bin_df)[names(bin_df) == "pred_prob"] <- "mean_predicted"
  names(bin_df)[names(bin_df) == "y_true"] <- "observed_rate"
  
  n_df <- aggregate(
    sample_id ~ validation_dataset + bin,
    data = df,
    FUN = length
  )
  names(n_df)[names(n_df) == "sample_id"] <- "n_bin"
  
  out <- merge(bin_df, n_df, by = c("validation_dataset", "bin"), all.x = TRUE)
  out
}

save_calibration_plot <- function(cal_bin_df) {
  
  p <- ggplot2::ggplot(
    cal_bin_df,
    ggplot2::aes(x = mean_predicted, y = observed_rate)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = n_bin), alpha = 0.8) +
    ggplot2::geom_line(alpha = 0.75) +
    ggplot2::facet_wrap(~ validation_dataset) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(
      title = "LODO calibration plots by validation dataset",
      x = "Mean predicted probability",
      y = "Observed event rate",
      size = "Bin n"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11A_LODO_calibration_by_dataset.png"),
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11A_LODO_calibration_by_dataset.pdf"),
    plot = p,
    width = 10,
    height = 7
  )
  
  invisible(p)
}

save_observed_vs_predicted_plot <- function(cal_metrics) {
  
  plot_df <- cal_metrics
  plot_df$validation_dataset <- factor(plot_df$validation_dataset, levels = plot_df$validation_dataset[order(plot_df$observed_rate)])
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = mean_predicted, y = observed_rate, label = validation_dataset)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(size = 3, alpha = 0.85) +
    ggplot2::geom_text(vjust = -0.7, size = 3) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Observed event rate vs mean predicted probability",
      x = "Mean predicted probability",
      y = "Observed sepsis proportion"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11B_LODO_observed_vs_mean_predicted.png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11B_LODO_observed_vs_mean_predicted.pdf"),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

save_brier_plot <- function(cal_metrics) {
  
  plot_df <- cal_metrics
  plot_df$validation_dataset <- factor(plot_df$validation_dataset, levels = plot_df$validation_dataset[order(plot_df$Brier)])
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = validation_dataset, y = Brier)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Brier score by LODO validation dataset",
      x = "Validation dataset",
      y = "Brier score"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11C_LODO_Brier_by_dataset.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11C_LODO_Brier_by_dataset.pdf"),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

save_threshold_drift_plot <- function(threshold_df) {
  
  plot_df <- threshold_df
  plot_df$validation_dataset <- factor(plot_df$validation_dataset, levels = plot_df$validation_dataset[order(plot_df$threshold_shift)])
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = validation_dataset)
  ) +
    ggplot2::geom_point(ggplot2::aes(y = fixed_training_threshold), size = 3, alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(y = local_youden_threshold), size = 3, alpha = 0.85, shape = 17) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = validation_dataset,
        y = fixed_training_threshold,
        yend = local_youden_threshold
      ),
      linewidth = 0.6,
      alpha = 0.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Threshold drift across LODO validation datasets",
      x = "Validation dataset",
      y = "Probability threshold"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11D_LODO_threshold_drift.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11D_LODO_threshold_drift.pdf"),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

save_fixed_threshold_operating_plot <- function(threshold_df) {
  
  plot_df <- threshold_df[, c("validation_dataset", "fixed_sensitivity", "fixed_specificity"), drop = FALSE]
  long_df <- data.table::melt(
    data.table::as.data.table(plot_df),
    id.vars = "validation_dataset",
    variable.name = "metric",
    value.name = "value"
  )
  
  long_df$validation_dataset <- factor(long_df$validation_dataset, levels = unique(threshold_df$validation_dataset))
  
  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = validation_dataset, y = value, fill = metric)
  ) +
    ggplot2::geom_col(position = "dodge", alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Fixed training threshold operating characteristics",
      x = "Validation dataset",
      y = "Metric value",
      fill = "Metric"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11E_LODO_fixed_threshold_sensitivity_specificity.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F11E_LODO_fixed_threshold_sensitivity_specificity.pdf"),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

# ============================================================
# 读取输入
# ============================================================

message("Reading LODO prediction files...")

pred <- data.table::fread(pred_file, data.table = FALSE)
metrics <- data.table::fread(metric_file, data.table = FALSE)

required_pred_cols <- c(
  "sample_id",
  "validation_dataset",
  "clinical_group_main",
  "y_true",
  "pred_prob",
  "threshold"
)

if (!all(required_pred_cols %in% colnames(pred))) {
  stop("T08_LODO_predictions.csv 缺少必要列。")
}

pred$y_true <- as.numeric(pred$y_true)
pred$pred_prob <- clip_prob(pred$pred_prob, eps)
pred$threshold <- as.numeric(pred$threshold)

datasets <- sort(unique(pred$validation_dataset))

# ============================================================
# 逐验证集计算 calibration 和 threshold drift
# ============================================================

message("Calculating calibration and threshold metrics...")

cal_list <- list()
threshold_list <- list()
cal_bin_list <- list()

for (ds in datasets) {
  
  df <- pred[pred$validation_dataset == ds, , drop = FALSE]
  
  cal <- calc_calibration_metrics(df$y_true, df$pred_prob)
  cal$validation_dataset <- ds
  cal_list[[ds]] <- cal
  
  fixed_threshold <- unique(df$threshold)
  fixed_threshold <- fixed_threshold[is.finite(fixed_threshold)]
  fixed_threshold <- fixed_threshold[1]
  
  local_threshold <- choose_local_youden_threshold(df$y_true, df$pred_prob)
  
  fixed_metrics <- calc_classification_metrics(df$y_true, df$pred_prob, fixed_threshold)
  local_metrics <- calc_classification_metrics(df$y_true, df$pred_prob, local_threshold)
  
  threshold_list[[ds]] <- data.frame(
    validation_dataset = ds,
    fixed_training_threshold = fixed_threshold,
    local_youden_threshold = local_threshold,
    threshold_shift = local_threshold - fixed_threshold,
    abs_threshold_shift = abs(local_threshold - fixed_threshold),
    fixed_sensitivity = fixed_metrics$sensitivity,
    fixed_specificity = fixed_metrics$specificity,
    fixed_accuracy = fixed_metrics$accuracy,
    fixed_PPV = fixed_metrics$PPV,
    fixed_NPV = fixed_metrics$NPV,
    local_sensitivity = local_metrics$sensitivity,
    local_specificity = local_metrics$specificity,
    local_accuracy = local_metrics$accuracy,
    local_PPV = local_metrics$PPV,
    local_NPV = local_metrics$NPV,
    stringsAsFactors = FALSE
  )
  
  cal_bin <- make_calibration_bins(df, n_bins = n_calibration_bins)
  cal_bin_list[[ds]] <- cal_bin
}

cal_metrics <- do.call(rbind, cal_list)
cal_metrics <- cal_metrics[, c("validation_dataset", setdiff(colnames(cal_metrics), "validation_dataset")), drop = FALSE]

threshold_metrics <- do.call(rbind, threshold_list)
cal_bins <- do.call(rbind, cal_bin_list)

# 合并 AUROC/AUPRC 等已有指标
validation_metrics <- metrics[metrics$set == "validation", , drop = FALSE]

combined_metrics <- merge(
  validation_metrics,
  cal_metrics,
  by = "validation_dataset",
  all.x = TRUE,
  suffixes = c("_T08", "_cal")
)

# ============================================================
# 输出表
# ============================================================

data.table::fwrite(
  cal_metrics,
  file.path(val_dir, "T09_LODO_calibration_metrics.csv")
)

data.table::fwrite(
  threshold_metrics,
  file.path(val_dir, "T09_LODO_threshold_drift_metrics.csv")
)

data.table::fwrite(
  cal_bins,
  file.path(val_dir, "T09_LODO_calibration_bins.csv")
)

data.table::fwrite(
  combined_metrics,
  file.path(val_dir, "T09_LODO_combined_validation_calibration_metrics.csv")
)

# Excel 汇总
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "calibration_metrics")
openxlsx::writeData(wb, "calibration_metrics", cal_metrics)

openxlsx::addWorksheet(wb, "threshold_drift")
openxlsx::writeData(wb, "threshold_drift", threshold_metrics)

openxlsx::addWorksheet(wb, "calibration_bins")
openxlsx::writeData(wb, "calibration_bins", cal_bins)

openxlsx::addWorksheet(wb, "combined_metrics")
openxlsx::writeData(wb, "combined_metrics", combined_metrics)

openxlsx::saveWorkbook(
  wb,
  file.path(val_dir, "T09_LODO_calibration_threshold_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 绘图
# ============================================================

message("Saving calibration and threshold figures...")

save_calibration_plot(cal_bins)
save_observed_vs_predicted_plot(cal_metrics)
save_brier_plot(cal_metrics)
save_threshold_drift_plot(threshold_metrics)
save_fixed_threshold_operating_plot(threshold_metrics)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_09_LODO_calibration_threshold_analysis.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 09 LODO calibration and threshold analysis 完成 ============")
message("验证结果目录：", val_dir)
message("图输出目录：", fig_dir)

message("\nCalibration metrics:")
print(cal_metrics)

message("\nThreshold drift metrics:")
print(threshold_metrics)

message("\n关键输出：")
message("1) ", file.path(val_dir, "T09_LODO_calibration_metrics.csv"))
message("2) ", file.path(val_dir, "T09_LODO_threshold_drift_metrics.csv"))
message("3) ", file.path(val_dir, "T09_LODO_calibration_bins.csv"))
message("4) ", file.path(val_dir, "T09_LODO_combined_validation_calibration_metrics.csv"))
message("5) ", file.path(val_dir, "T09_LODO_calibration_threshold_summary.xlsx"))
message("6) ", fig_dir)