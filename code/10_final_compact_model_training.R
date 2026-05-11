# ============================================================
# 10_final_compact_model_training.R
# Final compact diagnostic model training
#
# 定位：
# 1. 使用全部 6 个成人 bulk 队列训练 final compact model
# 2. 候选基因限于 07b refined candidate genes
# 3. 使用 glmnet LASSO / elastic net logistic regression 选择模型
# 4. 输出最终模型基因、系数、训练集标准化参数、阈值
# 5. 报告 apparent performance
#
# 重要说明：
# 这一步不是独立外部验证。
# 独立性和 transportability 由 08/09 的 LODO 分析承担。
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(glmnet)
  library(pROC)
  library(PRROC)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

de_dir <- file.path(project_dir, "04_results", "differential_expression")
cand_dir <- file.path(project_dir, "04_results", "candidate_selection")
out_dir <- file.path(project_dir, "04_results", "final_model")
fig_dir <- file.path(project_dir, "05_figures", "final_model")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

expr_file <- file.path(de_dir, "merged_gene_expression_common_genes_unadjusted.csv")
pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")
candidate_file <- file.path(cand_dir, "T07_refined_modeling_candidate_genes.csv")

needed <- c(expr_file, pheno_file, candidate_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 参数
# ============================================================

set.seed(20260507)

alpha_grid <- c(1.00, 0.75, 0.50)
nfolds_cv <- 10

lambda_rule <- "lambda.1se"
allow_lambda_fallback <- TRUE

min_selected_genes <- 3
max_selected_genes <- 10

refit_final_glm <- TRUE

# ============================================================
# 工具函数
# ============================================================

read_expr_matrix <- function(file_path) {
  dt <- data.table::fread(file_path, data.table = FALSE, check.names = FALSE)
  
  gene_col <- colnames(dt)[1]
  gene_id <- as.character(dt[[gene_col]])
  dt[[gene_col]] <- NULL
  
  mat <- as.matrix(dt)
  mode(mat) <- "numeric"
  rownames(mat) <- gene_id
  
  mat
}

scale_training <- function(x) {
  mu <- colMeans(x, na.rm = TRUE)
  sdv <- apply(x, 2, sd, na.rm = TRUE)
  sdv[is.na(sdv) | sdv == 0] <- 1
  
  x_scaled <- sweep(sweep(x, 2, mu, "-"), 2, sdv, "/")
  
  list(
    x_scaled = x_scaled,
    center = mu,
    scale = sdv
  )
}

choose_threshold_youden <- function(y_true, pred_prob) {
  roc_obj <- pROC::roc(
    response = y_true,
    predictor = pred_prob,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  coords <- pROC::coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity"),
    transpose = FALSE
  )
  
  as.numeric(coords$threshold[1])
}

safe_auc_ci <- function(y_true, pred_prob) {
  idx <- !is.na(y_true) & is.finite(pred_prob)
  y_true <- y_true[idx]
  pred_prob <- pred_prob[idx]
  
  if (length(unique(y_true)) != 2) {
    return(c(auc = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  
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
  
  if (inherits(roc_obj, "try-error")) {
    return(c(auc = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  
  ci <- try(pROC::ci.auc(roc_obj), silent = TRUE)
  
  if (inherits(ci, "try-error")) {
    return(c(
      auc = as.numeric(pROC::auc(roc_obj)),
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }
  
  c(
    auc = as.numeric(pROC::auc(roc_obj)),
    ci_low = as.numeric(ci[1]),
    ci_high = as.numeric(ci[3])
  )
}

safe_auprc <- function(y_true, pred_prob) {
  idx <- !is.na(y_true) & is.finite(pred_prob)
  y_true <- y_true[idx]
  pred_prob <- pred_prob[idx]
  
  if (length(unique(y_true)) != 2) return(NA_real_)
  
  scores_pos <- pred_prob[y_true == 1]
  scores_neg <- pred_prob[y_true == 0]
  
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

calc_metrics <- function(y_true, pred_prob, threshold) {
  pred_class <- ifelse(pred_prob >= threshold, 1, 0)
  
  tp <- sum(pred_class == 1 & y_true == 1, na.rm = TRUE)
  tn <- sum(pred_class == 0 & y_true == 0, na.rm = TRUE)
  fp <- sum(pred_class == 1 & y_true == 0, na.rm = TRUE)
  fn <- sum(pred_class == 0 & y_true == 1, na.rm = TRUE)
  
  auc_ci <- safe_auc_ci(y_true, pred_prob)
  
  data.frame(
    n = length(y_true),
    n_case = sum(y_true == 1, na.rm = TRUE),
    n_control = sum(y_true == 0, na.rm = TRUE),
    prevalence = mean(y_true == 1, na.rm = TRUE),
    AUROC = auc_ci[["auc"]],
    AUROC_low = auc_ci[["ci_low"]],
    AUROC_high = auc_ci[["ci_high"]],
    AUPRC = safe_auprc(y_true, pred_prob),
    Brier = mean((pred_prob - y_true)^2, na.rm = TRUE),
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

fit_one_alpha <- function(x, y, alpha_value) {
  foldid <- rep(seq_len(nfolds_cv), length.out = length(y))
  foldid <- sample(foldid)
  
  cvfit <- glmnet::cv.glmnet(
    x = x,
    y = y,
    family = "binomial",
    alpha = alpha_value,
    nfolds = nfolds_cv,
    foldid = foldid,
    type.measure = "auc",
    standardize = FALSE,
    keep = TRUE
  )
  
  cv_auc_max <- suppressWarnings(max(cvfit$cvm, na.rm = TRUE))
  if (!is.finite(cv_auc_max)) cv_auc_max <- NA_real_
  
  list(
    alpha = alpha_value,
    alpha_name = as.character(alpha_value),
    cvfit = cvfit,
    cv_auc_max = cv_auc_max
  )
}

extract_coef_table <- function(cvfit, lambda_name) {
  co <- as.matrix(stats::coef(cvfit, s = lambda_name))
  
  out <- data.frame(
    term = rownames(co),
    coefficient = as.numeric(co[, 1]),
    stringsAsFactors = FALSE
  )
  
  out[out$coefficient != 0, , drop = FALSE]
}

extract_selected_genes <- function(cvfit, lambda_name) {
  co <- extract_coef_table(cvfit, lambda_name)
  setdiff(co$term, "(Intercept)")
}

save_cv_curve <- function(best_cvfit, selected_lambda_value) {
  
  cv_df <- data.frame(
    lambda = best_cvfit$lambda,
    log_lambda = log(best_cvfit$lambda),
    cv_auc = best_cvfit$cvm,
    cvsd = best_cvfit$cvsd,
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot(
    cv_df,
    ggplot2::aes(x = log_lambda, y = cv_auc)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.4, alpha = 0.75) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = cv_auc - cvsd, ymax = cv_auc + cvsd),
      width = 0.05,
      alpha = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = log(selected_lambda_value),
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Final model cross-validation curve",
      x = "log(lambda)",
      y = "Cross-validated AUC"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12A_final_model_cv_curve.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12A_final_model_cv_curve.pdf"),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

save_roc_plot <- function(y_true, pred_prob, auc_value) {
  
  roc_obj <- pROC::roc(
    response = y_true,
    predictor = pred_prob,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  roc_df <- data.frame(
    specificity = roc_obj$specificities,
    sensitivity = roc_obj$sensitivities,
    FPR = 1 - roc_obj$specificities,
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot(
    roc_df,
    ggplot2::aes(x = FPR, y = sensitivity)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = paste0("Final model apparent ROC, AUROC = ", round(auc_value, 3)),
      x = "1 - specificity",
      y = "Sensitivity"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12B_final_model_ROC_apparent.png"),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12B_final_model_ROC_apparent.pdf"),
    plot = p,
    width = 6,
    height = 6
  )
  
  invisible(p)
}

save_prediction_density <- function(pred_df) {
  
  p <- ggplot2::ggplot(
    pred_df,
    ggplot2::aes(x = pred_prob, color = clinical_group_main, fill = clinical_group_main)
  ) +
    ggplot2::geom_density(alpha = 0.25) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Final model apparent predicted probability distribution",
      x = "Predicted probability of sepsis",
      y = "Density",
      color = "Group",
      fill = "Group"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12C_final_model_prediction_density.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12C_final_model_prediction_density.pdf"),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

save_calibration_plot <- function(pred_df, n_bins = 10) {
  
  df <- pred_df
  df$rank_pred <- rank(df$pred_prob, ties.method = "first")
  df$bin <- cut(
    df$rank_pred,
    breaks = unique(quantile(df$rank_pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)),
    include.lowest = TRUE,
    labels = FALSE
  )
  
  cal <- aggregate(
    cbind(pred_prob, y_true) ~ bin,
    data = df,
    FUN = mean
  )
  
  n_df <- aggregate(sample_id ~ bin, data = df, FUN = length)
  names(n_df)[names(n_df) == "sample_id"] <- "n_bin"
  
  cal <- merge(cal, n_df, by = "bin")
  names(cal)[names(cal) == "pred_prob"] <- "mean_predicted"
  names(cal)[names(cal) == "y_true"] <- "observed_rate"
  
  p <- ggplot2::ggplot(
    cal,
    ggplot2::aes(x = mean_predicted, y = observed_rate)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(size = n_bin), alpha = 0.85) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Final model apparent calibration",
      x = "Mean predicted probability",
      y = "Observed sepsis proportion",
      size = "Bin n"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12D_final_model_calibration_apparent.png"),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F12D_final_model_calibration_apparent.pdf"),
    plot = p,
    width = 6,
    height = 6
  )
  
  cal
}

# ============================================================
# 读取数据
# ============================================================

message("Reading inputs...")

expr_mat <- read_expr_matrix(expr_file)
pheno <- data.table::fread(pheno_file, data.table = FALSE)
candidate_table <- data.table::fread(candidate_file, data.table = FALSE)

required_pheno_cols <- c("sample_id", "dataset", "clinical_group_main")
if (!all(required_pheno_cols %in% colnames(pheno))) {
  stop("phenotype 缺少 sample_id / dataset / clinical_group_main。")
}

if (!"gene_symbol" %in% colnames(candidate_table)) {
  stop("候选基因表缺少 gene_symbol 列。")
}

candidate_genes <- unique(as.character(candidate_table$gene_symbol))
candidate_genes <- candidate_genes[candidate_genes %in% rownames(expr_mat)]

if (length(candidate_genes) < min_selected_genes) {
  stop("候选基因不足。")
}

common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
expr_mat <- expr_mat[, common_samples, drop = FALSE]
pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]

if (!all(colnames(expr_mat) == pheno$sample_id)) {
  stop("表达矩阵和 phenotype 没有对齐。")
}

x <- t(expr_mat[candidate_genes, , drop = FALSE])
y <- ifelse(pheno$clinical_group_main == "Sepsis", 1, 0)

scaled <- scale_training(x)
x_scaled <- scaled$x_scaled

message("Training samples: ", nrow(x_scaled))
message("Candidate genes: ", ncol(x_scaled))
message("Cases: ", sum(y == 1), "; Controls: ", sum(y == 0))

# ============================================================
# glmnet CV across alpha values
# ============================================================

message("Training glmnet models across alpha grid...")

fit_list <- list()

for (a in alpha_grid) {
  message("  alpha = ", a)
  fit_list[[as.character(a)]] <- fit_one_alpha(x_scaled, y, alpha_value = a)
}

alpha_auc <- data.frame(
  alpha_name = names(fit_list),
  alpha = as.numeric(names(fit_list)),
  cv_auc_max = sapply(fit_list, function(z) z$cv_auc_max),
  stringsAsFactors = FALSE
)

valid_alpha_idx <- which(is.finite(alpha_auc$cv_auc_max))

if (length(valid_alpha_idx) == 0) {
  stop("所有 alpha 的 cv_auc_max 均为 NA 或非有限值，无法选择 best alpha。")
}

best_idx <- valid_alpha_idx[which.max(alpha_auc$cv_auc_max[valid_alpha_idx])]
best_alpha_name <- alpha_auc$alpha_name[best_idx]

best_fit <- fit_list[[best_alpha_name]]
best_cvfit <- best_fit$cvfit
best_alpha <- best_fit$alpha

selected_lambda <- lambda_rule
selected_genes <- extract_selected_genes(best_cvfit, selected_lambda)

if (allow_lambda_fallback && length(selected_genes) < min_selected_genes) {
  message("lambda.1se selected fewer than ", min_selected_genes, " genes. Falling back to lambda.min.")
  selected_lambda <- "lambda.min"
  selected_genes <- extract_selected_genes(best_cvfit, selected_lambda)
}

coef_table_glmnet <- extract_coef_table(best_cvfit, selected_lambda)
coef_no_intercept <- coef_table_glmnet[coef_table_glmnet$term != "(Intercept)", , drop = FALSE]
coef_no_intercept <- coef_no_intercept[order(abs(coef_no_intercept$coefficient), decreasing = TRUE), , drop = FALSE]

if (nrow(coef_no_intercept) > max_selected_genes) {
  selected_genes <- head(coef_no_intercept$term, max_selected_genes)
}

if (length(selected_genes) < min_selected_genes) {
  selected_genes <- head(candidate_genes, min_selected_genes)
  message("Selected genes still too few. Using top candidate genes: ", paste(selected_genes, collapse = ", "))
}

message("Best alpha: ", best_alpha)
message("Selected lambda rule: ", selected_lambda)
message("Selected genes: ", paste(selected_genes, collapse = ", "))

selected_lambda_value <- ifelse(
  selected_lambda == "lambda.1se",
  best_cvfit$lambda.1se,
  best_cvfit$lambda.min
)

# ============================================================
# Final refit using selected genes
# ============================================================

df_train <- data.frame(
  y = y,
  x_scaled[, selected_genes, drop = FALSE],
  check.names = FALSE
)

formula_str <- paste0("y ~ ", paste(sprintf("`%s`", selected_genes), collapse = " + "))

if (refit_final_glm) {
  final_fit <- stats::glm(
    stats::as.formula(formula_str),
    data = df_train,
    family = stats::binomial()
  )
  
  pred_prob <- as.numeric(stats::predict(final_fit, newdata = df_train, type = "response"))
  
  coef_final <- data.frame(
    term = names(stats::coef(final_fit)),
    coefficient = as.numeric(stats::coef(final_fit)),
    stringsAsFactors = FALSE
  )
  
} else {
  final_fit <- best_cvfit
  pred_prob <- as.numeric(stats::predict(best_cvfit, newx = x_scaled, s = selected_lambda, type = "response"))
  coef_final <- extract_coef_table(best_cvfit, selected_lambda)
}

threshold <- choose_threshold_youden(y, pred_prob)
metrics <- calc_metrics(y, pred_prob, threshold)

pred_df <- data.frame(
  sample_id = pheno$sample_id,
  dataset = pheno$dataset,
  clinical_group_main = pheno$clinical_group_main,
  y_true = y,
  pred_prob = pred_prob,
  threshold = threshold,
  pred_class = ifelse(pred_prob >= threshold, 1, 0),
  stringsAsFactors = FALSE
)

scaling_df <- data.frame(
  gene_symbol = selected_genes,
  center = scaled$center[selected_genes],
  scale = scaled$scale[selected_genes],
  stringsAsFactors = FALSE
)

selected_gene_table <- data.frame(
  selection_rank = seq_along(selected_genes),
  gene_symbol = selected_genes,
  stringsAsFactors = FALSE
)

# ============================================================
# 保存模型对象
# ============================================================

final_model_object <- list(
  model_type = "Final compact logistic model trained on all six adult bulk cohorts",
  note = "This is a final compact model for future independent validation; LODO results should be used as dataset-level transportability evidence.",
  selected_genes = selected_genes,
  alpha_grid = alpha_grid,
  alpha_auc = alpha_auc,
  best_alpha = best_alpha,
  lambda_rule_used = selected_lambda,
  selected_lambda_value = selected_lambda_value,
  glmnet_cvfit = best_cvfit,
  final_fit = final_fit,
  refit_final_glm = refit_final_glm,
  scaling_center = scaled$center[selected_genes],
  scaling_scale = scaled$scale[selected_genes],
  threshold_youden_apparent = threshold,
  apparent_metrics = metrics,
  coefficient_table = coef_final
)

saveRDS(
  final_model_object,
  file.path(out_dir, "M10_final_compact_model.rds")
)

# ============================================================
# 输出表格
# ============================================================

data.table::fwrite(
  coef_final,
  file.path(out_dir, "T10_final_model_coefficients.csv")
)

data.table::fwrite(
  scaling_df,
  file.path(out_dir, "T10_final_model_scaling_parameters.csv")
)

data.table::fwrite(
  pred_df,
  file.path(out_dir, "T10_final_model_apparent_predictions.csv")
)

data.table::fwrite(
  metrics,
  file.path(out_dir, "T10_final_model_apparent_metrics.csv")
)

data.table::fwrite(
  alpha_auc,
  file.path(out_dir, "T10_final_model_alpha_cv_auc.csv")
)

data.table::fwrite(
  selected_gene_table,
  file.path(out_dir, "T10_final_model_selected_genes.csv")
)

cal_bins <- save_calibration_plot(pred_df, n_bins = 10)

data.table::fwrite(
  cal_bins,
  file.path(out_dir, "T10_final_model_apparent_calibration_bins.csv")
)

# Excel 汇总
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "selected_genes")
openxlsx::writeData(wb, "selected_genes", selected_gene_table)

openxlsx::addWorksheet(wb, "coefficients")
openxlsx::writeData(wb, "coefficients", coef_final)

openxlsx::addWorksheet(wb, "scaling_parameters")
openxlsx::writeData(wb, "scaling_parameters", scaling_df)

openxlsx::addWorksheet(wb, "apparent_metrics")
openxlsx::writeData(wb, "apparent_metrics", metrics)

openxlsx::addWorksheet(wb, "alpha_cv_auc")
openxlsx::writeData(wb, "alpha_cv_auc", alpha_auc)

openxlsx::addWorksheet(wb, "apparent_predictions")
openxlsx::writeData(wb, "apparent_predictions", pred_df)

openxlsx::addWorksheet(wb, "calibration_bins")
openxlsx::writeData(wb, "calibration_bins", cal_bins)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T10_final_model_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 图
# ============================================================

save_cv_curve(best_cvfit, selected_lambda_value)
save_roc_plot(y, pred_prob, metrics$AUROC)
save_prediction_density(pred_df)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_10_final_compact_model_training.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 10 Final compact model training 完成 ============")
message("输出目录：", out_dir)
message("图输出目录：", fig_dir)

message("\nSelected genes:")
print(selected_gene_table)

message("\nCoefficient table:")
print(coef_final)

message("\nAlpha CV AUC:")
print(alpha_auc)

message("\nApparent performance:")
print(metrics)

message("\n关键输出：")
message("1) ", file.path(out_dir, "M10_final_compact_model.rds"))
message("2) ", file.path(out_dir, "T10_final_model_coefficients.csv"))
message("3) ", file.path(out_dir, "T10_final_model_scaling_parameters.csv"))
message("4) ", file.path(out_dir, "T10_final_model_apparent_predictions.csv"))
message("5) ", file.path(out_dir, "T10_final_model_apparent_metrics.csv"))
message("6) ", file.path(out_dir, "T10_final_model_summary.xlsx"))
message("7) ", fig_dir)