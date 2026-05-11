# ============================================================
# 08_LODO_diagnostic_modeling.R
# Leave-one-dataset-out diagnostic modeling
#
# 目的：
# 1. 以 dataset 为单位做 leave-one-dataset-out 外部验证
# 2. 每一轮留出 1 个 dataset 作验证集
# 3. 剩余 dataset 内重新做训练集候选基因筛选
# 4. 在训练集内用 LASSO / Elastic net logistic regression 训练模型
# 5. 验证集不参与特征筛选、标准化参数估计、lambda 选择和阈值选择
# 6. 输出每个留出队列的 AUROC、AUPRC、Brier、Sensitivity、Specificity
#
# 输入：
# 04_results/differential_expression/
#   merged_gene_expression_common_genes_unadjusted.csv
#   merged_pheno_for_DEG.csv
#
# 04_results/candidate_selection/
#   T07_refined_modeling_candidate_genes.csv
#
# 输出：
# 04_results/models/
#   M08_LODO_model_objects.rds
#
# 04_results/model_validation/
#   T08_LODO_validation_metrics.csv
#   T08_LODO_selected_genes_by_fold.csv
#   T08_LODO_predictions.csv
#   T08_LODO_modeling_summary.xlsx
#
# 05_figures/model_validation/
#   F10A_LODO_ROC_by_dataset.png/pdf
#   F10B_LODO_PR_by_dataset.png/pdf
#   F10C_LODO_AUROC_barplot.png/pdf
#   F10D_LODO_prediction_density.png/pdf
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
model_dir <- file.path(project_dir, "04_results", "models")
val_dir <- file.path(project_dir, "04_results", "model_validation")
fig_dir <- file.path(project_dir, "05_figures", "model_validation")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(val_dir, recursive = TRUE, showWarnings = FALSE)
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

nfolds_inner_cv <- 5

lambda_rule <- "lambda.1se"
# 可选："lambda.min" 或 "lambda.1se"
# lambda.1se 更保守，基因数通常更少，更适合转化叙事

min_selected_genes <- 3
max_selected_genes <- 10

# 如果 lambda.1se 太稀疏，则回退到 lambda.min
allow_lambda_fallback <- TRUE

# 固定阈值选择只在训练集内做
threshold_metric <- "youden"

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

make_xy <- function(expr_mat, pheno, genes) {
  genes <- genes[genes %in% rownames(expr_mat)]
  
  if (length(genes) == 0) {
    stop("没有候选基因存在于表达矩阵。")
  }
  
  common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
  expr_sub <- expr_mat[genes, common_samples, drop = FALSE]
  pheno_sub <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]
  
  if (!all(colnames(expr_sub) == pheno_sub$sample_id)) {
    stop("表达矩阵和 phenotype 未对齐。")
  }
  
  x <- t(expr_sub)
  y <- ifelse(pheno_sub$clinical_group_main == "Sepsis", 1, 0)
  
  list(
    x = x,
    y = y,
    pheno = pheno_sub,
    genes = genes
  )
}

scale_train_apply <- function(x_train, x_test) {
  mu <- colMeans(x_train, na.rm = TRUE)
  sdv <- apply(x_train, 2, sd, na.rm = TRUE)
  sdv[is.na(sdv) | sdv == 0] <- 1
  
  x_train_scaled <- sweep(sweep(x_train, 2, mu, "-"), 2, sdv, "/")
  x_test_scaled <- sweep(sweep(x_test, 2, mu, "-"), 2, sdv, "/")
  
  list(
    x_train = x_train_scaled,
    x_test = x_test_scaled,
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

safe_auc_ci <- function(y_true, pred_prob) {
  idx <- !is.na(y_true) & is.finite(pred_prob)
  y_true <- y_true[idx]
  pred_prob <- pred_prob[idx]
  
  if (length(unique(y_true)) != 2) {
    return(c(NA_real_, NA_real_, NA_real_))
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
    return(c(NA_real_, NA_real_, NA_real_))
  }
  
  ci <- try(pROC::ci.auc(roc_obj), silent = TRUE)
  if (inherits(ci, "try-error")) {
    return(c(as.numeric(pROC::auc(roc_obj)), NA_real_, NA_real_))
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
  
  sensitivity <- tp / max(tp + fn, 1)
  specificity <- tn / max(tn + fp, 1)
  ppv <- tp / max(tp + fp, 1)
  npv <- tn / max(tn + fn, 1)
  accuracy <- (tp + tn) / max(tp + tn + fp + fn, 1)
  brier <- mean((pred_prob - y_true)^2, na.rm = TRUE)
  prevalence <- mean(y_true == 1, na.rm = TRUE)
  
  data.frame(
    n = length(y_true),
    n_case = sum(y_true == 1, na.rm = TRUE),
    n_control = sum(y_true == 0, na.rm = TRUE),
    prevalence = prevalence,
    AUROC = auc_ci[["auc"]],
    AUROC_low = auc_ci[["ci_low"]],
    AUROC_high = auc_ci[["ci_high"]],
    AUPRC = safe_auprc(y_true, pred_prob),
    Brier = brier,
    threshold = threshold,
    sensitivity = sensitivity,
    specificity = specificity,
    PPV = ppv,
    NPV = npv,
    accuracy = accuracy,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn,
    stringsAsFactors = FALSE
  )
}

fit_glmnet_one_alpha <- function(x_train, y_train, alpha_value) {
  foldid <- rep(seq_len(nfolds_inner_cv), length.out = length(y_train))
  foldid <- sample(foldid)
  
  cvfit <- glmnet::cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = alpha_value,
    nfolds = nfolds_inner_cv,
    foldid = foldid,
    type.measure = "auc",
    standardize = FALSE
  )
  
  cv_auc_max <- max(cvfit$cvm, na.rm = TRUE)
  
  list(
    cvfit = cvfit,
    alpha = alpha_value,
    cv_auc_max = cv_auc_max
  )
}

extract_selected_genes <- function(cvfit, lambda_name) {
  co <- as.matrix(stats::coef(cvfit, s = lambda_name))
  nz <- rownames(co)[as.numeric(co[, 1]) != 0]
  setdiff(nz, "(Intercept)")
}

get_coef_table <- function(cvfit, lambda_name) {
  co <- as.matrix(stats::coef(cvfit, s = lambda_name))
  out <- data.frame(
    term = rownames(co),
    coefficient = as.numeric(co[, 1]),
    stringsAsFactors = FALSE
  )
  out[out$coefficient != 0, , drop = FALSE]
}

save_roc_plot <- function(pred_df) {
  
  roc_plot_list <- list()
  
  datasets <- unique(pred_df$validation_dataset)
  
  for (ds in datasets) {
    df <- pred_df[pred_df$validation_dataset == ds, , drop = FALSE]
    if (length(unique(df$y_true)) != 2) next
    
    roc_obj <- pROC::roc(
      response = df$y_true,
      predictor = df$pred_prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
    
    coords_df <- data.frame(
      specificity = roc_obj$specificities,
      sensitivity = roc_obj$sensitivities,
      validation_dataset = ds,
      stringsAsFactors = FALSE
    )
    
    coords_df$FPR <- 1 - coords_df$specificity
    roc_plot_list[[ds]] <- coords_df
  }
  
  roc_df <- do.call(rbind, roc_plot_list)
  
  p <- ggplot2::ggplot(
    roc_df,
    ggplot2::aes(x = FPR, y = sensitivity, color = validation_dataset)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Leave-one-dataset-out ROC curves",
      x = "1 - specificity",
      y = "Sensitivity",
      color = "Validation dataset"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10A_LODO_ROC_by_dataset.png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10A_LODO_ROC_by_dataset.pdf"),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

save_pr_plot <- function(pred_df) {
  
  pr_plot_list <- list()
  
  datasets <- unique(pred_df$validation_dataset)
  
  for (ds in datasets) {
    df <- pred_df[pred_df$validation_dataset == ds, , drop = FALSE]
    if (length(unique(df$y_true)) != 2) next
    
    ord <- order(df$pred_prob, decreasing = TRUE)
    y <- df$y_true[ord]
    
    tp <- cumsum(y == 1)
    fp <- cumsum(y == 0)
    
    precision <- tp / pmax(tp + fp, 1)
    recall <- tp / max(sum(y == 1), 1)
    
    pr_plot_list[[ds]] <- data.frame(
      recall = recall,
      precision = precision,
      validation_dataset = ds,
      stringsAsFactors = FALSE
    )
  }
  
  pr_df <- do.call(rbind, pr_plot_list)
  
  p <- ggplot2::ggplot(
    pr_df,
    ggplot2::aes(x = recall, y = precision, color = validation_dataset)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Leave-one-dataset-out precision-recall curves",
      x = "Recall",
      y = "Precision",
      color = "Validation dataset"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10B_LODO_PR_by_dataset.png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10B_LODO_PR_by_dataset.pdf"),
    plot = p,
    width = 7,
    height = 6
  )
  
  invisible(p)
}

save_auc_barplot <- function(metrics_df) {
  
  plot_df <- metrics_df[metrics_df$set == "validation", , drop = FALSE]
  plot_df$validation_dataset <- factor(plot_df$validation_dataset, levels = plot_df$validation_dataset[order(plot_df$AUROC)])
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = validation_dataset, y = AUROC)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = AUROC_low, ymax = AUROC_high),
      width = 0.2,
      linewidth = 0.4
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "External validation AUROC by leave-one-dataset-out fold",
      x = "Validation dataset",
      y = "AUROC"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10C_LODO_AUROC_barplot.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10C_LODO_AUROC_barplot.pdf"),
    plot = p,
    width = 7,
    height = 5
  )
  
  invisible(p)
}

save_prediction_density <- function(pred_df) {
  
  pred_df$group <- ifelse(pred_df$y_true == 1, "Sepsis", "Control")
  
  p <- ggplot2::ggplot(
    pred_df,
    ggplot2::aes(x = pred_prob, color = group, fill = group)
  ) +
    ggplot2::geom_density(alpha = 0.25) +
    ggplot2::facet_wrap(~ validation_dataset, scales = "free_y") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(
      title = "LODO validation predicted probability distribution",
      x = "Predicted probability of sepsis",
      y = "Density",
      color = "Group",
      fill = "Group"
    )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10D_LODO_prediction_density.png"),
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  ggplot2::ggsave(
    file.path(fig_dir, "F10D_LODO_prediction_density.pdf"),
    plot = p,
    width = 10,
    height = 7
  )
  
  invisible(p)
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

if (length(candidate_genes) < 3) {
  stop("候选基因不足 3 个，无法训练模型。")
}

common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
expr_mat <- expr_mat[, common_samples, drop = FALSE]
pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]

if (!all(colnames(expr_mat) == pheno$sample_id)) {
  stop("表达矩阵和 phenotype 没有对齐。")
}

dataset_list <- sort(unique(pheno$dataset))

message("Datasets for LODO: ", paste(dataset_list, collapse = ", "))
message("Candidate genes: ", length(candidate_genes))

# ============================================================
# LODO 建模
# ============================================================

model_objects <- list()
pred_list <- list()
metrics_list <- list()
selected_gene_list <- list()

for (heldout_ds in dataset_list) {
  
  message("\n==================================================")
  message("LODO validation dataset: ", heldout_ds)
  message("==================================================")
  
  train_idx <- pheno$dataset != heldout_ds
  test_idx <- pheno$dataset == heldout_ds
  
  ph_train <- pheno[train_idx, , drop = FALSE]
  ph_test <- pheno[test_idx, , drop = FALSE]
  
  if (!all(c("Control", "Sepsis") %in% unique(ph_train$clinical_group_main))) {
    warning("训练集缺少某一类别，跳过：", heldout_ds)
    next
  }
  
  if (!all(c("Control", "Sepsis") %in% unique(ph_test$clinical_group_main))) {
    warning("验证集缺少某一类别，跳过：", heldout_ds)
    next
  }
  
  train_obj <- make_xy(expr_mat, ph_train, candidate_genes)
  test_obj <- make_xy(expr_mat, ph_test, candidate_genes)
  
  common_genes_fold <- intersect(train_obj$genes, test_obj$genes)
  x_train <- train_obj$x[, common_genes_fold, drop = FALSE]
  x_test <- test_obj$x[, common_genes_fold, drop = FALSE]
  y_train <- train_obj$y
  y_test <- test_obj$y
  
  scaled <- scale_train_apply(x_train, x_test)
  x_train_sc <- scaled$x_train
  x_test_sc <- scaled$x_test
  
  alpha_fits <- list()
  
  for (a in alpha_grid) {
    fit_a <- fit_glmnet_one_alpha(x_train_sc, y_train, alpha_value = a)
    alpha_fits[[as.character(a)]] <- fit_a
  }
  
  alpha_auc <- sapply(alpha_fits, function(z) z$cv_auc_max)
  best_alpha_name <- names(which.max(alpha_auc))
  best_fit <- alpha_fits[[best_alpha_name]]
  best_cvfit <- best_fit$cvfit
  best_alpha <- best_fit$alpha
  
  selected_lambda <- lambda_rule
  selected_genes <- extract_selected_genes(best_cvfit, selected_lambda)
  
  if (allow_lambda_fallback && length(selected_genes) < min_selected_genes) {
    message("lambda.1se selected < ", min_selected_genes, " genes. Falling back to lambda.min.")
    selected_lambda <- "lambda.min"
    selected_genes <- extract_selected_genes(best_cvfit, selected_lambda)
  }
  
  # 如果仍然过多，保留绝对系数最大的前 max_selected_genes 个，并用普通 logistic refit
  coef_table <- get_coef_table(best_cvfit, selected_lambda)
  coef_table_no_intercept <- coef_table[coef_table$term != "(Intercept)", , drop = FALSE]
  coef_table_no_intercept <- coef_table_no_intercept[order(abs(coef_table_no_intercept$coefficient), decreasing = TRUE), , drop = FALSE]
  
  if (nrow(coef_table_no_intercept) > max_selected_genes) {
    selected_genes <- head(coef_table_no_intercept$term, max_selected_genes)
  }
  
  if (length(selected_genes) < min_selected_genes) {
    # 最后一层保险：取候选表前 min_selected_genes 个
    selected_genes <- head(candidate_genes, min_selected_genes)
    message("Selected genes still too few. Using top candidate genes: ", paste(selected_genes, collapse = ", "))
  }
  
  # 用筛选后的基因在训练集上 refit 一个普通 logistic，便于锁定系数和输出公式
  df_train <- data.frame(
    y = y_train,
    x_train_sc[, selected_genes, drop = FALSE],
    check.names = FALSE
  )
  
  formula_str <- paste0("y ~ ", paste(sprintf("`%s`", selected_genes), collapse = " + "))
  glm_fit <- stats::glm(
    stats::as.formula(formula_str),
    data = df_train,
    family = stats::binomial()
  )
  
  df_test <- data.frame(
    x_test_sc[, selected_genes, drop = FALSE],
    check.names = FALSE
  )
  
  train_prob <- as.numeric(stats::predict(glm_fit, newdata = df_train, type = "response"))
  test_prob <- as.numeric(stats::predict(glm_fit, newdata = df_test, type = "response"))
  
  threshold <- choose_threshold_youden(y_train, train_prob)
  
  train_metrics <- calc_metrics(y_train, train_prob, threshold)
  train_metrics$set <- "training"
  train_metrics$validation_dataset <- heldout_ds
  train_metrics$alpha <- best_alpha
  train_metrics$lambda_rule_used <- selected_lambda
  train_metrics$n_selected_genes <- length(selected_genes)
  
  test_metrics <- calc_metrics(y_test, test_prob, threshold)
  test_metrics$set <- "validation"
  test_metrics$validation_dataset <- heldout_ds
  test_metrics$alpha <- best_alpha
  test_metrics$lambda_rule_used <- selected_lambda
  test_metrics$n_selected_genes <- length(selected_genes)
  
  metrics_list[[heldout_ds]] <- rbind(train_metrics, test_metrics)
  
  pred_fold <- data.frame(
    sample_id = test_obj$pheno$sample_id,
    validation_dataset = heldout_ds,
    dataset = test_obj$pheno$dataset,
    clinical_group_main = test_obj$pheno$clinical_group_main,
    y_true = y_test,
    pred_prob = test_prob,
    threshold = threshold,
    pred_class = ifelse(test_prob >= threshold, 1, 0),
    stringsAsFactors = FALSE
  )
  
  pred_list[[heldout_ds]] <- pred_fold
  
  selected_gene_list[[heldout_ds]] <- data.frame(
    validation_dataset = heldout_ds,
    gene_symbol = selected_genes,
    selection_rank = seq_along(selected_genes),
    stringsAsFactors = FALSE
  )
  
  model_objects[[heldout_ds]] <- list(
    validation_dataset = heldout_ds,
    selected_genes = selected_genes,
    best_alpha = best_alpha,
    lambda_rule_used = selected_lambda,
    glm_fit = glm_fit,
    scaling_center = scaled$center[selected_genes],
    scaling_scale = scaled$scale[selected_genes],
    training_threshold = threshold,
    cv_glmnet = best_cvfit,
    alpha_auc = alpha_auc
  )
  
  message("Best alpha: ", best_alpha)
  message("Lambda rule: ", selected_lambda)
  message("Selected genes: ", paste(selected_genes, collapse = ", "))
  message("Validation AUROC: ", round(test_metrics$AUROC, 3))
}

metrics_df <- do.call(rbind, metrics_list)
pred_df <- do.call(rbind, pred_list)
selected_genes_df <- do.call(rbind, selected_gene_list)

# ============================================================
# 输出
# ============================================================

data.table::fwrite(
  metrics_df,
  file.path(val_dir, "T08_LODO_validation_metrics.csv")
)

data.table::fwrite(
  pred_df,
  file.path(val_dir, "T08_LODO_predictions.csv")
)

data.table::fwrite(
  selected_genes_df,
  file.path(val_dir, "T08_LODO_selected_genes_by_fold.csv")
)

saveRDS(
  model_objects,
  file.path(model_dir, "M08_LODO_model_objects.rds")
)

# 选择频率
gene_frequency <- aggregate(
  validation_dataset ~ gene_symbol,
  data = selected_genes_df,
  FUN = length
)
colnames(gene_frequency)[2] <- "n_selected_folds"
gene_frequency$selection_frequency <- gene_frequency$n_selected_folds / length(dataset_list)
gene_frequency <- gene_frequency[order(-gene_frequency$n_selected_folds, gene_frequency$gene_symbol), , drop = FALSE]

data.table::fwrite(
  gene_frequency,
  file.path(val_dir, "T08_LODO_gene_selection_frequency.csv")
)

# Excel 汇总
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "validation_metrics")
openxlsx::writeData(wb, "validation_metrics", metrics_df)

openxlsx::addWorksheet(wb, "predictions")
openxlsx::writeData(wb, "predictions", pred_df)

openxlsx::addWorksheet(wb, "selected_genes")
openxlsx::writeData(wb, "selected_genes", selected_genes_df)

openxlsx::addWorksheet(wb, "gene_selection_frequency")
openxlsx::writeData(wb, "gene_selection_frequency", gene_frequency)

openxlsx::saveWorkbook(
  wb,
  file.path(val_dir, "T08_LODO_modeling_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 绘图
# ============================================================

save_roc_plot(pred_df)
save_pr_plot(pred_df)
save_auc_barplot(metrics_df)
save_prediction_density(pred_df)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_08_LODO_diagnostic_modeling.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 08 LODO diagnostic modeling 完成 ============")
message("模型目录：", model_dir)
message("验证结果目录：", val_dir)
message("图输出目录：", fig_dir)

message("\nLODO validation metrics:")
print(metrics_df[metrics_df$set == "validation", ])

message("\nGene selection frequency:")
print(gene_frequency)

message("\n关键输出：")
message("1) ", file.path(val_dir, "T08_LODO_validation_metrics.csv"))
message("2) ", file.path(val_dir, "T08_LODO_predictions.csv"))
message("3) ", file.path(val_dir, "T08_LODO_selected_genes_by_fold.csv"))
message("4) ", file.path(val_dir, "T08_LODO_gene_selection_frequency.csv"))
message("5) ", file.path(val_dir, "T08_LODO_modeling_summary.xlsx"))
message("6) ", file.path(model_dir, "M08_LODO_model_objects.rds"))
message("7) ", fig_dir)