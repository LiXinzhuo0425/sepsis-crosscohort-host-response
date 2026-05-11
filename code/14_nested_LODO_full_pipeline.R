# ============================================================
# 14_nested_LODO_full_pipeline.R
# Strict nested leave-one-dataset-out diagnostic modeling
#
# 目的：
# 1. 每一折留出 1 个 dataset 作为 held-out validation dataset
# 2. 只在训练的 5 个 datasets 内完成：
#    DEG
#    候选基因筛选
#    单基因 AUROC
#    方向一致性
#    相关性去冗余
#    glmnet 建模
#    标准化参数估计
#    阈值选择
# 3. held-out dataset 只用于验证
# 4. 输出 nested LODO 性能、预测、候选基因、模型对象和图
#
# 输入：
# 04_results/differential_expression/
#   merged_gene_expression_common_genes_unadjusted.csv
#   merged_pheno_for_DEG.csv
#
# 输出：
# 04_results/nested_LODO/
#   T14_nested_LODO_validation_metrics.csv
#   T14_nested_LODO_predictions.csv
#   T14_nested_LODO_selected_genes_by_fold.csv
#   T14_nested_LODO_gene_selection_frequency.csv
#   T14_nested_LODO_fold_candidate_genes.csv
#   T14_nested_LODO_fold_dataset_level_gene_metrics.csv
#   T14_nested_LODO_calibration_metrics.csv
#   T14_nested_LODO_threshold_drift.csv
#   T14_nested_LODO_summary.xlsx
#   M14_nested_LODO_model_objects.rds
#
# 05_figures/nested_LODO/
#   F14A_nested_LODO_ROC_by_dataset.png/pdf
#   F14B_nested_LODO_AUROC_barplot.png/pdf
#   F14C_nested_LODO_AUPRC_barplot.png/pdf
#   F14D_nested_LODO_Brier_barplot.png/pdf
#   F14E_nested_LODO_gene_selection_frequency.png/pdf
#   F14F_nested_LODO_prediction_density.png/pdf
#   F14G_nested_LODO_threshold_drift.png/pdf
#   F14H_nested_LODO_observed_vs_mean_predicted.png/pdf
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(limma)
  library(glmnet)
  library(pROC)
  library(PRROC)
  library(ggplot2)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

de_dir <- file.path(project_dir, "04_results", "differential_expression")
out_dir <- file.path(project_dir, "04_results", "nested_LODO")
fig_dir <- file.path(project_dir, "05_figures", "nested_LODO")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

expr_file <- file.path(de_dir, "merged_gene_expression_common_genes_unadjusted.csv")
pheno_file <- file.path(de_dir, "merged_pheno_for_DEG.csv")

needed <- c(expr_file, pheno_file)
missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 参数
# ============================================================

set.seed(20260507)

deg_padj_cutoff <- 0.05
deg_logfc_cutoff <- 0.5
max_initial_candidate_genes <- 300

min_direction_consistency_strict <- 1.00
min_mean_auc_strict <- 0.80
min_min_auc_strict <- 0.50
min_auc_ge_070_fraction_strict <- 0.70

min_direction_consistency_relaxed <- 0.80
min_mean_auc_relaxed <- 0.70
min_min_auc_relaxed <- 0.45
min_auc_ge_070_fraction_relaxed <- 0.50

min_candidate_pool_after_refinement <- 10
max_candidate_pool_after_refinement <- 50

cor_cutoff <- 0.85

alpha_grid <- c(1.00, 0.75, 0.50)
nfolds_inner_cv <- 5
lambda_rule <- "lambda.1se"
allow_lambda_fallback <- TRUE

min_selected_genes <- 3
max_selected_genes <- 10

eps <- 1e-6

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

safe_auc <- function(y_true, pred) {
  idx <- !is.na(y_true) & is.finite(pred)
  y_true <- y_true[idx]
  pred <- pred[idx]
  
  if (length(unique(y_true)) != 2) return(NA_real_)
  if (length(unique(pred)) < 2) return(NA_real_)
  
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
  
  if (inherits(roc_obj, "try-error")) return(NA_real_)
  as.numeric(pROC::auc(roc_obj))
}

safe_auc_ci <- function(y_true, pred) {
  idx <- !is.na(y_true) & is.finite(pred)
  y_true <- y_true[idx]
  pred <- pred[idx]
  
  if (length(unique(y_true)) != 2) {
    return(c(auc = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
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

safe_wilcox_p <- function(x_control, x_sepsis) {
  if (length(unique(c(x_control, x_sepsis))) < 2) return(NA_real_)
  
  out <- try(
    stats::wilcox.test(x_control, x_sepsis)$p.value,
    silent = TRUE
  )
  
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out)
}

get_direction <- function(logfc) {
  if (is.na(logfc)) return(NA_character_)
  if (logfc > 0) return("Up_in_Sepsis")
  if (logfc < 0) return("Down_in_Sepsis")
  "No_change"
}

scale_train_apply <- function(x_train, x_test) {
  center <- colMeans(x_train, na.rm = TRUE)
  scale <- apply(x_train, 2, stats::sd, na.rm = TRUE)
  scale[is.na(scale) | scale == 0] <- 1
  
  x_train_scaled <- sweep(sweep(x_train, 2, center, "-"), 2, scale, "/")
  x_test_scaled <- sweep(sweep(x_test, 2, center, "-"), 2, scale, "/")
  
  list(
    x_train = x_train_scaled,
    x_test = x_test_scaled,
    center = center,
    scale = scale
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

calc_validation_metrics <- function(y_true, pred_prob, threshold) {
  auc_ci <- safe_auc_ci(y_true, pred_prob)
  cls <- calc_classification_metrics(y_true, pred_prob, threshold)
  
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
    threshold = cls$threshold,
    sensitivity = cls$sensitivity,
    specificity = cls$specificity,
    PPV = cls$PPV,
    NPV = cls$NPV,
    accuracy = cls$accuracy,
    TP = cls$TP,
    TN = cls$TN,
    FP = cls$FP,
    FN = cls$FN,
    stringsAsFactors = FALSE
  )
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

run_train_only_limma <- function(expr_train, pheno_train) {
  pheno_train <- pheno_train[match(colnames(expr_train), pheno_train$sample_id), , drop = FALSE]
  
  if (!all(colnames(expr_train) == pheno_train$sample_id)) {
    stop("run_train_only_limma: 表达矩阵和 phenotype 未对齐。")
  }
  
  group <- factor(pheno_train$clinical_group_main, levels = c("Control", "Sepsis"))
  batch <- factor(pheno_train$dataset)
  
  if (length(unique(group)) != 2) {
    stop("训练集缺少 Control 或 Sepsis。")
  }
  
  design <- stats::model.matrix(~ 0 + group + batch)
  colnames(design) <- make.names(colnames(design))
  
  fit <- limma::lmFit(expr_train, design)
  
  cm <- limma::makeContrasts(
    contrasts = "groupSepsis - groupControl",
    levels = design
  )
  
  fit2 <- limma::contrasts.fit(fit, cm)
  fit2 <- limma::eBayes(fit2)
  
  deg <- limma::topTable(fit2, coef = 1, n = Inf, sort.by = "P")
  deg$gene_symbol <- rownames(deg)
  deg <- deg[, c("gene_symbol", setdiff(colnames(deg), "gene_symbol"))]
  
  deg$global_direction <- ifelse(deg$logFC > 0, "Up_in_Sepsis", "Down_in_Sepsis")
  
  deg
}

compute_dataset_gene_metrics <- function(expr_train, pheno_train, candidate_genes) {
  dataset_list <- sort(unique(pheno_train$dataset))
  out_list <- list()
  k <- 1
  
  for (g in candidate_genes) {
    if (!g %in% rownames(expr_train)) next
    
    x_all <- as.numeric(expr_train[g, ])
    
    for (ds in dataset_list) {
      idx <- pheno_train$dataset == ds
      ph_ds <- pheno_train[idx, , drop = FALSE]
      x_ds <- x_all[idx]
      
      if (!all(c("Control", "Sepsis") %in% unique(ph_ds$clinical_group_main))) next
      
      x_control <- x_ds[ph_ds$clinical_group_main == "Control"]
      x_sepsis <- x_ds[ph_ds$clinical_group_main == "Sepsis"]
      
      mean_control <- mean(x_control, na.rm = TRUE)
      mean_sepsis <- mean(x_sepsis, na.rm = TRUE)
      logfc_ds <- mean_sepsis - mean_control
      
      y_ds <- ifelse(ph_ds$clinical_group_main == "Sepsis", 1, 0)
      
      out_list[[k]] <- data.frame(
        gene_symbol = g,
        dataset = ds,
        n_control = sum(ph_ds$clinical_group_main == "Control"),
        n_sepsis = sum(ph_ds$clinical_group_main == "Sepsis"),
        mean_control = mean_control,
        mean_sepsis = mean_sepsis,
        median_control = median(x_control, na.rm = TRUE),
        median_sepsis = median(x_sepsis, na.rm = TRUE),
        dataset_logFC = logfc_ds,
        dataset_direction = get_direction(logfc_ds),
        auc = safe_auc(y_ds, x_ds),
        wilcox_p = safe_wilcox_p(x_control, x_sepsis),
        stringsAsFactors = FALSE
      )
      
      k <- k + 1
    }
  }
  
  if (length(out_list) == 0) return(data.frame())
  do.call(rbind, out_list)
}

summarise_candidates <- function(deg_candidates, dataset_metrics, n_training_datasets) {
  genes <- unique(deg_candidates$gene_symbol)
  out_list <- list()
  
  for (g in genes) {
    d <- deg_candidates[deg_candidates$gene_symbol == g, , drop = FALSE]
    d <- d[1, , drop = FALSE]
    
    m <- dataset_metrics[dataset_metrics$gene_symbol == g, , drop = FALSE]
    
    if (nrow(m) == 0) next
    
    global_direction <- d$global_direction[1]
    n_tested <- nrow(m)
    n_direction_consistent <- sum(m$dataset_direction == global_direction, na.rm = TRUE)
    direction_consistency <- n_direction_consistent / max(n_tested, 1)
    
    mean_auc <- mean(m$auc, na.rm = TRUE)
    median_auc <- median(m$auc, na.rm = TRUE)
    min_auc <- suppressWarnings(min(m$auc, na.rm = TRUE))
    if (!is.finite(min_auc)) min_auc <- NA_real_
    
    n_auc_ge_070 <- sum(m$auc >= 0.70, na.rm = TRUE)
    required_auc_ge_070_strict <- ceiling(n_training_datasets * min_auc_ge_070_fraction_strict)
    required_auc_ge_070_relaxed <- ceiling(n_training_datasets * min_auc_ge_070_fraction_relaxed)
    
    strict_pass <- (
      direction_consistency >= min_direction_consistency_strict &&
        is.finite(mean_auc) &&
        mean_auc >= min_mean_auc_strict &&
        is.finite(min_auc) &&
        min_auc >= min_min_auc_strict &&
        n_auc_ge_070 >= required_auc_ge_070_strict
    )
    
    relaxed_pass <- (
      direction_consistency >= min_direction_consistency_relaxed &&
        is.finite(mean_auc) &&
        mean_auc >= min_mean_auc_relaxed &&
        is.finite(min_auc) &&
        min_auc >= min_min_auc_relaxed &&
        n_auc_ge_070 >= required_auc_ge_070_relaxed
    )
    
    score <- 0.35 * direction_consistency +
      0.35 * mean_auc +
      0.15 * ifelse(is.na(min_auc), 0, min_auc) +
      0.10 * min(abs(d$logFC[1]) / 3, 1) +
      0.05 * min((-log10(d$adj.P.Val[1] + 1e-300)) / 100, 1)
    
    out_list[[g]] <- data.frame(
      gene_symbol = g,
      train_logFC = d$logFC[1],
      train_adjP = d$adj.P.Val[1],
      train_direction = global_direction,
      n_dataset_tested = n_tested,
      n_direction_consistent = n_direction_consistent,
      direction_consistency = direction_consistency,
      mean_auc = mean_auc,
      median_auc = median_auc,
      min_auc = min_auc,
      n_auc_ge_070 = n_auc_ge_070,
      strict_pass = strict_pass,
      relaxed_pass = relaxed_pass,
      priority_score = score,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(out_list) == 0) return(data.frame())
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  
  out <- out[order(
    -out$strict_pass,
    -out$relaxed_pass,
    -out$priority_score,
    out$train_adjP
  ), , drop = FALSE]
  
  out
}

greedy_correlation_prune <- function(expr_train, candidate_table, cor_cutoff = 0.85, max_genes = 50) {
  genes <- candidate_table$gene_symbol
  genes <- genes[genes %in% rownames(expr_train)]
  
  if (length(genes) <= 1) return(genes)
  
  expr_sub <- t(expr_train[genes, , drop = FALSE])
  cor_mat <- stats::cor(expr_sub, use = "pairwise.complete.obs", method = "spearman")
  cor_mat[is.na(cor_mat)] <- 0
  
  selected <- character()
  
  for (g in genes) {
    if (length(selected) == 0) {
      selected <- c(selected, g)
    } else {
      max_cor <- max(abs(cor_mat[g, selected]), na.rm = TRUE)
      if (!is.finite(max_cor) || max_cor < cor_cutoff) {
        selected <- c(selected, g)
      }
    }
    
    if (length(selected) >= max_genes) break
  }
  
  selected
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

fit_nested_model <- function(x_train_scaled, y_train, candidate_genes) {
  fit_list <- list()
  
  for (a in alpha_grid) {
    fit_list[[as.character(a)]] <- fit_glmnet_one_alpha(x_train_scaled, y_train, alpha_value = a)
  }
  
  alpha_auc <- data.frame(
    alpha_name = names(fit_list),
    alpha = as.numeric(names(fit_list)),
    cv_auc_max = sapply(fit_list, function(z) z$cv_auc_max),
    stringsAsFactors = FALSE
  )
  
  valid_idx <- which(is.finite(alpha_auc$cv_auc_max))
  
  if (length(valid_idx) == 0) {
    stop("所有 alpha 的 cv_auc_max 均不可用。")
  }
  
  best_idx <- valid_idx[which.max(alpha_auc$cv_auc_max[valid_idx])]
  best_alpha_name <- alpha_auc$alpha_name[best_idx]
  best_fit <- fit_list[[best_alpha_name]]
  best_cvfit <- best_fit$cvfit
  best_alpha <- best_fit$alpha
  
  selected_lambda <- lambda_rule
  selected_genes <- extract_selected_genes(best_cvfit, selected_lambda)
  
  if (allow_lambda_fallback && length(selected_genes) < min_selected_genes) {
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
  }
  
  selected_genes <- selected_genes[selected_genes %in% colnames(x_train_scaled)]
  
  if (length(selected_genes) < 1) {
    stop("最终入模基因数为 0。")
  }
  
  df_train <- data.frame(
    y = y_train,
    x_train_scaled[, selected_genes, drop = FALSE],
    check.names = FALSE
  )
  
  formula_str <- paste0(
    "y ~ ",
    paste(sprintf("`%s`", selected_genes), collapse = " + ")
  )
  
  glm_fit <- try(
    stats::glm(
      stats::as.formula(formula_str),
      data = df_train,
      family = stats::binomial()
    ),
    silent = TRUE
  )
  
  if (inherits(glm_fit, "try-error")) {
    stop("普通 logistic refit 失败。")
  }
  
  train_prob <- as.numeric(stats::predict(glm_fit, newdata = df_train, type = "response"))
  threshold <- choose_youden_threshold(y_train, train_prob)
  
  coef_final <- data.frame(
    term = names(stats::coef(glm_fit)),
    coefficient = as.numeric(stats::coef(glm_fit)),
    stringsAsFactors = FALSE
  )
  
  list(
    selected_genes = selected_genes,
    glm_fit = glm_fit,
    train_prob = train_prob,
    threshold = threshold,
    best_alpha = best_alpha,
    selected_lambda = selected_lambda,
    best_cvfit = best_cvfit,
    alpha_auc = alpha_auc,
    coef_final = coef_final
  )
}

make_calibration_bins <- function(pred_df, n_bins = 5) {
  df <- pred_df[is.finite(pred_df$pred_prob), , drop = FALSE]
  
  if (nrow(df) < 2) return(data.frame())
  
  n_bins <- min(n_bins, nrow(df))
  df$rank_pred <- rank(df$pred_prob, ties.method = "first")
  
  breaks <- unique(stats::quantile(
    df$rank_pred,
    probs = seq(0, 1, length.out = n_bins + 1),
    na.rm = TRUE
  ))
  
  if (length(breaks) < 3) return(data.frame())
  
  df$bin <- cut(
    df$rank_pred,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  bin_mean <- aggregate(
    cbind(pred_prob, y_true) ~ validation_dataset + bin,
    data = df,
    FUN = mean
  )
  
  bin_n <- aggregate(
    sample_id ~ validation_dataset + bin,
    data = df,
    FUN = length
  )
  
  names(bin_mean)[names(bin_mean) == "pred_prob"] <- "mean_predicted"
  names(bin_mean)[names(bin_mean) == "y_true"] <- "observed_rate"
  names(bin_n)[names(bin_n) == "sample_id"] <- "n_bin"
  
  merge(bin_mean, bin_n, by = c("validation_dataset", "bin"), all.x = TRUE)
}

save_roc_plot <- function(pred_df) {
  plot_list <- list()
  
  for (ds in unique(pred_df$validation_dataset)) {
    df <- pred_df[pred_df$validation_dataset == ds, , drop = FALSE]
    if (length(unique(df$y_true)) != 2) next
    
    roc_obj <- pROC::roc(
      response = df$y_true,
      predictor = df$pred_prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
    
    z <- data.frame(
      validation_dataset = ds,
      FPR = 1 - roc_obj$specificities,
      sensitivity = roc_obj$sensitivities,
      stringsAsFactors = FALSE
    )
    
    plot_list[[ds]] <- z
  }
  
  if (length(plot_list) == 0) return(NULL)
  
  roc_df <- do.call(rbind, plot_list)
  
  p <- ggplot2::ggplot(
    roc_df,
    ggplot2::aes(x = FPR, y = sensitivity, color = validation_dataset)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Nested LODO ROC curves",
      x = "1 - specificity",
      y = "Sensitivity",
      color = "Held-out dataset"
    )
  
  ggplot2::ggsave(file.path(fig_dir, "F14A_nested_LODO_ROC_by_dataset.png"), p, width = 7, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, "F14A_nested_LODO_ROC_by_dataset.pdf"), p, width = 7, height = 6)
  
  invisible(p)
}

save_metric_barplot <- function(metrics_df, metric, file_prefix, title_text, y_label) {
  df <- metrics_df
  df$validation_dataset <- factor(df$validation_dataset, levels = df$validation_dataset[order(df[[metric]])])
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = validation_dataset, y = .data[[metric]])
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = title_text,
      x = "Held-out dataset",
      y = y_label
    )
  
  ggplot2::ggsave(file.path(fig_dir, paste0(file_prefix, ".png")), p, width = 7, height = 5, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, paste0(file_prefix, ".pdf")), p, width = 7, height = 5)
  
  invisible(p)
}

save_gene_frequency_plot <- function(gene_frequency) {
  if (nrow(gene_frequency) == 0) return(NULL)
  
  df <- gene_frequency
  df$gene_symbol <- factor(df$gene_symbol, levels = rev(df$gene_symbol[order(df$n_selected_folds)]))
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = gene_symbol, y = n_selected_folds)
  ) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Gene selection frequency across nested LODO folds",
      x = "Gene",
      y = "Number of folds selected"
    )
  
  ggplot2::ggsave(file.path(fig_dir, "F14E_nested_LODO_gene_selection_frequency.png"), p, width = 7, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, "F14E_nested_LODO_gene_selection_frequency.pdf"), p, width = 7, height = 6)
  
  invisible(p)
}

save_prediction_density <- function(pred_df) {
  df <- pred_df
  df$group <- ifelse(df$y_true == 1, "Sepsis", "Control")
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = pred_prob, color = group, fill = group)
  ) +
    ggplot2::geom_density(alpha = 0.25) +
    ggplot2::facet_wrap(~ validation_dataset, scales = "free_y") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(
      title = "Nested LODO prediction density",
      x = "Predicted probability",
      y = "Density",
      color = "Group",
      fill = "Group"
    )
  
  ggplot2::ggsave(file.path(fig_dir, "F14F_nested_LODO_prediction_density.png"), p, width = 10, height = 7, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, "F14F_nested_LODO_prediction_density.pdf"), p, width = 10, height = 7)
  
  invisible(p)
}

save_threshold_drift_plot <- function(threshold_df) {
  df <- threshold_df
  df$validation_dataset <- factor(df$validation_dataset, levels = df$validation_dataset[order(df$threshold_shift)])
  
  p <- ggplot2::ggplot(
    df,
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
      alpha = 0.7
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      title = "Nested LODO threshold drift",
      x = "Held-out dataset",
      y = "Probability threshold"
    )
  
  ggplot2::ggsave(file.path(fig_dir, "F14G_nested_LODO_threshold_drift.png"), p, width = 7, height = 5, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, "F14G_nested_LODO_threshold_drift.pdf"), p, width = 7, height = 5)
  
  invisible(p)
}

save_observed_vs_predicted_plot <- function(cal_df) {
  df <- cal_df
  df$validation_dataset <- factor(df$validation_dataset, levels = df$validation_dataset[order(df$observed_rate)])
  
  p <- ggplot2::ggplot(
    df,
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
  
  ggplot2::ggsave(file.path(fig_dir, "F14H_nested_LODO_observed_vs_mean_predicted.png"), p, width = 7, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(fig_dir, "F14H_nested_LODO_observed_vs_mean_predicted.pdf"), p, width = 7, height = 6)
  
  invisible(p)
}

# ============================================================
# 读取输入
# ============================================================

message("Reading inputs...")

expr_mat <- read_expr_matrix(expr_file)
pheno <- data.table::fread(pheno_file, data.table = FALSE)

required_pheno_cols <- c("sample_id", "dataset", "clinical_group_main")

if (!all(required_pheno_cols %in% colnames(pheno))) {
  stop("phenotype 缺少 sample_id / dataset / clinical_group_main。")
}

pheno$sample_id <- as.character(pheno$sample_id)
pheno$dataset <- as.character(pheno$dataset)
pheno$clinical_group_main <- as.character(pheno$clinical_group_main)

common_samples <- intersect(colnames(expr_mat), pheno$sample_id)
expr_mat <- expr_mat[, common_samples, drop = FALSE]
pheno <- pheno[match(common_samples, pheno$sample_id), , drop = FALSE]

if (!all(colnames(expr_mat) == pheno$sample_id)) {
  stop("表达矩阵和 phenotype 未对齐。")
}

pheno <- pheno[pheno$clinical_group_main %in% c("Control", "Sepsis"), , drop = FALSE]
expr_mat <- expr_mat[, pheno$sample_id, drop = FALSE]

dataset_list <- sort(unique(pheno$dataset))

message("Datasets: ", paste(dataset_list, collapse = ", "))
message("Total samples: ", ncol(expr_mat))
message("Cases: ", sum(pheno$clinical_group_main == "Sepsis"))
message("Controls: ", sum(pheno$clinical_group_main == "Control"))

# ============================================================
# Nested LODO 主循环
# ============================================================

model_objects <- list()
metrics_list <- list()
pred_list <- list()
selected_gene_list <- list()
fold_candidate_list <- list()
fold_gene_metric_list <- list()
calibration_list <- list()
threshold_list <- list()
calibration_bin_list <- list()
training_metrics_list <- list()

for (heldout_ds in dataset_list) {
  
  message("\n==================================================")
  message("Nested LODO held-out dataset: ", heldout_ds)
  message("==================================================")
  
  fold_dir <- file.path(out_dir, paste0("fold_", heldout_ds))
  dir.create(fold_dir, recursive = TRUE, showWarnings = FALSE)
  
  train_idx <- pheno$dataset != heldout_ds
  test_idx <- pheno$dataset == heldout_ds
  
  pheno_train <- pheno[train_idx, , drop = FALSE]
  pheno_test <- pheno[test_idx, , drop = FALSE]
  
  expr_train <- expr_mat[, pheno_train$sample_id, drop = FALSE]
  expr_test <- expr_mat[, pheno_test$sample_id, drop = FALSE]
  
  if (!all(c("Control", "Sepsis") %in% unique(pheno_train$clinical_group_main))) {
    warning("训练集缺少某一类别，跳过：", heldout_ds)
    next
  }
  
  if (!all(c("Control", "Sepsis") %in% unique(pheno_test$clinical_group_main))) {
    warning("验证集缺少某一类别，跳过：", heldout_ds)
    next
  }
  
  message("Training samples: ", ncol(expr_train))
  message("Validation samples: ", ncol(expr_test))
  
  # 1. 训练侧 DEG
  message("Step 1: train-only DEG...")
  
  deg_train <- run_train_only_limma(expr_train, pheno_train)
  
  data.table::fwrite(
    deg_train,
    file.path(fold_dir, paste0("DEG_train_only_heldout_", heldout_ds, ".csv"))
  )
  
  deg_candidates <- deg_train[
    deg_train$adj.P.Val < deg_padj_cutoff &
      abs(deg_train$logFC) >= deg_logfc_cutoff,
    ,
    drop = FALSE
  ]
  
  deg_candidates <- deg_candidates[
    order(deg_candidates$adj.P.Val, -abs(deg_candidates$logFC)),
    ,
    drop = FALSE
  ]
  
  if (nrow(deg_candidates) == 0) {
    message("严格 DEG 候选为 0，放宽为按 adj.P.Val 排序前 ", max_initial_candidate_genes, " 个。")
    deg_candidates <- head(deg_train[order(deg_train$adj.P.Val), , drop = FALSE], max_initial_candidate_genes)
  } else if (nrow(deg_candidates) > max_initial_candidate_genes) {
    deg_candidates <- head(deg_candidates, max_initial_candidate_genes)
  }
  
  initial_candidate_genes <- unique(deg_candidates$gene_symbol)
  
  message("Initial train-only candidate genes: ", length(initial_candidate_genes))
  
  # 2. 训练侧单基因 metrics
  message("Step 2: train-only dataset-level gene metrics...")
  
  gene_metrics <- compute_dataset_gene_metrics(
    expr_train = expr_train,
    pheno_train = pheno_train,
    candidate_genes = initial_candidate_genes
  )
  
  gene_metrics$heldout_dataset <- heldout_ds
  
  data.table::fwrite(
    gene_metrics,
    file.path(fold_dir, paste0("T14_fold_", heldout_ds, "_train_dataset_level_gene_metrics.csv"))
  )
  
  # 3. 训练侧候选总结
  message("Step 3: train-only candidate summarization...")
  
  candidate_summary <- summarise_candidates(
    deg_candidates = deg_candidates,
    dataset_metrics = gene_metrics,
    n_training_datasets = length(unique(pheno_train$dataset))
  )
  
  candidate_summary$heldout_dataset <- heldout_ds
  
  if (nrow(candidate_summary) == 0) {
    stop("候选基因总结为空：", heldout_ds)
  }
  
  strict_pool <- candidate_summary[candidate_summary$strict_pass == TRUE, , drop = FALSE]
  relaxed_pool <- candidate_summary[candidate_summary$relaxed_pass == TRUE, , drop = FALSE]
  
  if (nrow(strict_pool) >= min_candidate_pool_after_refinement) {
    base_pool <- strict_pool
    pool_used <- "strict"
  } else if (nrow(relaxed_pool) >= min_candidate_pool_after_refinement) {
    base_pool <- relaxed_pool
    pool_used <- "relaxed"
  } else {
    base_pool <- head(candidate_summary, max_candidate_pool_after_refinement)
    pool_used <- "top_ranked_fallback"
  }
  
  base_pool <- base_pool[order(-base_pool$priority_score, base_pool$train_adjP), , drop = FALSE]
  
  message("Strict pool: ", nrow(strict_pool))
  message("Relaxed pool: ", nrow(relaxed_pool))
  message("Pool used: ", pool_used, "; n = ", nrow(base_pool))
  
  # 4. 相关性去冗余
  message("Step 4: train-only correlation pruning...")
  
  selected_candidate_genes <- greedy_correlation_prune(
    expr_train = expr_train,
    candidate_table = base_pool,
    cor_cutoff = cor_cutoff,
    max_genes = max_candidate_pool_after_refinement
  )
  
  if (length(selected_candidate_genes) < min_candidate_pool_after_refinement) {
    remaining <- setdiff(base_pool$gene_symbol, selected_candidate_genes)
    add_n <- min(
      min_candidate_pool_after_refinement - length(selected_candidate_genes),
      length(remaining)
    )
    if (add_n > 0) {
      selected_candidate_genes <- c(selected_candidate_genes, head(remaining, add_n))
    }
  }
  
  refined_candidates <- base_pool[base_pool$gene_symbol %in% selected_candidate_genes, , drop = FALSE]
  refined_candidates$selection_order_candidate_pool <- match(refined_candidates$gene_symbol, selected_candidate_genes)
  refined_candidates$pool_used <- pool_used
  refined_candidates <- refined_candidates[order(refined_candidates$selection_order_candidate_pool), , drop = FALSE]
  
  data.table::fwrite(
    refined_candidates,
    file.path(fold_dir, paste0("T14_fold_", heldout_ds, "_refined_candidate_genes.csv"))
  )
  
  fold_candidate_list[[heldout_ds]] <- refined_candidates
  fold_gene_metric_list[[heldout_ds]] <- gene_metrics
  
  message("Refined candidate genes: ", nrow(refined_candidates))
  
  # 5. 建模矩阵
  message("Step 5: train-only modeling...")
  
  modeling_genes <- refined_candidates$gene_symbol
  modeling_genes <- modeling_genes[modeling_genes %in% rownames(expr_train)]
  
  x_train <- t(expr_train[modeling_genes, , drop = FALSE])
  x_test <- t(expr_test[modeling_genes, , drop = FALSE])
  
  y_train <- ifelse(pheno_train$clinical_group_main == "Sepsis", 1, 0)
  y_test <- ifelse(pheno_test$clinical_group_main == "Sepsis", 1, 0)
  
  scaled <- scale_train_apply(x_train, x_test)
  
  model_fit <- fit_nested_model(
    x_train_scaled = scaled$x_train,
    y_train = y_train,
    candidate_genes = modeling_genes
  )
  
  selected_genes <- model_fit$selected_genes
  
  df_train_model <- data.frame(
    scaled$x_train[, selected_genes, drop = FALSE],
    check.names = FALSE
  )
  
  df_test_model <- data.frame(
    scaled$x_test[, selected_genes, drop = FALSE],
    check.names = FALSE
  )
  
  train_prob <- as.numeric(stats::predict(model_fit$glm_fit, newdata = df_train_model, type = "response"))
  test_prob <- as.numeric(stats::predict(model_fit$glm_fit, newdata = df_test_model, type = "response"))
  
  fixed_threshold <- model_fit$threshold
  local_threshold <- choose_youden_threshold(y_test, test_prob)
  
  train_metrics <- calc_validation_metrics(y_train, train_prob, fixed_threshold)
  train_metrics$set <- "training"
  train_metrics$validation_dataset <- heldout_ds
  train_metrics$best_alpha <- model_fit$best_alpha
  train_metrics$lambda_rule_used <- model_fit$selected_lambda
  train_metrics$n_refined_candidates <- length(modeling_genes)
  train_metrics$n_selected_genes <- length(selected_genes)
  train_metrics$pool_used <- pool_used
  
  test_metrics <- calc_validation_metrics(y_test, test_prob, fixed_threshold)
  test_metrics$set <- "validation"
  test_metrics$validation_dataset <- heldout_ds
  test_metrics$best_alpha <- model_fit$best_alpha
  test_metrics$lambda_rule_used <- model_fit$selected_lambda
  test_metrics$n_refined_candidates <- length(modeling_genes)
  test_metrics$n_selected_genes <- length(selected_genes)
  test_metrics$pool_used <- pool_used
  
  training_metrics_list[[heldout_ds]] <- train_metrics
  metrics_list[[heldout_ds]] <- test_metrics
  
  pred_fold <- data.frame(
    sample_id = pheno_test$sample_id,
    validation_dataset = heldout_ds,
    dataset = pheno_test$dataset,
    clinical_group_main = pheno_test$clinical_group_main,
    y_true = y_test,
    pred_prob = test_prob,
    fixed_training_threshold = fixed_threshold,
    local_youden_threshold = local_threshold,
    pred_class_fixed = ifelse(test_prob >= fixed_threshold, 1, 0),
    pred_class_local = ifelse(test_prob >= local_threshold, 1, 0),
    stringsAsFactors = FALSE
  )
  
  pred_list[[heldout_ds]] <- pred_fold
  
  selected_gene_df <- data.frame(
    validation_dataset = heldout_ds,
    gene_symbol = selected_genes,
    selection_rank = seq_along(selected_genes),
    stringsAsFactors = FALSE
  )
  
  selected_gene_list[[heldout_ds]] <- selected_gene_df
  
  # 6. calibration 和 threshold drift
  cal <- calc_calibration_metrics(y_test, test_prob)
  cal$validation_dataset <- heldout_ds
  cal$n <- length(y_test)
  cal$n_case <- sum(y_test == 1)
  cal$n_control <- sum(y_test == 0)
  cal$AUROC <- test_metrics$AUROC
  cal$AUPRC <- test_metrics$AUPRC
  calibration_list[[heldout_ds]] <- cal
  
  fixed_cls <- calc_classification_metrics(y_test, test_prob, fixed_threshold)
  local_cls <- calc_classification_metrics(y_test, test_prob, local_threshold)
  
  threshold_df <- data.frame(
    validation_dataset = heldout_ds,
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
  
  threshold_list[[heldout_ds]] <- threshold_df
  
  cal_bins <- make_calibration_bins(pred_fold, n_bins = 5)
  calibration_bin_list[[heldout_ds]] <- cal_bins
  
  model_objects[[heldout_ds]] <- list(
    validation_dataset = heldout_ds,
    training_datasets = sort(unique(pheno_train$dataset)),
    heldout_dataset = heldout_ds,
    pool_used = pool_used,
    deg_train = deg_train,
    refined_candidates = refined_candidates,
    selected_genes = selected_genes,
    selected_gene_df = selected_gene_df,
    glm_fit = model_fit$glm_fit,
    coef_final = model_fit$coef_final,
    best_alpha = model_fit$best_alpha,
    lambda_rule_used = model_fit$selected_lambda,
    alpha_auc = model_fit$alpha_auc,
    fixed_training_threshold = fixed_threshold,
    local_youden_threshold = local_threshold,
    scaling_center = scaled$center[selected_genes],
    scaling_scale = scaled$scale[selected_genes],
    training_metrics = train_metrics,
    validation_metrics = test_metrics
  )
  
  message("Selected genes: ", paste(selected_genes, collapse = ", "))
  message("Validation AUROC: ", round(test_metrics$AUROC, 3))
  message("Validation AUPRC: ", round(test_metrics$AUPRC, 3))
  message("Validation Brier: ", round(test_metrics$Brier, 3))
}

# ============================================================
# 合并输出
# ============================================================

validation_metrics <- do.call(rbind, metrics_list)
training_metrics <- do.call(rbind, training_metrics_list)
all_metrics <- rbind(training_metrics, validation_metrics)

predictions <- do.call(rbind, pred_list)
selected_genes_by_fold <- do.call(rbind, selected_gene_list)
fold_candidates <- do.call(rbind, fold_candidate_list)
fold_gene_metrics <- do.call(rbind, fold_gene_metric_list)
calibration_metrics <- do.call(rbind, calibration_list)
threshold_drift <- do.call(rbind, threshold_list)
calibration_bins <- do.call(rbind, calibration_bin_list)

rownames(validation_metrics) <- NULL
rownames(training_metrics) <- NULL
rownames(all_metrics) <- NULL
rownames(predictions) <- NULL
rownames(selected_genes_by_fold) <- NULL
rownames(fold_candidates) <- NULL
rownames(fold_gene_metrics) <- NULL
rownames(calibration_metrics) <- NULL
rownames(threshold_drift) <- NULL
rownames(calibration_bins) <- NULL

gene_frequency <- aggregate(
  validation_dataset ~ gene_symbol,
  data = selected_genes_by_fold,
  FUN = length
)
colnames(gene_frequency)[2] <- "n_selected_folds"
gene_frequency$selection_frequency <- gene_frequency$n_selected_folds / length(dataset_list)
gene_frequency <- gene_frequency[order(-gene_frequency$n_selected_folds, gene_frequency$gene_symbol), , drop = FALSE]

summary_df <- data.frame(
  metric = c(
    "n_validation_datasets",
    "median_AUROC",
    "min_AUROC",
    "max_AUROC",
    "median_AUPRC",
    "median_Brier",
    "n_AUROC_ge_0.80",
    "n_AUROC_lt_0.80",
    "median_abs_calibration_in_the_large",
    "median_abs_threshold_shift",
    "max_abs_threshold_shift",
    "n_fixed_sensitivity_lt_0.20",
    "n_fixed_specificity_lt_0.20",
    "n_genes_selected_in_all_folds"
  ),
  value = c(
    nrow(validation_metrics),
    median(validation_metrics$AUROC, na.rm = TRUE),
    min(validation_metrics$AUROC, na.rm = TRUE),
    max(validation_metrics$AUROC, na.rm = TRUE),
    median(validation_metrics$AUPRC, na.rm = TRUE),
    median(validation_metrics$Brier, na.rm = TRUE),
    sum(validation_metrics$AUROC >= 0.80, na.rm = TRUE),
    sum(validation_metrics$AUROC < 0.80, na.rm = TRUE),
    median(abs(calibration_metrics$calibration_in_the_large), na.rm = TRUE),
    median(abs(threshold_drift$threshold_shift), na.rm = TRUE),
    max(abs(threshold_drift$threshold_shift), na.rm = TRUE),
    sum(threshold_drift$fixed_sensitivity < 0.20, na.rm = TRUE),
    sum(threshold_drift$fixed_specificity < 0.20, na.rm = TRUE),
    sum(gene_frequency$n_selected_folds == length(dataset_list), na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

go_no_go <- data.frame(
  criterion = c(
    "median_AUROC_ge_0.85",
    "at_least_4_of_6_AUROC_ge_0.80",
    "minimum_AUROC_ge_0.65",
    "at_least_3_recurrent_genes"
  ),
  pass = c(
    median(validation_metrics$AUROC, na.rm = TRUE) >= 0.85,
    sum(validation_metrics$AUROC >= 0.80, na.rm = TRUE) >= 4,
    min(validation_metrics$AUROC, na.rm = TRUE) >= 0.65,
    sum(gene_frequency$n_selected_folds >= 3, na.rm = TRUE) >= 3
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存表格
# ============================================================

data.table::fwrite(validation_metrics, file.path(out_dir, "T14_nested_LODO_validation_metrics.csv"))
data.table::fwrite(training_metrics, file.path(out_dir, "T14_nested_LODO_training_metrics.csv"))
data.table::fwrite(all_metrics, file.path(out_dir, "T14_nested_LODO_all_metrics.csv"))
data.table::fwrite(predictions, file.path(out_dir, "T14_nested_LODO_predictions.csv"))
data.table::fwrite(selected_genes_by_fold, file.path(out_dir, "T14_nested_LODO_selected_genes_by_fold.csv"))
data.table::fwrite(gene_frequency, file.path(out_dir, "T14_nested_LODO_gene_selection_frequency.csv"))
data.table::fwrite(fold_candidates, file.path(out_dir, "T14_nested_LODO_fold_candidate_genes.csv"))
data.table::fwrite(fold_gene_metrics, file.path(out_dir, "T14_nested_LODO_fold_dataset_level_gene_metrics.csv"))
data.table::fwrite(calibration_metrics, file.path(out_dir, "T14_nested_LODO_calibration_metrics.csv"))
data.table::fwrite(threshold_drift, file.path(out_dir, "T14_nested_LODO_threshold_drift.csv"))
data.table::fwrite(calibration_bins, file.path(out_dir, "T14_nested_LODO_calibration_bins.csv"))
data.table::fwrite(summary_df, file.path(out_dir, "T14_nested_LODO_summary_metrics.csv"))
data.table::fwrite(go_no_go, file.path(out_dir, "T14_nested_LODO_go_no_go.csv"))

saveRDS(model_objects, file.path(out_dir, "M14_nested_LODO_model_objects.rds"))

# Excel 汇总
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary")
openxlsx::writeData(wb, "summary", summary_df)

openxlsx::addWorksheet(wb, "go_no_go")
openxlsx::writeData(wb, "go_no_go", go_no_go)

openxlsx::addWorksheet(wb, "validation_metrics")
openxlsx::writeData(wb, "validation_metrics", validation_metrics)

openxlsx::addWorksheet(wb, "training_metrics")
openxlsx::writeData(wb, "training_metrics", training_metrics)

openxlsx::addWorksheet(wb, "selected_genes")
openxlsx::writeData(wb, "selected_genes", selected_genes_by_fold)

openxlsx::addWorksheet(wb, "gene_frequency")
openxlsx::writeData(wb, "gene_frequency", gene_frequency)

openxlsx::addWorksheet(wb, "calibration")
openxlsx::writeData(wb, "calibration", calibration_metrics)

openxlsx::addWorksheet(wb, "threshold_drift")
openxlsx::writeData(wb, "threshold_drift", threshold_drift)

openxlsx::addWorksheet(wb, "fold_candidates")
openxlsx::writeData(wb, "fold_candidates", fold_candidates)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T14_nested_LODO_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# 作图
# ============================================================

save_roc_plot(predictions)

save_metric_barplot(
  validation_metrics,
  metric = "AUROC",
  file_prefix = "F14B_nested_LODO_AUROC_barplot",
  title_text = "Nested LODO validation AUROC",
  y_label = "AUROC"
)

save_metric_barplot(
  validation_metrics,
  metric = "AUPRC",
  file_prefix = "F14C_nested_LODO_AUPRC_barplot",
  title_text = "Nested LODO validation AUPRC",
  y_label = "AUPRC"
)

save_metric_barplot(
  validation_metrics,
  metric = "Brier",
  file_prefix = "F14D_nested_LODO_Brier_barplot",
  title_text = "Nested LODO validation Brier score",
  y_label = "Brier score"
)

save_gene_frequency_plot(gene_frequency)
save_prediction_density(predictions)
save_threshold_drift_plot(threshold_drift)
save_observed_vs_predicted_plot(calibration_metrics)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_14_nested_LODO_full_pipeline.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 14 Nested LODO full pipeline 完成 ============")
message("输出目录：", out_dir)
message("图输出目录：", fig_dir)

message("\nNested LODO validation metrics:")
print(validation_metrics)

message("\nNested LODO summary:")
print(summary_df)

message("\nGo / No-Go:")
print(go_no_go)

message("\nGene selection frequency:")
print(gene_frequency)

message("\n关键输出：")
message("1) ", file.path(out_dir, "T14_nested_LODO_validation_metrics.csv"))
message("2) ", file.path(out_dir, "T14_nested_LODO_predictions.csv"))
message("3) ", file.path(out_dir, "T14_nested_LODO_selected_genes_by_fold.csv"))
message("4) ", file.path(out_dir, "T14_nested_LODO_gene_selection_frequency.csv"))
message("5) ", file.path(out_dir, "T14_nested_LODO_calibration_metrics.csv"))
message("6) ", file.path(out_dir, "T14_nested_LODO_threshold_drift.csv"))
message("7) ", file.path(out_dir, "T14_nested_LODO_summary.xlsx"))
message("8) ", file.path(out_dir, "M14_nested_LODO_model_objects.rds"))
message("9) ", fig_dir)