# ============================================================
# 16b_dataset_platform_classification_and_RNAseq_eligibility.R
# Local dataset platform classification and RNA-seq eligibility screening
#
# 当前目的：
# 1. 扫描本地 bulk expression matrix 和 phenotype 文件
# 2. 识别每个 GSE 数据集的表达矩阵、表型文件、分组、样本量和目标基因可用性
# 3. 初步判断 platform type:
#    microarray_likely / RNAseq_counts_likely / RNAseq_normalized_likely / unknown
# 4. 判断是否符合 RNA-seq external platform validation 最低条件
# 5. 输出 Go / No-Go 表格
#
# 注意：
# 本脚本不进行模型预测，不使用 RNA-seq 数据参与候选基因筛选、模型训练、标准化参数估计或阈值选择。
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

script_dir <- file.path(project_dir, "03_scripts")
expr_dir <- file.path(project_dir, "02_processed_data", "bulk_gene_matrix")
pheno_dir <- file.path(project_dir, "02_processed_data", "phenotype")
main_pheno_dir <- file.path(project_dir, "02_processed_data", "main_pheno")
rna_dir <- file.path(project_dir, "04_results", "RNAseq_validation")
report_dir <- file.path(project_dir, "04_results", "reporting")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(rna_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 目标基因
# ============================================================

final_10_genes <- c(
  "RAB31", "VNN1", "TCN1", "EMILIN2", "ZDHHC19",
  "ANKRD22", "PGLYRP1", "FAM20A", "RNASE3", "HK3"
)

nested_recurrent_genes <- c(
  "CD177", "TDRD9", "LILRA6", "RAB31", "VNN1",
  "ANKRD22", "ARG1", "C3AR1", "RNASE3", "WFDC1"
)

target_genes <- unique(c(final_10_genes, nested_recurrent_genes))

# ============================================================
# 本地主队列平台信息，防止误把 microarray 当 RNA-seq
# ============================================================

known_microarray_platforms <- data.frame(
  dataset = c(
    "GSE137340", "GSE236713", "GSE54514",
    "GSE57065", "GSE65682", "GSE95233",
    "GSE26378", "GSE28750"
  ),
  known_platform = c(
    "GPL10558", "GPL17077", "GPL6947",
    "GPL570", "GPL13667", "GPL570",
    "GPL570", "GPL570"
  ),
  known_platform_type = "microarray_known",
  stringsAsFactors = FALSE
)

# 可扩展的人工注释表
manual_annotation_file <- file.path(rna_dir, "T16b_manual_dataset_annotation_template.csv")

# ============================================================
# 工具函数
# ============================================================

safe_fread <- function(file, nrows = Inf, select = NULL) {
  out <- tryCatch(
    {
      data.table::fread(
        file,
        nrows = nrows,
        select = select,
        data.table = FALSE,
        check.names = FALSE,
        fill = TRUE,
        showProgress = FALSE
      )
    },
    error = function(e) NULL,
    warning = function(w) {
      suppressWarnings(
        data.table::fread(
          file,
          nrows = nrows,
          select = select,
          data.table = FALSE,
          check.names = FALSE,
          fill = TRUE,
          showProgress = FALSE
        )
      )
    }
  )
  
  out
}

extract_dataset_id <- function(x) {
  m <- regmatches(x, regexpr("GSE[0-9]+", x))
  if (length(m) == 0 || is.na(m) || m == "") return(NA_character_)
  m
}

is_numeric_like <- function(x) {
  suppressWarnings({
    y <- as.numeric(x)
  })
  mean(!is.na(y)) >= 0.8
}

detect_gene_col <- function(df, target_genes = character()) {
  if (is.null(df) || ncol(df) == 0) return(NA_character_)
  
  cn <- colnames(df)
  lower_cn <- tolower(cn)
  
  priority_names <- c(
    "gene_symbol", "genesymbol", "symbol", "gene", "genes",
    "hgnc_symbol", "external_gene_name", "feature", "id",
    "gene_id", "ensembl_gene_id", "x", "v1"
  )
  
  hit <- which(lower_cn %in% priority_names)
  if (length(hit) > 0) return(cn[hit[1]])
  
  n_check <- min(ncol(df), 5)
  scores <- numeric(n_check)
  
  for (j in seq_len(n_check)) {
    v <- as.character(df[[j]])
    v <- gsub("\\s+", "", v)
    scores[j] <- mean(v %in% target_genes, na.rm = TRUE)
  }
  
  if (max(scores, na.rm = TRUE) > 0) {
    return(cn[which.max(scores)])
  }
  
  # fallback: 第一列通常是 gene symbol / probe / feature id
  first_col_numeric <- is_numeric_like(df[[1]])
  if (!first_col_numeric) return(cn[1])
  
  NA_character_
}

get_numeric_sample_cols <- function(df, gene_col = NA_character_) {
  if (is.null(df) || ncol(df) == 0) return(character())
  
  cn <- colnames(df)
  candidate_cols <- cn
  
  if (!is.na(gene_col) && gene_col %in% candidate_cols) {
    candidate_cols <- setdiff(candidate_cols, gene_col)
  }
  
  numeric_cols <- character()
  
  for (cc in candidate_cols) {
    x <- df[[cc]]
    if (is_numeric_like(x)) {
      numeric_cols <- c(numeric_cols, cc)
    }
  }
  
  numeric_cols
}

guess_expression_scale <- function(df, sample_cols) {
  default <- list(
    expression_scale_guess = "not_expression_or_unreadable",
    numeric_min = NA_real_,
    numeric_median = NA_real_,
    numeric_max = NA_real_,
    integer_like_fraction = NA_real_,
    negative_fraction = NA_real_,
    zero_fraction = NA_real_
  )
  
  if (is.null(df) || length(sample_cols) == 0) return(default)
  
  cols_use <- sample_cols[seq_len(min(length(sample_cols), 30))]
  
  values <- unlist(df[, cols_use, drop = FALSE], use.names = FALSE)
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  
  if (length(values) < 100) return(default)
  
  numeric_min <- min(values, na.rm = TRUE)
  numeric_median <- median(values, na.rm = TRUE)
  numeric_max <- max(values, na.rm = TRUE)
  integer_like_fraction <- mean(abs(values - round(values)) < 1e-8, na.rm = TRUE)
  negative_fraction <- mean(values < 0, na.rm = TRUE)
  zero_fraction <- mean(values == 0, na.rm = TRUE)
  
  scale_guess <- "unknown_numeric_matrix"
  
  if (
    numeric_min >= 0 &&
    integer_like_fraction > 0.95 &&
    zero_fraction > 0.05 &&
    numeric_max > 50
  ) {
    scale_guess <- "RNAseq_raw_counts_like"
  } else if (
    numeric_min >= 0 &&
    numeric_median <= 50 &&
    numeric_max > 50 &&
    integer_like_fraction < 0.95
  ) {
    scale_guess <- "RNAseq_TPM_FPKM_or_normalized_nonlog_likely"
  } else if (
    numeric_min >= 0 &&
    numeric_median <= 20 &&
    numeric_max <= 30 &&
    zero_fraction < 0.80
  ) {
    scale_guess <- "log2_or_log_normalized_likely"
  } else if (
    negative_fraction > 0.05 &&
    numeric_min < 0 &&
    numeric_max < 30
  ) {
    scale_guess <- "centered_or_batch_adjusted_log_matrix_likely"
  }
  
  list(
    expression_scale_guess = scale_guess,
    numeric_min = numeric_min,
    numeric_median = numeric_median,
    numeric_max = numeric_max,
    integer_like_fraction = integer_like_fraction,
    negative_fraction = negative_fraction,
    zero_fraction = zero_fraction
  )
}

detect_target_genes_full <- function(file, gene_col_guess, target_genes) {
  result <- list(
    gene_col_used = NA_character_,
    n_features = NA_integer_,
    detected_target_genes = character(),
    n_detected_target_genes = 0L
  )
  
  header_df <- safe_fread(file, nrows = 5)
  if (is.null(header_df) || ncol(header_df) == 0) return(result)
  
  cn <- colnames(header_df)
  
  gene_col_use <- gene_col_guess
  
  if (is.na(gene_col_use) || !gene_col_use %in% cn) {
    gene_col_use <- detect_gene_col(header_df, target_genes)
  }
  
  if (is.na(gene_col_use) || !gene_col_use %in% cn) {
    return(result)
  }
  
  one_col <- safe_fread(file, select = gene_col_use)
  if (is.null(one_col) || ncol(one_col) == 0) return(result)
  
  genes <- as.character(one_col[[1]])
  genes <- gsub("\\s+", "", genes)
  
  detected <- intersect(target_genes, genes)
  
  result$gene_col_used <- gene_col_use
  result$n_features <- length(unique(genes[!is.na(genes) & genes != ""]))
  result$detected_target_genes <- detected
  result$n_detected_target_genes <- length(detected)
  
  result
}

find_pheno_file <- function(dataset) {
  candidates <- c(
    file.path(main_pheno_dir, paste0(dataset, "_main_pheno.csv")),
    file.path(pheno_dir, paste0(dataset, "_pheno_harmonized.csv")),
    file.path(pheno_dir, paste0(dataset, "_pheno.csv"))
  )
  
  candidates[file.exists(candidates)][1]
}

infer_group_column <- function(pheno) {
  if (is.null(pheno) || ncol(pheno) == 0) return(NA_character_)
  
  candidates <- c(
    "clinical_group_main", "clinical_group", "group",
    "condition", "disease", "disease_status",
    "phenotype", "status", "diagnosis"
  )
  
  hit <- candidates[candidates %in% colnames(pheno)]
  if (length(hit) > 0) return(hit[1])
  
  # fallback: 找含 sepsis/control/healthy/sirs 的列
  scores <- sapply(colnames(pheno), function(cc) {
    x <- tolower(as.character(pheno[[cc]]))
    mean(grepl("sepsis|septic|control|healthy|sirs", x), na.rm = TRUE)
  })
  
  if (length(scores) > 0 && max(scores, na.rm = TRUE) > 0.10) {
    return(names(scores)[which.max(scores)])
  }
  
  NA_character_
}

infer_sample_id_column <- function(pheno) {
  if (is.null(pheno) || ncol(pheno) == 0) return(NA_character_)
  
  candidates <- c("sample_id", "geo_accession", "gsm", "GSM", "sample", "Sample", "id")
  hit <- candidates[candidates %in% colnames(pheno)]
  if (length(hit) > 0) return(hit[1])
  
  scores <- sapply(colnames(pheno), function(cc) {
    x <- as.character(pheno[[cc]])
    mean(grepl("^GSM[0-9]+", x), na.rm = TRUE)
  })
  
  if (length(scores) > 0 && max(scores, na.rm = TRUE) > 0.10) {
    return(names(scores)[which.max(scores)])
  }
  
  NA_character_
}

summarize_pheno <- function(pheno_file) {
  default <- list(
    pheno_readable = FALSE,
    pheno_n_rows = NA_integer_,
    sample_id_col = NA_character_,
    group_col = NA_character_,
    n_sepsis = NA_integer_,
    n_control = NA_integer_,
    n_healthy = NA_integer_,
    n_sirs = NA_integer_,
    n_other_or_unknown = NA_integer_,
    has_sepsis_control_contrast = FALSE,
    has_sirs_or_clinical_mimic = FALSE,
    sample_ids = character(),
    group_values_collapsed = NA_character_
  )
  
  if (is.na(pheno_file) || length(pheno_file) == 0 || !file.exists(pheno_file)) {
    return(default)
  }
  
  pheno <- safe_fread(pheno_file)
  if (is.null(pheno) || nrow(pheno) == 0) return(default)
  
  sample_col <- infer_sample_id_column(pheno)
  group_col <- infer_group_column(pheno)
  
  sample_ids <- character()
  if (!is.na(sample_col) && sample_col %in% colnames(pheno)) {
    sample_ids <- as.character(pheno[[sample_col]])
  }
  
  n_sepsis <- NA_integer_
  n_control <- NA_integer_
  n_healthy <- NA_integer_
  n_sirs <- NA_integer_
  n_other <- NA_integer_
  has_contrast <- FALSE
  has_sirs <- FALSE
  group_values_collapsed <- NA_character_
  
  if (!is.na(group_col) && group_col %in% colnames(pheno)) {
    g <- tolower(as.character(pheno[[group_col]]))
    
    is_sepsis <- grepl("sepsis|septic", g) & !grepl("non.?sepsis|nonsepsis", g)
    is_control <- grepl("control|healthy|normal", g)
    is_healthy <- grepl("healthy", g)
    is_sirs <- grepl("sirs", g)
    
    n_sepsis <- sum(is_sepsis, na.rm = TRUE)
    n_control <- sum(is_control, na.rm = TRUE)
    n_healthy <- sum(is_healthy, na.rm = TRUE)
    n_sirs <- sum(is_sirs, na.rm = TRUE)
    n_other <- length(g) - sum(is_sepsis | is_control | is_sirs, na.rm = TRUE)
    
    has_contrast <- n_sepsis > 0 && n_control > 0
    has_sirs <- n_sirs > 0
    
    vals <- unique(as.character(pheno[[group_col]]))
    vals <- vals[!is.na(vals) & vals != ""]
    group_values_collapsed <- paste(head(vals, 20), collapse = "; ")
  }
  
  list(
    pheno_readable = TRUE,
    pheno_n_rows = nrow(pheno),
    sample_id_col = sample_col,
    group_col = group_col,
    n_sepsis = n_sepsis,
    n_control = n_control,
    n_healthy = n_healthy,
    n_sirs = n_sirs,
    n_other_or_unknown = n_other,
    has_sepsis_control_contrast = has_contrast,
    has_sirs_or_clinical_mimic = has_sirs,
    sample_ids = sample_ids,
    group_values_collapsed = group_values_collapsed
  )
}

classify_platform <- function(dataset, file_name, file_path, scale_guess, known_platform_type) {
  
  text <- tolower(paste(file_name, file_path, sep = " "))
  
  if (!is.na(known_platform_type) && known_platform_type == "microarray_known") {
    return("microarray_known")
  }
  
  if (grepl("rnaseq|rna_seq|rna-seq|counts|count_matrix|tpm|fpkm|htseq|star|salmon|kallisto", text)) {
    if (scale_guess == "RNAseq_raw_counts_like") {
      return("RNAseq_counts_likely")
    }
    if (scale_guess %in% c("RNAseq_TPM_FPKM_or_normalized_nonlog_likely", "log2_or_log_normalized_likely")) {
      return("RNAseq_normalized_likely")
    }
    return("RNAseq_name_based_candidate")
  }
  
  if (scale_guess == "RNAseq_raw_counts_like") {
    return("RNAseq_counts_likely")
  }
  
  if (scale_guess == "RNAseq_TPM_FPKM_or_normalized_nonlog_likely") {
    return("RNAseq_normalized_possible")
  }
  
  if (scale_guess %in% c("log2_or_log_normalized_likely", "centered_or_batch_adjusted_log_matrix_likely")) {
    return("microarray_or_log_normalized_unknown")
  }
  
  "unknown"
}

make_eligibility_decision <- function(platform_class, n_total, n_sepsis, n_control, n_detected_genes, has_contrast) {
  
  is_rnaseq <- platform_class %in% c(
    "RNAseq_counts_likely",
    "RNAseq_normalized_likely",
    "RNAseq_name_based_candidate"
  )
  
  is_possible_rnaseq <- platform_class %in% c(
    "RNAseq_normalized_possible"
  )
  
  eligible_strict <- is_rnaseq &&
    isTRUE(has_contrast) &&
    !is.na(n_total) &&
    n_total >= 30 &&
    !is.na(n_detected_genes) &&
    n_detected_genes >= 7 &&
    !is.na(n_sepsis) &&
    n_sepsis > 0 &&
    !is.na(n_control) &&
    n_control > 0
  
  eligible_possible <- is_possible_rnaseq &&
    isTRUE(has_contrast) &&
    !is.na(n_total) &&
    n_total >= 30 &&
    !is.na(n_detected_genes) &&
    n_detected_genes >= 7
  
  if (eligible_strict) {
    return("Eligible_for_16_RNAseq_validation")
  }
  
  if (eligible_possible) {
    return("Possible_RNAseq_requires_manual_verification")
  }
  
  if (!is_rnaseq && !is_possible_rnaseq) {
    return("Not_eligible_platform_not_RNAseq_or_unclear")
  }
  
  if (!isTRUE(has_contrast)) {
    return("Not_eligible_no_clear_sepsis_control_contrast")
  }
  
  if (is.na(n_total) || n_total < 30) {
    return("Not_eligible_sample_size_lt_30_or_unknown")
  }
  
  if (is.na(n_detected_genes) || n_detected_genes < 7) {
    return("Not_eligible_target_genes_lt_7")
  }
  
  "Not_eligible_other"
}

collapse_reason <- function(platform_class, n_total, n_sepsis, n_control, n_detected_genes, has_contrast) {
  reasons <- character()
  
  if (!platform_class %in% c(
    "RNAseq_counts_likely",
    "RNAseq_normalized_likely",
    "RNAseq_name_based_candidate",
    "RNAseq_normalized_possible"
  )) {
    reasons <- c(reasons, "platform_not_RNAseq_or_unclear")
  }
  
  if (!isTRUE(has_contrast)) {
    reasons <- c(reasons, "no_clear_sepsis_control_contrast")
  }
  
  if (is.na(n_total) || n_total < 30) {
    reasons <- c(reasons, "sample_size_lt_30_or_unknown")
  }
  
  if (is.na(n_detected_genes) || n_detected_genes < 7) {
    reasons <- c(reasons, "target_genes_lt_7")
  }
  
  if (length(reasons) == 0) reasons <- "meets_minimum_screening_criteria"
  paste(reasons, collapse = ";")
}

# ============================================================
# 扫描表达矩阵
# ============================================================

message("Scanning local expression matrix files...")

expr_files <- list.files(
  expr_dir,
  pattern = "_expr\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# 也扫描 RNAseq_validation 和 Downloads 中可能存在的表达文件
extra_dirs <- c(
  file.path(project_dir, "01_raw_data"),
  file.path(project_dir, "02_processed_data"),
  "/Users/felix/Downloads"
)

extra_files <- character()

for (d in extra_dirs) {
  if (dir.exists(d)) {
    z <- list.files(
      d,
      pattern = "\\.(csv|tsv|txt|csv\\.gz|tsv\\.gz|txt\\.gz)$",
      full.names = TRUE,
      recursive = TRUE
    )
    z <- z[grepl("expr|expression|count|counts|tpm|fpkm|rnaseq|rna_seq|RNAseq|matrix", basename(z), ignore.case = TRUE)]
    extra_files <- c(extra_files, z)
  }
}

expr_files <- unique(c(expr_files, extra_files))
expr_files <- expr_files[file.exists(expr_files)]

if (length(expr_files) == 0) {
  stop("没有发现候选表达矩阵文件。")
}

inventory <- data.frame(
  file_path = expr_files,
  file_name = basename(expr_files),
  dataset = vapply(expr_files, extract_dataset_id, character(1)),
  size_mb = round(file.info(expr_files)$size / 1024^2, 3),
  stringsAsFactors = FALSE
)

inventory <- inventory[!is.na(inventory$dataset), , drop = FALSE]

if (nrow(inventory) == 0) {
  stop("候选表达矩阵中没有识别到 GSE 编号。")
}

# 每个 dataset 优先选择 02_processed_data/bulk_gene_matrix 下的 *_expr.csv
inventory$priority <- 99
inventory$priority[grepl("/bulk_gene_matrix/", inventory$file_path) & grepl("_expr\\.csv$", inventory$file_name)] <- 1
inventory$priority[grepl("rnaseq|rna_seq|rna-seq|count|tpm|fpkm", inventory$file_name, ignore.case = TRUE)] <- 2

inventory <- inventory[order(inventory$dataset, inventory$priority, -inventory$size_mb), , drop = FALSE]

selected_inventory <- inventory[!duplicated(inventory$dataset), , drop = FALSE]

# ============================================================
# 逐数据集分类
# ============================================================

message("Classifying dataset platform and RNA-seq eligibility...")

result_list <- list()

for (i in seq_len(nrow(selected_inventory))) {
  
  f <- selected_inventory$file_path[i]
  dataset <- selected_inventory$dataset[i]
  fname <- selected_inventory$file_name[i]
  
  message("Processing dataset: ", dataset)
  
  preview <- safe_fread(f, nrows = 120)
  
  readable <- !is.null(preview) && nrow(preview) > 0 && ncol(preview) > 0
  
  gene_col_guess <- NA_character_
  sample_cols <- character()
  scale_info <- list(
    expression_scale_guess = "not_expression_or_unreadable",
    numeric_min = NA_real_,
    numeric_median = NA_real_,
    numeric_max = NA_real_,
    integer_like_fraction = NA_real_,
    negative_fraction = NA_real_,
    zero_fraction = NA_real_
  )
  
  if (readable) {
    gene_col_guess <- detect_gene_col(preview, target_genes)
    sample_cols <- get_numeric_sample_cols(preview, gene_col_guess)
    scale_info <- guess_expression_scale(preview, sample_cols)
  }
  
  gene_detect <- detect_target_genes_full(f, gene_col_guess, target_genes)
  
  pheno_file <- find_pheno_file(dataset)
  pheno_info <- summarize_pheno(pheno_file)
  
  known <- known_microarray_platforms[known_microarray_platforms$dataset == dataset, , drop = FALSE]
  known_platform <- ifelse(nrow(known) > 0, known$known_platform[1], NA_character_)
  known_platform_type <- ifelse(nrow(known) > 0, known$known_platform_type[1], NA_character_)
  
  platform_class <- classify_platform(
    dataset = dataset,
    file_name = fname,
    file_path = f,
    scale_guess = scale_info$expression_scale_guess,
    known_platform_type = known_platform_type
  )
  
  # 样本匹配
  expr_sample_cols <- sample_cols
  sample_ids <- pheno_info$sample_ids
  
  n_expr_sample_cols <- length(expr_sample_cols)
  n_matched_samples <- if (length(sample_ids) > 0) {
    sum(expr_sample_cols %in% sample_ids)
  } else {
    NA_integer_
  }
  
  n_total <- if (!is.na(pheno_info$pheno_n_rows)) {
    pheno_info$pheno_n_rows
  } else {
    n_expr_sample_cols
  }
  
  eligibility <- make_eligibility_decision(
    platform_class = platform_class,
    n_total = n_total,
    n_sepsis = pheno_info$n_sepsis,
    n_control = pheno_info$n_control,
    n_detected_genes = gene_detect$n_detected_target_genes,
    has_contrast = pheno_info$has_sepsis_control_contrast
  )
  
  reason <- collapse_reason(
    platform_class = platform_class,
    n_total = n_total,
    n_sepsis = pheno_info$n_sepsis,
    n_control = pheno_info$n_control,
    n_detected_genes = gene_detect$n_detected_target_genes,
    has_contrast = pheno_info$has_sepsis_control_contrast
  )
  
  result_list[[dataset]] <- data.frame(
    dataset = dataset,
    expression_file = f,
    expression_file_name = fname,
    expression_size_mb = selected_inventory$size_mb[i],
    expression_readable_preview = readable,
    n_preview_rows = ifelse(readable, nrow(preview), NA_integer_),
    n_preview_cols = ifelse(readable, ncol(preview), NA_integer_),
    gene_col_guess = gene_col_guess,
    gene_col_used_for_detection = gene_detect$gene_col_used,
    n_features_detected = gene_detect$n_features,
    n_numeric_sample_like_cols = n_expr_sample_cols,
    expression_scale_guess = scale_info$expression_scale_guess,
    numeric_min = scale_info$numeric_min,
    numeric_median = scale_info$numeric_median,
    numeric_max = scale_info$numeric_max,
    integer_like_fraction = scale_info$integer_like_fraction,
    negative_fraction = scale_info$negative_fraction,
    zero_fraction = scale_info$zero_fraction,
    known_platform = known_platform,
    known_platform_type = known_platform_type,
    platform_class = platform_class,
    pheno_file = ifelse(is.na(pheno_file), NA_character_, pheno_file),
    pheno_readable = pheno_info$pheno_readable,
    pheno_n_rows = pheno_info$pheno_n_rows,
    pheno_sample_id_col = pheno_info$sample_id_col,
    pheno_group_col = pheno_info$group_col,
    n_expr_sample_cols = n_expr_sample_cols,
    n_matched_samples_between_expr_and_pheno = n_matched_samples,
    n_total_inferred = n_total,
    n_sepsis = pheno_info$n_sepsis,
    n_control = pheno_info$n_control,
    n_healthy = pheno_info$n_healthy,
    n_sirs = pheno_info$n_sirs,
    n_other_or_unknown = pheno_info$n_other_or_unknown,
    has_sepsis_control_contrast = pheno_info$has_sepsis_control_contrast,
    has_sirs_or_clinical_mimic = pheno_info$has_sirs_or_clinical_mimic,
    group_values_collapsed = pheno_info$group_values_collapsed,
    detected_target_genes = paste(gene_detect$detected_target_genes, collapse = ";"),
    n_detected_target_genes = gene_detect$n_detected_target_genes,
    n_final10_detected = length(intersect(final_10_genes, gene_detect$detected_target_genes)),
    n_nested_recurrent_detected = length(intersect(nested_recurrent_genes, gene_detect$detected_target_genes)),
    RNAseq_validation_eligibility = eligibility,
    exclusion_or_review_reason = reason,
    stringsAsFactors = FALSE
  )
}

eligibility_table <- do.call(rbind, result_list)
rownames(eligibility_table) <- NULL

# ============================================================
# Gene availability long table
# ============================================================

gene_availability_list <- list()

for (i in seq_len(nrow(eligibility_table))) {
  ds <- eligibility_table$dataset[i]
  detected <- unlist(strsplit(eligibility_table$detected_target_genes[i], ";", fixed = TRUE))
  detected <- detected[detected != ""]
  
  gene_availability_list[[ds]] <- data.frame(
    dataset = ds,
    gene_symbol = target_genes,
    detected = target_genes %in% detected,
    gene_set = ifelse(
      target_genes %in% final_10_genes & target_genes %in% nested_recurrent_genes,
      "Final10_and_nested_recurrent",
      ifelse(
        target_genes %in% final_10_genes,
        "Final10_only",
        "Nested_recurrent_only"
      )
    ),
    stringsAsFactors = FALSE
  )
}

gene_availability <- do.call(rbind, gene_availability_list)
rownames(gene_availability) <- NULL

# ============================================================
# Go / No-Go summary
# ============================================================

eligible <- eligibility_table[
  eligibility_table$RNAseq_validation_eligibility == "Eligible_for_16_RNAseq_validation",
  ,
  drop = FALSE
]

possible <- eligibility_table[
  eligibility_table$RNAseq_validation_eligibility == "Possible_RNAseq_requires_manual_verification",
  ,
  drop = FALSE
]

go_no_go_summary <- data.frame(
  metric = c(
    "n_datasets_scanned",
    "n_known_microarray_datasets",
    "n_RNAseq_counts_likely",
    "n_RNAseq_normalized_likely",
    "n_RNAseq_normalized_possible",
    "n_eligible_for_16_RNAseq_validation",
    "n_possible_requires_manual_verification",
    "eligible_datasets",
    "possible_datasets",
    "go_no_go_decision",
    "recommended_next_step"
  ),
  value = c(
    nrow(eligibility_table),
    sum(eligibility_table$platform_class == "microarray_known", na.rm = TRUE),
    sum(eligibility_table$platform_class == "RNAseq_counts_likely", na.rm = TRUE),
    sum(eligibility_table$platform_class == "RNAseq_normalized_likely", na.rm = TRUE),
    sum(eligibility_table$platform_class == "RNAseq_normalized_possible", na.rm = TRUE),
    nrow(eligible),
    nrow(possible),
    paste(eligible$dataset, collapse = ";"),
    paste(possible$dataset, collapse = ";"),
    ifelse(
      nrow(eligible) > 0,
      "GO_directly_to_16_RNAseq_external_platform_validation",
      ifelse(
        nrow(possible) > 0,
        "MANUAL_REVIEW_BEFORE_16",
        "NO_GO_no_eligible_RNAseq_dataset_detected"
      )
    ),
    ifelse(
      nrow(eligible) > 0,
      "Generate and run 16_RNAseq_external_platform_validation.R for eligible datasets.",
      ifelse(
        nrow(possible) > 0,
        "Manually verify possible RNA-seq datasets using GEO page, original paper, and expression file provenance before running validation.",
        "Record attempted RNA-seq screening transparently and proceed to final_freeze without RNA-seq validation."
      )
    )
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 人工核对模板
# ============================================================

manual_review_template <- eligibility_table[, c(
  "dataset",
  "expression_file_name",
  "platform_class",
  "known_platform",
  "expression_scale_guess",
  "n_total_inferred",
  "n_sepsis",
  "n_control",
  "n_sirs",
  "n_detected_target_genes",
  "RNAseq_validation_eligibility",
  "exclusion_or_review_reason"
), drop = FALSE]

manual_review_template$manual_platform_confirmed <- ""
manual_review_template$manual_sample_type <- ""
manual_review_template$manual_timepoint <- ""
manual_review_template$manual_adult_or_mixed <- ""
manual_review_template$manual_control_type <- ""
manual_review_template$manual_include_for_16 <- ""
manual_review_template$manual_notes <- ""

# ============================================================
# 保存输出
# ============================================================

message("Saving 16b outputs...")

data.table::fwrite(
  eligibility_table,
  file.path(rna_dir, "T16b_dataset_platform_classification.csv")
)

data.table::fwrite(
  eligibility_table,
  file.path(rna_dir, "T16b_RNAseq_eligibility_table.csv")
)

data.table::fwrite(
  gene_availability,
  file.path(rna_dir, "T16b_target_gene_availability_by_dataset.csv")
)

data.table::fwrite(
  go_no_go_summary,
  file.path(rna_dir, "T16b_RNAseq_go_no_go_summary.csv")
)

data.table::fwrite(
  manual_review_template,
  manual_annotation_file
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "go_no_go_summary")
openxlsx::writeData(wb, "go_no_go_summary", go_no_go_summary)

openxlsx::addWorksheet(wb, "eligibility_table")
openxlsx::writeData(wb, "eligibility_table", eligibility_table)

openxlsx::addWorksheet(wb, "gene_availability")
openxlsx::writeData(wb, "gene_availability", gene_availability)

openxlsx::addWorksheet(wb, "manual_review_template")
openxlsx::writeData(wb, "manual_review_template", manual_review_template)

openxlsx::saveWorkbook(
  wb,
  file.path(rna_dir, "T16b_RNAseq_go_no_go_summary.xlsx"),
  overwrite = TRUE
)

sink(file.path(log_dir, "sessionInfo_16b_dataset_platform_classification_and_RNAseq_eligibility.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 16b RNA-seq eligibility screening 完成 ============")
message("输出目录：", rna_dir)

message("\nGo / No-Go summary:")
print(go_no_go_summary)

message("\nEligible datasets:")
if (nrow(eligible) > 0) {
  print(eligible[, c(
    "dataset",
    "platform_class",
    "expression_scale_guess",
    "n_total_inferred",
    "n_sepsis",
    "n_control",
    "n_detected_target_genes",
    "RNAseq_validation_eligibility"
  ), drop = FALSE])
} else {
  message("未发现可直接进入 16_RNAseq_external_platform_validation.R 的 RNA-seq 数据集。")
}

message("\nPossible datasets requiring manual verification:")
if (nrow(possible) > 0) {
  print(possible[, c(
    "dataset",
    "platform_class",
    "expression_scale_guess",
    "n_total_inferred",
    "n_sepsis",
    "n_control",
    "n_detected_target_genes",
    "RNAseq_validation_eligibility",
    "exclusion_or_review_reason"
  ), drop = FALSE])
} else {
  message("未发现需要人工核对的 possible RNA-seq 数据集。")
}

message("\nTop eligibility table:")
print(
  eligibility_table[
    order(
      eligibility_table$RNAseq_validation_eligibility,
      -eligibility_table$n_detected_target_genes,
      -eligibility_table$n_total_inferred
    ),
    c(
      "dataset",
      "platform_class",
      "known_platform",
      "expression_scale_guess",
      "n_total_inferred",
      "n_sepsis",
      "n_control",
      "n_sirs",
      "n_detected_target_genes",
      "detected_target_genes",
      "RNAseq_validation_eligibility",
      "exclusion_or_review_reason",
      "expression_file"
    ),
    drop = FALSE
  ]
)

message("\n关键输出：")
message("1) ", file.path(rna_dir, "T16b_dataset_platform_classification.csv"))
message("2) ", file.path(rna_dir, "T16b_RNAseq_eligibility_table.csv"))
message("3) ", file.path(rna_dir, "T16b_target_gene_availability_by_dataset.csv"))
message("4) ", file.path(rna_dir, "T16b_RNAseq_go_no_go_summary.xlsx"))
message("5) ", manual_annotation_file)

message("\n下一步：")
message("把 Go / No-Go summary、Eligible datasets、Possible datasets requiring manual verification、Top eligibility table 贴给我。")
message("我会判断是否继续生成 16_RNAseq_external_platform_validation.R。")