# ============================================================
# 16a_scan_local_RNAseq_files.R
# Scan local files for possible bulk RNA-seq validation datasets
#
# 稳定版：
# 1. 不使用 invokeRestart("muffleWarning")
# 2. 跳过 GEO series matrix、GPL、SOFT、已生成结果目录
# 3. 只扫描可能的 RNA-seq 表达矩阵 / phenotype 文件
# 4. 输出候选文件清单，供 16_RNAseq_external_platform_validation.R 使用
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

out_dir <- file.path(project_dir, "04_results", "RNAseq_validation")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 扫描目录
# ============================================================

scan_dirs <- c(
  file.path(project_dir, "01_raw_data"),
  file.path(project_dir, "02_processed_data"),
  "/Users/felix/Downloads"
)

scan_dirs <- unique(scan_dirs[file.exists(scan_dirs)])

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
# 工具函数
# ============================================================

safe_file_info <- function(files) {
  if (length(files) == 0) return(data.frame())
  
  info <- file.info(files)
  
  data.frame(
    file_path = files,
    file_name = basename(files),
    dir_name = dirname(files),
    extension = tolower(tools::file_ext(files)),
    size_mb = round(info$size / 1024^2, 3),
    modified_time = as.character(info$mtime),
    stringsAsFactors = FALSE
  )
}

guess_file_role_from_name <- function(file_name) {
  x <- tolower(file_name)
  
  if (grepl("series_matrix|soft|gpl|platform|annot|annotation", x)) {
    return("geo_annotation_or_series_matrix")
  }
  
  if (grepl("pheno|phenotype|clinical|metadata|meta|sample|group|design|condition", x)) {
    return("phenotype_or_metadata")
  }
  
  if (grepl("count|counts|rawcount|raw_count|readcount|read_count", x)) {
    return("expression_raw_counts_candidate")
  }
  
  if (grepl("tpm", x)) {
    return("expression_TPM_candidate")
  }
  
  if (grepl("fpkm|rpkm", x)) {
    return("expression_FPKM_RPKM_candidate")
  }
  
  if (grepl("expr|expression|matrix|normalized|norm|vst|logcpm|log2|rsem|salmon|kallisto", x)) {
    return("expression_normalized_candidate")
  }
  
  "unknown"
}

should_skip_file <- function(file_path, file_name, role_guess) {
  x <- tolower(file_name)
  p <- tolower(file_path)
  
  if (role_guess == "geo_annotation_or_series_matrix") return(TRUE)
  
  skip_patterns <- c(
    "/geo_series_matrix/",
    "/platform_annotation/",
    "/eset_rds/",
    "/main_expr/",
    "/main_pheno/",
    "/phenotype/",
    "/differential_expression/",
    "/nested_lodo/",
    "/reporting/",
    "/candidate_selection/",
    "/enrichment/",
    "/final_model/",
    "/model_validation/",
    "/sensitivity/",
    "/logs/",
    "/03_scripts/",
    "series_matrix",
    "\\.soft",
    "gpl[0-9]+",
    "sessioninfo"
  )
  
  any(grepl(paste(skip_patterns, collapse = "|"), p)) ||
    any(grepl(paste(skip_patterns, collapse = "|"), x))
}

safe_read_preview <- function(file_path, n_max = 80) {
  
  out <- try({
    
    if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
      sheets <- openxlsx::getSheetNames(file_path)
      if (length(sheets) == 0) return(NULL)
      df <- openxlsx::read.xlsx(file_path, sheet = sheets[1], rows = 1:(n_max + 1))
      return(as.data.frame(df))
    }
    
    if (grepl("\\.csv$|\\.tsv$|\\.txt$|\\.csv\\.gz$|\\.tsv\\.gz$|\\.txt\\.gz$",
              file_path,
              ignore.case = TRUE)) {
      df <- suppressWarnings(
        data.table::fread(
          file_path,
          nrows = n_max,
          data.table = FALSE,
          showProgress = FALSE,
          fill = TRUE,
          blank.lines.skip = TRUE
        )
      )
      return(as.data.frame(df))
    }
    
    NULL
  }, silent = TRUE)
  
  if (inherits(out, "try-error")) return(NULL)
  out
}

detect_gene_column <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(NA_character_)
  
  cn <- colnames(df)
  cn_low <- tolower(cn)
  
  priority_patterns <- c(
    "^gene$",
    "gene_symbol",
    "genesymbol",
    "symbol",
    "hgnc",
    "external_gene_name",
    "gene_name",
    "geneid",
    "gene_id",
    "ensembl",
    "feature"
  )
  
  for (pat in priority_patterns) {
    hit <- which(grepl(pat, cn_low))
    if (length(hit) > 0) return(cn[hit[1]])
  }
  
  first_col <- as.character(df[[1]])
  
  n_gene_like <- sum(
    grepl("^ENSG", first_col) |
      grepl("^[A-Za-z][A-Za-z0-9\\-\\.]{1,15}$", first_col),
    na.rm = TRUE
  )
  
  if (n_gene_like >= max(3, floor(0.3 * length(first_col)))) {
    return(cn[1])
  }
  
  NA_character_
}

detect_sample_like_columns <- function(df, gene_col = NA_character_) {
  if (is.null(df) || ncol(df) == 0) return(character())
  
  cn <- colnames(df)
  candidate_cols <- setdiff(cn, gene_col)
  sample_like <- character()
  
  for (cc in candidate_cols) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    prop_numeric <- mean(!is.na(x))
    if (is.finite(prop_numeric) && prop_numeric >= 0.6) {
      sample_like <- c(sample_like, cc)
    }
  }
  
  sample_like
}

empty_scale_info <- function(label) {
  list(
    expression_scale_guess = label,
    numeric_min = NA_real_,
    numeric_median = NA_real_,
    numeric_max = NA_real_,
    integer_like_fraction = NA_real_,
    negative_fraction = NA_real_,
    zero_fraction = NA_real_
  )
}

guess_expression_scale <- function(df, sample_cols) {
  if (is.null(df) || length(sample_cols) == 0) {
    return(empty_scale_info("not_expression_or_unreadable"))
  }
  
  sub <- df[, sample_cols, drop = FALSE]
  vals <- suppressWarnings(as.numeric(unlist(sub, use.names = FALSE)))
  vals <- vals[is.finite(vals)]
  
  if (length(vals) == 0) {
    return(empty_scale_info("not_numeric"))
  }
  
  if (length(vals) > 100000) {
    set.seed(20260507)
    vals <- sample(vals, 100000)
  }
  
  numeric_min <- min(vals, na.rm = TRUE)
  numeric_median <- median(vals, na.rm = TRUE)
  numeric_max <- max(vals, na.rm = TRUE)
  integer_like_fraction <- mean(abs(vals - round(vals)) < 1e-8, na.rm = TRUE)
  negative_fraction <- mean(vals < 0, na.rm = TRUE)
  zero_fraction <- mean(vals == 0, na.rm = TRUE)
  
  guess <- "unknown_numeric_matrix"
  
  if (integer_like_fraction > 0.95 && numeric_min >= 0 && numeric_max > 50) {
    guess <- "raw_counts_likely"
  } else if (numeric_min >= 0 && numeric_max > 50 && integer_like_fraction <= 0.95) {
    guess <- "TPM_FPKM_or_normalized_nonlog_likely"
  } else if (numeric_min >= 0 && numeric_max <= 30 && numeric_median <= 10) {
    guess <- "log2_TPM_FPKM_or_log_normalized_likely"
  } else if (negative_fraction > 0.01 && numeric_max <= 30) {
    guess <- "centered_or_batch_adjusted_log_matrix_likely"
  }
  
  list(
    expression_scale_guess = guess,
    numeric_min = numeric_min,
    numeric_median = numeric_median,
    numeric_max = numeric_max,
    integer_like_fraction = integer_like_fraction,
    negative_fraction = negative_fraction,
    zero_fraction = zero_fraction
  )
}

detect_target_genes_in_preview <- function(df, gene_col, target_genes) {
  if (is.null(df) || is.na(gene_col) || !gene_col %in% colnames(df)) {
    return(character())
  }
  
  genes <- toupper(as.character(df[[gene_col]]))
  target_upper <- toupper(target_genes)
  
  target_genes[target_upper %in% genes]
}

is_likely_expression_file <- function(role_guess, sample_cols, scale_guess, n_detected_genes) {
  if (role_guess == "geo_annotation_or_series_matrix") return(FALSE)
  
  role_expr <- grepl("expression", role_guess)
  enough_numeric_cols <- length(sample_cols) >= 3
  scale_expr <- !scale_guess %in% c("not_expression_or_unreadable", "not_numeric")
  has_target_gene <- !is.na(n_detected_genes) && n_detected_genes > 0
  
  role_expr || (enough_numeric_cols && scale_expr && has_target_gene)
}

is_likely_pheno_file <- function(role_guess, df) {
  if (is.null(df)) return(FALSE)
  
  role_pheno <- role_guess == "phenotype_or_metadata"
  cn <- tolower(colnames(df))
  
  has_pheno_cols <- any(grepl(
    "sample|gsm|run|srr|group|condition|diagnosis|disease|sepsis|control|phenotype|clinical",
    cn
  ))
  
  role_pheno || has_pheno_cols
}

# ============================================================
# 扫描文件
# ============================================================

message("Scanning directories:")
print(scan_dirs)

file_patterns <- c(
  "\\.csv$",
  "\\.tsv$",
  "\\.txt$",
  "\\.csv\\.gz$",
  "\\.tsv\\.gz$",
  "\\.txt\\.gz$",
  "\\.xlsx$"
)

all_files <- character()

for (d in scan_dirs) {
  message("Scanning: ", d)
  
  ff <- list.files(
    d,
    pattern = paste(file_patterns, collapse = "|"),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  all_files <- c(all_files, ff)
}

all_files <- unique(all_files)
file_inventory <- safe_file_info(all_files)

if (nrow(file_inventory) == 0) {
  stop("没有扫描到候选文件。请确认 RNA-seq 文件路径。")
}

file_inventory$role_guess_from_name <- vapply(file_inventory$file_name, guess_file_role_from_name, character(1))
file_inventory$skip_preview <- mapply(
  should_skip_file,
  file_path = file_inventory$file_path,
  file_name = file_inventory$file_name,
  role_guess = file_inventory$role_guess_from_name
)

file_inventory$preview_attempted <- FALSE
file_inventory$readable_preview <- FALSE
file_inventory$n_preview_rows <- NA_integer_
file_inventory$n_preview_cols <- NA_integer_
file_inventory$gene_col_guess <- NA_character_
file_inventory$n_numeric_sample_like_cols <- NA_integer_
file_inventory$expression_scale_guess <- NA_character_
file_inventory$numeric_min <- NA_real_
file_inventory$numeric_median <- NA_real_
file_inventory$numeric_max <- NA_real_
file_inventory$integer_like_fraction <- NA_real_
file_inventory$negative_fraction <- NA_real_
file_inventory$zero_fraction <- NA_real_
file_inventory$detected_target_genes_in_preview <- NA_character_
file_inventory$n_detected_target_genes_in_preview <- NA_integer_
file_inventory$likely_expression_file <- FALSE
file_inventory$likely_pheno_file <- FALSE

preview_idx <- which(
  file_inventory$skip_preview == FALSE &
    file_inventory$size_mb <= 500 &
    grepl("\\.csv$|\\.tsv$|\\.txt$|\\.csv\\.gz$|\\.tsv\\.gz$|\\.txt\\.gz$|\\.xlsx$",
          file_inventory$file_name,
          ignore.case = TRUE)
)

message("Total candidate files: ", nrow(file_inventory))
message("Files skipped before preview: ", sum(file_inventory$skip_preview, na.rm = TRUE))
message("Files selected for preview: ", length(preview_idx))

if (length(preview_idx) > 0) {
  for (i in preview_idx) {
    f <- file_inventory$file_path[i]
    file_inventory$preview_attempted[i] <- TRUE
    
    df <- safe_read_preview(f, n_max = 80)
    
    if (is.null(df) || ncol(df) == 0) {
      next
    }
    
    file_inventory$readable_preview[i] <- TRUE
    file_inventory$n_preview_rows[i] <- nrow(df)
    file_inventory$n_preview_cols[i] <- ncol(df)
    
    gene_col <- detect_gene_column(df)
    sample_cols <- detect_sample_like_columns(df, gene_col)
    scale_info <- guess_expression_scale(df, sample_cols)
    detected_genes <- detect_target_genes_in_preview(df, gene_col, target_genes)
    
    file_inventory$gene_col_guess[i] <- gene_col
    file_inventory$n_numeric_sample_like_cols[i] <- length(sample_cols)
    file_inventory$expression_scale_guess[i] <- scale_info$expression_scale_guess
    file_inventory$numeric_min[i] <- scale_info$numeric_min
    file_inventory$numeric_median[i] <- scale_info$numeric_median
    file_inventory$numeric_max[i] <- scale_info$numeric_max
    file_inventory$integer_like_fraction[i] <- scale_info$integer_like_fraction
    file_inventory$negative_fraction[i] <- scale_info$negative_fraction
    file_inventory$zero_fraction[i] <- scale_info$zero_fraction
    file_inventory$detected_target_genes_in_preview[i] <- paste(detected_genes, collapse = ";")
    file_inventory$n_detected_target_genes_in_preview[i] <- length(detected_genes)
    
    file_inventory$likely_expression_file[i] <- is_likely_expression_file(
      role_guess = file_inventory$role_guess_from_name[i],
      sample_cols = sample_cols,
      scale_guess = scale_info$expression_scale_guess,
      n_detected_genes = length(detected_genes)
    )
    
    file_inventory$likely_pheno_file[i] <- is_likely_pheno_file(
      role_guess = file_inventory$role_guess_from_name[i],
      df = df
    )
  }
}

# ============================================================
# 候选文件
# ============================================================

candidate_expression_files <- file_inventory[
  file_inventory$likely_expression_file == TRUE,
  ,
  drop = FALSE
]

if (nrow(candidate_expression_files) > 0) {
  candidate_expression_files <- candidate_expression_files[
    order(
      -candidate_expression_files$n_detected_target_genes_in_preview,
      -candidate_expression_files$n_numeric_sample_like_cols,
      candidate_expression_files$size_mb
    ),
    ,
    drop = FALSE
  ]
}

candidate_pheno_files <- file_inventory[
  file_inventory$likely_pheno_file == TRUE,
  ,
  drop = FALSE
]

if (nrow(candidate_pheno_files) > 0) {
  candidate_pheno_files <- candidate_pheno_files[
    order(candidate_pheno_files$size_mb),
    ,
    drop = FALSE
  ]
}

gene_detection_summary <- data.frame(
  gene_symbol = target_genes,
  detected_in_any_preview = vapply(
    target_genes,
    function(g) {
      any(grepl(
        paste0("(^|;)", g, "(;|$)"),
        file_inventory$detected_target_genes_in_preview,
        ignore.case = TRUE
      ), na.rm = TRUE)
    },
    logical(1)
  ),
  stringsAsFactors = FALSE
)

gene_detection_summary$file_hits <- vapply(
  gene_detection_summary$gene_symbol,
  function(g) {
    hits <- file_inventory$file_name[
      grepl(
        paste0("(^|;)", g, "(;|$)"),
        file_inventory$detected_target_genes_in_preview,
        ignore.case = TRUE
      )
    ]
    paste(unique(hits), collapse = "; ")
  },
  character(1)
)

scan_summary <- data.frame(
  metric = c(
    "n_scan_dirs",
    "n_total_files_scanned",
    "n_skipped_preview",
    "n_preview_attempted",
    "n_readable_preview",
    "n_candidate_expression_files",
    "n_candidate_pheno_files",
    "n_target_genes_detected_in_preview"
  ),
  value = c(
    length(scan_dirs),
    nrow(file_inventory),
    sum(file_inventory$skip_preview, na.rm = TRUE),
    sum(file_inventory$preview_attempted, na.rm = TRUE),
    sum(file_inventory$readable_preview, na.rm = TRUE),
    nrow(candidate_expression_files),
    nrow(candidate_pheno_files),
    sum(gene_detection_summary$detected_in_any_preview, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# 保存
# ============================================================

data.table::fwrite(
  file_inventory,
  file.path(out_dir, "T16a_local_RNAseq_file_inventory.csv")
)

data.table::fwrite(
  candidate_expression_files,
  file.path(out_dir, "T16a_candidate_expression_files.csv")
)

data.table::fwrite(
  candidate_pheno_files,
  file.path(out_dir, "T16a_candidate_pheno_files.csv")
)

data.table::fwrite(
  gene_detection_summary,
  file.path(out_dir, "T16a_gene_detection_summary.csv")
)

data.table::fwrite(
  scan_summary,
  file.path(out_dir, "T16a_scan_summary.csv")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "scan_summary")
openxlsx::writeData(wb, "scan_summary", scan_summary)

openxlsx::addWorksheet(wb, "candidate_expression")
openxlsx::writeData(wb, "candidate_expression", candidate_expression_files)

openxlsx::addWorksheet(wb, "candidate_pheno")
openxlsx::writeData(wb, "candidate_pheno", candidate_pheno_files)

openxlsx::addWorksheet(wb, "gene_detection")
openxlsx::writeData(wb, "gene_detection", gene_detection_summary)

openxlsx::addWorksheet(wb, "full_inventory")
openxlsx::writeData(wb, "full_inventory", file_inventory)

openxlsx::saveWorkbook(
  wb,
  file.path(out_dir, "T16a_RNAseq_scan_summary.xlsx"),
  overwrite = TRUE
)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_16a_scan_local_RNAseq_files.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 16a Local RNA-seq file scan 完成 ============")
message("扫描目录：")
print(scan_dirs)

message("\nScan summary:")
print(scan_summary)

message("\nTop candidate expression files:")
if (nrow(candidate_expression_files) > 0) {
  print(
    head(
      candidate_expression_files[, c(
        "file_name",
        "file_path",
        "size_mb",
        "role_guess_from_name",
        "gene_col_guess",
        "n_numeric_sample_like_cols",
        "expression_scale_guess",
        "n_detected_target_genes_in_preview"
      ), drop = FALSE],
      30
    )
  )
} else {
  message("未发现候选表达矩阵文件。")
}

message("\nTop candidate phenotype files:")
if (nrow(candidate_pheno_files) > 0) {
  print(
    head(
      candidate_pheno_files[, c(
        "file_name",
        "file_path",
        "size_mb",
        "role_guess_from_name",
        "n_preview_cols"
      ), drop = FALSE],
      30
    )
  )
} else {
  message("未发现候选 phenotype 文件。")
}

message("\nTarget gene detection summary:")
print(gene_detection_summary)

message("\n关键输出：")
message("1) ", file.path(out_dir, "T16a_RNAseq_scan_summary.xlsx"))
message("2) ", file.path(out_dir, "T16a_candidate_expression_files.csv"))
message("3) ", file.path(out_dir, "T16a_candidate_pheno_files.csv"))
message("4) ", file.path(out_dir, "T16a_gene_detection_summary.csv"))
message("5) ", file.path(out_dir, "T16a_local_RNAseq_file_inventory.csv"))

message("\n下一步：")
message("请把 Scan summary、Top candidate expression files、Top candidate phenotype files 贴给我。")
message("我会根据扫描结果生成 16_RNAseq_external_platform_validation.R。")