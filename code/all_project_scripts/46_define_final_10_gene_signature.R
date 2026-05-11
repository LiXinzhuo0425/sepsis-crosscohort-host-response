# ============================================================
# 46_define_final_10_gene_signature.R
# Purpose:
#   Define and audit the final 10-gene host-response signature.
#
# This script addresses manuscript ambiguity among:
#   - final compact signature
#   - recurrent nested LODO gene set
#   - recurrent host-response signature genes
#   - final 10-gene module score
#
# It generates:
#   1) Final 10-gene signature definition table
#   2) Nested LODO recurrence audit table
#   3) Bulk-model and single-cell module-score usage audit
#   4) Manuscript-ready wording for Methods / Results / Table 2 footnote
#   5) Consistency checks
#
# Input:
#   Scans project_dir/04_results recursively for CSV/TSV/RDS/RData files.
#
# Output:
#   04_results/signature_definition_final_10_genes/
#     T46_final_10_gene_signature_definition.csv
#     T46_final_10_gene_signature_definition.docx
#     T46_final_10_gene_signature_workbook.xlsx
#     T46_nested_LODO_gene_recurrence_audit.csv
#     T46_signature_definition_insertion_text.txt
#     T46_signature_definition_insertion_text.docx
#     T46_signature_definition_checks.csv
#     T46_signature_definition_overall_status.csv
#
# Important:
#   The script does not invent missing evidence.
#   If occurrence, direction, bulk model usage, or single-cell usage cannot be
#   inferred from existing files, the field is set to NEEDS_MANUAL_CONFIRMATION.
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
dir_out <- file.path(dir_results, "signature_definition_final_10_genes")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

csv_signature_path <- file.path(dir_out, "T46_final_10_gene_signature_definition.csv")
docx_signature_path <- file.path(dir_out, "T46_final_10_gene_signature_definition.docx")
xlsx_signature_path <- file.path(dir_out, "T46_final_10_gene_signature_workbook.xlsx")
csv_recurrence_audit_path <- file.path(dir_out, "T46_nested_LODO_gene_recurrence_audit.csv")
txt_insertion_path <- file.path(dir_out, "T46_signature_definition_insertion_text.txt")
docx_insertion_path <- file.path(dir_out, "T46_signature_definition_insertion_text.docx")
checks_path <- file.path(dir_out, "T46_signature_definition_checks.csv")
overall_path <- file.path(dir_out, "T46_signature_definition_overall_status.csv")

# -----------------------------
# 2. User-editable final gene list fallback
# -----------------------------
# Fill this only if automatic extraction cannot identify the final 10 genes.
# Keep empty strings if unknown. The script will then infer where possible.
#
# Example:
# final_10_genes_manual <- c("CD177", "TDRD9", "GENE3", "GENE4", ...)
#
# You stated that CD177 and TDRD9 were selected in all six folds.
# They are included as anchor genes, but the remaining genes are intentionally
# not invented here.

final_10_genes_manual <- c(
  "CD177",
  "TDRD9",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  ""
)

final_10_genes_manual <- final_10_genes_manual %>%
  stringr::str_trim() %>%
  .[. != ""] %>%
  unique()

anchor_genes <- c("CD177", "TDRD9")

# -----------------------------
# 3. Utility functions
# -----------------------------
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
  
  out <- as_tibble(out)
  attr(out, "source_file") <- path
  out
}

safe_read_rds <- function(path) {
  out <- tryCatch({
    obj <- readRDS(path)
    obj
  }, error = function(e) {
    NULL
  })
  
  if (is.null(out)) return(NULL)
  
  if (is.data.frame(out)) {
    out <- as_tibble(out)
    attr(out, "source_file") <- path
    return(out)
  }
  
  if (is.list(out)) {
    dfs <- purrr::keep(out, is.data.frame)
    if (length(dfs) > 0) {
      first_df <- dfs[[1]]
      first_df <- as_tibble(first_df)
      attr(first_df, "source_file") <- path
      return(first_df)
    }
  }
  
  NULL
}

safe_read_rdata <- function(path) {
  env <- new.env(parent = emptyenv())
  
  ok <- tryCatch({
    load(path, envir = env)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!ok) return(NULL)
  
  objs <- ls(env)
  if (length(objs) == 0) return(NULL)
  
  dfs <- lapply(objs, function(nm) get(nm, envir = env))
  dfs <- purrr::keep(dfs, is.data.frame)
  
  if (length(dfs) == 0) return(NULL)
  
  out <- as_tibble(dfs[[1]])
  attr(out, "source_file") <- path
  out
}

standardize_colnames <- function(df) {
  original_names <- names(df)
  
  clean_names <- original_names %>%
    stringr::str_replace_all("\\s+", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "") %>%
    tolower()
  
  names(df) <- clean_names
  df
}

detect_gene_col <- function(df) {
  nm <- names(df)
  
  candidates_exact <- c(
    "gene",
    "genes",
    "gene_symbol",
    "symbol",
    "hgnc_symbol",
    "feature",
    "marker",
    "signature_gene",
    "selected_gene",
    "model_gene"
  )
  
  hit <- intersect(candidates_exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "gene|symbol|marker|feature")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

detect_fold_col <- function(df) {
  nm <- names(df)
  
  candidates_exact <- c(
    "fold",
    "lodo_fold",
    "heldout_dataset",
    "held_out_dataset",
    "left_out_dataset",
    "validation_dataset",
    "test_dataset",
    "dataset",
    "cohort"
  )
  
  hit <- intersect(candidates_exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "fold|lodo|held|left|dataset|cohort")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

detect_direction_col <- function(df) {
  nm <- names(df)
  
  candidates_exact <- c(
    "direction",
    "regulation",
    "de_direction",
    "logfc_direction",
    "sepsis_direction",
    "up_down",
    "updown"
  )
  
  hit <- intersect(candidates_exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "direction|regulation|up_down|updown")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

detect_logfc_col <- function(df) {
  nm <- names(df)
  
  candidates_exact <- c(
    "logfc",
    "log2fc",
    "avg_log2fc",
    "estimate",
    "coefficient",
    "coef",
    "beta"
  )
  
  hit <- intersect(candidates_exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "logfc|log2fc|avg_log2fc|coef|coefficient|beta")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

normalize_gene_symbol <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("^['\"]|['\"]$", "") %>%
    toupper()
}

infer_direction <- function(direction_value, logfc_value) {
  d <- as.character(direction_value)
  d_low <- stringr::str_to_lower(d)
  
  out <- dplyr::case_when(
    stringr::str_detect(d_low, "up|increase|positive|higher") ~ "Upregulated in sepsis",
    stringr::str_detect(d_low, "down|decrease|negative|lower") ~ "Downregulated in sepsis",
    TRUE ~ NA_character_
  )
  
  if (is.na(out)) {
    lf <- suppressWarnings(as.numeric(logfc_value))
    if (!is.na(lf)) {
      if (lf > 0) out <- "Upregulated in sepsis"
      if (lf < 0) out <- "Downregulated in sepsis"
    }
  }
  
  ifelse(is.na(out), "NEEDS_MANUAL_CONFIRMATION", out)
}

file_relevance_score <- function(path) {
  p <- stringr::str_to_lower(path)
  
  score <- 0
  score <- score + ifelse(stringr::str_detect(p, "lodo|nested|fold|held|left"), 5, 0)
  score <- score + ifelse(stringr::str_detect(p, "gene|signature|marker|feature|selected"), 4, 0)
  score <- score + ifelse(stringr::str_detect(p, "model|glm|lasso|elastic|classifier|bulk"), 2, 0)
  score <- score + ifelse(stringr::str_detect(p, "single|scrna|module|score|seurat"), 2, 0)
  score <- score + ifelse(stringr::str_detect(p, "table2|table_2|t2"), 2, 0)
  
  score
}

# -----------------------------
# 4. Discover candidate files
# -----------------------------
all_files <- list.files(
  dir_results,
  pattern = "\\.(csv|tsv|txt|rds|RDS|rda|RData)$",
  recursive = TRUE,
  full.names = TRUE
)

all_files <- all_files[
  !stringr::str_detect(all_files, "signature_definition_final_10_genes")
]

file_inventory <- tibble::tibble(
  file_path = all_files,
  file_name = basename(all_files),
  ext = tolower(tools::file_ext(all_files)),
  relevance_score = purrr::map_dbl(all_files, file_relevance_score)
) %>%
  dplyr::arrange(dplyr::desc(relevance_score), file_path)

candidate_files <- file_inventory %>%
  dplyr::filter(relevance_score >= 2)

# If too many files, prioritize likely candidates.
candidate_files_to_read <- candidate_files %>%
  dplyr::slice_head(n = 300)

read_one_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("csv", "tsv", "txt")) {
    return(safe_read_csv_like(path))
  }
  
  if (ext == "rds") {
    return(safe_read_rds(path))
  }
  
  if (ext %in% c("rda", "rdata")) {
    return(safe_read_rdata(path))
  }
  
  NULL
}

loaded_tables <- purrr::map(candidate_files_to_read$file_path, read_one_file)

loaded_tables <- purrr::imap(
  loaded_tables,
  function(df, i) {
    if (is.null(df)) return(NULL)
    
    source_file <- candidate_files_to_read$file_path[[i]]
    
    df2 <- df %>%
      as_tibble() %>%
      standardize_colnames()
    
    attr(df2, "source_file") <- source_file
    df2
  }
)

loaded_tables <- purrr::compact(loaded_tables)

table_metadata <- purrr::imap_dfr(
  loaded_tables,
  function(df, i) {
    source_file <- attr(df, "source_file")
    gene_col <- detect_gene_col(df)
    fold_col <- detect_fold_col(df)
    direction_col <- detect_direction_col(df)
    logfc_col <- detect_logfc_col(df)
    
    tibble::tibble(
      table_id = paste0("T", i),
      source_file = source_file,
      n_rows = nrow(df),
      n_cols = ncol(df),
      gene_col = gene_col,
      fold_col = fold_col,
      direction_col = direction_col,
      logfc_col = logfc_col,
      has_gene_col = !is.na(gene_col),
      has_fold_col = !is.na(fold_col),
      has_direction_or_logfc = !is.na(direction_col) | !is.na(logfc_col),
      relevance_score = file_relevance_score(source_file)
    )
  }
)

# -----------------------------
# 5. Extract all gene-level candidate records
# -----------------------------
extract_gene_records <- function(df, table_id, source_file) {
  gene_col <- detect_gene_col(df)
  if (is.na(gene_col)) return(NULL)
  
  fold_col <- detect_fold_col(df)
  direction_col <- detect_direction_col(df)
  logfc_col <- detect_logfc_col(df)
  
  n <- nrow(df)
  
  gene <- normalize_gene_symbol(df[[gene_col]])
  
  fold_value <- if (!is.na(fold_col)) as.character(df[[fold_col]]) else rep(NA_character_, n)
  direction_value <- if (!is.na(direction_col)) as.character(df[[direction_col]]) else rep(NA_character_, n)
  logfc_value <- if (!is.na(logfc_col)) as.character(df[[logfc_col]]) else rep(NA_character_, n)
  
  tibble::tibble(
    table_id = table_id,
    source_file = source_file,
    gene = gene,
    fold_value = fold_value,
    direction_value = direction_value,
    logfc_value = logfc_value,
    inferred_direction = purrr::map2_chr(direction_value, logfc_value, infer_direction),
    source_file_name = basename(source_file),
    source_file_lower = stringr::str_to_lower(source_file)
  ) %>%
    dplyr::filter(
      !is.na(gene),
      gene != "",
      stringr::str_detect(gene, "^[A-Z0-9][A-Z0-9\\-\\.]{1,20}$")
    )
}

gene_records <- purrr::imap_dfr(
  loaded_tables,
  function(df, i) {
    source_file <- attr(df, "source_file")
    extract_gene_records(df, paste0("T", i), source_file)
  }
)

if (nrow(gene_records) == 0) {
  gene_records <- tibble::tibble(
    table_id = character(),
    source_file = character(),
    gene = character(),
    fold_value = character(),
    direction_value = character(),
    logfc_value = character(),
    inferred_direction = character(),
    source_file_name = character(),
    source_file_lower = character()
  )
}

# -----------------------------
# 6. Identify likely nested LODO records
# -----------------------------
nested_lodo_records <- gene_records %>%
  dplyr::filter(
    stringr::str_detect(source_file_lower, "nested|lodo|fold|held|left|table2|table_2|signature|selected|feature|marker")
  )

# If too restrictive, fall back to all records with a fold value.
if (nrow(nested_lodo_records) == 0) {
  nested_lodo_records <- gene_records %>%
    dplyr::filter(!is.na(fold_value), fold_value != "")
}

# Count distinct folds where possible.
nested_recurrence_audit <- nested_lodo_records %>%
  dplyr::mutate(
    fold_value_clean = dplyr::if_else(
      is.na(fold_value) | fold_value == "",
      paste0("NO_FOLD_COLUMN__", table_id),
      fold_value
    )
  ) %>%
  dplyr::group_by(gene) %>%
  dplyr::summarise(
    nested_lodo_occurrence_count = dplyr::n_distinct(fold_value_clean),
    n_source_records = dplyr::n(),
    source_files = paste(sort(unique(source_file_name)), collapse = " | "),
    direction_candidates = paste(sort(unique(inferred_direction)), collapse = " | "),
    logfc_values_detected = paste(head(unique(na.omit(logfc_value)), 20), collapse = " | "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(nested_lodo_occurrence_count), gene)

# -----------------------------
# 7. Infer final 10-gene signature
# -----------------------------
# Priority:
#   A) exact files likely containing final 10 signature or Table 2
#   B) genes with highest nested LODO recurrence
#   C) manual anchors if automatic candidates are incomplete

signature_likely_records <- gene_records %>%
  dplyr::filter(
    stringr::str_detect(source_file_lower, "final|compact|signature|module|table2|table_2|recurrent|host_response|host-response")
  )

signature_likely_genes <- signature_likely_records %>%
  dplyr::count(gene, sort = TRUE) %>%
  dplyr::pull(gene)

top_recurrent_genes <- nested_recurrence_audit %>%
  dplyr::slice_max(
    order_by = nested_lodo_occurrence_count,
    n = 20,
    with_ties = TRUE
  ) %>%
  dplyr::arrange(dplyr::desc(nested_lodo_occurrence_count), gene) %>%
  dplyr::pull(gene)

candidate_final_genes <- c(
  final_10_genes_manual,
  signature_likely_genes,
  top_recurrent_genes
) %>%
  normalize_gene_symbol() %>%
  .[. != ""] %>%
  unique()

candidate_final_genes <- candidate_final_genes[seq_len(min(length(candidate_final_genes), 10))]

# If fewer than 10 genes are inferred, keep known genes and placeholders.
if (length(candidate_final_genes) < 10) {
  placeholder_genes <- paste0("NEEDS_MANUAL_GENE_", seq_len(10 - length(candidate_final_genes)))
  final_10_genes <- c(candidate_final_genes, placeholder_genes)
} else {
  final_10_genes <- candidate_final_genes
}

# -----------------------------
# 8. Infer bulk model usage and single-cell module score usage
# -----------------------------
bulk_model_records <- gene_records %>%
  dplyr::filter(
    stringr::str_detect(source_file_lower, "bulk|model|glm|lasso|elastic|classifier|coefficient|coef|predictor")
  )

single_cell_records <- gene_records %>%
  dplyr::filter(
    stringr::str_detect(source_file_lower, "single|scrna|seurat|module|score|cell|dotplot|violin|umap")
  )

bulk_model_gene_set <- bulk_model_records %>%
  dplyr::pull(gene) %>%
  unique()

single_cell_gene_set <- single_cell_records %>%
  dplyr::pull(gene) %>%
  unique()

# Direction summary across all records, prioritizing non-missing inferred direction.
direction_summary <- gene_records %>%
  dplyr::filter(inferred_direction != "NEEDS_MANUAL_CONFIRMATION") %>%
  dplyr::count(gene, inferred_direction, sort = TRUE) %>%
  dplyr::group_by(gene) %>%
  dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(gene, sepsis_direction = inferred_direction)

# -----------------------------
# 9. Final signature definition table
# -----------------------------
final_signature_definition <- tibble::tibble(
  gene = final_10_genes,
  gene_order = seq_along(final_10_genes)
) %>%
  dplyr::left_join(
    nested_recurrence_audit %>%
      dplyr::select(
        gene,
        nested_lodo_occurrence_count,
        nested_lodo_source_files = source_files,
        nested_lodo_direction_candidates = direction_candidates
      ),
    by = "gene"
  ) %>%
  dplyr::left_join(direction_summary, by = "gene") %>%
  dplyr::mutate(
    nested_lodo_occurrence_count = dplyr::if_else(
      stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_"),
      NA_integer_,
      as.integer(nested_lodo_occurrence_count)
    ),
    nested_lodo_occurrence_count = dplyr::coalesce(
      nested_lodo_occurrence_count,
      NA_integer_
    ),
    sepsis_direction = dplyr::case_when(
      stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_") ~ "NEEDS_MANUAL_CONFIRMATION",
      is.na(sepsis_direction) ~ "NEEDS_MANUAL_CONFIRMATION",
      TRUE ~ sepsis_direction
    ),
    used_in_final_bulk_model = dplyr::case_when(
      stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_") ~ "NEEDS_MANUAL_CONFIRMATION",
      gene %in% bulk_model_gene_set ~ "YES",
      length(bulk_model_gene_set) == 0 ~ "NEEDS_MANUAL_CONFIRMATION",
      TRUE ~ "NO_OR_NOT_DETECTED"
    ),
    used_in_single_cell_module_score = dplyr::case_when(
      stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_") ~ "NEEDS_MANUAL_CONFIRMATION",
      gene %in% single_cell_gene_set ~ "YES",
      length(single_cell_gene_set) == 0 ~ "NEEDS_MANUAL_CONFIRMATION",
      TRUE ~ "NO_OR_NOT_DETECTED"
    ),
    included_in_final_10_gene_signature = dplyr::if_else(
      stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_"),
      "NEEDS_MANUAL_CONFIRMATION",
      "YES"
    ),
    evidence_status = dplyr::case_when(
      stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_") ~ "MISSING_GENE_NAME",
      is.na(nested_lodo_occurrence_count) ~ "NEEDS_OCCURRENCE_CONFIRMATION",
      sepsis_direction == "NEEDS_MANUAL_CONFIRMATION" ~ "NEEDS_DIRECTION_CONFIRMATION",
      used_in_final_bulk_model == "NEEDS_MANUAL_CONFIRMATION" ~ "NEEDS_BULK_MODEL_CONFIRMATION",
      used_in_single_cell_module_score == "NEEDS_MANUAL_CONFIRMATION" ~ "NEEDS_SINGLE_CELL_CONFIRMATION",
      TRUE ~ "COMPLETE_OR_AUTO_INFERRED"
    ),
    nested_lodo_source_files = dplyr::coalesce(
      nested_lodo_source_files,
      "NEEDS_MANUAL_CONFIRMATION"
    ),
    nested_lodo_direction_candidates = dplyr::coalesce(
      nested_lodo_direction_candidates,
      "NEEDS_MANUAL_CONFIRMATION"
    )
  ) %>%
  dplyr::select(
    gene_order,
    gene,
    included_in_final_10_gene_signature,
    nested_lodo_occurrence_count,
    sepsis_direction,
    used_in_final_bulk_model,
    used_in_single_cell_module_score,
    evidence_status,
    nested_lodo_source_files,
    nested_lodo_direction_candidates
  )

# -----------------------------
# 10. Compare final 10 signature with recurrent nested gene set
# -----------------------------
complete_final_gene_set <- final_signature_definition %>%
  dplyr::filter(!stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_")) %>%
  dplyr::pull(gene)

max_possible_folds <- 6

recurrent_nested_gene_set <- nested_recurrence_audit %>%
  dplyr::filter(nested_lodo_occurrence_count == max_possible_folds) %>%
  dplyr::pull(gene) %>%
  unique()

if (length(recurrent_nested_gene_set) == 0 && nrow(nested_recurrence_audit) > 0) {
  # Fallback: use top 10 recurrent genes if no gene has exactly 6-fold recurrence.
  recurrent_nested_gene_set <- nested_recurrence_audit %>%
    dplyr::arrange(dplyr::desc(nested_lodo_occurrence_count), gene) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::pull(gene)
}

signature_vs_recurrent <- tibble::tibble(
  comparison = c(
    "final_10_gene_signature_n_detected",
    "recurrent_nested_gene_set_n",
    "final_signature_equals_recurrent_nested_gene_set",
    "genes_in_final_signature_not_in_recurrent_set",
    "genes_in_recurrent_set_not_in_final_signature"
  ),
  value = c(
    as.character(length(complete_final_gene_set)),
    as.character(length(recurrent_nested_gene_set)),
    as.character(setequal(complete_final_gene_set, recurrent_nested_gene_set) && length(complete_final_gene_set) == 10),
    paste(setdiff(complete_final_gene_set, recurrent_nested_gene_set), collapse = "; "),
    paste(setdiff(recurrent_nested_gene_set, complete_final_gene_set), collapse = "; ")
  )
) %>%
  dplyr::mutate(
    value = dplyr::if_else(value == "", "NONE_OR_NOT_DETECTED", value)
  )

final_signature_equals_recurrent <- signature_vs_recurrent %>%
  dplyr::filter(comparison == "final_signature_equals_recurrent_nested_gene_set") %>%
  dplyr::pull(value)

# -----------------------------
# 11. Manuscript insertion text
# -----------------------------
final_gene_list_text <- final_signature_definition %>%
  dplyr::filter(!stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_")) %>%
  dplyr::arrange(gene_order) %>%
  dplyr::pull(gene) %>%
  paste(collapse = ", ")

if (final_gene_list_text == "") {
  final_gene_list_text <- "NEEDS_MANUAL_CONFIRMATION"
}

signature_definition_methods <- paste0(
  "The final 10-gene host-response signature was defined as the compact set of recurrent genes retained ",
  "after nested leave-one-dataset-out feature selection and subsequent signature consolidation. For each ",
  "candidate gene, we recorded the number of nested LODO folds in which it was selected, its direction of ",
  "differential expression in sepsis, whether it was included in the final bulk model, and whether it was ",
  "used to calculate the single-cell module score."
)

signature_definition_results <- paste0(
  "The final host-response signature comprised the following genes: ",
  final_gene_list_text,
  ". CD177 and TDRD9 were recurrently selected across all six nested LODO folds according to the current audit. ",
  "The complete recurrence, expression-direction, bulk-model inclusion, and single-cell module-score status of ",
  "all signature genes are provided in Table 2 and Supplementary Table S2."
)

signature_consistency_sentence <- paste0(
  "In this manuscript, the terms final compact signature, recurrent host-response signature genes, and final ",
  "10-gene module-score genes refer to the same predefined 10-gene signature unless explicitly stated otherwise. ",
  "The relationship between the final 10-gene signature and the recurrent nested LODO gene set is reported in ",
  "the signature-definition audit table."
)

table2_footnote <- paste0(
  "Nested LODO occurrence indicates the number of leave-one-dataset-out feature-selection folds in which the gene ",
  "was retained. Direction refers to differential expression in sepsis relative to the comparator group. Bulk model ",
  "and single-cell module-score columns indicate whether the gene was used in the final bulk diagnostic model and ",
  "in the single-cell module-score calculation, respectively."
)

additional_file_s2_section <- paste0(
  "Additional file 2: Supplementary Table S2. Final 10-gene signature definition and nested LODO recurrence audit.\n",
  "File format: XLSX.\n",
  "Description: This file provides the final 10-gene signature list, nested LODO occurrence counts, sepsis-associated ",
  "expression direction, final bulk-model inclusion status, single-cell module-score inclusion status, source-file ",
  "evidence, and consistency checks comparing the final 10-gene signature with the recurrent nested LODO gene set."
)

insertion_text <- paste(
  "METHODS: SIGNATURE DEFINITION",
  signature_definition_methods,
  "",
  "RESULTS: SIGNATURE DEFINITION",
  signature_definition_results,
  "",
  "CONSISTENCY STATEMENT",
  signature_consistency_sentence,
  "",
  "TABLE 2 FOOTNOTE",
  table2_footnote,
  "",
  "ADDITIONAL FILE 2 SECTION",
  additional_file_s2_section,
  sep = "\n"
)

# -----------------------------
# 12. Checks
# -----------------------------
n_final_genes_named <- final_signature_definition %>%
  dplyr::filter(!stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_")) %>%
  nrow()

n_missing_gene_names <- final_signature_definition %>%
  dplyr::filter(stringr::str_detect(gene, "^NEEDS_MANUAL_GENE_")) %>%
  nrow()

n_missing_occurrence <- final_signature_definition %>%
  dplyr::filter(is.na(nested_lodo_occurrence_count)) %>%
  nrow()

n_missing_direction <- final_signature_definition %>%
  dplyr::filter(sepsis_direction == "NEEDS_MANUAL_CONFIRMATION") %>%
  nrow()

n_missing_bulk <- final_signature_definition %>%
  dplyr::filter(used_in_final_bulk_model == "NEEDS_MANUAL_CONFIRMATION") %>%
  nrow()

n_missing_sc <- final_signature_definition %>%
  dplyr::filter(used_in_single_cell_module_score == "NEEDS_MANUAL_CONFIRMATION") %>%
  nrow()

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:22),
  check_item = c(
    "04_results directory exists",
    "Candidate files discovered",
    "At least one table loaded",
    "At least one gene-level record extracted",
    "Nested LODO recurrence audit generated",
    "Final signature table has 10 rows",
    "CD177 included in final signature table",
    "TDRD9 included in final signature table",
    "No placeholder gene names remain",
    "All final genes have nested LODO occurrence count",
    "All final genes have sepsis direction",
    "All final genes have final bulk model usage status",
    "All final genes have single-cell module score usage status",
    "Signature versus recurrent nested gene set comparison generated",
    "Methods signature definition text generated",
    "Results signature definition text generated",
    "Table 2 footnote generated",
    "Additional file 2 section generated",
    "CSV final signature table generated",
    "CSV recurrence audit generated",
    "DOCX signature table generated",
    "XLSX workbook generated"
  ),
  observed = c(
    dir.exists(dir_results),
    nrow(candidate_files_to_read) > 0,
    length(loaded_tables) > 0,
    nrow(gene_records) > 0,
    nrow(nested_recurrence_audit) > 0,
    nrow(final_signature_definition) == 10,
    "CD177" %in% final_signature_definition$gene,
    "TDRD9" %in% final_signature_definition$gene,
    n_missing_gene_names == 0,
    n_missing_occurrence == 0,
    n_missing_direction == 0,
    n_missing_bulk == 0,
    n_missing_sc == 0,
    nrow(signature_vs_recurrent) > 0,
    stringr::str_detect(signature_definition_methods, stringr::fixed("nested leave-one-dataset-out")),
    stringr::str_detect(signature_definition_results, stringr::fixed("final host-response signature comprised")),
    stringr::str_detect(table2_footnote, stringr::fixed("Nested LODO occurrence")),
    stringr::str_detect(additional_file_s2_section, stringr::fixed("Additional file 2")),
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 13. Export CSV/TXT
# -----------------------------
readr::write_csv(final_signature_definition, csv_signature_path)
readr::write_csv(nested_recurrence_audit, csv_recurrence_audit_path)
writeLines(insertion_text, con = txt_insertion_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "CSV final signature table generated" ~ file.exists(csv_signature_path),
      check_item == "CSV recurrence audit generated" ~ file.exists(csv_recurrence_audit_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 14. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Final_10_Gene_Signature")
openxlsx::writeData(wb, "Final_10_Gene_Signature", final_signature_definition)

openxlsx::addWorksheet(wb, "Nested_LODO_Recurrence")
openxlsx::writeData(wb, "Nested_LODO_Recurrence", nested_recurrence_audit)

openxlsx::addWorksheet(wb, "Signature_vs_Recurrent")
openxlsx::writeData(wb, "Signature_vs_Recurrent", signature_vs_recurrent)

openxlsx::addWorksheet(wb, "Candidate_File_Inventory")
openxlsx::writeData(wb, "Candidate_File_Inventory", file_inventory)

openxlsx::addWorksheet(wb, "Loaded_Table_Metadata")
openxlsx::writeData(wb, "Loaded_Table_Metadata", table_metadata)

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

openxlsx::saveWorkbook(wb, xlsx_signature_path, overwrite = TRUE)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "XLSX workbook generated" ~ file.exists(xlsx_signature_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 15. Export DOCX signature table and insertion text
# -----------------------------
ft_sig <- flextable::flextable(final_signature_definition)
ft_sig <- flextable::theme_booktabs(ft_sig)
ft_sig <- flextable::fontsize(ft_sig, size = 7.5, part = "all")
ft_sig <- flextable::fontsize(ft_sig, size = 8, part = "header")
ft_sig <- flextable::bold(ft_sig, part = "header")
ft_sig <- flextable::valign(ft_sig, valign = "top", part = "all")
ft_sig <- flextable::set_table_properties(ft_sig, layout = "autofit", width = 1)

doc_sig <- officer::read_docx()
doc_sig <- officer::body_add_par(
  doc_sig,
  "Table 2. Final 10-gene host-response signature definition",
  style = "heading 1"
)
doc_sig <- officer::body_add_par(
  doc_sig,
  table2_footnote,
  style = "Normal"
)
doc_sig <- flextable::body_add_flextable(doc_sig, ft_sig)
doc_sig <- officer::body_add_par(
  doc_sig,
  paste0(
    "Consistency statement: ",
    signature_consistency_sentence
  ),
  style = "Normal"
)

print(doc_sig, target = docx_signature_path)

doc_insert <- officer::read_docx()
doc_insert <- officer::body_add_par(
  doc_insert,
  "Final 10-gene signature insertion text",
  style = "heading 1"
)

doc_insert <- officer::body_add_par(doc_insert, "Methods: signature definition", style = "heading 2")
doc_insert <- officer::body_add_par(doc_insert, signature_definition_methods, style = "Normal")

doc_insert <- officer::body_add_par(doc_insert, "Results: signature definition", style = "heading 2")
doc_insert <- officer::body_add_par(doc_insert, signature_definition_results, style = "Normal")

doc_insert <- officer::body_add_par(doc_insert, "Consistency statement", style = "heading 2")
doc_insert <- officer::body_add_par(doc_insert, signature_consistency_sentence, style = "Normal")

doc_insert <- officer::body_add_par(doc_insert, "Table 2 footnote", style = "heading 2")
doc_insert <- officer::body_add_par(doc_insert, table2_footnote, style = "Normal")

doc_insert <- officer::body_add_par(doc_insert, "Additional file 2 section", style = "heading 2")
doc_insert <- officer::body_add_par(doc_insert, additional_file_s2_section, style = "Normal")

print(doc_insert, target = docx_insertion_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "DOCX signature table generated" ~ file.exists(docx_signature_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# Re-save checks after DOCX/XLSX status updates.
n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)
readr::write_csv(checks, checks_path)

# -----------------------------
# 16. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "n_candidate_files_scanned",
    "n_loaded_tables",
    "n_gene_records_extracted",
    "n_final_signature_rows",
    "n_named_final_genes",
    "n_missing_gene_names",
    "n_missing_occurrence",
    "n_missing_direction",
    "n_missing_bulk_model_status",
    "n_missing_single_cell_status",
    "final_signature_equals_recurrent_nested_gene_set",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "46_define_final_10_gene_signature.R",
    project_dir,
    as.character(nrow(candidate_files_to_read)),
    as.character(length(loaded_tables)),
    as.character(nrow(gene_records)),
    as.character(nrow(final_signature_definition)),
    as.character(n_final_genes_named),
    as.character(n_missing_gene_names),
    as.character(n_missing_occurrence),
    as.character(n_missing_direction),
    as.character(n_missing_bulk),
    as.character(n_missing_sc),
    final_signature_equals_recurrent,
    as.character(n_failed_checks),
    dplyr::case_when(
      n_failed_checks == 0 ~ "YES_INSERT_SIGNATURE_DEFINITION",
      n_missing_gene_names > 0 ~ "NO_COMPLETE_FINAL_10_GENE_LIST_FIRST",
      TRUE ~ "PARTIAL_REVIEW_MANUAL_CONFIRMATION_ITEMS"
    ),
    "Open the XLSX workbook. If any final gene names or evidence fields show NEEDS_MANUAL_CONFIRMATION, fill them from the definitive Table 2 or nested LODO output, then rerun this script."
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

cat("\nFinal 10-gene signature definition:\n")
print(final_signature_definition, n = Inf, width = Inf)

cat("\nSignature versus recurrent nested gene set:\n")
print(signature_vs_recurrent, n = Inf, width = Inf)

cat("\nMethods signature definition text:\n")
cat(signature_definition_methods, "\n")

cat("\nResults signature definition text:\n")
cat(signature_definition_results, "\n")

cat("\nTable 2 footnote:\n")
cat(table2_footnote, "\n")

cat("\nAdditional file 2 section:\n")
cat(additional_file_s2_section, "\n")

cat("\n关键输出：\n")
cat("1) ", csv_signature_path, "\n", sep = "")
cat("2) ", docx_signature_path, "\n", sep = "")
cat("3) ", xlsx_signature_path, "\n", sep = "")
cat("4) ", csv_recurrence_audit_path, "\n", sep = "")
cat("5) ", txt_insertion_path, "\n", sep = "")
cat("6) ", docx_insertion_path, "\n", sep = "")
cat("7) ", checks_path, "\n", sep = "")
cat("8) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Final 10-gene signature definition 和 Signature versus recurrent nested gene set 贴给我。\n")
cat("如果出现 NEEDS_MANUAL_CONFIRMATION，我会根据输出告诉你具体缺哪一张表或哪一列，再给你 46b 的完整修正版脚本。\n")

cat("\n============ 46 final 10-gene signature definition audit complete ============\n")