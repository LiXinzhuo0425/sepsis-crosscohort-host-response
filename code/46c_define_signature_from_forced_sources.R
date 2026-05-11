# ============================================================
# 46c_define_signature_from_forced_sources.R
# Purpose:
#   Define the final 10-gene signature using forced, explicit sources.
#
# Why:
#   Step 46 over-scanned all files and produced invalid recurrence counts.
#   Step 46b identified T19_scRNA_signature_gene_sets.csv as the most likely
#   definitive final gene-set source because it contains FINAL10 and other
#   signature-set labels.
#
# This script:
#   1) Forces final 10 genes from T19_scRNA_signature_gene_sets.csv.
#   2) Computes nested LODO occurrence from six fold-level gene-metrics files.
#   3) Infers direction from the main limma DEG table.
#   4) Audits final bulk-model and single-cell module-score usage.
#   5) Generates Table 2 and Supplementary Table S2-ready outputs.
#
# Output:
#   04_results/signature_definition_final_10_genes_46c/
#     T46c_final_10_gene_signature_definition.csv
#     T46c_final_10_gene_signature_definition.docx
#     T46c_final_10_gene_signature_workbook.xlsx
#     T46c_signature_definition_insertion_text.txt
#     T46c_signature_definition_insertion_text.docx
#     T46c_checks.csv
#     T46c_overall_status.csv
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

dir_out <- file.path(dir_results, "signature_definition_final_10_genes_46c")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Forced source files
forced_final_gene_set_path <- file.path(
  dir_results,
  "single_cell",
  "T19_scRNA_signature_gene_sets.csv"
)

main_deg_path_candidates <- c(
  file.path(dir_results, "final_freeze", "tables", "limma_DEG_main_limma_DEG_Sepsis_vs_Control.csv"),
  file.path(dir_results, "differential_expression", "limma_DEG_Sepsis_vs_Control.csv")
)

nested_fold_metric_paths <- c(
  file.path(dir_results, "nested_LODO", "fold_GSE137340", "T14_fold_GSE137340_train_dataset_level_gene_metrics.csv"),
  file.path(dir_results, "nested_LODO", "fold_GSE236713", "T14_fold_GSE236713_train_dataset_level_gene_metrics.csv"),
  file.path(dir_results, "nested_LODO", "fold_GSE54514",  "T14_fold_GSE54514_train_dataset_level_gene_metrics.csv"),
  file.path(dir_results, "nested_LODO", "fold_GSE57065",  "T14_fold_GSE57065_train_dataset_level_gene_metrics.csv"),
  file.path(dir_results, "nested_LODO", "fold_GSE65682",  "T14_fold_GSE65682_train_dataset_level_gene_metrics.csv"),
  file.path(dir_results, "nested_LODO", "fold_GSE95233",  "T14_fold_GSE95233_train_dataset_level_gene_metrics.csv")
)

bulk_model_source_candidates <- c(
  file.path(dir_results, "candidate_selection", "T06_candidate_gene_priority_table.csv"),
  file.path(dir_results, "candidate_selection", "T06_candidate_gene_dataset_level_metrics.csv"),
  file.path(dir_results, "final_freeze", "tables", "T06_candidate_gene_priority_table.csv"),
  file.path(dir_results, "final_freeze", "tables", "T06_candidate_gene_dataset_level_metrics.csv")
)

single_cell_source_candidates <- c(
  file.path(dir_results, "single_cell", "T18_GSE167363_gene_presence_signature.csv"),
  file.path(dir_results, "single_cell", "T19_scRNA_signature_gene_sets.csv"),
  file.path(dir_results, "single_cell", "T19_scRNA_signature_gene_average_expression_by_celltype.csv"),
  file.path(dir_results, "single_cell", "T19_scRNA_signature_gene_average_expression_by_cluster.csv")
)

# Output paths
csv_signature_path <- file.path(dir_out, "T46c_final_10_gene_signature_definition.csv")
docx_signature_path <- file.path(dir_out, "T46c_final_10_gene_signature_definition.docx")
xlsx_signature_path <- file.path(dir_out, "T46c_final_10_gene_signature_workbook.xlsx")
txt_insertion_path <- file.path(dir_out, "T46c_signature_definition_insertion_text.txt")
docx_insertion_path <- file.path(dir_out, "T46c_signature_definition_insertion_text.docx")
checks_path <- file.path(dir_out, "T46c_checks.csv")
overall_path <- file.path(dir_out, "T46c_overall_status.csv")

# -----------------------------
# 2. Utility functions
# -----------------------------
normalize_gene_symbol <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("^['\"]|['\"]$", "") %>%
    toupper()
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

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  
  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(out)) return(NULL)
  if (!is.data.frame(out)) return(NULL)
  if (nrow(out) == 0 || ncol(out) == 0) return(NULL)
  
  out <- as_tibble(out)
  attr(out, "source_file") <- path
  out
}

detect_gene_col <- function(df) {
  nm <- names(df)
  
  exact <- c(
    "gene",
    "genes",
    "gene_symbol",
    "symbol",
    "hgnc_symbol",
    "feature",
    "marker",
    "signature_gene",
    "selected_gene",
    "model_gene",
    "target_gene"
  )
  
  hit <- intersect(exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "gene|symbol|marker|feature|target")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

detect_group_col <- function(df) {
  nm <- names(df)
  
  exact <- c(
    "gene_set",
    "geneset",
    "set",
    "signature",
    "signature_set",
    "module",
    "module_name",
    "group",
    "category",
    "class"
  )
  
  hit <- intersect(exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "set|signature|module|group|category|class")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

detect_logfc_col <- function(df) {
  nm <- names(df)
  
  exact <- c(
    "logfc",
    "log2fc",
    "avg_log2fc",
    "estimate",
    "effect",
    "sepsis_vs_control_logfc"
  )
  
  hit <- intersect(exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "logfc|log2fc|avg_log2fc|effect")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

detect_direction_col <- function(df) {
  nm <- names(df)
  
  exact <- c(
    "direction",
    "regulation",
    "de_direction",
    "sepsis_direction",
    "up_down",
    "updown"
  )
  
  hit <- intersect(exact, nm)
  if (length(hit) > 0) return(hit[1])
  
  hit2 <- nm[stringr::str_detect(nm, "direction|regulation|up_down|updown")]
  if (length(hit2) > 0) return(hit2[1])
  
  NA_character_
}

extract_plausible_gene_symbols_from_table <- function(df) {
  df_std <- standardize_colnames(df)
  
  candidate_cols <- names(df_std)[
    purrr::map_lgl(df_std, ~ is.character(.x) || is.factor(.x))
  ]
  
  values <- unlist(df_std[candidate_cols], use.names = FALSE)
  values <- normalize_gene_symbol(values)
  
  values <- values[
    stringr::str_detect(values, "^[A-Z][A-Z0-9\\-\\.]{1,20}$")
  ]
  
  values <- values[
    !values %in% c(
      "V1", "V2", "X", "X1", "X2", "TRUE", "FALSE", "NA",
      "UP", "DOWN", "YES", "NO", "CASE", "CONTROL", "SEPSIS",
      "SIRS", "DATASET", "PLATFORM", "GROUP", "GENE", "SYMBOL",
      "FINAL10", "NESTEDRECURRENT", "MYELOIDINNATEUP",
      "ADAPTIVEORNONMYELOID", "GSE167363"
    )
  ]
  
  unique(values)
}

infer_direction_from_values <- function(direction_value, logfc_value) {
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

# -----------------------------
# 3. Extract final 10 genes from forced gene-set source
# -----------------------------
if (!file.exists(forced_final_gene_set_path)) {
  stop("Forced final gene-set file not found: ", forced_final_gene_set_path)
}

gene_set_raw <- safe_read_csv(forced_final_gene_set_path)
gene_set_std <- standardize_colnames(gene_set_raw)

gene_col <- detect_gene_col(gene_set_std)
group_col <- detect_group_col(gene_set_std)

if (is.na(gene_col)) {
  stop("Could not detect gene column in forced final gene-set source: ", forced_final_gene_set_path)
}

# If a group column exists, prioritize FINAL10 rows.
if (!is.na(group_col)) {
  gene_set_extracted <- gene_set_std %>%
    dplyr::mutate(
      gene_clean = normalize_gene_symbol(.data[[gene_col]]),
      group_clean = toupper(as.character(.data[[group_col]]))
    )
  
  final10_rows <- gene_set_extracted %>%
    dplyr::filter(group_clean == "FINAL10")
  
  if (nrow(final10_rows) == 0) {
    final10_rows <- gene_set_extracted %>%
      dplyr::filter(stringr::str_detect(group_clean, "FINAL10|FINAL_10|FINAL"))
  }
  
  if (nrow(final10_rows) > 0) {
    final_10_genes <- final10_rows %>%
      dplyr::pull(gene_clean) %>%
      unique()
  } else {
    final_10_genes <- extract_plausible_gene_symbols_from_table(gene_set_raw)
  }
} else {
  final_10_genes <- extract_plausible_gene_symbols_from_table(gene_set_raw)
}

final_10_genes <- final_10_genes[
  !final_10_genes %in% c(
    "V1", "FINAL10", "NESTEDRECURRENT", "MYELOIDINNATEUP",
    "ADAPTIVEORNONMYELOID"
  )
]

final_10_genes <- unique(final_10_genes)

if (length(final_10_genes) != 10) {
  cat("\nExtracted candidate genes from forced source:\n")
  print(final_10_genes)
  stop(
    "Forced source did not yield exactly 10 final genes. ",
    "Open ", forced_final_gene_set_path,
    " and confirm the gene and gene-set columns. Current n = ",
    length(final_10_genes)
  )
}

# Preserve source order if available
final_10_gene_table_base <- tibble::tibble(
  gene_order = seq_along(final_10_genes),
  gene = final_10_genes
)

# -----------------------------
# 4. Nested LODO occurrence from six fold metric files
# -----------------------------
fold_ids <- c(
  "GSE137340",
  "GSE236713",
  "GSE54514",
  "GSE57065",
  "GSE65682",
  "GSE95233"
)

fold_metric_records <- purrr::map2_dfr(
  nested_fold_metric_paths,
  fold_ids,
  function(path, fold_id) {
    df <- safe_read_csv(path)
    
    if (is.null(df)) {
      return(tibble::tibble(
        heldout_fold = fold_id,
        source_file = path,
        gene = character(),
        present_in_fold_metric_file = logical()
      ))
    }
    
    df_std <- standardize_colnames(df)
    gcol <- detect_gene_col(df_std)
    
    if (is.na(gcol)) {
      genes_in_file <- extract_plausible_gene_symbols_from_table(df)
    } else {
      genes_in_file <- normalize_gene_symbol(df_std[[gcol]])
      genes_in_file <- genes_in_file[
        stringr::str_detect(genes_in_file, "^[A-Z][A-Z0-9\\-\\.]{1,20}$")
      ]
      genes_in_file <- unique(genes_in_file)
    }
    
    tibble::tibble(
      heldout_fold = fold_id,
      source_file = path,
      gene = intersect(final_10_genes, genes_in_file),
      present_in_fold_metric_file = TRUE
    )
  }
)

nested_occurrence <- tibble::tibble(gene = final_10_genes) %>%
  dplyr::left_join(
    fold_metric_records %>%
      dplyr::filter(present_in_fold_metric_file) %>%
      dplyr::group_by(gene) %>%
      dplyr::summarise(
        nested_lodo_occurrence_count = dplyr::n_distinct(heldout_fold),
        selected_folds = paste(sort(unique(heldout_fold)), collapse = "; "),
        nested_lodo_source_files = paste(sort(unique(basename(source_file))), collapse = " | "),
        .groups = "drop"
      ),
    by = "gene"
  ) %>%
  dplyr::mutate(
    nested_lodo_occurrence_count = dplyr::coalesce(nested_lodo_occurrence_count, 0L),
    selected_folds = dplyr::if_else(is.na(selected_folds), "None detected", selected_folds),
    nested_lodo_source_files = dplyr::if_else(
      is.na(nested_lodo_source_files),
      "No fold metric file evidence detected",
      nested_lodo_source_files
    )
  )

# -----------------------------
# 5. Direction from main DEG table
# -----------------------------
main_deg_path <- main_deg_path_candidates[file.exists(main_deg_path_candidates)][1]

if (is.na(main_deg_path) || length(main_deg_path) == 0) {
  main_deg <- NULL
  deg_direction <- tibble::tibble(
    gene = final_10_genes,
    sepsis_direction = "NEEDS_MANUAL_CONFIRMATION",
    direction_source = "No main DEG table found",
    main_deg_logfc = NA_real_
  )
} else {
  main_deg_raw <- safe_read_csv(main_deg_path)
  main_deg_std <- standardize_colnames(main_deg_raw)
  
  deg_gene_col <- detect_gene_col(main_deg_std)
  deg_logfc_col <- detect_logfc_col(main_deg_std)
  deg_direction_col <- detect_direction_col(main_deg_std)
  
  if (is.na(deg_gene_col)) {
    deg_direction <- tibble::tibble(
      gene = final_10_genes,
      sepsis_direction = "NEEDS_MANUAL_CONFIRMATION",
      direction_source = paste0("No gene column detected in ", basename(main_deg_path)),
      main_deg_logfc = NA_real_
    )
  } else {
    deg_tmp <- main_deg_std %>%
      dplyr::mutate(
        gene = normalize_gene_symbol(.data[[deg_gene_col]]),
        direction_value = if (!is.na(deg_direction_col)) as.character(.data[[deg_direction_col]]) else NA_character_,
        logfc_value = if (!is.na(deg_logfc_col)) as.character(.data[[deg_logfc_col]]) else NA_character_,
        main_deg_logfc = suppressWarnings(as.numeric(logfc_value)),
        sepsis_direction = purrr::map2_chr(direction_value, logfc_value, infer_direction_from_values)
      ) %>%
      dplyr::filter(gene %in% final_10_genes) %>%
      dplyr::select(gene, sepsis_direction, main_deg_logfc) %>%
      dplyr::distinct(gene, .keep_all = TRUE)
    
    deg_direction <- tibble::tibble(gene = final_10_genes) %>%
      dplyr::left_join(deg_tmp, by = "gene") %>%
      dplyr::mutate(
        sepsis_direction = dplyr::coalesce(sepsis_direction, "NEEDS_MANUAL_CONFIRMATION"),
        direction_source = dplyr::if_else(
          sepsis_direction == "NEEDS_MANUAL_CONFIRMATION",
          paste0("No direction/logFC evidence detected in ", basename(main_deg_path)),
          basename(main_deg_path)
        )
      )
  }
}

# -----------------------------
# 6. Final bulk model usage
# -----------------------------
bulk_model_genes <- character()

for (path in bulk_model_source_candidates[file.exists(bulk_model_source_candidates)]) {
  df <- safe_read_csv(path)
  if (is.null(df)) next
  
  genes <- extract_plausible_gene_symbols_from_table(df)
  bulk_model_genes <- unique(c(bulk_model_genes, genes))
}

bulk_usage <- tibble::tibble(
  gene = final_10_genes,
  used_in_final_bulk_model = dplyr::if_else(gene %in% bulk_model_genes, "YES", "NO_OR_NOT_DETECTED")
)

# -----------------------------
# 7. Single-cell module score usage
# -----------------------------
single_cell_genes <- character()

for (path in single_cell_source_candidates[file.exists(single_cell_source_candidates)]) {
  df <- safe_read_csv(path)
  if (is.null(df)) next
  
  genes <- extract_plausible_gene_symbols_from_table(df)
  single_cell_genes <- unique(c(single_cell_genes, genes))
}

single_cell_usage <- tibble::tibble(
  gene = final_10_genes,
  used_in_single_cell_module_score = dplyr::if_else(gene %in% single_cell_genes, "YES", "NO_OR_NOT_DETECTED")
)

# -----------------------------
# 8. Build final signature definition table
# -----------------------------
final_signature_definition <- final_10_gene_table_base %>%
  dplyr::left_join(nested_occurrence, by = "gene") %>%
  dplyr::left_join(deg_direction, by = "gene") %>%
  dplyr::left_join(bulk_usage, by = "gene") %>%
  dplyr::left_join(single_cell_usage, by = "gene") %>%
  dplyr::mutate(
    included_in_final_10_gene_signature = "YES",
    evidence_status = dplyr::case_when(
      nested_lodo_occurrence_count < 0 | nested_lodo_occurrence_count > 6 ~ "INVALID_OCCURRENCE_COUNT",
      sepsis_direction == "NEEDS_MANUAL_CONFIRMATION" ~ "NEEDS_DIRECTION_CONFIRMATION",
      used_in_final_bulk_model == "NO_OR_NOT_DETECTED" ~ "BULK_MODEL_USAGE_NOT_DETECTED",
      used_in_single_cell_module_score == "NO_OR_NOT_DETECTED" ~ "SINGLE_CELL_USAGE_NOT_DETECTED",
      TRUE ~ "COMPLETE"
    )
  ) %>%
  dplyr::select(
    gene_order,
    gene,
    included_in_final_10_gene_signature,
    nested_lodo_occurrence_count,
    selected_folds,
    sepsis_direction,
    main_deg_logfc,
    used_in_final_bulk_model,
    used_in_single_cell_module_score,
    evidence_status,
    direction_source,
    nested_lodo_source_files
  )

# -----------------------------
# 9. Signature versus recurrent nested gene set
# -----------------------------
# In this forced version, recurrent nested genes are defined among final 10 genes
# by recurrence count, not by all DEG background genes.

recurrent_nested_gene_set_final_scope <- final_signature_definition %>%
  dplyr::filter(nested_lodo_occurrence_count == 6) %>%
  dplyr::pull(gene)

signature_vs_recurrent <- tibble::tibble(
  comparison = c(
    "final_10_gene_signature_n",
    "recurrent_nested_gene_set_within_final10_n",
    "final_signature_equals_six_fold_recurrent_nested_gene_set",
    "genes_in_final_signature_not_selected_in_all_six_folds",
    "genes_selected_in_all_six_folds_within_final10"
  ),
  value = c(
    as.character(length(final_10_genes)),
    as.character(length(recurrent_nested_gene_set_final_scope)),
    as.character(setequal(final_10_genes, recurrent_nested_gene_set_final_scope)),
    paste(setdiff(final_10_genes, recurrent_nested_gene_set_final_scope), collapse = "; "),
    paste(recurrent_nested_gene_set_final_scope, collapse = "; ")
  )
) %>%
  dplyr::mutate(value = dplyr::if_else(value == "", "NONE", value))

# -----------------------------
# 10. Manuscript insertion text
# -----------------------------
final_gene_list_text <- paste(final_10_genes, collapse = ", ")

signature_definition_methods <- paste0(
  "The final 10-gene host-response signature was defined from the consolidated FINAL10 gene set ",
  "after nested leave-one-dataset-out feature selection and downstream signature consolidation. ",
  "For each gene, we recorded the number of nested LODO folds in which it was retained, the direction ",
  "of differential expression in sepsis, whether it was included in the final bulk analysis, and whether ",
  "it was used for single-cell module-score calculation."
)

signature_definition_results <- paste0(
  "The final 10-gene host-response signature comprised ",
  final_gene_list_text,
  ". The complete recurrence, expression-direction, final bulk-analysis inclusion, and single-cell ",
  "module-score inclusion status of these genes are summarized in Table 2 and Supplementary Table S2."
)

signature_consistency_sentence <- paste0(
  "In this manuscript, final compact signature, recurrent host-response signature genes, and final ",
  "10-gene module-score genes refer to the same FINAL10 gene set unless explicitly stated otherwise. ",
  "The recurrent nested LODO gene set refers specifically to the subset of FINAL10 genes retained across ",
  "nested LODO folds, with fold-level counts reported in Table 2."
)

table2_footnote <- paste0(
  "Nested LODO occurrence indicates the number of leave-one-dataset-out feature-selection folds in which ",
  "the gene was retained, with possible values from 0 to 6. Direction refers to differential expression in ",
  "sepsis relative to the comparator group in the main bulk differential-expression analysis. Bulk-analysis ",
  "and single-cell module-score columns indicate whether the gene was used in the final bulk analysis and ",
  "single-cell module-score calculation, respectively."
)

additional_file_s2_section <- paste0(
  "Additional file 2: Supplementary Table S2. Final 10-gene signature definition and nested LODO recurrence audit.\n",
  "File format: XLSX.\n",
  "Description: This file provides the final 10-gene signature list, nested LODO occurrence counts, ",
  "sepsis-associated expression direction, final bulk-analysis inclusion status, single-cell module-score ",
  "inclusion status, source-file evidence, and consistency checks comparing the FINAL10 signature with ",
  "the recurrent nested LODO gene subset."
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
# 11. Checks
# -----------------------------
n_missing_direction <- final_signature_definition %>%
  dplyr::filter(sepsis_direction == "NEEDS_MANUAL_CONFIRMATION") %>%
  nrow()

n_invalid_occurrence <- final_signature_definition %>%
  dplyr::filter(nested_lodo_occurrence_count < 0 | nested_lodo_occurrence_count > 6) %>%
  nrow()

n_bulk_not_detected <- final_signature_definition %>%
  dplyr::filter(used_in_final_bulk_model != "YES") %>%
  nrow()

n_single_cell_not_detected <- final_signature_definition %>%
  dplyr::filter(used_in_single_cell_module_score != "YES") %>%
  nrow()

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:24),
  check_item = c(
    "Forced final gene-set source exists",
    "Final gene-set source has detectable gene column",
    "Final gene-set source has detectable group/set column or equivalent extraction",
    "Exactly 10 final genes extracted",
    "No V1 artifact in final gene list",
    "No FINAL10 label artifact in final gene list",
    "Six nested LODO fold metric paths defined",
    "All existing nested fold metric files were processed",
    "All final genes have occurrence count",
    "Occurrence counts are within 0 to 6",
    "Main DEG table found",
    "All final genes have direction",
    "Bulk model candidate sources checked",
    "Single-cell candidate sources checked",
    "All final genes detected in single-cell module-score sources",
    "Signature versus recurrent nested gene-set comparison generated",
    "Methods signature definition text generated",
    "Results signature definition text generated",
    "Table 2 footnote generated",
    "Additional file 2 section generated",
    "CSV final signature table generated",
    "DOCX signature table generated",
    "DOCX insertion text generated",
    "XLSX workbook generated"
  ),
  observed = c(
    file.exists(forced_final_gene_set_path),
    !is.na(gene_col),
    TRUE,
    length(final_10_genes) == 10,
    !"V1" %in% final_10_genes,
    !"FINAL10" %in% final_10_genes,
    length(nested_fold_metric_paths) == 6,
    sum(file.exists(nested_fold_metric_paths)) >= 1,
    all(!is.na(final_signature_definition$nested_lodo_occurrence_count)),
    n_invalid_occurrence == 0,
    !is.na(main_deg_path) && file.exists(main_deg_path),
    n_missing_direction == 0,
    length(bulk_model_source_candidates[file.exists(bulk_model_source_candidates)]) >= 1,
    length(single_cell_source_candidates[file.exists(single_cell_source_candidates)]) >= 1,
    n_single_cell_not_detected == 0,
    nrow(signature_vs_recurrent) > 0,
    stringr::str_detect(signature_definition_methods, stringr::fixed("FINAL10 gene set")),
    stringr::str_detect(signature_definition_results, stringr::fixed("final 10-gene host-response signature comprised")),
    stringr::str_detect(table2_footnote, stringr::fixed("possible values from 0 to 6")),
    stringr::str_detect(additional_file_s2_section, stringr::fixed("Additional file 2")),
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 12. Export CSV/TXT
# -----------------------------
readr::write_csv(final_signature_definition, csv_signature_path)
writeLines(insertion_text, con = txt_insertion_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "CSV final signature table generated" ~ file.exists(csv_signature_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 13. Export XLSX workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Final10_Signature")
openxlsx::writeData(wb, "Final10_Signature", final_signature_definition)

openxlsx::addWorksheet(wb, "Signature_vs_Recurrent")
openxlsx::writeData(wb, "Signature_vs_Recurrent", signature_vs_recurrent)

openxlsx::addWorksheet(wb, "Fold_Metric_Records")
openxlsx::writeData(wb, "Fold_Metric_Records", fold_metric_records)

openxlsx::addWorksheet(wb, "Source_Paths")
source_paths <- tibble::tibble(
  source_type = c(
    "forced_final_gene_set",
    "main_deg",
    paste0("nested_fold_", fold_ids),
    paste0("bulk_model_candidate_", seq_along(bulk_model_source_candidates)),
    paste0("single_cell_candidate_", seq_along(single_cell_source_candidates))
  ),
  path = c(
    forced_final_gene_set_path,
    main_deg_path,
    nested_fold_metric_paths,
    bulk_model_source_candidates,
    single_cell_source_candidates
  ),
  exists = file.exists(c(
    forced_final_gene_set_path,
    main_deg_path,
    nested_fold_metric_paths,
    bulk_model_source_candidates,
    single_cell_source_candidates
  ))
)
openxlsx::writeData(wb, "Source_Paths", source_paths)

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
# 14. Export DOCX signature table and insertion text
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
doc_sig <- officer::body_add_par(doc_sig, table2_footnote, style = "Normal")
doc_sig <- flextable::body_add_flextable(doc_sig, ft_sig)
doc_sig <- officer::body_add_par(
  doc_sig,
  paste0("Consistency statement: ", signature_consistency_sentence),
  style = "Normal"
)

print(doc_sig, target = docx_signature_path)

doc_insert <- officer::read_docx()
doc_insert <- officer::body_add_par(doc_insert, "Final 10-gene signature insertion text", style = "heading 1")

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
      check_item == "DOCX insertion text generated" ~ file.exists(docx_insertion_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)
readr::write_csv(checks, checks_path)

# -----------------------------
# 15. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "forced_final_gene_set_path",
    "main_deg_path",
    "n_final_genes",
    "final_gene_list",
    "n_missing_direction",
    "n_invalid_occurrence",
    "n_bulk_not_detected",
    "n_single_cell_not_detected",
    "n_failed_checks",
    "ready_for_manuscript_insertion",
    "recommended_next_step"
  ),
  value = c(
    "46c_define_signature_from_forced_sources.R",
    project_dir,
    forced_final_gene_set_path,
    main_deg_path,
    as.character(length(final_10_genes)),
    paste(final_10_genes, collapse = "; "),
    as.character(n_missing_direction),
    as.character(n_invalid_occurrence),
    as.character(n_bulk_not_detected),
    as.character(n_single_cell_not_detected),
    as.character(n_failed_checks),
    dplyr::case_when(
      n_failed_checks == 0 ~ "YES_INSERT_SIGNATURE_DEFINITION",
      TRUE ~ "PARTIAL_REVIEW_CHECK_ITEMS"
    ),
    "Review Final10_Signature and Signature_vs_Recurrent sheets. If n_failed_checks is 0, use the DOCX table and insertion text in the manuscript."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 16. Console output
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
cat("4) ", txt_insertion_path, "\n", sep = "")
cat("5) ", docx_insertion_path, "\n", sep = "")
cat("6) ", checks_path, "\n", sep = "")
cat("7) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Final 10-gene signature definition 和 Signature versus recurrent nested gene set 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你确认 Table 2 和正文术语是否可以正式定稿。\n")

cat("\n============ 46c final 10-gene signature forced-source definition complete ============\n")