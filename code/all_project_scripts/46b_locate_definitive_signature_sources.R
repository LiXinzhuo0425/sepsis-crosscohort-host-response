# ============================================================
# 46b_locate_definitive_signature_sources.R
# Purpose:
#   Locate definitive source files for the final 10-gene signature.
#
# Why this script is needed:
#   Step 46 over-scanned all 04_results files and mistakenly counted DEG
#   background files as nested LODO recurrence evidence. This produced
#   impossible recurrence counts such as 17 or 19 for a six-fold LODO design
#   and incorrectly included V1 as a gene.
#
# This script does NOT define the final signature automatically.
# It finds the most likely source files containing:
#   - final 10-gene signature
#   - Table 2 candidate data
#   - nested LODO selected genes
#   - final bulk model genes
#   - single-cell module-score genes
#
# Output:
#   04_results/signature_definition_final_10_genes_46b/
#     T46b_candidate_signature_source_files.csv
#     T46b_candidate_tables_containing_known_signature_genes.csv
#     T46b_candidate_gene_sets_by_file.csv
#     T46b_manual_final_10_gene_template.csv
#     T46b_checks.csv
#     T46b_overall_status.csv
#
# Next step:
#   Use the output candidate file list to identify the definitive Table 2
#   or final signature CSV, then run 46c with that source as forced input.
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
})

# -----------------------------
# 1. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
dir_results <- file.path(project_dir, "04_results")

dir_out <- file.path(dir_results, "signature_definition_final_10_genes_46b")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

candidate_files_path <- file.path(dir_out, "T46b_candidate_signature_source_files.csv")
candidate_tables_path <- file.path(dir_out, "T46b_candidate_tables_containing_known_signature_genes.csv")
candidate_gene_sets_path <- file.path(dir_out, "T46b_candidate_gene_sets_by_file.csv")
manual_template_path <- file.path(dir_out, "T46b_manual_final_10_gene_template.csv")
xlsx_path <- file.path(dir_out, "T46b_signature_source_discovery_workbook.xlsx")
checks_path <- file.path(dir_out, "T46b_checks.csv")
overall_path <- file.path(dir_out, "T46b_overall_status.csv")

# -----------------------------
# 2. Known genes from Step 46
# -----------------------------
# V1 is intentionally excluded because it is probably an imported column name
# or row-name artifact rather than a valid gene symbol.
known_signature_genes <- c(
  "CD177",
  "TDRD9",
  "ANKRD22",
  "RAB31",
  "RNASE3",
  "VNN1",
  "EMILIN2",
  "FAM20A",
  "HK3"
)

known_signature_genes <- unique(toupper(known_signature_genes))

# -----------------------------
# 3. Utility functions
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

safe_read_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  out <- tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    } else if (ext %in% c("tsv", "txt")) {
      readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
    } else if (ext == "rds") {
      obj <- readRDS(path)
      if (is.data.frame(obj)) {
        obj
      } else if (is.list(obj)) {
        dfs <- purrr::keep(obj, is.data.frame)
        if (length(dfs) > 0) dfs[[1]] else NULL
      } else {
        NULL
      }
    } else if (ext %in% c("rda", "rdata")) {
      env <- new.env(parent = emptyenv())
      load(path, envir = env)
      objs <- lapply(ls(env), function(nm) get(nm, envir = env))
      dfs <- purrr::keep(objs, is.data.frame)
      if (length(dfs) > 0) dfs[[1]] else NULL
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

detect_gene_like_columns <- function(df) {
  nm <- names(df)
  
  direct_candidates <- nm[stringr::str_detect(
    nm,
    "gene|symbol|marker|feature|signature|predictor|variable|term|id"
  )]
  
  # Also inspect all character columns if names are uninformative.
  char_cols <- nm[purrr::map_lgl(df, ~ is.character(.x) || is.factor(.x))]
  
  unique(c(direct_candidates, char_cols))
}

extract_gene_hits_from_table <- function(df, path, known_genes) {
  df_std <- standardize_colnames(df)
  gene_like_cols <- detect_gene_like_columns(df_std)
  
  if (length(gene_like_cols) == 0) {
    return(NULL)
  }
  
  hit_rows <- purrr::map_dfr(
    gene_like_cols,
    function(col) {
      values <- normalize_gene_symbol(df_std[[col]])
      hits <- values[values %in% known_genes]
      
      if (length(hits) == 0) return(NULL)
      
      tibble::tibble(
        source_file = path,
        source_file_name = basename(path),
        column_with_gene_hit = col,
        matched_gene = hits
      )
    }
  )
  
  if (nrow(hit_rows) == 0) return(NULL)
  
  hit_rows
}

extract_all_plausible_gene_symbols <- function(df) {
  df_std <- standardize_colnames(df)
  gene_like_cols <- detect_gene_like_columns(df_std)
  
  if (length(gene_like_cols) == 0) {
    return(character())
  }
  
  values <- unlist(df_std[gene_like_cols], use.names = FALSE)
  values <- normalize_gene_symbol(values)
  
  values <- values[
    stringr::str_detect(values, "^[A-Z][A-Z0-9\\-\\.]{1,20}$")
  ]
  
  # Remove obvious non-gene artifacts frequently introduced by CSV import.
  values <- values[
    !values %in% c(
      "V1", "V2", "X", "X1", "X2", "TRUE", "FALSE", "NA",
      "UP", "DOWN", "YES", "NO", "CASE", "CONTROL", "SEPSIS",
      "SIRS", "DATASET", "PLATFORM", "GROUP", "GENE", "SYMBOL"
    )
  ]
  
  unique(values)
}

score_file_name <- function(path) {
  p <- stringr::str_to_lower(path)
  
  score <- 0
  score <- score + ifelse(stringr::str_detect(p, "final"), 8, 0)
  score <- score + ifelse(stringr::str_detect(p, "signature"), 8, 0)
  score <- score + ifelse(stringr::str_detect(p, "compact"), 6, 0)
  score <- score + ifelse(stringr::str_detect(p, "table2|table_2|t2"), 6, 0)
  score <- score + ifelse(stringr::str_detect(p, "selected|selection|feature|marker"), 5, 0)
  score <- score + ifelse(stringr::str_detect(p, "nested|lodo|fold|heldout|held_out|leftout|left_out"), 5, 0)
  score <- score + ifelse(stringr::str_detect(p, "model|glm|lasso|coef|coefficient|predictor"), 4, 0)
  score <- score + ifelse(stringr::str_detect(p, "module|score|single|scrna|seurat"), 3, 0)
  score <- score - ifelse(stringr::str_detect(p, "deg_train_only|all_deg|background|expression_matrix|normalized|sample|metadata"), 8, 0)
  score <- score - ifelse(stringr::str_detect(p, "table1|figure|fig|pca|roc|auc|calibration"), 4, 0)
  
  score
}

# -----------------------------
# 4. Inventory result files
# -----------------------------
all_files <- list.files(
  dir_results,
  pattern = "\\.(csv|tsv|txt|rds|RDS|rda|RData)$",
  recursive = TRUE,
  full.names = TRUE
)

all_files <- all_files[
  !stringr::str_detect(all_files, "signature_definition_final_10_genes_46b")
]

file_inventory <- tibble::tibble(
  source_file = all_files,
  source_file_name = basename(all_files),
  extension = tolower(tools::file_ext(all_files)),
  file_name_score = purrr::map_dbl(all_files, score_file_name)
) %>%
  dplyr::arrange(dplyr::desc(file_name_score), source_file)

# Prioritize likely files but still read broad enough.
files_to_read <- file_inventory %>%
  dplyr::filter(file_name_score >= -2) %>%
  dplyr::slice_head(n = 400)

# -----------------------------
# 5. Read files and detect known signature genes
# -----------------------------
loaded <- purrr::map(files_to_read$source_file, safe_read_table)

loaded <- purrr::imap(
  loaded,
  function(df, i) {
    if (is.null(df)) return(NULL)
    attr(df, "source_file") <- files_to_read$source_file[[i]]
    df
  }
)

loaded <- purrr::compact(loaded)

candidate_hit_tables <- purrr::map_dfr(
  loaded,
  function(df) {
    path <- attr(df, "source_file")
    extract_gene_hits_from_table(df, path, known_signature_genes)
  }
)

if (nrow(candidate_hit_tables) == 0) {
  candidate_hit_tables <- tibble::tibble(
    source_file = character(),
    source_file_name = character(),
    column_with_gene_hit = character(),
    matched_gene = character()
  )
}

candidate_table_summary <- candidate_hit_tables %>%
  dplyr::group_by(source_file, source_file_name) %>%
  dplyr::summarise(
    n_known_signature_genes_matched = dplyr::n_distinct(matched_gene),
    matched_known_signature_genes = paste(sort(unique(matched_gene)), collapse = "; "),
    columns_with_gene_hits = paste(sort(unique(column_with_gene_hit)), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    file_inventory %>% dplyr::select(source_file, file_name_score),
    by = "source_file"
  ) %>%
  dplyr::mutate(
    total_priority_score = file_name_score + 3 * n_known_signature_genes_matched,
    likely_source_type = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(source_file), "final|signature|compact|table2|table_2|t2") &
        n_known_signature_genes_matched >= 8 ~ "LIKELY_FINAL_SIGNATURE_OR_TABLE2",
      stringr::str_detect(stringr::str_to_lower(source_file), "nested|lodo|fold|selected|selection") &
        n_known_signature_genes_matched >= 5 ~ "LIKELY_NESTED_LODO_SELECTION",
      stringr::str_detect(stringr::str_to_lower(source_file), "module|score|single|scrna|seurat") &
        n_known_signature_genes_matched >= 5 ~ "LIKELY_SINGLE_CELL_MODULE_SCORE_SOURCE",
      stringr::str_detect(stringr::str_to_lower(source_file), "model|glm|lasso|coef|coefficient|predictor") &
        n_known_signature_genes_matched >= 5 ~ "LIKELY_FINAL_BULK_MODEL_SOURCE",
      n_known_signature_genes_matched >= 8 ~ "HIGH_MATCH_BUT_SOURCE_TYPE_UNCLEAR",
      TRUE ~ "LOW_OR_MODERATE_MATCH"
    )
  ) %>%
  dplyr::arrange(
    dplyr::desc(n_known_signature_genes_matched),
    dplyr::desc(total_priority_score),
    source_file
  )

# -----------------------------
# 6. Extract candidate gene sets from top files
# -----------------------------
top_source_files <- candidate_table_summary %>%
  dplyr::filter(n_known_signature_genes_matched >= 5) %>%
  dplyr::slice_head(n = 50) %>%
  dplyr::pull(source_file)

candidate_gene_sets <- purrr::map_dfr(
  top_source_files,
  function(path) {
    df <- safe_read_table(path)
    if (is.null(df)) return(NULL)
    
    genes <- extract_all_plausible_gene_symbols(df)
    if (length(genes) == 0) return(NULL)
    
    tibble::tibble(
      source_file = path,
      source_file_name = basename(path),
      n_plausible_gene_symbols = length(genes),
      n_known_signature_genes_matched = sum(known_signature_genes %in% genes),
      known_signature_genes_matched = paste(sort(intersect(known_signature_genes, genes)), collapse = "; "),
      plausible_gene_symbols_preview = paste(head(sort(genes), 60), collapse = "; ")
    )
  }
) %>%
  dplyr::left_join(
    candidate_table_summary %>%
      dplyr::select(source_file, likely_source_type, total_priority_score),
    by = "source_file"
  ) %>%
  dplyr::arrange(
    dplyr::desc(n_known_signature_genes_matched),
    n_plausible_gene_symbols,
    dplyr::desc(total_priority_score)
  )

if (nrow(candidate_gene_sets) == 0) {
  candidate_gene_sets <- tibble::tibble(
    source_file = character(),
    source_file_name = character(),
    n_plausible_gene_symbols = integer(),
    n_known_signature_genes_matched = integer(),
    known_signature_genes_matched = character(),
    plausible_gene_symbols_preview = character(),
    likely_source_type = character(),
    total_priority_score = numeric()
  )
}

# -----------------------------
# 7. Generate manual final 10-gene template
# -----------------------------
manual_final_10_gene_template <- tibble::tibble(
  gene_order = 1:10,
  gene = c(known_signature_genes, "FILL_MISSING_10TH_GENE")[1:10],
  nested_lodo_occurrence_count = c(
    rep(NA_integer_, 10)
  ),
  sepsis_direction = rep("FILL_UPREGULATED_OR_DOWNREGULATED_IN_SEPSIS", 10),
  used_in_final_bulk_model = rep("FILL_YES_OR_NO", 10),
  used_in_single_cell_module_score = rep("FILL_YES_OR_NO", 10),
  source_note = c(
    rep("Known from Step 46 auto-detected candidate set; verify against definitive Table 2 or final signature file.", 9),
    "Missing gene. Fill from definitive final 10-gene signature source."
  )
)

# -----------------------------
# 8. Checks
# -----------------------------
n_high_match_files <- candidate_table_summary %>%
  dplyr::filter(n_known_signature_genes_matched >= 8) %>%
  nrow()

n_likely_final_files <- candidate_table_summary %>%
  dplyr::filter(likely_source_type == "LIKELY_FINAL_SIGNATURE_OR_TABLE2") %>%
  nrow()

n_candidate_gene_sets_exact_or_small <- candidate_gene_sets %>%
  dplyr::filter(
    n_known_signature_genes_matched >= 8,
    n_plausible_gene_symbols <= 30
  ) %>%
  nrow()

checks <- tibble::tibble(
  check_id = sprintf("C%02d", 1:14),
  check_item = c(
    "04_results directory exists",
    "Files discovered",
    "Files loaded",
    "Known signature genes defined",
    "V1 excluded from known signature genes",
    "Candidate tables containing known genes found",
    "At least one file matches eight or more known signature genes",
    "At least one likely final signature or Table 2 file found",
    "Candidate gene sets extracted",
    "At least one high-match small gene-set file found",
    "Manual final 10-gene template generated",
    "Candidate file CSV generated",
    "Candidate gene-set CSV generated",
    "XLSX workbook generated"
  ),
  observed = c(
    dir.exists(dir_results),
    nrow(file_inventory) > 0,
    length(loaded) > 0,
    length(known_signature_genes) == 9,
    !"V1" %in% known_signature_genes,
    nrow(candidate_table_summary) > 0,
    n_high_match_files > 0,
    n_likely_final_files > 0,
    nrow(candidate_gene_sets) > 0,
    n_candidate_gene_sets_exact_or_small > 0,
    nrow(manual_final_10_gene_template) == 10,
    FALSE,
    FALSE,
    FALSE
  )
) %>%
  dplyr::mutate(status = dplyr::if_else(observed, "PASS", "CHECK"))

# -----------------------------
# 9. Export outputs
# -----------------------------
readr::write_csv(candidate_table_summary, candidate_files_path)
readr::write_csv(candidate_hit_tables, candidate_tables_path)
readr::write_csv(candidate_gene_sets, candidate_gene_sets_path)
readr::write_csv(manual_final_10_gene_template, manual_template_path)

checks <- checks %>%
  dplyr::mutate(
    observed = dplyr::case_when(
      check_item == "Candidate file CSV generated" ~ file.exists(candidate_files_path),
      check_item == "Candidate gene-set CSV generated" ~ file.exists(candidate_gene_sets_path),
      TRUE ~ observed
    ),
    status = dplyr::if_else(observed, "PASS", "CHECK")
  )

# -----------------------------
# 10. Export workbook
# -----------------------------
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Candidate_Source_Files")
openxlsx::writeData(wb, "Candidate_Source_Files", candidate_table_summary)

openxlsx::addWorksheet(wb, "Candidate_Gene_Sets")
openxlsx::writeData(wb, "Candidate_Gene_Sets", candidate_gene_sets)

openxlsx::addWorksheet(wb, "All_Gene_Hits")
openxlsx::writeData(wb, "All_Gene_Hits", candidate_hit_tables)

openxlsx::addWorksheet(wb, "Manual_Final_10_Template")
openxlsx::writeData(wb, "Manual_Final_10_Template", manual_final_10_gene_template)

openxlsx::addWorksheet(wb, "File_Inventory")
openxlsx::writeData(wb, "File_Inventory", file_inventory)

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
# 11. Overall status
# -----------------------------
overall_status <- tibble::tibble(
  metric = c(
    "script",
    "project_dir",
    "n_files_inventory",
    "n_files_read",
    "n_candidate_source_files",
    "n_high_match_files_8_or_more_genes",
    "n_likely_final_signature_or_table2_files",
    "n_high_match_small_gene_set_files",
    "n_failed_checks",
    "ready_for_46c",
    "recommended_next_step"
  ),
  value = c(
    "46b_locate_definitive_signature_sources.R",
    project_dir,
    as.character(nrow(file_inventory)),
    as.character(length(loaded)),
    as.character(nrow(candidate_table_summary)),
    as.character(n_high_match_files),
    as.character(n_likely_final_files),
    as.character(n_candidate_gene_sets_exact_or_small),
    as.character(n_failed_checks),
    dplyr::case_when(
      n_likely_final_files > 0 | n_candidate_gene_sets_exact_or_small > 0 ~ "YES_IDENTIFY_DEFINITIVE_SOURCE_FROM_OUTPUT",
      TRUE ~ "NO_MANUAL_TABLE2_OR_FINAL_SIGNATURE_FILE_NEEDED"
    ),
    "Open the XLSX workbook. Review Candidate_Source_Files and Candidate_Gene_Sets. Identify the file that contains the definitive final 10-gene signature or Table 2, then paste its path and the displayed gene list so 46c can be generated with forced input."
  )
)

readr::write_csv(overall_status, overall_path)

# -----------------------------
# 12. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nTop candidate source files:\n")
print(
  candidate_table_summary %>%
    dplyr::select(
      source_file,
      n_known_signature_genes_matched,
      matched_known_signature_genes,
      likely_source_type,
      total_priority_score
    ) %>%
    dplyr::slice_head(n = 30),
  n = 30,
  width = Inf
)

cat("\nTop candidate gene sets:\n")
print(
  candidate_gene_sets %>%
    dplyr::select(
      source_file,
      n_plausible_gene_symbols,
      n_known_signature_genes_matched,
      known_signature_genes_matched,
      likely_source_type,
      plausible_gene_symbols_preview
    ) %>%
    dplyr::slice_head(n = 30),
  n = 30,
  width = Inf
)

cat("\nManual final 10-gene template:\n")
print(manual_final_10_gene_template, n = Inf, width = Inf)

cat("\n关键输出：\n")
cat("1) ", candidate_files_path, "\n", sep = "")
cat("2) ", candidate_tables_path, "\n", sep = "")
cat("3) ", candidate_gene_sets_path, "\n", sep = "")
cat("4) ", manual_template_path, "\n", sep = "")
cat("5) ", xlsx_path, "\n", sep = "")
cat("6) ", checks_path, "\n", sep = "")
cat("7) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Top candidate source files、Top candidate gene sets 贴给我。\n")
cat("如果其中某个文件明显就是最终 Table 2 或 final signature，我会给你 46c 的完整强制输入版代码。\n")

cat("\n============ 46b definitive signature source discovery complete ============\n")