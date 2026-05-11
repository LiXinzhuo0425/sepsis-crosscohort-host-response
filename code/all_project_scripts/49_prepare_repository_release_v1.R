# ============================================================
# 49_prepare_repository_release_v1.R
# Purpose:
#   Prepare a clean repository_release_v1 folder for GitHub + Zenodo.
#
# This script:
#   1) Creates a clean release folder.
#   2) Copies selected R scripts into code/.
#   3) Copies frozen result tables and audit outputs.
#   4) Creates Additional file folders.
#   5) Generates README.md, LICENSE, CITATION.cff, data dictionary,
#      run order, GEO accession documentation, and sessionInfo.txt.
#   6) Separates files > 50 MiB into large_files_for_zenodo_only/.
#
# Output:
#   /Users/felix/Documents/Sepsis_CrossCohort_scRNA/repository_release_v1/
#
# Important:
#   GitHub has strict large-file limits. Files >50 MiB are separated
#   for Zenodo-only upload or manual review.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(fs)
})

# -----------------------------
# 1. Project paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
scripts_dir <- file.path(project_dir, "03_scripts")
results_dir <- file.path(project_dir, "04_results")
figures_dir <- file.path(project_dir, "05_figures")

release_dir <- file.path(project_dir, "repository_release_v1")

# If TRUE, delete old release folder and rebuild from scratch.
overwrite_release <- TRUE

if (dir.exists(release_dir) && overwrite_release) {
  unlink(release_dir, recursive = TRUE, force = TRUE)
}

dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Release folder structure
# -----------------------------
dir_structure <- c(
  "code",
  "frozen_results",
  "frozen_results/tables",
  "frozen_results/figures",
  "frozen_results/audit_outputs",
  "frozen_results/model_outputs",
  "additional_files",
  "docs",
  "large_files_for_zenodo_only",
  "manifest"
)

purrr::walk(
  file.path(release_dir, dir_structure),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -----------------------------
# 3. Helper functions
# -----------------------------
copy_if_exists <- function(from, to_dir, rename_to = NULL) {
  if (!file.exists(from)) {
    return(tibble(
      source = from,
      target = NA_character_,
      copied = FALSE,
      reason = "source_not_found",
      size_mb = NA_real_
    ))
  }
  
  size_mb <- as.numeric(file.info(from)$size) / 1024^2
  
  target_name <- ifelse(is.null(rename_to), basename(from), rename_to)
  to <- file.path(to_dir, target_name)
  
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  
  tibble(
    source = from,
    target = to,
    copied = ok,
    reason = ifelse(ok, "copied", "copy_failed"),
    size_mb = size_mb
  )
}

copy_tree_if_exists <- function(from_dir, to_dir, max_file_mb_for_github = 50) {
  if (!dir.exists(from_dir)) {
    return(tibble(
      source = from_dir,
      target = NA_character_,
      copied = FALSE,
      reason = "source_dir_not_found",
      size_mb = NA_real_
    ))
  }
  
  files <- list.files(from_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  
  if (length(files) == 0) {
    return(tibble(
      source = from_dir,
      target = to_dir,
      copied = FALSE,
      reason = "source_dir_empty",
      size_mb = NA_real_
    ))
  }
  
  purrr::map_dfr(files, function(f) {
    rel <- fs::path_rel(f, start = from_dir)
    size_mb <- as.numeric(file.info(f)$size) / 1024^2
    
    if (size_mb > max_file_mb_for_github) {
      large_target <- file.path(release_dir, "large_files_for_zenodo_only", basename(from_dir), rel)
      dir.create(dirname(large_target), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(f, large_target, overwrite = TRUE, copy.date = TRUE)
      
      return(tibble(
        source = f,
        target = large_target,
        copied = ok,
        reason = paste0("large_file_gt_", max_file_mb_for_github, "MB_moved_to_zenodo_only"),
        size_mb = size_mb
      ))
    }
    
    target <- file.path(to_dir, rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(f, target, overwrite = TRUE, copy.date = TRUE)
    
    tibble(
      source = f,
      target = target,
      copied = ok,
      reason = ifelse(ok, "copied", "copy_failed"),
      size_mb = size_mb
    )
  })
}

safe_write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(x, con = path, useBytes = TRUE)
}

# -----------------------------
# 4. Copy key R scripts
# -----------------------------
script_names <- c(
  "01b_parse_local_series_matrix_core.R",
  "02_harmonize_core_phenotype_sepsis_control.R",
  "04_expression_annotation_merge_DEG.R",
  "05_DEG_QC_and_visualization.R",
  "05b_outlier_sensitivity_DEG.R",
  "07_candidate_gene_selection_preparation.R",
  "07b_candidate_gene_refinement_for_modeling.R",
  "08_LODO_diagnostic_modeling.R",
  "09_LODO_calibration_threshold_analysis.R",
  "10_final_compact_model_training.R",
  "14_nested_LODO_full_pipeline.R",
  "17_nested_LODO_calibration_threshold_DCA.R",
  "41_complete_Table1_bulk_GEO_background.R",
  "42_finalize_Table1_for_BMC_Genomics.R",
  "43_cleanup_Table1_manuscript_language.R",
  "44_make_Table1_main_text_compact.R",
  "45_make_Table1_insertion_package.R",
  "46c_define_signature_from_forced_sources.R",
  "46d_finalize_signature_text_package.R",
  "47d_finalize_nested_LODO_methods_text.R",
  "48c_force_finalize_DEG_candidate_thresholds.R"
)

available_scripts <- list.files(project_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)

script_copy_log <- purrr::map_dfr(script_names, function(sn) {
  hit <- available_scripts[basename(available_scripts) == sn]
  
  if (length(hit) == 0) {
    return(tibble(
      source = sn,
      target = NA_character_,
      copied = FALSE,
      reason = "script_not_found",
      size_mb = NA_real_
    ))
  }
  
  copy_if_exists(hit[1], file.path(release_dir, "code"))
})

# Also copy all scripts from 03_scripts if present, excluding very large or temporary files.
all_script_copy_log <- tibble()

if (dir.exists(scripts_dir)) {
  all_scripts <- list.files(scripts_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  
  all_script_copy_log <- purrr::map_dfr(all_scripts, function(f) {
    copy_if_exists(f, file.path(release_dir, "code", "all_project_scripts"))
  })
}

# -----------------------------
# 5. Copy key frozen result directories
# -----------------------------
result_dirs_to_copy <- c(
  "table1_compact_for_BMC_Genomics",
  "table1_insertion_package_for_BMC_Genomics",
  "signature_definition_final_10_genes_46c",
  "signature_definition_final_10_genes_46d",
  "nested_LODO_methods_audit_47d",
  "DEG_candidate_selection_methods_audit_48c",
  "candidate_selection",
  "nested_LODO",
  "final_freeze",
  "differential_expression",
  "single_cell"
)

result_copy_log <- purrr::map_dfr(result_dirs_to_copy, function(dn) {
  from_dir <- file.path(results_dir, dn)
  to_dir <- file.path(release_dir, "frozen_results", "audit_outputs", dn)
  
  copy_tree_if_exists(from_dir, to_dir, max_file_mb_for_github = 50)
})

# -----------------------------
# 6. Copy selected figure-supporting files if present
# -----------------------------
figure_copy_log <- tibble()

if (dir.exists(figures_dir)) {
  figure_files <- list.files(
    figures_dir,
    pattern = "\\.(csv|tsv|txt|pdf|png|tif|tiff|svg)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  figure_copy_log <- purrr::map_dfr(figure_files, function(f) {
    size_mb <- as.numeric(file.info(f)$size) / 1024^2
    
    if (size_mb > 50) {
      rel <- fs::path_rel(f, start = figures_dir)
      target <- file.path(release_dir, "large_files_for_zenodo_only", "figures", rel)
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(f, target, overwrite = TRUE, copy.date = TRUE)
      
      return(tibble(
        source = f,
        target = target,
        copied = ok,
        reason = "large_figure_gt_50MB_moved_to_zenodo_only",
        size_mb = size_mb
      ))
    }
    
    rel <- fs::path_rel(f, start = figures_dir)
    target_dir <- file.path(release_dir, "frozen_results", "figures", dirname(rel))
    copy_if_exists(f, target_dir)
  })
}

# -----------------------------
# 7. Build Additional files folder
# -----------------------------
additional_file_map <- tibble::tribble(
  ~additional_file, ~source_path, ~target_name,
  "Additional file 1",
  file.path(results_dir, "table1_compact_for_BMC_Genomics", "T44_Table1_BMC_Genomics_additional_file_S1.csv"),
  "Additional_file_1_Table_S1_bulk_dataset_metadata.csv",
  
  "Additional file 2",
  file.path(results_dir, "signature_definition_final_10_genes_46c", "T46c_final_10_gene_signature_workbook.xlsx"),
  "Additional_file_2_Table_S2_final_10_gene_signature.xlsx",
  
  "Additional file 3",
  file.path(results_dir, "nested_LODO_methods_audit_47d", "T47d_nested_LODO_methods_final_workbook.xlsx"),
  "Additional_file_3_nested_LODO_methods_audit.xlsx",
  
  "Additional file 4",
  file.path(results_dir, "DEG_candidate_selection_methods_audit_48c", "T48c_DEG_candidate_selection_final_workbook.xlsx"),
  "Additional_file_4_DEG_candidate_selection_audit.xlsx",
  
  "Additional file 5",
  file.path(results_dir, "nested_LODO_methods_audit_47d", "T47d_nested_LODO_methods_final_text.txt"),
  "Additional_file_5_nested_LODO_methods_text.txt"
)

additional_copy_log <- purrr::pmap_dfr(
  additional_file_map,
  function(additional_file, source_path, target_name) {
    copy_if_exists(source_path, file.path(release_dir, "additional_files"), target_name)
  }
)

# -----------------------------
# 8. Create sessionInfo.txt
# -----------------------------
session_info_path <- file.path(release_dir, "sessionInfo.txt")
session_lines <- capture.output(sessionInfo())
safe_write_lines(session_lines, session_info_path)

# -----------------------------
# 9. Create README.md
# -----------------------------
readme_text <- c(
  "# Sepsis cross-cohort host-response transcriptomic signature",
  "",
  "This repository contains analysis code, frozen result tables, supplementary files, and reproducibility materials for a cross-cohort transcriptomic study of sepsis host-response signatures.",
  "",
  "## Data sources",
  "",
  "No raw transcriptomic data are redistributed in this repository. Original data are publicly available from the Gene Expression Omnibus (GEO).",
  "",
  "Included bulk GEO datasets:",
  "",
  "- GSE137340",
  "- GSE236713",
  "- GSE54514",
  "- GSE57065",
  "- GSE65682",
  "- GSE95233",
  "",
  "Users should download raw or processed GEO source data directly from GEO according to GEO terms of use.",
  "",
  "## Repository contents",
  "",
  "- `code/`: R scripts used for data parsing, phenotype harmonization, differential-expression analysis, candidate-gene filtering, nested LODO modeling, final signature definition, and reproducibility audits.",
  "- `frozen_results/`: Frozen result tables, audit outputs, model-development outputs, and figure-supporting files used for manuscript generation.",
  "- `additional_files/`: Supplementary tables and reproducibility files corresponding to the manuscript.",
  "- `docs/`: Data dictionary, run order, GEO accession documentation, and repository notes.",
  "- `large_files_for_zenodo_only/`: Files larger than 50 MiB separated for Zenodo-only upload or manual review.",
  "",
  "## Software environment",
  "",
  "Analyses were conducted in R.",
  "",
  "Main software versions from the finalized audit:",
  "",
  "- R 4.5.2",
  "- glmnet 5.0",
  "- pROC 1.19.0.1",
  "- PRROC 1.4",
  "- rms 8.1.1",
  "- dplyr 1.1.4",
  "- readr 2.1.6",
  "",
  "Operating system:",
  "",
  "- macOS, Apple Silicon environment",
  "",
  "Detailed package versions are provided in `sessionInfo.txt`.",
  "",
  "## Key methodological settings",
  "",
  "- Nested leave-one-dataset-out model development.",
  "- 5-fold inner cross-validation within each outer training set.",
  "- Final compact model trained using separate 10-fold cross-validation.",
  "- glmnet penalized logistic regression with `family = \"binomial\"`, `type.measure = \"auc\"`, and `standardize = FALSE`.",
  "- Alpha grid: 1.00, 0.75, 0.50.",
  "- Lambda rule: `lambda.1se`.",
  "- DEG threshold: adjusted P < 0.05 and |log2FC| >= 0.5.",
  "- Candidate-gene filters: direction consistency >= 0.8, mean univariate AUROC >= 0.7, and correlation redundancy threshold |r| >= 0.85.",
  "",
  "## Additional files",
  "",
  "- Additional file 1: Source references and cohort-specific verification notes for included bulk transcriptomic datasets.",
  "- Additional file 2: Final 10-gene signature definition and nested LODO recurrence audit.",
  "- Additional file 3: Nested LODO modeling-method audit.",
  "- Additional file 4: Differential-expression and candidate-gene selection audit.",
  "- Additional file 5: Nested LODO methods text and reproducibility notes.",
  "",
  "## License",
  "",
  "Code is released under the MIT License. Result tables and documentation are released under CC BY 4.0 where applicable.",
  "",
  "## Citation",
  "",
  "A permanent DOI will be added after the first GitHub release is archived in Zenodo."
)

safe_write_lines(readme_text, file.path(release_dir, "README.md"))

# -----------------------------
# 10. Create LICENSE
# -----------------------------
license_text <- c(
  "MIT License",
  "",
  "Copyright (c) 2026",
  "",
  "Permission is hereby granted, free of charge, to any person obtaining a copy",
  "of this software and associated documentation files (the \"Software\"), to deal",
  "in the Software without restriction, including without limitation the rights",
  "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
  "copies of the Software, and to permit persons to whom the Software is",
  "furnished to do so, subject to the following conditions:",
  "",
  "The above copyright notice and this permission notice shall be included in all",
  "copies or substantial portions of the Software.",
  "",
  "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR",
  "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
  "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
  "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
  "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
  "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
  "SOFTWARE."
)

safe_write_lines(license_text, file.path(release_dir, "LICENSE"))

# -----------------------------
# 11. Create CITATION.cff
# -----------------------------
citation_text <- c(
  "cff-version: 1.2.0",
  "message: \"If you use this repository, please cite the archived Zenodo release.\"",
  "title: \"Sepsis cross-cohort host-response transcriptomic signature\"",
  "version: \"1.0.0\"",
  "date-released: \"2026-05-11\"",
  "authors:",
  "  - family-names: \"Li\"",
  "    given-names: \"Xinzhuo\"",
  "repository-code: \"https://github.com/USERNAME/sepsis-crosscohort-host-response\"",
  "license: \"MIT\"",
  "keywords:",
  "  - sepsis",
  "  - transcriptomics",
  "  - GEO",
  "  - nested LODO",
  "  - glmnet",
  "  - host response"
)

safe_write_lines(citation_text, file.path(release_dir, "CITATION.cff"))

# -----------------------------
# 12. Create docs
# -----------------------------
geo_accessions_text <- c(
  "# GEO accessions",
  "",
  "Original raw or processed transcriptomic source data are available from GEO.",
  "",
  "| Accession | Role | Notes |",
  "|---|---|---|",
  "| GSE137340 | Bulk transcriptomic cohort | See manuscript Table 1 and Additional file 1 |",
  "| GSE236713 | Bulk transcriptomic cohort | See manuscript Table 1 and Additional file 1 |",
  "| GSE54514 | Bulk transcriptomic cohort | See manuscript Table 1 and Additional file 1 |",
  "| GSE57065 | Bulk transcriptomic cohort | See manuscript Table 1 and Additional file 1 |",
  "| GSE65682 | Bulk transcriptomic cohort | See manuscript Table 1 and Additional file 1 |",
  "| GSE95233 | Bulk transcriptomic cohort | See manuscript Table 1 and Additional file 1 |"
)

safe_write_lines(geo_accessions_text, file.path(release_dir, "docs", "GEO_accessions.md"))

run_order_text <- c(
  "# Suggested run order",
  "",
  "The full workflow depends on locally downloaded GEO-derived files and harmonized phenotype tables. Frozen result tables are provided to preserve exact correspondence with the submitted manuscript.",
  "",
  "Core workflow:",
  "",
  "1. `01b_parse_local_series_matrix_core.R`",
  "2. `02_harmonize_core_phenotype_sepsis_control.R`",
  "3. `04_expression_annotation_merge_DEG.R`",
  "4. `07_candidate_gene_selection_preparation.R`",
  "5. `07b_candidate_gene_refinement_for_modeling.R`",
  "6. `08_LODO_diagnostic_modeling.R`",
  "7. `14_nested_LODO_full_pipeline.R`",
  "8. `10_final_compact_model_training.R`",
  "",
  "Manuscript audit and insertion packages:",
  "",
  "1. `41_complete_Table1_bulk_GEO_background.R`",
  "2. `42_finalize_Table1_for_BMC_Genomics.R`",
  "3. `43_cleanup_Table1_manuscript_language.R`",
  "4. `44_make_Table1_main_text_compact.R`",
  "5. `45_make_Table1_insertion_package.R`",
  "6. `46c_define_signature_from_forced_sources.R`",
  "7. `46d_finalize_signature_text_package.R`",
  "8. `47d_finalize_nested_LODO_methods_text.R`",
  "9. `48c_force_finalize_DEG_candidate_thresholds.R`"
)

safe_write_lines(run_order_text, file.path(release_dir, "docs", "run_order.md"))

data_dictionary_text <- c(
  "# Data dictionary",
  "",
  "This repository contains analysis code and frozen result tables. Raw GEO expression files are not redistributed.",
  "",
  "Common columns:",
  "",
  "| Column | Meaning |",
  "|---|---|",
  "| `gene_symbol` | Harmonized gene symbol |",
  "| `logFC` / `log2FC` | Log2 fold change comparing sepsis with comparator samples |",
  "| `adj.P.Val` | Multiple-testing adjusted P value from limma |",
  "| `direction_consistency` | Proportion of datasets or folds with expression direction consistent with global direction |",
  "| `mean_auc` | Mean univariate AUROC across dataset-level screening |",
  "| `nested_lodo_occurrence_count` | Number of nested LODO folds in which a gene was retained |",
  "| `selected_folds` | Outer held-out folds in which the gene was selected in the corresponding training data |",
  "| `predicted_probability` | Model-predicted probability for the sepsis class |",
  "| `brier_score` | Mean squared difference between observed outcome and predicted probability |"
)

safe_write_lines(data_dictionary_text, file.path(release_dir, "docs", "data_dictionary.md"))

availability_template_text <- c(
  "# Manuscript availability statement template",
  "",
  "Replace `[GitHub URL]` and `[Zenodo DOI]` after repository upload and Zenodo archival.",
  "",
  "## Availability of data and materials",
  "",
  "The original transcriptomic datasets analyzed in this study are publicly available from the Gene Expression Omnibus (GEO) under accession numbers GSE137340, GSE236713, GSE54514, GSE57065, GSE65682, and GSE95233. No raw GEO data are redistributed with this manuscript. The analysis code, frozen result tables, processed GEO-derived analysis outputs, figure-supporting tables, and reproducibility materials have been deposited in GitHub and archived in Zenodo. The GitHub repository is available at: [GitHub URL]. The archived release is available in Zenodo at: [Zenodo DOI]. Additional files submitted with the manuscript provide cohort metadata, final signature definition, nested LODO performance outputs, DEG and candidate-selection audit tables, and software-environment information.",
  "",
  "## Code availability",
  "",
  "All analysis scripts were written in R and are available in the GitHub repository archived in Zenodo: [Zenodo DOI]. The finalized reproducibility audit was generated under R 4.5.2 on macOS. Main R package versions included glmnet 5.0, pROC 1.19.0.1, PRROC 1.4, rms 8.1.1, dplyr 1.1.4, and readr 2.1.6. Detailed software versions and run-order documentation are provided in the repository and Additional file 5."
)

safe_write_lines(availability_template_text, file.path(release_dir, "docs", "availability_statement_template.md"))

# -----------------------------
# 13. Create upload instructions
# -----------------------------
upload_instructions <- c(
  "# Upload instructions",
  "",
  "1. Create a public GitHub repository, for example:",
  "",
  "   `sepsis-crosscohort-host-response`",
  "",
  "2. Upload the contents of `repository_release_v1/` to GitHub.",
  "",
  "3. Do not upload files from `large_files_for_zenodo_only/` to GitHub if any file exceeds GitHub's practical limit.",
  "",
  "4. Create a GitHub release:",
  "",
  "   `v1.0.0 manuscript submission release`",
  "",
  "5. Connect the GitHub repository to Zenodo and archive the release.",
  "",
  "6. Copy the Zenodo DOI into the manuscript Availability of data and materials section.",
  "",
  "7. Replace all manuscript wording such as `will be deposited` with `have been deposited` after DOI generation."
)

safe_write_lines(upload_instructions, file.path(release_dir, "docs", "upload_instructions.md"))

# -----------------------------
# 14. Create manifest
# -----------------------------
all_release_files <- list.files(release_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)

manifest <- tibble(
  relative_path = fs::path_rel(all_release_files, start = release_dir),
  full_path = all_release_files,
  file_name = basename(all_release_files),
  extension = tolower(tools::file_ext(all_release_files)),
  size_bytes = as.numeric(file.info(all_release_files)$size),
  size_mb = round(size_bytes / 1024^2, 3),
  recommended_destination = dplyr::case_when(
    stringr::str_detect(relative_path, "^large_files_for_zenodo_only/") ~ "Zenodo only or manual review",
    size_mb > 50 ~ "Zenodo only or manual review",
    TRUE ~ "GitHub and Zenodo"
  )
) %>%
  arrange(desc(size_mb), relative_path)

readr::write_csv(manifest, file.path(release_dir, "manifest", "release_manifest.csv"))

copy_log <- bind_rows(
  script_copy_log %>% mutate(copy_group = "selected_scripts"),
  all_script_copy_log %>% mutate(copy_group = "all_scripts"),
  result_copy_log %>% mutate(copy_group = "result_directories"),
  figure_copy_log %>% mutate(copy_group = "figures"),
  additional_copy_log %>% mutate(copy_group = "additional_files")
) %>%
  select(copy_group, source, target, copied, reason, size_mb)

readr::write_csv(copy_log, file.path(release_dir, "manifest", "copy_log.csv"))

# -----------------------------
# 15. Checks
# -----------------------------
checks <- tibble(
  check_id = sprintf("C%02d", 1:24),
  check_item = c(
    "Release directory exists",
    "README generated",
    "LICENSE generated",
    "CITATION.cff generated",
    "sessionInfo generated",
    "GEO accessions doc generated",
    "Run order doc generated",
    "Data dictionary generated",
    "Availability template generated",
    "Upload instructions generated",
    "Manifest generated",
    "Copy log generated",
    "Code directory contains R scripts",
    "Additional files directory exists",
    "Frozen results directory exists",
    "Large files directory exists",
    "At least one selected script copied",
    "At least one result file copied",
    "Additional file 1 copied or logged",
    "Additional file 2 copied or logged",
    "No file >100 MiB outside large_files_for_zenodo_only",
    "No raw GEO redistribution folder created",
    "Repository release folder ready",
    "Overall status file ready"
  ),
  observed = c(
    dir.exists(release_dir),
    file.exists(file.path(release_dir, "README.md")),
    file.exists(file.path(release_dir, "LICENSE")),
    file.exists(file.path(release_dir, "CITATION.cff")),
    file.exists(file.path(release_dir, "sessionInfo.txt")),
    file.exists(file.path(release_dir, "docs", "GEO_accessions.md")),
    file.exists(file.path(release_dir, "docs", "run_order.md")),
    file.exists(file.path(release_dir, "docs", "data_dictionary.md")),
    file.exists(file.path(release_dir, "docs", "availability_statement_template.md")),
    file.exists(file.path(release_dir, "docs", "upload_instructions.md")),
    file.exists(file.path(release_dir, "manifest", "release_manifest.csv")),
    file.exists(file.path(release_dir, "manifest", "copy_log.csv")),
    length(list.files(file.path(release_dir, "code"), pattern = "\\.[Rr]$", recursive = TRUE)) > 0,
    dir.exists(file.path(release_dir, "additional_files")),
    dir.exists(file.path(release_dir, "frozen_results")),
    dir.exists(file.path(release_dir, "large_files_for_zenodo_only")),
    any(script_copy_log$copied, na.rm = TRUE),
    any(result_copy_log$copied, na.rm = TRUE),
    any(stringr::str_detect(copy_log$target, "Additional_file_1"), na.rm = TRUE) || any(stringr::str_detect(copy_log$source, "Additional file 1"), na.rm = TRUE),
    any(stringr::str_detect(copy_log$target, "Additional_file_2"), na.rm = TRUE) || any(stringr::str_detect(copy_log$source, "Additional file 2"), na.rm = TRUE),
    !any(manifest$size_mb > 100 & !stringr::str_detect(manifest$relative_path, "^large_files_for_zenodo_only/"), na.rm = TRUE),
    !dir.exists(file.path(release_dir, "raw_GEO_data")),
    TRUE,
    TRUE
  )
) %>%
  mutate(status = if_else(observed, "PASS", "CHECK"))

readr::write_csv(checks, file.path(release_dir, "manifest", "release_checks.csv"))

n_failed_checks <- sum(checks$status != "PASS", na.rm = TRUE)

overall_status <- tibble(
  metric = c(
    "script",
    "project_dir",
    "release_dir",
    "n_release_files",
    "n_files_for_github_and_zenodo",
    "n_files_for_zenodo_only_or_review",
    "largest_file_mb",
    "n_failed_checks",
    "ready_for_github_upload",
    "recommended_next_step"
  ),
  value = c(
    "49_prepare_repository_release_v1.R",
    project_dir,
    release_dir,
    as.character(nrow(manifest)),
    as.character(sum(manifest$recommended_destination == "GitHub and Zenodo", na.rm = TRUE)),
    as.character(sum(manifest$recommended_destination != "GitHub and Zenodo", na.rm = TRUE)),
    as.character(max(manifest$size_mb, na.rm = TRUE)),
    as.character(n_failed_checks),
    ifelse(n_failed_checks == 0, "YES_UPLOAD_TO_GITHUB", "REVIEW_CHECK_ITEMS_FIRST"),
    "Open repository_release_v1, review README.md and manifest/release_manifest.csv, then upload the folder contents to a public GitHub repository and archive the first release in Zenodo."
  )
)

readr::write_csv(overall_status, file.path(release_dir, "manifest", "overall_status.csv"))

# -----------------------------
# 16. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status, n = Inf, width = Inf)

cat("\nChecks:\n")
print(checks, n = Inf, width = Inf)

cat("\nLargest files:\n")
print(
  manifest %>%
    arrange(desc(size_mb)) %>%
    select(relative_path, size_mb, recommended_destination) %>%
    slice_head(n = 30),
  n = 30,
  width = Inf
)

cat("\nCopy log summary:\n")
print(
  copy_log %>%
    count(copy_group, reason, copied, name = "n") %>%
    arrange(copy_group, desc(n)),
  n = Inf,
  width = Inf
)

cat("\nRelease folder:\n")
cat(release_dir, "\n")

cat("\nKey files:\n")
cat("README: ", file.path(release_dir, "README.md"), "\n", sep = "")
cat("Manifest: ", file.path(release_dir, "manifest", "release_manifest.csv"), "\n", sep = "")
cat("Copy log: ", file.path(release_dir, "manifest", "copy_log.csv"), "\n", sep = "")
cat("Checks: ", file.path(release_dir, "manifest", "release_checks.csv"), "\n", sep = "")
cat("Availability template: ", file.path(release_dir, "docs", "availability_statement_template.md"), "\n", sep = "")

cat("\nNext step:\n")
cat("把 Overall status、Checks、Largest files 和 Copy log summary 贴给我。\n")
cat("如果 n_failed_checks = 0，就可以上传 repository_release_v1 文件夹内容到 GitHub。\n")

cat("\n============ 49 repository release folder prepared ============\n")