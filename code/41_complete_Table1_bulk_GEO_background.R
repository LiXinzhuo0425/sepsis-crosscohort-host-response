# ============================================================
# 41_complete_Table1_bulk_GEO_background.R
# Project: Sepsis_CrossCohort_scRNA
# Purpose:
#   Complete Table 1 background information for 6 bulk GEO cohorts
#   for BMC Genomics manuscript.
#
# Outputs:
#   1. XLSX workbook with automatic GEO metadata and submission-ready draft
#   2. CSV version of completed Table 1 draft
#   3. DOCX Table 1 draft for direct manuscript insertion
#   4. Manual verification checklist
#
# Notes:
#   This script does not modify your frozen analysis results.
#   It only builds a richer cohort-background table.
# ============================================================

options(stringsAsFactors = FALSE)
options(width = 180)
options(max.print = 300)

cat("\n============ 41 Complete Table 1 bulk GEO background ============\n")

# -----------------------------
# 0. Paths
# -----------------------------
project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

if (!dir.exists(project_dir)) {
  stop("Project directory not found: ", project_dir)
}

setwd(project_dir)

dir_scripts <- file.path(project_dir, "03_scripts")
dir_results <- file.path(project_dir, "04_results")
dir_freeze  <- file.path(dir_results, "final_freeze")
dir_tables  <- file.path(dir_freeze, "tables")
dir_manuscript <- file.path(project_dir, "07_manuscript")
dir_out <- file.path(dir_results, "table1_background_completion")

if (!dir.exists(dir_out)) {
  dir.create(dir_out, recursive = TRUE)
}

existing_table1_path <- file.path(
  dir_tables,
  "T20_table1_T20_table1_cohort_context.csv"
)

# -----------------------------
# 1. Package management
# -----------------------------
install_if_missing_cran <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  }
}

install_if_missing_bioc <- function(pkgs) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      BiocManager::install(p, ask = FALSE, update = FALSE)
    }
  }
}

cran_pkgs <- c(
  "dplyr",
  "stringr",
  "purrr",
  "tidyr",
  "readr",
  "tibble",
  "openxlsx",
  "officer",
  "flextable",
  "rentrez"
)

bioc_pkgs <- c("GEOquery")

install_if_missing_cran(cran_pkgs)
install_if_missing_bioc(bioc_pkgs)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(openxlsx)
  library(GEOquery)
  library(officer)
  library(flextable)
})

has_rentrez <- requireNamespace("rentrez", quietly = TRUE)

# -----------------------------
# 2. Cohorts
# -----------------------------
bulk_gse <- c(
  "GSE137340",
  "GSE236713",
  "GSE54514",
  "GSE57065",
  "GSE65682",
  "GSE95233"
)

# -----------------------------
# 3. Helper functions
# -----------------------------
safe_chr <- function(x, collapse = "; ") {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(unique(x), collapse = collapse)
}

clean_long_text <- function(x, max_chars = 1200) {
  x <- safe_chr(x)
  if (is.na(x)) return(NA_character_)
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  if (nchar(x) > max_chars) {
    x <- paste0(substr(x, 1, max_chars), " ...")
  }
  x
}

contains_any <- function(x, patterns) {
  x <- tolower(paste(x, collapse = " ; "))
  any(str_detect(x, patterns))
}

extract_first_regex <- function(x, pattern) {
  x <- paste(as.character(x), collapse = " ")
  hit <- str_match(x, pattern)
  if (nrow(hit) == 0 || is.na(hit[1, 2])) return(NA_character_)
  hit[1, 2]
}

safe_file_size_mb <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

guess_dataset_col <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(NA_character_)
  candidates <- names(df)[
    str_detect(
      tolower(names(df)),
      "dataset|geo|accession|gse|cohort"
    )
  ]
  if (length(candidates) > 0) return(candidates[1])
  NA_character_
}

normalize_dataset_id <- function(x) {
  x <- as.character(x)
  hit <- str_extract(x, "GSE[0-9]+")
  hit
}

# -----------------------------
# 4. Fetch GEO metadata
# -----------------------------
fetch_one_gse <- function(gse_id) {
  cat("\nFetching GEO metadata: ", gse_id, "\n", sep = "")
  
  gse_obj <- tryCatch(
    {
      GEOquery::getGEO(gse_id, GSEMatrix = FALSE)
    },
    error = function(e) {
      warning("Failed to fetch ", gse_id, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(gse_obj)) {
    return(list(
      gse_id = gse_id,
      series_meta = tibble(
        dataset_id = gse_id,
        fetch_ok = FALSE,
        title = NA_character_,
        summary = NA_character_,
        overall_design = NA_character_,
        platform_id_auto = NA_character_,
        sample_count_auto = NA_integer_,
        pubmed_id_auto = NA_character_,
        submission_date = NA_character_,
        last_update_date = NA_character_,
        organism = NA_character_,
        experiment_type = NA_character_
      ),
      sample_meta = tibble()
    ))
  }
  
  m <- GEOquery::Meta(gse_obj)
  gsm_list <- GEOquery::GSMList(gse_obj)
  
  platform_id_auto <- safe_chr(m$platform_id)
  pubmed_id_auto <- safe_chr(m$pubmed_id)
  sample_ids <- names(gsm_list)
  
  series_meta <- tibble(
    dataset_id = gse_id,
    fetch_ok = TRUE,
    title = clean_long_text(m$title, max_chars = 500),
    summary = clean_long_text(m$summary, max_chars = 1500),
    overall_design = clean_long_text(m$overall_design, max_chars = 1500),
    platform_id_auto = platform_id_auto,
    sample_count_auto = length(sample_ids),
    pubmed_id_auto = pubmed_id_auto,
    submission_date = safe_chr(m$submission_date),
    last_update_date = safe_chr(m$last_update_date),
    organism = safe_chr(m$sample_organism),
    experiment_type = safe_chr(m$type)
  )
  
  sample_meta <- purrr::map_dfr(sample_ids, function(gsm_id) {
    gsm <- gsm_list[[gsm_id]]
    gm <- GEOquery::Meta(gsm)
    
    characteristics <- gm$characteristics_ch1
    source_name <- gm$source_name_ch1
    title <- gm$title
    
    all_text <- paste(
      title,
      source_name,
      characteristics,
      gm$description,
      collapse = " ; "
    )
    
    inferred_group <- dplyr::case_when(
      str_detect(tolower(all_text), "septic shock") ~ "septic shock",
      str_detect(tolower(all_text), "sepsis") ~ "sepsis",
      str_detect(tolower(all_text), "\\bsirs\\b") ~ "SIRS",
      str_detect(tolower(all_text), "healthy|normal|volunteer") ~ "healthy/control",
      str_detect(tolower(all_text), "control") ~ "control",
      TRUE ~ "unclear"
    )
    
    inferred_sample_type <- dplyr::case_when(
      str_detect(tolower(all_text), "whole blood|blood") ~ "blood or whole blood mentioned",
      str_detect(tolower(all_text), "pbmc|peripheral blood mononuclear") ~ "PBMC mentioned",
      TRUE ~ "unclear"
    )
    
    tibble(
      dataset_id = gse_id,
      gsm_id = gsm_id,
      gsm_title = clean_long_text(title, max_chars = 300),
      source_name_ch1 = clean_long_text(source_name, max_chars = 300),
      characteristics_ch1 = clean_long_text(characteristics, max_chars = 1200),
      molecule_ch1 = safe_chr(gm$molecule_ch1),
      platform_id_gsm = safe_chr(gm$platform_id),
      inferred_group_from_text = inferred_group,
      inferred_sample_type_from_text = inferred_sample_type
    )
  })
  
  list(
    gse_id = gse_id,
    series_meta = series_meta,
    sample_meta = sample_meta
  )
}

geo_list <- purrr::map(bulk_gse, fetch_one_gse)

series_meta <- purrr::map_dfr(geo_list, "series_meta")
sample_meta <- purrr::map_dfr(geo_list, "sample_meta")

# -----------------------------
# 5. Sample-level summaries
# -----------------------------
sample_summary <- sample_meta %>%
  group_by(dataset_id) %>%
  summarise(
    n_gsm_auto = n(),
    platform_ids_from_gsm = safe_chr(unique(platform_id_gsm)),
    source_name_examples = clean_long_text(unique(source_name_ch1), max_chars = 900),
    characteristics_examples = clean_long_text(unique(characteristics_ch1), max_chars = 1600),
    inferred_group_counts = paste(
      names(table(inferred_group_from_text)),
      as.integer(table(inferred_group_from_text)),
      sep = "=",
      collapse = "; "
    ),
    inferred_sample_type_counts = paste(
      names(table(inferred_sample_type_from_text)),
      as.integer(table(inferred_sample_type_from_text)),
      sep = "=",
      collapse = "; "
    ),
    .groups = "drop"
  )

# -----------------------------
# 6. PMID and DOI retrieval
# -----------------------------
fetch_pubmed_doi <- function(pmid_string) {
  if (is.na(pmid_string) || !nzchar(pmid_string)) {
    return(tibble(
      pmid = NA_character_,
      article_title = NA_character_,
      journal = NA_character_,
      year = NA_character_,
      doi = NA_character_,
      pubmed_fetch_status = "NO_PMID"
    ))
  }
  
  pmids <- unlist(str_split(pmid_string, ";|,|\\s+"))
  pmids <- pmids[str_detect(pmids, "^[0-9]+$")]
  pmids <- unique(pmids)
  
  if (length(pmids) == 0) {
    return(tibble(
      pmid = NA_character_,
      article_title = NA_character_,
      journal = NA_character_,
      year = NA_character_,
      doi = NA_character_,
      pubmed_fetch_status = "NO_VALID_PMID"
    ))
  }
  
  if (!has_rentrez) {
    return(tibble(
      pmid = pmids,
      article_title = NA_character_,
      journal = NA_character_,
      year = NA_character_,
      doi = NA_character_,
      pubmed_fetch_status = "RENTREZ_NOT_AVAILABLE"
    ))
  }
  
  purrr::map_dfr(pmids, function(pmid) {
    Sys.sleep(0.35)
    
    sm <- tryCatch(
      rentrez::entrez_summary(db = "pubmed", id = pmid),
      error = function(e) NULL
    )
    
    if (is.null(sm)) {
      return(tibble(
        pmid = pmid,
        article_title = NA_character_,
        journal = NA_character_,
        year = NA_character_,
        doi = NA_character_,
        pubmed_fetch_status = "FETCH_FAILED"
      ))
    }
    
    doi <- NA_character_
    
    if (!is.null(sm$elocationid)) {
      doi <- extract_first_regex(sm$elocationid, "(10\\.[^\\s;]+)")
    }
    
    if (is.na(doi)) {
      medline <- tryCatch(
        rentrez::entrez_fetch(db = "pubmed", id = pmid, rettype = "medline", retmode = "text"),
        error = function(e) NA_character_
      )
      doi <- extract_first_regex(medline, "AID - (10\\.[^\\s\\[]+)")
    }
    
    tibble(
      pmid = pmid,
      article_title = safe_chr(sm$title),
      journal = safe_chr(sm$fulljournalname),
      year = safe_chr(sm$pubdate),
      doi = doi,
      pubmed_fetch_status = "OK"
    )
  })
}

pmid_doi <- series_meta %>%
  select(dataset_id, pubmed_id_auto) %>%
  mutate(pubmed_id_auto = ifelse(is.na(pubmed_id_auto), "", pubmed_id_auto)) %>%
  rowwise() %>%
  do({
    tmp <- fetch_pubmed_doi(.$pubmed_id_auto)
    tmp$dataset_id <- .$dataset_id
    tmp
  }) %>%
  ungroup() %>%
  select(dataset_id, everything())

pmid_doi_summary <- pmid_doi %>%
  group_by(dataset_id) %>%
  summarise(
    PMID = safe_chr(pmid),
    DOI = safe_chr(doi),
    original_article_title = safe_chr(article_title),
    journal = safe_chr(journal),
    pubmed_year = safe_chr(year),
    pubmed_fetch_status = safe_chr(pubmed_fetch_status),
    .groups = "drop"
  )

# -----------------------------
# 7. Existing Table 1 integration
# -----------------------------
existing_table1 <- NULL

if (file.exists(existing_table1_path)) {
  existing_table1 <- read.csv(existing_table1_path, check.names = FALSE)
  dataset_col <- guess_dataset_col(existing_table1)
  
  if (!is.na(dataset_col)) {
    existing_table1 <- existing_table1 %>%
      mutate(dataset_id = normalize_dataset_id(.data[[dataset_col]]))
  } else {
    warning("Could not identify dataset column in existing Table 1.")
    existing_table1$dataset_id <- NA_character_
  }
} else {
  warning("Existing Table 1 not found: ", existing_table1_path)
  existing_table1 <- tibble(dataset_id = bulk_gse)
}

# Keep only relevant rows if possible
existing_table1_relevant <- existing_table1 %>%
  filter(dataset_id %in% bulk_gse | is.na(dataset_id))

# -----------------------------
# 8. Manual curation scaffold
# -----------------------------
manual_seed <- tibble::tribble(
  ~dataset_id, ~sample_type_manual_draft, ~case_definition_manual_draft, ~control_type_manual_draft, ~intermediate_or_excluded_groups_manual_draft, ~main_limitation_manual_draft,
  
  "GSE137340",
  "Peripheral blood / whole blood, verify exact wording from GSM source_name_ch1.",
  "Sepsis cases sampled at diagnosis and again after 24 hours. Current analysis should verify whether only first diagnostic time point was retained.",
  "Age- and sex-matched healthy controls without inflammatory disease, according to GEO design text.",
  "No SIRS intermediate group is apparent from GEO summary; verify from GSM fields.",
  "Repeated sepsis sampling in the original design may introduce time-point heterogeneity. Current analysis must document which time point was used.",
  
  "GSE236713",
  "Blood or whole blood, verify exact sample source from GSM fields.",
  "Adult multi-center severe systemic inflammation study designed to identify mRNA biomarkers differentiating sepsis from SIRS.",
  "Control definition must be verified from GSM metadata and original paper. If SIRS samples exist, clearly state whether they were excluded or used as clinical controls.",
  "SIRS is central to the original design. Current analysis must explicitly document handling of SIRS or other intermediate groups.",
  "Potential mixture of sepsis, SIRS and non-sepsis inflammatory states; strong risk of label/context heterogeneity if SIRS handling is unclear.",
  
  "GSE54514",
  "Whole blood, according to GEO summary.",
  "Critically ill ICU septic patients, with whole blood collected daily for up to 5 days.",
  "Healthy controls in many secondary descriptions; verify exact control group and current selected controls from GSM metadata.",
  "Original dataset contains repeated longitudinal samples. Current analysis must document whether repeated samples or selected baseline samples were used.",
  "Longitudinal ICU sampling and immune dysfunction monitoring design may differ from admission diagnosis cohorts, explaining lower transport performance.",
  
  "GSE57065",
  "Whole blood, verify exact wording from GSM source_name_ch1.",
  "Septic shock patients with early and dynamic gene-expression changes.",
  "Healthy controls or control samples, verify exact control type from GSM metadata and original paper.",
  "Potential serial time-point samples; verify whether only baseline/early samples were retained.",
  "Septic shock-specific and dynamic sampling design may limit comparability with broader sepsis cohorts.",
  
  "GSE65682",
  "Whole blood leukocyte or whole blood expression profiling, verify exact sample source from GEO and GSM metadata.",
  "Critically ill patients with sepsis or septic shock in a large prospective systems-biology cohort.",
  "Selected healthy controls in the current manuscript table; verify whether original non-infectious ICU controls existed and how controls were selected.",
  "Original cohort may include complex ICU diagnostic categories. Current analysis must state the exact inclusion/exclusion logic.",
  "Large heterogeneous critical-care cohort; selected subset and control definition must be transparent to avoid inflated diagnostic contrast.",
  
  "GSE95233",
  "Whole blood, verify exact wording from GSM source_name_ch1.",
  "Septic shock patients profiled early to identify prognostic biomarkers according to 28-day mortality.",
  "Healthy volunteers or healthy controls, verify exact control definition from GEO/GSM and original paper.",
  "No SIRS intermediate group is apparent from the GEO summary; verify from GSM fields.",
  "Septic shock prognosis-oriented cohort rather than pure diagnostic cohort; case severity spectrum may be narrower than general sepsis."
)

# -----------------------------
# 9. Build completed Table 1 draft
# -----------------------------
auto_table <- series_meta %>%
  left_join(sample_summary, by = "dataset_id") %>%
  left_join(pmid_doi_summary, by = "dataset_id") %>%
  left_join(manual_seed, by = "dataset_id")

# Pull current analysis sample counts from existing Table 1 if available
existing_cols_to_keep <- existing_table1_relevant %>%
  select(dataset_id, everything())

table1_draft <- auto_table %>%
  left_join(existing_cols_to_keep, by = "dataset_id", suffix = c("", "_existing")) %>%
  mutate(
    GEO_accession = dataset_id,
    Platform_GPL = dplyr::coalesce(platform_ids_from_gsm, platform_id_auto),
    Platform_title_or_notes = title,
    Sample_type = sample_type_manual_draft,
    Sepsis_case_definition = case_definition_manual_draft,
    Control_type = control_type_manual_draft,
    Intermediate_groups_and_exclusion = intermediate_or_excluded_groups_manual_draft,
    Original_PMID = PMID,
    Original_DOI = DOI,
    Main_limitations = main_limitation_manual_draft,
    GEO_summary_for_verification = summary,
    GEO_overall_design_for_verification = overall_design,
    GSM_source_examples = source_name_examples,
    GSM_characteristics_examples = characteristics_examples,
    Auto_group_counts_from_GSM_text = inferred_group_counts,
    Auto_sample_type_counts_from_GSM_text = inferred_sample_type_counts,
    Verification_status = case_when(
      is.na(Platform_GPL) | is.na(Sample_type) | is.na(Sepsis_case_definition) | is.na(Control_type) ~
        "NEEDS_MANUAL_VERIFICATION",
      str_detect(
        paste(
          Sample_type,
          Sepsis_case_definition,
          Control_type,
          Intermediate_groups_and_exclusion,
          Main_limitations,
          sep = " "
        ),
        regex("verify|must|Current analysis", ignore_case = TRUE)
      ) ~ "DRAFT_WITH_MANUAL_VERIFICATION_REQUIRED",
      TRUE ~ "READY_AFTER_AUTHOR_CONFIRMATION"
    )
  ) %>%
  select(
    Dataset = dataset_id,
    GEO_accession,
    Platform_GPL,
    Platform_title_or_notes,
    Sample_type,
    Sepsis_case_definition,
    Control_type,
    Intermediate_groups_and_exclusion,
    Original_PMID,
    Original_DOI,
    Main_limitations,
    Verification_status,
    sample_count_auto,
    Auto_group_counts_from_GSM_text,
    Auto_sample_type_counts_from_GSM_text,
    GEO_summary_for_verification,
    GEO_overall_design_for_verification,
    GSM_source_examples,
    GSM_characteristics_examples,
    everything()
  )

# -----------------------------
# 10. BMC submission-ready compact table
# -----------------------------
table1_submission <- table1_draft %>%
  transmute(
    `Dataset` = Dataset,
    `GEO accession` = GEO_accession,
    `Platform` = Platform_GPL,
    `Sample source` = Sample_type,
    `Case definition` = Sepsis_case_definition,
    `Control context` = Control_type,
    `Intermediate groups handled in this analysis` = Intermediate_groups_and_exclusion,
    `Original PMID/DOI` = ifelse(
      is.na(Original_DOI) | !nzchar(Original_DOI),
      Original_PMID,
      paste0("PMID: ", Original_PMID, "; DOI: ", Original_DOI)
    ),
    `Main limitation for transportability analysis` = Main_limitations
  )

manual_checklist <- table1_draft %>%
  transmute(
    Dataset,
    item_to_verify = paste(
      "Confirm platform, whole-blood status, exact case/control definition, SIRS/intermediate-group handling, PMID/DOI, and current-analysis sample subset.",
      sep = ""
    ),
    fields_most_likely_requiring_manual_confirmation = paste(
      "Sample_type;",
      "Sepsis_case_definition;",
      "Control_type;",
      "Intermediate_groups_and_exclusion;",
      "Original_PMID/DOI;",
      "Main_limitations"
    ),
    verification_status = Verification_status,
    GEO_summary_for_verification,
    GEO_overall_design_for_verification,
    GSM_source_examples,
    GSM_characteristics_examples
  )

# -----------------------------
# 11. Write outputs
# -----------------------------
xlsx_path <- file.path(dir_out, "T41_Table1_bulk_GEO_background_completed_draft.xlsx")
csv_submission_path <- file.path(dir_out, "T41_Table1_submission_ready_draft.csv")
csv_full_path <- file.path(dir_out, "T41_Table1_full_GEO_background_audit.csv")
manual_check_path <- file.path(dir_out, "T41_Table1_manual_verification_checklist.csv")
docx_path <- file.path(dir_out, "T41_Table1_submission_ready_draft.docx")
overall_path <- file.path(dir_out, "T41_Table1_completion_overall_status.csv")

write.csv(table1_submission, csv_submission_path, row.names = FALSE)
write.csv(table1_draft, csv_full_path, row.names = FALSE)
write.csv(manual_checklist, manual_check_path, row.names = FALSE)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Table1_submission_draft")
openxlsx::writeData(wb, "Table1_submission_draft", table1_submission)

openxlsx::addWorksheet(wb, "Full_GEO_audit")
openxlsx::writeData(wb, "Full_GEO_audit", table1_draft)

openxlsx::addWorksheet(wb, "Series_metadata")
openxlsx::writeData(wb, "Series_metadata", series_meta)

openxlsx::addWorksheet(wb, "Sample_summary")
openxlsx::writeData(wb, "Sample_summary", sample_summary)

openxlsx::addWorksheet(wb, "Sample_metadata_all")
openxlsx::writeData(wb, "Sample_metadata_all", sample_meta)

openxlsx::addWorksheet(wb, "PMID_DOI")
openxlsx::writeData(wb, "PMID_DOI", pmid_doi)

openxlsx::addWorksheet(wb, "Manual_checklist")
openxlsx::writeData(wb, "Manual_checklist", manual_checklist)

openxlsx::addWorksheet(wb, "Existing_T20_Table1")
openxlsx::writeData(wb, "Existing_T20_Table1", existing_table1_relevant)

# Basic styling
for (sh in names(wb)) {
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb, sh, cols = 1:80, widths = "auto")
}

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)

body_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

for (sh in names(wb)) {
  n_cols <- ncol(openxlsx::readWorkbook(wb, sh))
  if (n_cols > 0) {
    openxlsx::addStyle(wb, sh, header_style, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, body_style, rows = 2:5000, cols = 1:n_cols, gridExpand = TRUE)
  }
}

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# -----------------------------
# 12. DOCX export
# -----------------------------
doc <- officer::read_docx()

doc <- officer::body_add_par(
  doc,
  "Table 1. Clinical and technical background of included bulk sepsis transcriptomic cohorts",
  style = "heading 1"
)

doc <- officer::body_add_par(
  doc,
  paste0(
    "This draft table was generated by combining the frozen manuscript cohort table with automatically retrieved GEO metadata. ",
    "Fields marked in the table should be verified against GEO sample annotations and the original publications before final submission."
  ),
  style = "Normal"
)

ft <- flextable::flextable(table1_submission)
ft <- flextable::theme_booktabs(ft)
ft <- flextable::fontsize(ft, size = 8, part = "all")
ft <- flextable::font(ft, fontname = "Arial", part = "all")
ft <- flextable::align(ft, align = "left", part = "all")
ft <- flextable::align(ft, align = "center", part = "header")
ft <- flextable::valign(ft, valign = "top", part = "all")
ft <- flextable::autofit(ft)

doc <- flextable::body_add_flextable(doc, ft)

doc <- officer::body_add_par(
  doc,
  "Table note. GEO, Gene Expression Omnibus; ICU, intensive care unit; SIRS, systemic inflammatory response syndrome; PMID, PubMed identifier; DOI, digital object identifier. The table is intended for manuscript insertion after author verification of exact case and control definitions, platform identifiers, and subgroup exclusions.",
  style = "Normal"
)

print(doc, target = docx_path)

# -----------------------------
# 13. Automated checks
# -----------------------------
required_cols <- c(
  "Dataset",
  "GEO accession",
  "Platform",
  "Sample source",
  "Case definition",
  "Control context",
  "Intermediate groups handled in this analysis",
  "Original PMID/DOI",
  "Main limitation for transportability analysis"
)

checks <- tibble(
  check_id = paste0("C", sprintf("%02d", 1:12)),
  check_item = c(
    "All six expected GSE datasets present",
    "GEO accession column complete",
    "Platform column has no missing value",
    "Sample source column has no missing value",
    "Case definition column has no missing value",
    "Control context column has no missing value",
    "Intermediate group handling column has no missing value",
    "PMID/DOI column has at least one identifier or placeholder",
    "Main limitation column has no missing value",
    "XLSX output exists",
    "CSV submission draft exists",
    "DOCX submission draft exists"
  ),
  observed = c(
    all(bulk_gse %in% table1_submission$Dataset),
    all(!is.na(table1_submission$`GEO accession`) & nzchar(table1_submission$`GEO accession`)),
    all(!is.na(table1_submission$Platform) & nzchar(table1_submission$Platform)),
    all(!is.na(table1_submission$`Sample source`) & nzchar(table1_submission$`Sample source`)),
    all(!is.na(table1_submission$`Case definition`) & nzchar(table1_submission$`Case definition`)),
    all(!is.na(table1_submission$`Control context`) & nzchar(table1_submission$`Control context`)),
    all(!is.na(table1_submission$`Intermediate groups handled in this analysis`) & nzchar(table1_submission$`Intermediate groups handled in this analysis`)),
    all(!is.na(table1_submission$`Original PMID/DOI`) & nzchar(table1_submission$`Original PMID/DOI`)),
    all(!is.na(table1_submission$`Main limitation for transportability analysis`) & nzchar(table1_submission$`Main limitation for transportability analysis`)),
    file.exists(xlsx_path),
    file.exists(csv_submission_path),
    file.exists(docx_path)
  )
) %>%
  mutate(status = ifelse(observed, "PASS", "CHECK"))

n_failed_checks <- sum(checks$status != "PASS")

overall_status <- tibble(
  metric = c(
    "script",
    "project_dir",
    "n_expected_bulk_datasets",
    "n_series_metadata_rows",
    "n_sample_metadata_rows",
    "n_submission_table_rows",
    "n_failed_checks",
    "ready_for_manual_verification",
    "recommended_next_step"
  ),
  value = c(
    "41_complete_Table1_bulk_GEO_background.R",
    project_dir,
    length(bulk_gse),
    nrow(series_meta),
    nrow(sample_meta),
    nrow(table1_submission),
    n_failed_checks,
    ifelse(n_failed_checks == 0, "YES", "YES_BUT_FIX_CHECK_ITEMS"),
    "Open the XLSX workbook, verify each cohort against GEO sample metadata and original PMID/DOI, then paste the compact DOCX table into the manuscript."
  )
)

write.csv(overall_status, overall_path, row.names = FALSE)
checks_path <- file.path(dir_out, "T41_Table1_completion_checks.csv")
write.csv(checks, checks_path, row.names = FALSE)

# -----------------------------
# 14. Console output
# -----------------------------
cat("\nOverall status:\n")
print(overall_status)

cat("\nChecks:\n")
print(checks)

cat("\nSubmission-ready Table 1 draft:\n")
print(table1_submission, row.names = FALSE)

cat("\nManual verification checklist:\n")
print(
  manual_checklist %>%
    select(Dataset, verification_status, fields_most_likely_requiring_manual_confirmation),
  row.names = FALSE
)

cat("\n关键输出：\n")
cat("1) ", xlsx_path, "\n", sep = "")
cat("2) ", csv_submission_path, "\n", sep = "")
cat("3) ", csv_full_path, "\n", sep = "")
cat("4) ", manual_check_path, "\n", sep = "")
cat("5) ", docx_path, "\n", sep = "")
cat("6) ", checks_path, "\n", sep = "")
cat("7) ", overall_path, "\n", sep = "")

cat("\n下一步：\n")
cat("把 Overall status、Checks、Submission-ready Table 1 draft 贴给我。\n")
cat("如果 n_failed_checks = 0，我会帮你逐行审查 Table 1 哪些字段可以直接进入正文，哪些必须回到 GEO/GSM 或原文再确认。\n")

cat("\n============ 41 Table 1 completion complete ============\n")