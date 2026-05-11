# ============================================================
# 26_BMC_Genomics_citation_replacement_plan.R
# Prepare citation replacement plan for BMC Genomics manuscript v0.3
#
# 修正版：
# 1. 能识别 [REF_A; REF_B] 这种同一括号内多个 REF 的情况
# 2. 修复 current_occurrences 错误显示为行数总量的问题
# 3. 暂不自动替换正文，先生成引用替换计划供人工确认
#
# 输出：
# 07_manuscript/BMC_Genomics_draft/
#   BMC_Genomics_citation_replacement_map_v0.3.csv
#   BMC_Genomics_reference_candidates_v0.3.csv
#   BMC_Genomics_citation_placeholder_locations_v0.3.csv
#   BMC_Genomics_citation_replacement_plan_v0.3.md
#   BMC_Genomics_citation_replacement_plan_v0.3_index.xlsx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

manuscript_file <- file.path(draft_dir, "BMC_Genomics_full_manuscript_v0.3.md")
placeholder_file <- file.path(draft_dir, "BMC_Genomics_citation_placeholder_list_v0.3.csv")

if (!file.exists(manuscript_file)) {
  stop("缺少 manuscript 文件：", manuscript_file)
}

if (!file.exists(placeholder_file)) {
  stop("缺少 citation placeholder 文件：", placeholder_file)
}

# ============================================================
# 工具函数
# ============================================================

read_lines_utf8 <- function(path) {
  readLines(path, encoding = "UTF-8", warn = FALSE)
}

write_text_file <- function(text, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
}

# 识别 REF_A，也能识别 [REF_A; REF_B] 里面的两个 REF
extract_ref_placeholders <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  hits <- gregexpr("REF_[A-Z0-9_]+", txt, perl = TRUE)
  refs <- regmatches(txt, hits)[[1]]
  
  if (length(refs) == 0 || all(refs == "")) {
    return(character())
  }
  
  refs <- paste0("[", refs, "]")
  sort(unique(refs))
}

# 正确统计每个 REF key 的出现次数
count_placeholder_occurrences <- function(lines, placeholder) {
  key <- gsub("\\[|\\]", "", placeholder)
  hits <- gregexpr(key, lines, fixed = TRUE)
  
  sum(vapply(
    hits,
    function(z) {
      if (length(z) == 1 && z[1] == -1) {
        return(0L)
      }
      length(z)
    },
    integer(1)
  ))
}

# 找到包含某个 REF key 的行
find_lines_with_placeholder <- function(lines, placeholder) {
  key <- gsub("\\[|\\]", "", placeholder)
  idx <- grep(key, lines, fixed = TRUE)
  
  if (length(idx) == 0) {
    return(data.frame())
  }
  
  data.frame(
    placeholder = placeholder,
    ref_key = key,
    line_number = idx,
    line_text = lines[idx],
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 读取 manuscript 和 placeholder
# ============================================================

message("Reading manuscript and citation placeholder list...")

manuscript_lines <- read_lines_utf8(manuscript_file)
placeholder_table <- data.table::fread(placeholder_file, data.table = FALSE)

detected_placeholders <- extract_ref_placeholders(manuscript_lines)

placeholder_locations_list <- lapply(
  detected_placeholders,
  function(x) find_lines_with_placeholder(manuscript_lines, x)
)

placeholder_locations <- do.call(rbind, placeholder_locations_list)

if (is.null(placeholder_locations) || nrow(placeholder_locations) == 0) {
  placeholder_locations <- data.frame(
    placeholder = character(),
    ref_key = character(),
    line_number = integer(),
    line_text = character(),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# Reference candidates
# ============================================================

reference_candidates <- data.frame(
  ref_key = c(
    "Singer_Sepsis3_JAMA_2016",
    "van_der_Poll_Sepsis_Immunology_NatRevImmunol_2017",
    "Sweeney_Sepsis_GeneExpression_SciTranslMed_2015",
    "Scicluna_MARS_Sepsis_AJRespCritCareMed_2017",
    "Collins_TRIPOD_AI_BMJ_2024",
    "Moons_TRIPOD_AnnInternMed_2015",
    "Wolff_PROBAST_AnnInternMed_2019",
    "Steyerberg_PredictionModels_Book",
    "VanCalster_Calibration_StatMed_2019",
    "Vickers_DCA_MDM_2006",
    "Vickers_DCA_Guide_DiagnPrognRes_2019",
    "Hao_Seurat5_NatBiotechnol_2024",
    "Satija_Seurat_NatBiotechnol_2015",
    "GSE137340_source_placeholder",
    "GSE236713_source_placeholder",
    "GSE54514_source_placeholder",
    "GSE57065_source_placeholder",
    "GSE65682_source_placeholder",
    "GSE95233_source_placeholder",
    "GSE167363_source_placeholder"
  ),
  citation_label = c(
    "Singer et al., JAMA, 2016",
    "van der Poll et al., Nature Reviews Immunology, 2017",
    "Sweeney et al., Science Translational Medicine, 2015",
    "Scicluna et al., American Journal of Respiratory and Critical Care Medicine, 2017",
    "Collins et al., BMJ, 2024",
    "Moons et al., Annals of Internal Medicine, 2015",
    "Wolff et al., Annals of Internal Medicine, 2019",
    "Steyerberg, Clinical Prediction Models",
    "Van Calster et al., Statistics in Medicine, 2019",
    "Vickers and Elkin, Medical Decision Making, 2006",
    "Vickers et al., Diagnostic and Prognostic Research, 2019",
    "Hao et al., Nature Biotechnology, 2024",
    "Satija et al., Nature Biotechnology, 2015",
    "Original publication or GEO record for GSE137340",
    "Original publication or GEO record for GSE236713",
    "Original publication or GEO record for GSE54514",
    "Original publication or GEO record for GSE57065",
    "Original publication or GEO record for GSE65682",
    "Original publication or GEO record for GSE95233",
    "Original publication or GEO record for GSE167363"
  ),
  intended_use = c(
    "Sepsis definition and Sepsis-3 conceptual framing.",
    "Sepsis host immune dysregulation and immunopathology.",
    "Representative sepsis transcriptomic diagnostic signature literature.",
    "Representative sepsis blood transcriptomic cohort and host-response/endotype literature.",
    "Prediction model reporting guidance for regression and machine-learning models.",
    "Original TRIPOD reporting guidance.",
    "Risk of bias and applicability in prediction model studies.",
    "General clinical prediction modeling, calibration and validation principles.",
    "Calibration interpretation and reporting.",
    "Decision-curve analysis original method.",
    "Step-by-step interpretation of decision-curve analysis.",
    "Current Seurat / single-cell analysis reference if using Seurat v5.",
    "Classic Seurat reference.",
    "Dataset source citation for GSE137340.",
    "Dataset source citation for GSE236713.",
    "Dataset source citation for GSE54514.",
    "Dataset source citation for GSE57065.",
    "Dataset source citation for GSE65682.",
    "Dataset source citation for GSE95233.",
    "Dataset source citation for scRNA-seq localization."
  ),
  priority = c(
    "Essential",
    "High",
    "High",
    "High",
    "Essential",
    "High",
    "High",
    "Medium",
    "High",
    "Essential",
    "High",
    "High",
    "Medium",
    "Essential",
    "Essential",
    "Essential",
    "Essential",
    "Essential",
    "Essential",
    "Essential"
  ),
  status = c(
    "Use",
    "Verify",
    "Verify",
    "Verify",
    "Use",
    "Verify",
    "Verify",
    "Optional",
    "Verify",
    "Use",
    "Use",
    "Verify",
    "Optional",
    "Need exact source",
    "Need exact source",
    "Need exact source",
    "Need exact source",
    "Need exact source",
    "Need exact source",
    "Need exact source"
  ),
  notes = c(
    "Use for REF_SEPSIS_DEFINITION.",
    "Use for REF_SEPSIS_HOST_RESPONSE if suitable.",
    "Use for existing transcriptomic signatures if still appropriate after manual check.",
    "Relevant because GSE65682/MARS-related cohort may be included.",
    "Use for REF_TRIPOD_AI and prediction model reporting.",
    "Optional if using TRIPOD+AI as main reporting reference.",
    "Useful in Discussion for bias/applicability framing.",
    "Optional background support.",
    "Use for REF_CALIBRATION_PREDICTION_MODELS.",
    "Use for REF_DECISION_CURVE_ANALYSIS.",
    "Use for explanation of DCA interpretation.",
    "Use if package version aligns.",
    "Use if manuscript mentions Seurat generally.",
    "Verify exact source paper or GEO citation before final insertion.",
    "Verify exact source paper or GEO citation before final insertion.",
    "Verify exact source paper or GEO citation before final insertion.",
    "Verify exact source paper or GEO citation before final insertion.",
    "Verify exact source paper or GEO citation before final insertion.",
    "Verify exact source paper or GEO citation before final insertion.",
    "Verify exact source paper or GEO citation before final insertion."
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Placeholder to reference mapping
# ============================================================

all_possible_placeholders <- c(
  "[REF_SEPSIS_DEFINITION]",
  "[REF_SEPSIS_HOST_RESPONSE]",
  "[REF_SEPSIS_TRANSCRIPTOMIC_SIGNATURES]",
  "[REF_SEPSIS_SINGLE_CELL]",
  "[REF_PREDICTION_MODEL_REPORTING]",
  "[REF_TRIPOD_AI]",
  "[REF_CALIBRATION_PREDICTION_MODELS]",
  "[REF_DECISION_CURVE_ANALYSIS]",
  "[REF_SINGLE_CELL_MODULE_SCORE]",
  "[REF_GEO_DATASETS]"
)

citation_map <- data.frame(
  placeholder = all_possible_placeholders,
  recommended_refs = c(
    "Singer_Sepsis3_JAMA_2016",
    "van_der_Poll_Sepsis_Immunology_NatRevImmunol_2017",
    "Sweeney_Sepsis_GeneExpression_SciTranslMed_2015; Scicluna_MARS_Sepsis_AJRespCritCareMed_2017",
    "GSE167363_source_placeholder; representative sepsis scRNA-seq immune landscape paper",
    "Collins_TRIPOD_AI_BMJ_2024; Moons_TRIPOD_AnnInternMed_2015; Wolff_PROBAST_AnnInternMed_2019",
    "Collins_TRIPOD_AI_BMJ_2024",
    "VanCalster_Calibration_StatMed_2019; Steyerberg_PredictionModels_Book",
    "Vickers_DCA_MDM_2006; Vickers_DCA_Guide_DiagnPrognRes_2019",
    "Hao_Seurat5_NatBiotechnol_2024; Satija_Seurat_NatBiotechnol_2015",
    "GSE137340_source_placeholder; GSE236713_source_placeholder; GSE54514_source_placeholder; GSE57065_source_placeholder; GSE65682_source_placeholder; GSE95233_source_placeholder"
  ),
  replacement_style = c(
    "[Singer et al.]",
    "[sepsis immunology review]",
    "[representative transcriptomic signature studies]",
    "[scRNA source and sepsis single-cell immune landscape paper]",
    "[TRIPOD+AI/TRIPOD/PROBAST]",
    "[TRIPOD+AI]",
    "[calibration methods reference]",
    "[DCA references]",
    "[Seurat/module score reference]",
    "[original GEO dataset references]"
  ),
  manuscript_section = c(
    "Background paragraph 1",
    "Background paragraph 1; Discussion biological interpretation",
    "Background paragraph 2; Discussion previous work",
    "Background paragraph 4; Discussion scRNA interpretation",
    "Background paragraph 3; Discussion methodological implications",
    "Background paragraph 3",
    "Background paragraph 3; Discussion threshold transportability",
    "Methods DCA; Results DCA; Discussion DCA",
    "Methods scRNA; Discussion scRNA",
    "Methods data sources; Table 1; supplementary dataset description"
  ),
  current_occurrences = vapply(
    all_possible_placeholders,
    function(x) count_placeholder_occurrences(manuscript_lines, x),
    integer(1)
  ),
  action = c(
    "Replace after confirming exact reference format.",
    "Select one high-quality review or primary paper.",
    "Use 2 to 4 representative studies, avoid overloading Background.",
    "Use source dataset and one representative scRNA sepsis paper.",
    "Use TRIPOD+AI as primary reporting reference; add PROBAST if risk-of-bias discussion remains.",
    "Use BMJ TRIPOD+AI.",
    "Use calibration-focused reference.",
    "Use Vickers 2006 and optional 2019 guide.",
    "Use Seurat reference matching package version.",
    "Replace with exact original dataset publications and GEO accession citations."
  ),
  stringsAsFactors = FALSE
)

citation_map$detected_in_manuscript <- citation_map$placeholder %in% detected_placeholders

# 保留当前正文中实际出现过的 REF，以及需要追踪但未出现的 REF
citation_map$priority_for_next_step <- ifelse(
  citation_map$detected_in_manuscript,
  "Insert_or_replace",
  "Track_only_not_currently_in_text"
)

# ============================================================
# Markdown plan
# ============================================================

plan_lines <- c(
  "# BMC Genomics citation replacement plan v0.3",
  "",
  "## Current status",
  "",
  paste0("- Manuscript file: ", manuscript_file),
  paste0("- Detected unique placeholders: ", length(detected_placeholders)),
  paste0("- Total placeholder-location rows: ", nrow(placeholder_locations)),
  "",
  "## Detected placeholders",
  "",
  if (length(detected_placeholders) > 0) {
    paste0("- ", detected_placeholders)
  } else {
    "- None detected."
  },
  "",
  "## Important correction applied",
  "",
  "- This version detects REF keys inside compound citation brackets such as [REF_PREDICTION_MODEL_REPORTING; REF_TRIPOD_AI].",
  "- This version counts actual REF key occurrences instead of incorrectly counting all lines.",
  "",
  "## Replacement principles",
  "",
  "1. Replace citation placeholders only after confirming exact source papers and journal style.",
  "2. Use Sepsis-3 for the sepsis definition.",
  "3. Use TRIPOD+AI for prediction model reporting and performance transparency.",
  "4. Use calibration-specific references for calibration and threshold transportability discussion.",
  "5. Use Vickers and Elkin for decision-curve analysis.",
  "6. Use exact original GEO dataset publications or GEO records for cohort source citations.",
  "7. Do not overload the manuscript with generic biomarker citations.",
  "8. Avoid citing weak or repetitive GEO-machine-learning sepsis papers unless needed for novelty positioning.",
  "",
  "## Recommended replacement map",
  "",
  apply(
    citation_map,
    1,
    function(x) {
      paste0(
        "### ", x[["placeholder"]], "\n",
        "- Detected in manuscript: ", x[["detected_in_manuscript"]], "\n",
        "- Current occurrences: ", x[["current_occurrences"]], "\n",
        "- Recommended refs: ", x[["recommended_refs"]], "\n",
        "- Section: ", x[["manuscript_section"]], "\n",
        "- Action: ", x[["action"]], "\n"
      )
    }
  ),
  "",
  "## Manual priority",
  "",
  "### First priority",
  "",
  "- Sepsis-3 definition",
  "- TRIPOD+AI",
  "- Calibration reference",
  "- DCA references",
  "- Exact GEO cohort source papers or GEO records",
  "",
  "### Second priority",
  "",
  "- Representative sepsis transcriptomic signature papers",
  "- Sepsis immune dysregulation review",
  "- Sepsis scRNA-seq immune landscape paper",
  "",
  "### Third priority",
  "",
  "- Seurat/module score reference",
  "- PROBAST or additional calibration-focused methods references if needed"
)

# ============================================================
# 输出文件
# ============================================================

message("Writing citation replacement plan outputs...")

map_path <- file.path(draft_dir, "BMC_Genomics_citation_replacement_map_v0.3.csv")
candidate_path <- file.path(draft_dir, "BMC_Genomics_reference_candidates_v0.3.csv")
location_path <- file.path(draft_dir, "BMC_Genomics_citation_placeholder_locations_v0.3.csv")
plan_path <- file.path(draft_dir, "BMC_Genomics_citation_replacement_plan_v0.3.md")
index_path <- file.path(draft_dir, "BMC_Genomics_citation_replacement_plan_v0.3_index.xlsx")

data.table::fwrite(citation_map, map_path)
data.table::fwrite(reference_candidates, candidate_path)
data.table::fwrite(placeholder_locations, location_path)
write_text_file(plan_lines, plan_path)

output_index <- data.frame(
  file = c(
    "BMC_Genomics_citation_replacement_map_v0.3.csv",
    "BMC_Genomics_reference_candidates_v0.3.csv",
    "BMC_Genomics_citation_placeholder_locations_v0.3.csv",
    "BMC_Genomics_citation_replacement_plan_v0.3.md",
    "BMC_Genomics_citation_replacement_plan_v0.3_index.xlsx"
  ),
  path = c(
    map_path,
    candidate_path,
    location_path,
    plan_path,
    index_path
  ),
  purpose = c(
    "Placeholder-to-reference replacement map",
    "Candidate reference list for manual verification",
    "Line-level placeholder locations in manuscript",
    "Human-readable citation replacement plan",
    "Workbook index"
  ),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "output_index")
openxlsx::writeData(wb, "output_index", output_index)

openxlsx::addWorksheet(wb, "citation_map")
openxlsx::writeData(wb, "citation_map", citation_map)

openxlsx::addWorksheet(wb, "reference_candidates")
openxlsx::writeData(wb, "reference_candidates", reference_candidates)

openxlsx::addWorksheet(wb, "placeholder_locations")
openxlsx::writeData(wb, "placeholder_locations", placeholder_locations)

openxlsx::saveWorkbook(wb, index_path, overwrite = TRUE)

sink(file.path(log_dir, "sessionInfo_26_BMC_Genomics_citation_replacement_plan.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 26 BMC Genomics citation replacement plan 完成 ============")
message("输出目录：", draft_dir)

message("\nDetected placeholders:")
print(detected_placeholders)

message("\nCitation map:")
print(citation_map)

message("\nReference candidates:")
print(reference_candidates)

message("\nPlaceholder locations:")
print(placeholder_locations)

message("\nGenerated files:")
print(output_index)

message("\n关键输出：")
message("1) ", map_path)
message("2) ", candidate_path)
message("3) ", location_path)
message("4) ", plan_path)
message("5) ", index_path)

message("\n下一步：")
message("把 Detected placeholders 和 Citation map 贴给我。")
message("确认引用计划后，再做 27_BMC_Genomics_reference_insertion_v0.3.R。")