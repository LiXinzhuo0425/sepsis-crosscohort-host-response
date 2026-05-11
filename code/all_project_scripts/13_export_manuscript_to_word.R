# ============================================================
# 13_export_manuscript_to_word.R
# Export manuscript markdown draft and tables to Word document
#
# 目标：
# 1. 读取 12 生成的 manuscript_main_draft_v0.1.md
# 2. 读取 reporting 表格
# 3. 生成 Word 初稿
# 4. 附加主要表格和 figure/table plan
#
# 输入：
# 07_manuscript/
#   manuscript_main_draft_v0.1.md
#   abstract_structured_v0.1.md
#   figure_table_plan_v0.1.md
#   reporting_positioning_notes_v0.1.md
#
# 04_results/reporting/
#   T11_table1_cohort_sample_summary.csv
#   T11_table2_final_model_coefficients.csv
#   T11_table3_LODO_validation_performance.csv
#   T11_table4_calibration_threshold_drift.csv
#   T11_table5_key_GO_terms.csv
#   T12_manuscript_key_numbers.csv
#   T12_figure_table_plan.csv
#
# 输出：
# 07_manuscript/
#   Sepsis_transcriptomic_model_manuscript_v0.1.docx
#   Sepsis_transcriptomic_model_tables_only_v0.1.docx
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(officer)
  library(flextable)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

manuscript_dir <- file.path(project_dir, "07_manuscript")
report_dir <- file.path(project_dir, "04_results", "reporting")
log_dir <- file.path(project_dir, "04_results", "logs")

dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 输入文件
# ============================================================

main_md_file <- file.path(manuscript_dir, "manuscript_main_draft_v0.1.md")
abstract_md_file <- file.path(manuscript_dir, "abstract_structured_v0.1.md")
figure_plan_md_file <- file.path(manuscript_dir, "figure_table_plan_v0.1.md")
positioning_md_file <- file.path(manuscript_dir, "reporting_positioning_notes_v0.1.md")

table1_file <- file.path(report_dir, "T11_table1_cohort_sample_summary.csv")
table2_file <- file.path(report_dir, "T11_table2_final_model_coefficients.csv")
table3_file <- file.path(report_dir, "T11_table3_LODO_validation_performance.csv")
table4_file <- file.path(report_dir, "T11_table4_calibration_threshold_drift.csv")
table5_file <- file.path(report_dir, "T11_table5_key_GO_terms.csv")
key_numbers_file <- file.path(report_dir, "T12_manuscript_key_numbers.csv")
figure_plan_file <- file.path(report_dir, "T12_figure_table_plan.csv")

needed <- c(
  main_md_file,
  abstract_md_file,
  table1_file,
  table2_file,
  table3_file,
  table4_file,
  table5_file,
  key_numbers_file,
  figure_plan_file
)

missing <- needed[!file.exists(needed)]

if (length(missing) > 0) {
  stop("缺少必要输入文件：\n", paste(missing, collapse = "\n"))
}

# ============================================================
# 输出文件
# ============================================================

out_docx <- file.path(manuscript_dir, "Sepsis_transcriptomic_model_manuscript_v0.1.docx")
out_tables_docx <- file.path(manuscript_dir, "Sepsis_transcriptomic_model_tables_only_v0.1.docx")

# ============================================================
# 工具函数
# ============================================================

read_text_file <- function(file) {
  if (!file.exists(file)) return(character())
  readLines(file, warn = FALSE, encoding = "UTF-8")
}

safe_fread <- function(file) {
  if (!file.exists(file)) return(data.frame())
  data.table::fread(file, data.table = FALSE)
}

clean_md_line <- function(x) {
  x <- gsub("\\*\\*", "", x)
  x <- gsub("__", "", x)
  x <- gsub("`", "", x)
  x <- gsub("^[-*]\\s+", "• ", x)
  x
}

detect_heading_level <- function(line) {
  if (grepl("^#\\s+", line)) return(1)
  if (grepl("^##\\s+", line)) return(2)
  if (grepl("^###\\s+", line)) return(3)
  if (grepl("^####\\s+", line)) return(4)
  return(0)
}

strip_heading_marks <- function(line) {
  gsub("^#{1,6}\\s+", "", line)
}

add_markdown_lines_to_doc <- function(doc, lines) {
  
  if (length(lines) == 0) return(doc)
  
  in_code_block <- FALSE
  code_buffer <- character()
  
  for (line in lines) {
    
    if (grepl("^```", line)) {
      if (!in_code_block) {
        in_code_block <- TRUE
        code_buffer <- character()
      } else {
        in_code_block <- FALSE
        if (length(code_buffer) > 0) {
          doc <- body_add_par(doc, paste(code_buffer, collapse = "\n"), style = "Normal")
        }
        code_buffer <- character()
      }
      next
    }
    
    if (in_code_block) {
      code_buffer <- c(code_buffer, line)
      next
    }
    
    if (trimws(line) == "") {
      doc <- body_add_par(doc, "", style = "Normal")
      next
    }
    
    level <- detect_heading_level(line)
    
    if (level == 1) {
      doc <- body_add_par(doc, strip_heading_marks(line), style = "heading 1")
    } else if (level == 2) {
      doc <- body_add_par(doc, strip_heading_marks(line), style = "heading 2")
    } else if (level == 3) {
      doc <- body_add_par(doc, strip_heading_marks(line), style = "heading 3")
    } else if (level == 4) {
      doc <- body_add_par(doc, strip_heading_marks(line), style = "heading 4")
    } else {
      doc <- body_add_par(doc, clean_md_line(line), style = "Normal")
    }
  }
  
  doc
}

make_ft <- function(df, max_rows = Inf, font_size = 8) {
  
  if (is.null(df) || nrow(df) == 0) {
    df <- data.frame(Note = "No data available")
  }
  
  if (is.finite(max_rows) && nrow(df) > max_rows) {
    df <- head(df, max_rows)
  }
  
  ft <- flextable::flextable(df)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::autofit(ft)
  ft
}

add_table_section <- function(doc, title, caption, df, max_rows = Inf, font_size = 8) {
  
  doc <- body_add_par(doc, title, style = "heading 2")
  
  if (!is.null(caption) && caption != "") {
    doc <- body_add_par(doc, caption, style = "Normal")
  }
  
  ft <- make_ft(df, max_rows = max_rows, font_size = font_size)
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "", style = "Normal")
  
  doc
}

add_document_title_page <- function(doc) {
  
  doc <- body_add_par(
    doc,
    "Cross-cohort transportability of a compact blood transcriptomic host-response model for adult sepsis diagnosis",
    style = "Title"
  )
  
  doc <- body_add_par(doc, "Draft version: v0.1", style = "Normal")
  doc <- body_add_par(doc, paste0("Generated on: ", Sys.Date()), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
  
  doc <- body_add_par(doc, "Important positioning note", style = "heading 1")
  doc <- body_add_par(
    doc,
    "This Word draft is generated from the current analysis outputs. The final compact model should be described as a candidate model trained on pooled public adult bulk transcriptome cohorts. Leave-one-dataset-out analysis should be described as dataset-level transportability evaluation, not fully independent prospective validation.",
    style = "Normal"
  )
  
  doc <- body_add_break(doc)
  
  doc
}

# ============================================================
# 读取数据
# ============================================================

message("Reading manuscript and table inputs...")

main_lines <- read_text_file(main_md_file)
abstract_lines <- read_text_file(abstract_md_file)
figure_plan_lines <- read_text_file(figure_plan_md_file)
positioning_lines <- read_text_file(positioning_md_file)

table1 <- safe_fread(table1_file)
table2 <- safe_fread(table2_file)
table3 <- safe_fread(table3_file)
table4 <- safe_fread(table4_file)
table5 <- safe_fread(table5_file)
key_numbers <- safe_fread(key_numbers_file)
figure_plan <- safe_fread(figure_plan_file)

# ============================================================
# 清理表格显示
# ============================================================

# Table 2 只保留核心系数列
if (nrow(table2) > 0) {
  keep2 <- intersect(c("term", "coefficient", "coefficient_rounded"), colnames(table2))
  table2_display <- table2[, keep2, drop = FALSE]
} else {
  table2_display <- table2
}

# Table 3 只保留投稿主表常用列
if (nrow(table3) > 0) {
  keep3 <- intersect(
    c(
      "validation_dataset", "n", "n_case", "n_control",
      "AUROC_CI", "AUPRC", "Brier",
      "threshold", "sensitivity", "specificity", "PPV", "NPV", "accuracy",
      "n_selected_genes"
    ),
    colnames(table3)
  )
  table3_display <- table3[, keep3, drop = FALSE]
} else {
  table3_display <- table3
}

# Table 4 只保留校准和阈值迁移核心列
if (nrow(table4) > 0) {
  keep4 <- intersect(
    c(
      "validation_dataset", "n", "observed_rate", "mean_predicted",
      "calibration_in_the_large", "calibration_intercept", "calibration_slope",
      "Brier", "fixed_training_threshold", "local_youden_threshold",
      "threshold_shift", "abs_threshold_shift",
      "fixed_sensitivity", "fixed_specificity",
      "local_sensitivity", "local_specificity"
    ),
    colnames(table4)
  )
  table4_display <- table4[, keep4, drop = FALSE]
} else {
  table4_display <- table4
}

# Table 5 GO terms 只保留前 30 行，避免 Word 过长
if (nrow(table5) > 0) {
  keep5 <- intersect(
    c("gene_set", "ID", "Description", "GeneRatio", "BgRatio", "p.adjust", "Count"),
    colnames(table5)
  )
  table5_display <- table5[, keep5, drop = FALSE]
} else {
  table5_display <- table5
}

# ============================================================
# 生成完整 Word 初稿
# ============================================================

message("Creating full manuscript Word document...")

doc <- read_docx()
doc <- add_document_title_page(doc)

doc <- body_add_par(doc, "Main manuscript draft", style = "heading 1")
doc <- add_markdown_lines_to_doc(doc, main_lines)

doc <- body_add_break(doc)

doc <- body_add_par(doc, "Manuscript-ready tables", style = "heading 1")

doc <- add_table_section(
  doc,
  title = "Table 1. Included cohorts and sample composition",
  caption = "Summary of adult blood transcriptome cohorts included in the main diagnostic analysis.",
  df = table1,
  max_rows = Inf,
  font_size = 8
)

doc <- add_table_section(
  doc,
  title = "Table 2. Final compact model coefficients",
  caption = "Coefficients are applied to standardized gene expression values using the development-set center and scale parameters.",
  df = table2_display,
  max_rows = Inf,
  font_size = 8
)

doc <- add_table_section(
  doc,
  title = "Table 3. Leave-one-dataset-out validation performance",
  caption = "Dataset-level transportability evaluation. Apparent pooled performance should not be interpreted as independent validation.",
  df = table3_display,
  max_rows = Inf,
  font_size = 7
)

doc <- add_table_section(
  doc,
  title = "Table 4. Calibration and threshold drift across validation datasets",
  caption = "Calibration-in-the-large is mean predicted probability minus observed sepsis proportion. Threshold shift is local Youden threshold minus fixed training threshold.",
  df = table4_display,
  max_rows = Inf,
  font_size = 7
)

doc <- add_table_section(
  doc,
  title = "Table 5. Key GO Biological Process enrichment terms",
  caption = "Representative GO terms for upregulated, downregulated and all significant differentially expressed genes.",
  df = table5_display,
  max_rows = 30,
  font_size = 7
)

doc <- body_add_break(doc)

doc <- body_add_par(doc, "Figure and table plan", style = "heading 1")
doc <- add_markdown_lines_to_doc(doc, figure_plan_lines)

doc <- body_add_break(doc)

doc <- body_add_par(doc, "Reporting and positioning notes", style = "heading 1")
doc <- add_markdown_lines_to_doc(doc, positioning_lines)

print(doc, target = out_docx)

# ============================================================
# 生成 tables-only Word 文件
# ============================================================

message("Creating tables-only Word document...")

doc_tables <- read_docx()

doc_tables <- body_add_par(
  doc_tables,
  "Manuscript-ready tables for sepsis transcriptomic model paper",
  style = "Title"
)

doc_tables <- body_add_par(doc_tables, paste0("Generated on: ", Sys.Date()), style = "Normal")
doc_tables <- body_add_par(doc_tables, "", style = "Normal")

doc_tables <- add_table_section(
  doc_tables,
  title = "Key numbers",
  caption = "Automatically extracted key values for manuscript drafting.",
  df = key_numbers,
  max_rows = Inf,
  font_size = 8
)

doc_tables <- add_table_section(
  doc_tables,
  title = "Table 1. Included cohorts and sample composition",
  caption = "Summary of adult blood transcriptome cohorts.",
  df = table1,
  max_rows = Inf,
  font_size = 8
)

doc_tables <- add_table_section(
  doc_tables,
  title = "Table 2. Final compact model coefficients",
  caption = "Model coefficients.",
  df = table2_display,
  max_rows = Inf,
  font_size = 8
)

doc_tables <- add_table_section(
  doc_tables,
  title = "Table 3. Leave-one-dataset-out validation performance",
  caption = "Dataset-level validation performance.",
  df = table3_display,
  max_rows = Inf,
  font_size = 7
)

doc_tables <- add_table_section(
  doc_tables,
  title = "Table 4. Calibration and threshold drift",
  caption = "Calibration and fixed-threshold transportability metrics.",
  df = table4_display,
  max_rows = Inf,
  font_size = 7
)

doc_tables <- add_table_section(
  doc_tables,
  title = "Table 5. Key GO Biological Process enrichment terms",
  caption = "Representative GO terms.",
  df = table5_display,
  max_rows = 30,
  font_size = 7
)

doc_tables <- add_table_section(
  doc_tables,
  title = "Figure and table plan",
  caption = "Planned manuscript display items.",
  df = figure_plan,
  max_rows = Inf,
  font_size = 7
)

print(doc_tables, target = out_tables_docx)

# ============================================================
# sessionInfo
# ============================================================

sink(file.path(log_dir, "sessionInfo_13_export_manuscript_to_word.txt"))
print(sessionInfo())
sink()

# ============================================================
# 控制台输出
# ============================================================

message("\n============ 13 Export manuscript to Word 完成 ============")
message("Word 初稿：", out_docx)
message("Word 表格版：", out_tables_docx)

message("\n请重点检查：")
message("1) Word 初稿中标题层级是否正常。")
message("2) Results 是否准确呈现 LODO 和 calibration。")
message("3) Discussion 是否明确说明 final model 是 candidate model。")
message("4) 表格是否因列太多需要横向页面或拆分。")

message("\n下一步建议：")
message("打开 Word 初稿后，先不要改格式，先检查科学叙事是否正确。")