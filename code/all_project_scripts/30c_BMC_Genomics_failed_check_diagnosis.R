# ============================================================
# 30c_BMC_Genomics_failed_check_diagnosis.R
# Diagnose the remaining failed core check after 30b
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
draft_dir <- file.path(project_dir, "07_manuscript", "BMC_Genomics_draft")
log_dir <- file.path(project_dir, "04_results", "logs")

input_xlsx <- file.path(draft_dir, "BMC_Genomics_submission_package_check_v0.7.1.xlsx")

if (!file.exists(input_xlsx)) {
  stop("缺少 30b 输出文件：", input_xlsx)
}

message("Reading 30b check workbook...")

sheets <- openxlsx::getSheetNames(input_xlsx)

read_sheet_safe <- function(sheet) {
  tryCatch(
    openxlsx::read.xlsx(input_xlsx, sheet = sheet),
    error = function(e) data.frame()
  )
}

sheet_list <- setNames(lapply(sheets, read_sheet_safe), sheets)

failed_all <- data.frame()

for (s in names(sheet_list)) {
  df <- sheet_list[[s]]
  if (nrow(df) == 0) next
  if (!"status" %in% names(df)) next
  
  fail <- df[df$status != "PASS", , drop = FALSE]
  if (nrow(fail) > 0) {
    fail$source_sheet <- s
    failed_all <- rbind(failed_all, fail)
  }
}

if (nrow(failed_all) == 0) {
  failed_all <- data.frame(
    source_sheet = character(),
    check_id = character(),
    requirement = character(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

out_csv <- file.path(draft_dir, "BMC_Genomics_failed_core_check_diagnosis_v0.7.1.csv")
data.table::fwrite(failed_all, out_csv)

message("\n============ 30c failed check diagnosis 完成 ============")
message("输出目录：", draft_dir)

message("\nFailed checks:")
print(failed_all)

message("\n关键输出：")
message("1) ", out_csv)

message("\n下一步：")
message("把 Failed checks 贴给我。")