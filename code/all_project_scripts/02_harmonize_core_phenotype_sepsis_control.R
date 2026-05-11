# ============================================================
# 02_harmonize_core_phenotype_sepsis_control.R
# 构建核心队列 Sepsis vs Control 样本级分组表
#
# 修正版：
# 1. 合并前强制所有 phenotype 列转 character，避免 bind_rows 类型冲突；
# 2. 显式使用 dplyr::select / filter / mutate 等，避免函数冲突；
# 3. 不使用 T01 数据集级筛选表；
# 4. 不做表达矩阵合并；
# 5. 不做 Combat；
# 6. 只输出每个样本 include/exclude 和分组理由。
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"

pheno_dir <- file.path(project_dir, "02_processed_data", "phenotype")
table_dir <- file.path(project_dir, "04_results", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

core_gse <- c("GSE65682", "GSE54514", "GSE95233")

# ---------- 工具函数 ----------
clean_value <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), "", x)
  x <- stringr::str_replace(x, "^[^:]+:\\s*", "")
  x <- stringr::str_trim(x)
  x
}

get_col_safe <- function(df, col) {
  if (col %in% names(df)) {
    return(as.character(df[[col]]))
  } else {
    return(rep(NA_character_, nrow(df)))
  }
}

# ---------- GSE54514 ----------
harmonize_gse54514 <- function(ph) {
  
  disease_raw <- get_col_safe(ph, "disease status:ch1")
  group_day_raw <- get_col_safe(ph, "group_day:ch1")
  title_raw <- get_col_safe(ph, "title")
  source_raw <- get_col_safe(ph, "source_name_ch1")
  
  disease <- stringr::str_to_lower(clean_value(disease_raw))
  group_day <- clean_value(group_day_raw)
  title_lower <- stringr::str_to_lower(title_raw)
  source_lower <- stringr::str_to_lower(source_raw)
  
  patient_id <- clean_value(get_col_safe(ph, "group_id:ch1"))
  
  clinical_group <- dplyr::case_when(
    disease == "healthy" ~ "Control",
    disease %in% c("sepsis survivor", "sepsis nonsurvivor") ~ "Sepsis",
    TRUE ~ NA_character_
  )
  
  timepoint <- dplyr::case_when(
    stringr::str_detect(group_day, "_D1$") ~ "D1",
    stringr::str_detect(group_day, "_D2$") ~ "D2",
    stringr::str_detect(group_day, "_D3$") ~ "D3",
    stringr::str_detect(group_day, "_D4$") ~ "D4",
    stringr::str_detect(group_day, "_D5$") ~ "D5",
    stringr::str_detect(title_lower, "day_1|day 1") ~ "D1",
    stringr::str_detect(source_lower, "day_1|day 1") ~ "D1",
    TRUE ~ NA_character_
  )
  
  survival_status <- dplyr::case_when(
    disease == "sepsis survivor" ~ "Survivor",
    disease == "sepsis nonsurvivor" ~ "Non-survivor",
    disease == "healthy" ~ NA_character_,
    TRUE ~ NA_character_
  )
  
  include_main <- dplyr::case_when(
    clinical_group == "Control" & timepoint == "D1" ~ TRUE,
    clinical_group == "Sepsis" & timepoint == "D1" ~ TRUE,
    TRUE ~ FALSE
  )
  
  exclude_reason <- dplyr::case_when(
    is.na(clinical_group) ~ "Cannot assign clinical group",
    is.na(timepoint) ~ "Cannot assign timepoint",
    timepoint != "D1" ~ "Non-baseline follow-up sample",
    TRUE ~ ""
  )
  
  out <- ph %>%
    dplyr::mutate(
      dataset = "GSE54514",
      sample_id = sample_id,
      clinical_group = clinical_group,
      disease_status_clean = disease,
      timepoint = timepoint,
      patient_id = patient_id,
      survival_status = survival_status,
      time_to_event_28days = NA_character_,
      include_main = include_main,
      exclude_reason = exclude_reason,
      harmonization_rule = "GSE54514: include HC_D1 as Control; include S_D1 and NS_D1 as Sepsis; exclude D2-D5 follow-up samples."
    )
  
  return(out)
}

# ---------- GSE95233 ----------
harmonize_gse95233 <- function(ph) {
  
  source_raw <- get_col_safe(ph, "source_name_ch1")
  title_raw  <- get_col_safe(ph, "title")
  time_raw   <- get_col_safe(ph, "time point:ch1")
  survival_raw <- get_col_safe(ph, "survival:ch1")
  
  source_lower <- stringr::str_to_lower(source_raw)
  title_lower  <- stringr::str_to_lower(title_raw)
  timepoint_clean <- clean_value(time_raw)
  survival_clean <- clean_value(survival_raw)
  
  clinical_group <- dplyr::case_when(
    stringr::str_detect(source_lower, "^control") ~ "Control",
    stringr::str_detect(source_lower, "^patient") ~ "Sepsis",
    stringr::str_detect(title_lower, "blood-cs|blood-pc") ~ "Control",
    stringr::str_detect(title_lower, "blood-sv") ~ "Sepsis",
    TRUE ~ NA_character_
  )
  
  timepoint <- dplyr::case_when(
    clinical_group == "Control" ~ "D0_control",
    timepoint_clean %in% c("D01", "D1") ~ "D1",
    timepoint_clean %in% c("D02", "D2") ~ "D2",
    timepoint_clean %in% c("D03", "D3") ~ "D3",
    stringr::str_detect(source_lower, "day1") ~ "D1",
    stringr::str_detect(source_lower, "day2") ~ "D2",
    stringr::str_detect(source_lower, "day3") ~ "D3",
    stringr::str_detect(title_lower, "_d01$") ~ "D1",
    stringr::str_detect(title_lower, "_d02$") ~ "D2",
    stringr::str_detect(title_lower, "_d03$") ~ "D3",
    TRUE ~ NA_character_
  )
  
  patient_id <- dplyr::case_when(
    clinical_group == "Control" ~ stringr::str_extract(source_raw, "\\d+"),
    clinical_group == "Sepsis" ~ stringr::str_extract(source_raw, "\\d+"),
    TRUE ~ NA_character_
  )
  
  survival_status <- dplyr::case_when(
    stringr::str_to_lower(survival_clean) == "survivor" ~ "Survivor",
    stringr::str_to_lower(survival_clean) == "non survivor" ~ "Non-survivor",
    TRUE ~ NA_character_
  )
  
  include_main <- dplyr::case_when(
    clinical_group == "Control" ~ TRUE,
    clinical_group == "Sepsis" & timepoint == "D1" ~ TRUE,
    TRUE ~ FALSE
  )
  
  exclude_reason <- dplyr::case_when(
    is.na(clinical_group) ~ "Cannot assign clinical group",
    clinical_group == "Sepsis" & timepoint != "D1" ~ "Non-baseline follow-up sample",
    TRUE ~ ""
  )
  
  out <- ph %>%
    dplyr::mutate(
      dataset = "GSE95233",
      sample_id = sample_id,
      clinical_group = clinical_group,
      disease_status_clean = clinical_group,
      timepoint = timepoint,
      patient_id = patient_id,
      survival_status = survival_status,
      time_to_event_28days = NA_character_,
      include_main = include_main,
      exclude_reason = exclude_reason,
      harmonization_rule = "GSE95233: include controls and patient Day1/D01 as baseline Sepsis; exclude patient Day2/Day3 follow-up samples."
    )
  
  return(out)
}

# ---------- GSE65682 ----------
harmonize_gse65682 <- function(ph) {
  
  title_raw <- get_col_safe(ph, "title")
  title_lower <- stringr::str_to_lower(title_raw)
  
  icua_raw <- get_col_safe(ph, "icu_acquired_infection:ch1")
  icua_pair_raw <- get_col_safe(ph, "icu_acquired_infection_paired:ch1")
  abdo_raw <- get_col_safe(ph, "abdominal_sepsis_and_controls:ch1")
  mort_raw <- get_col_safe(ph, "mortality_event_28days:ch1")
  tte_raw  <- get_col_safe(ph, "time_to_event_28days:ch1")
  
  icua <- clean_value(icua_raw)
  icua_pair <- clean_value(icua_pair_raw)
  abdo <- clean_value(abdo_raw)
  
  candidate_subset <- dplyr::case_when(
    abdo == "abdo_s" ~ "abdominal_sepsis",
    abdo == "ctrl_GI" ~ "abdominal_control",
    icua == "healthy" | icua_pair == "healthy" | stringr::str_detect(title_lower, "healthy subject") ~ "healthy_subject",
    icua == "ICUA" | icua_pair == "Event_ICUAI" ~ "icu_acquired_infection_event",
    icua == "No_ICUA" | icua_pair == "ADMIS_NO_ICUAI" ~ "icu_no_acquired_infection_admission",
    icua_pair == "Admis_ICUAI" ~ "icu_infection_admission",
    TRUE ~ "unclassified_icu_or_other"
  )
  
  clinical_group_candidate <- dplyr::case_when(
    candidate_subset == "abdominal_sepsis" ~ "Sepsis",
    candidate_subset == "abdominal_control" ~ "Control",
    candidate_subset == "healthy_subject" ~ "Control",
    TRUE ~ NA_character_
  )
  
  include_main <- rep(FALSE, nrow(ph))
  
  exclude_reason <- dplyr::case_when(
    candidate_subset %in% c("abdominal_sepsis", "abdominal_control") ~ 
      "GSE65682 candidate abdominal subset; temporarily held out for manual confirmation",
    candidate_subset == "healthy_subject" ~ 
      "Healthy subject in GSE65682; temporarily held out until matched case subset is confirmed",
    TRUE ~ 
      "Complex ICU cohort; not used in main Sepsis vs Control table at this stage"
  )
  
  survival_status <- dplyr::case_when(
    clean_value(mort_raw) == "0" ~ "Alive_28d",
    clean_value(mort_raw) == "1" ~ "Dead_28d",
    TRUE ~ NA_character_
  )
  
  out <- ph %>%
    dplyr::mutate(
      dataset = "GSE65682",
      sample_id = sample_id,
      clinical_group = clinical_group_candidate,
      disease_status_clean = candidate_subset,
      timepoint = NA_character_,
      patient_id = sample_id,
      survival_status = survival_status,
      time_to_event_28days = clean_value(tte_raw),
      include_main = include_main,
      exclude_reason = exclude_reason,
      harmonization_rule = "GSE65682: complex ICU/healthy/abdominal subset cohort. Temporarily excluded from main table until sepsis-control contrast is manually fixed."
    )
  
  return(out)
}

# ---------- 主程序 ----------
harmonized_list <- list()

for (acc in core_gse) {
  
  pheno_file <- file.path(pheno_dir, paste0("P00_", acc, "_pheno_raw.csv"))
  
  if (!file.exists(pheno_file)) {
    stop("缺少 phenotype 文件：", pheno_file)
  }
  
  message("正在处理 phenotype：", acc)
  
  ph <- read.csv(
    pheno_file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  ph[] <- lapply(ph, function(x) as.character(x))
  
  if (!"sample_id" %in% names(ph)) {
    stop(acc, " 缺少 sample_id 列。")
  }
  
  out <- switch(
    acc,
    "GSE54514" = harmonize_gse54514(ph),
    "GSE95233" = harmonize_gse95233(ph),
    "GSE65682" = harmonize_gse65682(ph),
    stop("未定义 harmonization 规则：", acc)
  )
  
  out[] <- lapply(out, function(x) as.character(x))
  
  harmonized_list[[acc]] <- out
}

harmonized_all <- dplyr::bind_rows(harmonized_list)

key_cols <- c(
  "dataset",
  "sample_id",
  "clinical_group",
  "disease_status_clean",
  "timepoint",
  "patient_id",
  "survival_status",
  "time_to_event_28days",
  "include_main",
  "exclude_reason",
  "harmonization_rule",
  "title",
  "source_name_ch1",
  "disease status:ch1",
  "group_day:ch1",
  "group_id:ch1",
  "time point:ch1",
  "survival:ch1",
  "icu_acquired_infection:ch1",
  "icu_acquired_infection_paired:ch1",
  "abdominal_sepsis_and_controls:ch1"
)

key_cols_existing <- key_cols[key_cols %in% names(harmonized_all)]

sample_map <- harmonized_all %>%
  dplyr::select(dplyr::all_of(key_cols_existing)) %>%
  dplyr::mutate(
    include_main = as.logical(include_main)
  )

summary_by_dataset <- sample_map %>%
  dplyr::group_by(dataset, include_main, clinical_group, disease_status_clean, timepoint) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(dataset, dplyr::desc(include_main), clinical_group, disease_status_clean, timepoint)

main_summary <- sample_map %>%
  dplyr::filter(include_main == TRUE) %>%
  dplyr::group_by(dataset, clinical_group) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(dataset, clinical_group)

excluded_summary <- sample_map %>%
  dplyr::filter(include_main == FALSE) %>%
  dplyr::group_by(dataset, exclude_reason) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(dataset, dplyr::desc(n))

out_xlsx <- file.path(table_dir, "T01_core_sample_group_harmonization.xlsx")
out_csv  <- file.path(table_dir, "T01_core_sample_group_harmonization.csv")

write.csv(sample_map, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "sample_map")
openxlsx::writeData(wb, "sample_map", sample_map)

openxlsx::addWorksheet(wb, "summary_by_dataset")
openxlsx::writeData(wb, "summary_by_dataset", summary_by_dataset)

openxlsx::addWorksheet(wb, "main_summary")
openxlsx::writeData(wb, "main_summary", main_summary)

openxlsx::addWorksheet(wb, "excluded_summary")
openxlsx::writeData(wb, "excluded_summary", excluded_summary)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("\n============================================================")
message("样本级分组表构建完成。")
message("输出：")
message("1) ", out_xlsx)
message("2) ", out_csv)
message("============================================================")

message("\n主分析纳入样本数：")
print(main_summary)

message("\n排除样本概览：")
print(excluded_summary, n = Inf)