# download_selected_geo_datasets.R

library(GEOquery)

project_dir <- "/Users/felix/Documents/Sepsis_CrossCohort_scRNA"
geo_out <- file.path(project_dir,"01_raw_data/geo_series_matrix")
dir.create(geo_out, recursive=TRUE, showWarnings=FALSE)

gse_list <- c("GSE65682", "GSE54514", "GSE95233")

for(gse in gse_list){
  message("Downloading ", gse)
  try({
    gobj <- getGEO(gse, GSEMatrix=TRUE)
    # some GSE returns list
    if(is.list(gobj)) gobj <- gobj[[1]]
    saveRDS(gobj, file.path(geo_out, paste0(gse,"_series_matrix.rds")))
    message("Saved ", gse)
  }, silent=FALSE)
}

message("Download complete. Check ", geo_out)