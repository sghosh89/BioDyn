path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("lakedata_cleaning_for_ZP.R")# clean the raw data
source("tail_analysis_zooplankton.R")# tail analysis results
source("rank_category_zooplankton.R") # compute and save categorized interaction freq between dominant groups for each site
source("summary_for_stabilitymetric_zooplankton.R") # stability metric for zooplankton
