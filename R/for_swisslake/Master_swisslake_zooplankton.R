path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("lakedata_cleaning_for_ZP.R")
source("tail_analysis_zooplankton.R")