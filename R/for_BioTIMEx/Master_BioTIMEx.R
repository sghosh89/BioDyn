path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("2.0_wrangling_raw_data.r")
source("3.0_get_tail_analysis_res.r")
