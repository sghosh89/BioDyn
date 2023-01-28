path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("wrangling_data.R") # clean and prepare data
source("insect_ta.R") # tail analysis
#source("insect_ta_nbin3.R") #tail analysis for each route, nbin=3, nyr>=41, max year sampled 37 years - so none found
source("summary_for_stabilitymetric.R")
#source("rank_category.R")