path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("wrangling_data.R") # clean and prepare data
source("insect_ta.R") # tail analysis
source("summary_for_stabilitymetric.R")
source("rank_category.R")