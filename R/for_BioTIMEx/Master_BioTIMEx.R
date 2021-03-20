path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("2.0_wrangling_raw_data.r") # cleaning data
source("3.0_get_tail_analysis_res.r") # tail analysis
source("rank_category.R") # compute and save categorized interaction freq between dominant groups for each site
source("summary_for_stabilitymetric.R") # for stability metric info

