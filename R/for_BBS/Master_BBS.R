path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("data_wrangling.R") # wrangling data and find good routes
source("BBS_ta.R")         # tail analysis for each route
#source("BBS_ta_Diet5Cat.R") 
#source("rank_category.R") # compute and save categorized interaction freq between dominant groups for each route
source("summary_for_stabilitymetric.R") # get stability metric for BBS data
