path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("data_wrangling.R") # wrangling data and find good routes
source("BBS_ta.R")         # tail analysis for each route
#source("BBS_ta_nbin3.R") #tail analysis for each route, nbin=3, nyr>=41, none found
#source("BBS_ta_Diet5Cat.R") 
#source("rank_category.R") # compute and save categorized interaction freq between dominant groups for each route
source("summary_for_stabilitymetric.R") # get stability metric for BBS data


#=================
source("process_bbs.R")
source("BBS_ta_with_stocdata.R")
source("compare_res_without_and_with_stoc.R")