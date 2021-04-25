path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("lakedata_cleaning_phytoplankton.R") # clean the raw data
source("select_sp_forBlake.R") # genus aggregation
source("get_input_spmat_phytoplankton.R") # get input matrix for tail analysis
source("tail_analysis_phytoplankton.R") # tail analysis results
source("rank_category_phytoplankton.R") # compute and save categorized interaction freq between dominant groups for each site
source("summary_for_stabilitymetric_phytoplankton.R") # stability metric for phytoplankton
