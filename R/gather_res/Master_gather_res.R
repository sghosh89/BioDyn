path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

if(!dir.exists("../../Results/gather_res/")){
  dir.create("../../Results/gather_res/")
}

#===============================================================
source("summary_stability.R") # to get stability metric table with all data
#===============================================================

source("datasummary.R") # summarizing metadata used in this study

#===========================================

#----- subset birds data and rerun analysis but with VR_LdM -------------
source("subset_BBS_21.R") # subsetting 21% bird data - do NOT run again this code
source("datasummary_subset_birds.R") # summarizing metadata for subsetted birds

#===========================================
# 100 runs to see the robustness of the results
source("get_reducedset15_for_eachtaxa.R")

source("./call_toymodel_fixed_realm_traditional_stability.R")
source("./conditionalplot_100runs_traditional_stability.R")


#source("compare_stability_definition.R") # defining stability figure in suppmat
source("check_collinearity.R")# extra file
source("sampling_effort_fig.R")
source("datasummary_descriptive_stats.R")

source("revision_test.R") # Extra models


