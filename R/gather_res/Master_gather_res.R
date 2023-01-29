path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

if(!dir.exists("../../Results/gather_res/")){
  dir.create("../../Results/gather_res/")
}

#===============================================================
#source("summary_rank_category_for_each_realm.R") # to get interaction freq plot by realm for dominant sp pair
source("summary_stability.R") # to get stability metric table with all data
#source("summary_ancova_res.R") # run ancova for stability-synchrony relationship
#===============================================================

source("datasummary.R") # summarizing metadata used in this study

# needs to be edited
#source("targetspecieslist_alldata.R") # summarizing target sp list used for tail analysis in this study

#===========================================

#----- subset birds data and rerun analysis but with VR_LdM -------------
source("subset_BBS_21.R") # subsetting 21% bird data - do NOT run again this code
#source("bayesian_model_realmlevel_simple_subset_birds_vr_LdM.R")
#source("model_summary_realmlevel_simple_subset_birds_vr_LdM.R")
#source("plot_posterior_realmlevel_simple_subset_birds_vr_LdM.R")

source("datasummary_subset_birds.R") # summarizing metadata for subsetted birds

#===========================================
# 100 runs to see the robustness of the results
source("get_reducedset15_for_eachtaxa.R")
source("call_toymodel_fixed_realm.R") # this is the main analysis
source("conditionalplot_100runs.R") # this is for conditional plotting


#source("revision_test.R") # on working mode



