path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

if(!dir.exists("../../Results/gather_res/")){
  dir.create("../../Results/gather_res/")
}

#===============================================================
source("summary_rank_category_for_each_realm.R") # to get interaction freq plot by realm for dominant sp pair
# NOTE: BioTIMEx needs to be included in the above plot

source("summary_stability.R") # to get stability metric table
source("summary_ancova_res.R") # run ancova for stability-synchrony relationship
#===============================================================

#=======================================================
# Bayesian based model to predict stability for freshwater and aquatic realm
source("compare_model_brms.R") # basic model to analyze stability
source("plotter_conditional_effect_brms_univar.R") # prediction for conditional effects and posterior parameter plot
#====================================================

source("datasummary.R") # summarizing data used in this study
source("targetspecieslist_alldata.R") # summarizing target sp list used for tail analysis in this study

