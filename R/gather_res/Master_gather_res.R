path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

if(!dir.exists("../../Results/gather_res/")){
  dir.create("../../Results/gather_res/")
}

#===============================================================
source("summary_rank_category_for_each_realm.R") # to get interaction freq plot by realm for dominant sp pair
source("summary_stability.R") # to get stability metric table
source("summary_ancova_res.R") # run ancova for stability-synchrony relationship
#===============================================================

source("datasummary.R") # summarizing metadata used in this study

# needs to be edited
#source("targetspecieslist_alldata.R") # summarizing target sp list used for tail analysis in this study

# Bayesian model
source("bayesian_model_realmlevel.R") # Realm level, TAXA as random effect
#source("bayesian_model_taxalevel.R") # TAXA as fixed effect

# plotting model results
source("plot_posterior_realmlevel.R")
#source("plot_posterior_taxalevel.R")

source("model_summary_res_table.R")
#===========================================

# subset birds data and rerun analysis
source("bayesian_model_realmlevel_subset_birds_31.R")
source("plot_posterior_realmlevel_subset_birds_31.R")














