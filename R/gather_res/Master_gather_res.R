path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

#===============================================================
source("summary_rank_category_for_each_realm.R") # to get interaction freq plot by realm for dominant sp pair
# NOTE: BioTIMEx needs to be included in the above plot

source("summary_stability.R") # to get stability metric table
source("summary_ancova_res.R") # run ancova for stability-synchrony relationship
#===============================================================
