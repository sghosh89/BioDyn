path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("cleaning_zoop_2014.R")# clean the raw data
source("tailanal_zoop_2014.R") # tail analysis results
#source("rank_category_zoop_2014.R") # compute and save categorized interaction freq between dominant groups for each site
source("summary_for_stabilitymetric.R") # get stability metric table for each site

# zoop2014 max 28 years sampled - so not included for nbin=3 analysis

