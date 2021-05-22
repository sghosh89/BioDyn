path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("get_rarefied_BioTIME_data.R") # rarefy bioTIME data
source("freshwater.R") # tail analysis for freshwater
source("marine.R")     # tail analysis for marine
source("terrestrial.R") # tail analysis for terrestrial
source("try_summarize.R") # summarize tail analysis res, plot for each realm
source("rank_category.R") # compute and save categorized interaction freq between dominant groups
source("summary_for_stabilitymetric.R") # get stability metric for BioTIME data

#------------ plotlevel analysis ---------------
#source("freshwater_plotlevel.R") # instead of source("freshwater.R")