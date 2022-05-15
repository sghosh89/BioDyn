path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

#------------ plotlevel analysis ---------------
source("get_BioTIME_data.R") # reading data
source("freshwater_plotlevel.R") 
source("terrestrial_plotlevel.R") 
#source("rank_category_plotlevel.R") 
source("summary_for_stabilitymetric_plotlevel.R") 
