path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

#===============================================================
source("summary_stability.R")
source("summary_ancova_res.R")
#===============================================================
