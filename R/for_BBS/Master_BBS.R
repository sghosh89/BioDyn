path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("data_wrangling.R")
source("BBS_ta.R")