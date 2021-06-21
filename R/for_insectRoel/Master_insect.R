path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("raugh.R") # clean and prepare data
