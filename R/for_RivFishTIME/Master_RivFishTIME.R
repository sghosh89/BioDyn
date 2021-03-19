path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("wrangling_data.R") # clean and prepare data
source("RivFishTIME.R") # tail analysis on each site
source("get_summary_plot_regional.R") # summary plot of tail analysis result based on different region: US/ Europe
source("rank_category.R") # compute and save categorized interaction freq between dominant groups for each site
