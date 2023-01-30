path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("wrangling_data.R") # clean and prepare data
source("RivFishTIME.R") # tail analysis on each site
source("RivFishTIME_nbin3.R") #tail analysis for each route, nbin=3, nyr>=41
source("RivFishTIME_nbin4.R") #tail analysis for each route, nbin=4, nyr>=41
#source("get_summary_plot_regional.R") # summary plot of tail analysis result based on different region: US/ Europe
#source("rank_category.R") # compute and save categorized interaction freq between dominant groups for each site
source("summary_for_stabilitymetric.R") # for stability metric info
