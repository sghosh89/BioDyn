path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

#------------ plotlevel analysis ---------------
source("get_BioTIME_data.R") # reading data
source("freshwater_plotlevel.R") 
source("terrestrial_plotlevel.R") 
#source("rank_category_plotlevel.R") 
source("summary_for_stabilitymetric_plotlevel.R") 


# for BioTIME we found 9 communities with >=41 years of sampling data
# they are all from terrestrial birds, shown below
#STUDY_ID              newsite
#1       39                   39
#2       46                   46
#3      339                  339
#4      413 STUDY_ID_413_PLOT_CF
#5      413 STUDY_ID_413_PLOT_FE
#6      413 STUDY_ID_413_PLOT_FI
#7      413  STUDY_ID_413_PLOT_G
#8      413 STUDY_ID_413_PLOT_OF
#9      414                  414
# so I run for both nbin=2, 3, 4 in the corresponding tail analysis file for terrestrial plots

