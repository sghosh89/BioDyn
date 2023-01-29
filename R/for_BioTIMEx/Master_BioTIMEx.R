path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
source("2.0_wrangling_raw_data.r") # cleaning data
source("3.0_get_tail_analysis_res.r") # tail analysis
source("summary_table_simple.R") # initial richness and summary table
#source("rank_category.R") # compute and save categorized interaction freq between dominant groups for each site
source("summary_for_stabilitymetric.R") # for stability metric info


# 25 communities have >=41 years sampled at least, so adding nbin=3 calculation for them
#STUDY_ID                           newsite
#1  oneida_fish_gillnets                          Buoy 113
#2  oneida_fish_gillnets                          Buoy 125
#3  oneida_fish_gillnets                          Buoy 129
#4  oneida_fish_gillnets                          Buoy 133
#5  oneida_fish_gillnets                    Bushnell Point
#6  oneida_fish_gillnets                         Cleveland
#7  oneida_fish_gillnets                       Dakin Shoal
#8  oneida_fish_gillnets                       Damon Point
#9  oneida_fish_gillnets                     Dunham Island
#10 oneida_fish_gillnets                   Dutchman Island
#11 oneida_fish_gillnets                            Jewell
#12 oneida_fish_gillnets                       Lewis Point
#13 oneida_fish_gillnets                    Phillips Point
#14 oneida_fish_gillnets                  Shackelton Point
#15 oneida_fish_gillnets                    Willard Island
#16    oneida_fish_trawl           billington bay_standard
#17    oneida_fish_trawl                 buoy 117_standard
#18    oneida_fish_trawl           buoy 125 north_standard
#19    oneida_fish_trawl                 buoy 125_standard
#20    oneida_fish_trawl                 buoy 133_standard
#21    oneida_fish_trawl           bushnell point_standard
#22    oneida_fish_trawl            delmarter bay_standard
#23    oneida_fish_trawl    shackelton point deep_standard
#24    oneida_fish_trawl shackelton point shallow_standard
#25    oneida_fish_trawl           three mile bay_standard


