# this notes ask questions and try to make some decision for each data file based on sampling issue

# =================================== carpenter_2016 ============================================================
rm(list=ls())
library(tidyverse)
x<-read.csv("../../DATA/for_BioTIMEx/raw_data/carpenter_2016/cascade_zooplankton_v0.5_upload.csv")
x<-x%>%filter(lakename%in%c("Paul Lake")) # Paul lake was the only long-term control site
c<-x%>%group_by(year4)%>%summarise(n_distinct(sampledate)) # sampling happens/ species occurence for each year

# Q: what should I do?
# year 1999: sampled biweekly, rest years sampled more/less on weekly basis, just drop 1999
#x1<-x%>%filter(year4==1999)
#x2<-x%>%filter(year4==2001)
# =================================== carpenter_2017 ============================================================
rm(list=ls())
library(tidyverse)
x<-read.csv("../../DATA/for_BioTIMEx/raw_data/carpenter_2017/cascade_phytoplankton_v0.1.csv_upload.csv")
x<-x%>%filter(lakename%in%c("Paul Lake")) # Paul lake was the only long-term control site
c<-x%>%group_by(year4)%>%summarise(n_distinct(sampledate)) # sampling happens/ species occurence for each year
# This data is too short for our purpose as only 15 years long: 1984-1995 sampled regularly 
# but then 2013-2015 sampled once in a year
# =================================== landis_2018 ============================================================
rm(list=ls())
library(tidyverse)
load("../../DATA/for_BioTIMEx/raw_data/landis_2018/ddata")
# there are 7 treatments: should I consider all or only T2: no-till
#x<-ddata%>%
# =================================== joern_2019 ============================================================
rm(list=ls())
library(tidyverse)
load("../../DATA/for_BioTIMEx/raw_data/joern_2019/ddata")
# control 000b but this control site has only 10 years of data: 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991
###################################################################

# collins 2018 data: collection of many (30) datasets
# some datasets are included in BioTIME

# ---------------------- Datasets ------------------------------------ Included in BioTIME(if yes, STUDY_ID) ------------------

#1. Benthic invertebrates in Oneida Lake, New York, 1956 to present.----------No-----
#2. Cascade Project at North Temperate Lakes LTER: Phytoplankton 1984 - 1995----------No-----
#3. CGR05 Effects of fire frequency on composition of grasshopper assemblages on Konza Prairie LTER Watersheds(1983)----------STUDY_ID: 301?---------
#4. Core Research Site Web Seasonal Biomass and Seasonal and Annual NPP Data for the Net Primary Production Study at the Sevilleta National Wildlife Refuge, New Mexico (1999-present)----------No-----
#5. Ecuador old fields permanent plot vegetation sampling----------No-----
#6. El Verde Grid long-term invertebrate data----------STUDY_ID: 54---------
#7. Insect Population Dynamics on the Main Cropping System Experiment at the Kellogg Biological Station, Hickory Corners, MI (1989 to 2013)----------STUDY_ID: 300---------
#8. Long-Term Core Site Grasshopper Dynamics for the Sevilleta National Wildlife Refuge, New Mexico (1992-2013)----------No-----
#9. Marsh plant species percent cover for Rowley River tidal creeks associated with long term fertilization experiments, Rowley and Ipswich, MA.----------No-----
#10. N and P fertilization experiment plant species composition data for South of saddle from 1990 - 2000, yearly except 1997 and 1999.----------No-----
#11. Nitrogen Fertilization Experiment (NFert): Net Primary Production Quadrat Data at the Sevilleta National Wildlife Refuge, New Mexico (2004-present)----------No-----
#12. North Temperate Lakes LTER: Fish Abundance 1981 - current----------STUDY_ID: 57---------
#13. North Temperate Lakes LTER: Zooplankton - Trout Lake Area 1982 - current----------STUDY_ID: 253---------
#14. NPP Study: Quadrat field measurement data----------No-----
#15. Phytoplankton in Oneida Lake, New York, 1975 -- present.----------No-----
#16. Plant aboveground biomass data: Long-Term Nitrogen Deposition During Grassland Succession----------No-----
#17. Plant aboveground biomass data: Long-Term Nitrogen Deposition: Population, Community, and Ecosystem Consequences----------No-----
#18. Plant Community and Ecosystem Responses to Long-term Fertilization & Disturbance at the Kellogg Biological Station, Hickory Corners, MI (1989 to 2014)----------No-----
#19. Point Quadrat Vegetation Data, Buxton Climate Change Experiment, UK, 1994-2009.----------No-----
#20. Point Quadrat Vegetation Data, Buxton Climate Change Experiment, UK, 1994-2009.----------No-----
#21. Post-logging community structure and biomass accumulation in Watershed 10, Andrews Experimental Forest , 1974 to present. Long-Term Ecological Research. Forest Science Data Bank, Corvallis, OR.----------No-----
#22. Primary succession on Mount St. Helens: long-term permanent plots. Mount St. Helens. Forest Science Data Bank, Corvallis, OR.----------No-----
#23. PVC02 Plant Species Composition on Selected Watersheds at Konza Prairie ----------STUDY_ID: 355---------
#24. Revegetation of landslides, vegetation <0.1m (Small landslide plots at the Luquillo Experimental Forest)----------No-----
#25. SGS-LTER Effects of grazing on ecosystem structure and function (GZTX): Net Primary Production on the Central Plains Experimental Range, Nunn, Colorado, USA 1992-2011, ARS Study Number 32----------No-----
#26. Species interactions during succession in the western Cascade Range of Oregon, 1990 to present. Long-Term Ecological Research. Forest Science Data Bank, Corvallis, OR.----------No-----
#27. Total numbers and species of insects taken from rock scrubbings during the summer of 1984-1988, 1993-1994, 1996-1998, in the Kuparuk River experimental reach near Toolik Field Station, North Slope Alaska..----------No-----
#28. Vegetation Plots of the Bonanza Creek LTER Control Plots: Species Percent Cover (1975 - 2009)----------STUDY_ID: 221---------
#29. WAT01 Konza Prairie Long-Term Irrigation Transect Study----------No-----
#30. Zooplankton survey of Oneida Lake, New York, 1964 - present.----------STUDY_ID: 247---------














