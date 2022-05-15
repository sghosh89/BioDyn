library(tidyverse)
`%notin%` <- Negate(`%in%`)
#######################
## landis_2018

# Package ID: knb-lter-kbs.23.26 Cataloging System:https://pasta.edirepository.org.
# Data set title: Insect Population Dynamics on the Main Cropping System Experiment at the Kellogg Biological Station, Hickory Corners, MI  (1989 to 2017).
# Data set creator:  Douglas Landis - Michigan State University
# Contact:    - Data Manager Kellogg Biological Station  - lter.data.manager@kbs.msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu


infile1 <- '../../DATA/for_BioTIMEx/raw_data/landis_2018/Insect+Populations+via+Sticky+Traps'
if(!dir.exists('../../DATA/for_BioTIMEx/raw_data/landis_2018/') || !file.exists(infile1))   {
   dir.create('../../DATA/for_BioTIMEx/raw_data/landis_2018/', showWarnings = FALSE)
   inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-kbs/23/26/8d33fa9169147f266d20bdcd09a07820"
   download.file(inUrl1,infile1,method="curl")
}

dt1 <-read.csv(infile1,header=F
               ,skip=29
               ,sep=","
               ,quot='"'
               , col.names=c(
                  "Sample_Date",
                  "Treatment",
                  "Replicate",
                  "Station",
                  "Species",
                  "Family",
                  "Order",
                  "Adults",
                  "utm_easting",
                  "utm_northing",
                  "Year"    ), check.names=TRUE)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$Sample_Date dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1Sample_Date<-as.Date(dt1$Sample_Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Sample_Date) == length(tmp1Sample_Date[!is.na(tmp1Sample_Date)])){dt1$Sample_Date <- tmp1Sample_Date } else {print("Date conversion failed for dt1$Sample_Date. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1Sample_Date)
if (class(dt1$Treatment)!="factor") dt1$Treatment<- as.factor(dt1$Treatment)
if (class(dt1$Replicate)!="factor") dt1$Replicate<- as.factor(dt1$Replicate)
if (class(dt1$Station)!="factor") dt1$Station<- as.factor(dt1$Station)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$Family)!="factor") dt1$Family<- as.factor(dt1$Family)
if (class(dt1$Order)!="factor") dt1$Order<- as.factor(dt1$Order)
if (class(dt1$Adults)=="factor") dt1$Adults <-as.numeric(levels(dt1$Adults))[as.integer(dt1$Adults) ]
if (class(dt1$Adults)=="character") dt1$Adults <-as.numeric(dt1$Adults)
if (class(dt1$utm_easting)=="factor") dt1$utm_easting <-as.numeric(levels(dt1$utm_easting))[as.integer(dt1$utm_easting) ]
if (class(dt1$utm_easting)=="character") dt1$utm_easting <-as.numeric(dt1$utm_easting)
if (class(dt1$utm_northing)=="factor") dt1$utm_northing <-as.numeric(levels(dt1$utm_northing))[as.integer(dt1$utm_northing) ]
if (class(dt1$utm_northing)=="character") dt1$utm_northing <-as.numeric(dt1$utm_northing)

# Convert Missing Values to NA for non-dates

ddata <- dt1
save(ddata, file = '../../DATA/for_BioTIMEx/raw_data/landis_2018/ddata')



if(FALSE) {
   # Here is the structure of the input data frame:
   str(dt1)
   attach(dt1)
   # The analyses below are basic descriptions of the variables. After testing, they should be replaced.
   
   summary(Sample_Date)
   summary(Treatment)
   summary(Replicate)
   summary(Station)
   summary(Species)
   summary(Family)
   summary(Order)
   summary(Adults)
   summary(utm_easting)
   summary(utm_northing)
   summary(Year)
   # Get more details on character variables
   
   summary(as.factor(dt1$Treatment))
   summary(as.factor(dt1$Replicate))
   summary(as.factor(dt1$Station))
   summary(as.factor(dt1$Species))
   summary(as.factor(dt1$Family))
   summary(as.factor(dt1$Order))
   detach(dt1)
   
}

##########################################################################################################
# wrangling data

## landis_2018
library(data.table)


dataset_id <- 'landis_2018'
load(file = '../../DATA/for_BioTIMEx/raw_data/landis_2018/ddata')
setDT(ddata)

setnames(ddata, old = c('Treatment', 'Replicate', 'Station', 'Species','Adults', 'Year','Sample_Date'),
         new = c('site', 'block', 'plot', 'species','value', 'year','date'))

ddata <- ddata[value > 0]

# Selecting surveys between beginning of June and end of August
ddata[, daynum := format(date, '%j')]
ddata <- ddata[daynum > 152 & daynum < 243]
# A = ddata[daynum > 121 & daynum < 243,length(unique(date)), by = .(site, year)]
# B = ddata[daynum > 152 & daynum < 243,length(unique(date)), by = .(site, year)]
# C = ddata[daynum > 152 & daynum < 243,length(unique(date)), by = .(site, year)]

#Effort is the number of surveys per year
effort<-ddata%>%group_by(site,year)%>%
   summarise(n_distinct(date))%>% # sampling happens/ species occurence for each year
   ungroup()

effort_by_site<-split(effort,f=effort$site)

effort_CF<-effort_by_site$CF # 21 years
effort_DF<-effort_by_site$DF # 20 years
effort_SF<-effort_by_site$SF # 21 years
effort_poplar<-effort_by_site$T5 # 29 years

# I am going to consider only poplar site as it is long enough to have 
# min 20 years of consistent sampling effort

# Community
#ddata[, ':='(
#   dataset_id = dataset_id,
#   treatment = fifelse(site == 'CF', 'coniferous forest',
#               fifelse(site == 'DF', 'decideous forest',
#               fifelse(site == 'SF', 'mid-successional',
#               fifelse(site == 'T1', 'conventional',
#               fifelse(site == 'T2', 'no-till',
#               fifelse(site == 'T3', 'reduced input',
#               fifelse(site == 'T4', 'organic',
#               fifelse(site == 'T5', 'poplar',
#               fifelse(site == 'T6', 'alfalfa',
#                     'early-successional'
#   ))))))))),
#   design = 'AI',
#   timepoints = paste0('T',seq_along(unique(year))[match(year, sort(unique(year)))]),
#   time_since_disturbance = year - 1989,
#   realm = 'terrestrial',
#   taxon = 'invertebrates',

#   comment = 'Hierarchical experimental design. Treatment is one of 10 culture treatments. Effort: 3 or 6 replicates (block) per treatment (site), each being sampled at 5 stations (plot) several time a year. Blocks and plots were pooled together to increase min N but it is still low. However, the number of surveys per year varies a lot so several biodiversity metrics each year are averaged. Effort gives the number of surveys per year.'
#)
#]

badyr<-effort_poplar$year[which(effort_poplar$`n_distinct(date)`<10)]
ddata_poplar<-ddata%>%filter(site=="T5")%>%filter(year%notin%badyr)
ddata_poplar<-ddata_poplar%>%group_by(species,year)%>%
                              summarise(value=mean(value))%>%
                              ungroup()%>%
                              spread(species, value, fill=0)%>%as.data.frame()

rownames(ddata_poplar)<-ddata_poplar$year
ddata_poplar<-ddata_poplar[,-1]

countnon_0<-apply(ddata_poplar,MARGIN=2,FUN=function(y){sum(y>0)})
# common sp present atleast 70% of sampling years, rest of the species are considered rare and aggregated into single one
presentyr<-0.7*nrow(ddata_poplar) 
commonsp<-which(countnon_0>=presentyr)
commonsp<-ddata_poplar[,commonsp]
rare_sp<-which(countnon_0<presentyr)
if(length(rare_sp)!=0){
   raresp<-ddata_poplar[,rare_sp]
   raresp<-apply(raresp,MARGIN=1,FUN=sum)
   commonsp$raresp<-raresp
}

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

write.csv2(commonsp,paste(resloc,dataset_id,"_poplarT5_inputmatrix_tailanal.csv",sep=""),row.names = T)
saveRDS(commonsp,paste(resloc,dataset_id,"_poplarT5_inputmatrix_tailanal.RDS",sep=""))

sitelist<-c("poplarT5")
saveRDS(sitelist,paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/sitelist.RDS',sep=''))