## carpenter_2016: zooplankton data
library(tidyverse)

#######################
# download data

infile1 <- '../../DATA/for_BioTIMEx/raw_data/carpenter_2016/cascade_zooplankton_v0.5_upload.csv'
if(!dir.exists('../../DATA/for_BioTIMEx/raw_data/carpenter_2016/') || !file.exists(infile1))   {
  dir.create('../../DATA/for_BioTIMEx/raw_data/carpenter_2016/', showWarnings = FALSE)
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/355/4/8084d8a30424cbf3feb4f69621e6c0a1"
  download.file(inUrl1, infile1, method="curl")
}


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "lakeid",
                 "lakename",
                 "year4",
                 "daynum",
                 "sampledate",
                 "standard_taxon_code",
                 "group_code",
                 "taxon_name",
                 "number_per_net",
                 "mean_length",
                 "mean_individ_biomass",
                 "net_efficiency_measured",
                 "net_efficiency_mean",
                 "abundance_raw",
                 "abundance",
                 "biomass"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$lakename)!="factor") dt1$lakename<- as.factor(dt1$lakename)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1sampledate)
if (class(dt1$standard_taxon_code)!="factor") dt1$standard_taxon_code<- as.factor(dt1$standard_taxon_code)
if (class(dt1$group_code)!="factor") dt1$group_code<- as.factor(dt1$group_code)
if (class(dt1$taxon_name)!="factor") dt1$taxon_name<- as.factor(dt1$taxon_name)
if (class(dt1$number_per_net)=="factor") dt1$number_per_net <-as.numeric(levels(dt1$number_per_net))[as.integer(dt1$number_per_net) ]
if (class(dt1$number_per_net)=="character") dt1$number_per_net <-as.numeric(dt1$number_per_net)
if (class(dt1$mean_length)=="factor") dt1$mean_length <-as.numeric(levels(dt1$mean_length))[as.integer(dt1$mean_length) ]
if (class(dt1$mean_length)=="character") dt1$mean_length <-as.numeric(dt1$mean_length)
if (class(dt1$mean_individ_biomass)=="factor") dt1$mean_individ_biomass <-as.numeric(levels(dt1$mean_individ_biomass))[as.integer(dt1$mean_individ_biomass) ]
if (class(dt1$mean_individ_biomass)=="character") dt1$mean_individ_biomass <-as.numeric(dt1$mean_individ_biomass)
if (class(dt1$net_efficiency_measured)=="factor") dt1$net_efficiency_measured <-as.numeric(levels(dt1$net_efficiency_measured))[as.integer(dt1$net_efficiency_measured) ]
if (class(dt1$net_efficiency_measured)=="character") dt1$net_efficiency_measured <-as.numeric(dt1$net_efficiency_measured)
if (class(dt1$net_efficiency_mean)=="factor") dt1$net_efficiency_mean <-as.numeric(levels(dt1$net_efficiency_mean))[as.integer(dt1$net_efficiency_mean) ]
if (class(dt1$net_efficiency_mean)=="character") dt1$net_efficiency_mean <-as.numeric(dt1$net_efficiency_mean)
if (class(dt1$abundance_raw)=="factor") dt1$abundance_raw <-as.numeric(levels(dt1$abundance_raw))[as.integer(dt1$abundance_raw) ]
if (class(dt1$abundance_raw)=="character") dt1$abundance_raw <-as.numeric(dt1$abundance_raw)
if (class(dt1$abundance)=="factor") dt1$abundance <-as.numeric(levels(dt1$abundance))[as.integer(dt1$abundance) ]
if (class(dt1$abundance)=="character") dt1$abundance <-as.numeric(dt1$abundance)
if (class(dt1$biomass)=="factor") dt1$biomass <-as.numeric(levels(dt1$biomass))[as.integer(dt1$biomass) ]
if (class(dt1$biomass)=="character") dt1$biomass <-as.numeric(dt1$biomass)

# Convert Missing Values to NA for non-dates

dt1$number_per_net <- ifelse((trimws(as.character(dt1$number_per_net))==trimws("NA")),NA,dt1$number_per_net)
dt1$mean_length <- ifelse((trimws(as.character(dt1$mean_length))==trimws("NA")),NA,dt1$mean_length)
dt1$mean_individ_biomass <- ifelse((trimws(as.character(dt1$mean_individ_biomass))==trimws("NA")),NA,dt1$mean_individ_biomass)
dt1$net_efficiency_measured <- ifelse((trimws(as.character(dt1$net_efficiency_measured))==trimws("NA")),NA,dt1$net_efficiency_measured)
dt1$net_efficiency_mean <- ifelse((trimws(as.character(dt1$net_efficiency_mean))==trimws("NA")),NA,dt1$net_efficiency_mean)
dt1$abundance_raw <- ifelse((trimws(as.character(dt1$abundance_raw))==trimws("NA")),NA,dt1$abundance_raw)
dt1$abundance <- ifelse((trimws(as.character(dt1$abundance))==trimws("NA")),NA,dt1$abundance)
dt1$biomass <- ifelse((trimws(as.character(dt1$biomass))==trimws("NA")),NA,dt1$biomass)


ddata <- dt1
save(ddata, file = '../../DATA/for_BioTIMEx/raw_data/carpenter_2016/ddata')

if( FALSE ) {
  # Here is the structure of the input data frame:
  str(dt1)
  attach(dt1)
  # The analyses below are basic descriptions of the variables. After testing, they should be replaced.
  
  summary(lakeid)
  summary(lakename)
  summary(year4)
  summary(daynum)
  summary(sampledate)
  summary(standard_taxon_code)
  summary(group_code)
  summary(taxon_name)
  summary(number_per_net)
  summary(mean_length)
  summary(mean_individ_biomass)
  summary(net_efficiency_measured)
  summary(net_efficiency_mean)
  summary(abundance_raw)
  summary(abundance)
  summary(biomass)
  detach(dt1)
  
}


#######################
# wrangling data

dataset_id <- 'carpenter_2016'
load(file='../../DATA/for_BioTIMEx/raw_data/carpenter_2016/ddata')

ddata<-ddata%>%rename(c(year=year4, site=lakename, species=taxon_name, value=abundance, date=sampledate))

# Include only Paul Lake: control site, other cortrol lake is Crampton lake but only with 3 sampling year - so not considered
ddata <- ddata%>%filter(site=="Paul Lake")%>%
               select(c(year,daynum,date,standard_taxon_code,group_code,species,value))

# Exclusion of samples outside of the first of May to 31st of August period
#ddata[, daynumber := format(date, '%j')]
ddata <- ddata%>%filter(daynum>121 & daynum<243)%>%
            filter(!is.na(value) & value > 0)

#Effort is the number of surveys per year
effort<-ddata%>%group_by(year)%>%
               summarise(n_distinct(date))%>% # sampling happens/ species occurence for each year
               ungroup()

# the sampling effort is more or less same throughout the years except for 1999
# so, we are going to drop that year
ddata<-ddata%>%filter(year!=1999)

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

# now for zooplankton data - we want to get the occurence data for all the species 
# and we then subset a set of common target species (Blake will help in this)
grouped_splist<-ddata%>%group_by(species)%>%
                        summarise(total_abund=sum(value))%>%
                        ungroup()
grouped_splist$sampled_yr<-length(unique(ddata$year))
grouped_splist$sampled_date<-length(unique(ddata$date))
presentyr<-ddata%>%group_by(species)%>%
                           summarise(present_yr=n_distinct(year),present_date=n_distinct(date))%>%ungroup()
grouped_splist<-inner_join(grouped_splist,presentyr,by="species")%>%mutate(included=0)

write.csv(grouped_splist,paste(resloc,"grouped_splist_",dataset_id,".csv",sep=""),row.names = F)

# after Blake will indicate which species he wants, I will make the input matrix for tail analysis as
# a matrix with species abundance time series along column, rownames will be for years






#fwrite(ddata, paste0('../../DATA/for_BioTIMEx/wrangled_data/', dataset_id, "/", dataset_id, '.csv'),
#          row.names=FALSE)
