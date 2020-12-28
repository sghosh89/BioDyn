# Phytoplankton data in oneida lake
# source: https://search.dataone.org/view/kgordon.31.64 
library(tidyverse)

dataset_id <- 'oneida_phytopl_1975'

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

# ------------ for 1975-95 ---------------
x75<-read.csv(file='../../DATA/for_BioTIMEx/raw_data/oneida_phytopl_1975/data/cbfs.32.11-PhytoData_75_95.csv')
x75_meta<-read.csv(file = "../../DATA/for_BioTIMEx/raw_data/oneida_phytopl_1975/data/cbfs.34.14-75_95_TaxaID.csv")
x75_meta<-x75_meta%>%rename(Notes_sp=Notes)
x75_all<-inner_join(x75,x75_meta,by=c("X75_95_TaxaID"="TaxonomicCode"))
xstation<-read.csv(file = "../../DATA/for_BioTIMEx/raw_data/oneida_phytopl_1975/data/cbfs.33.10-Stations.csv")
xstation<-xstation%>%rename(Notes_station=Notes)
x75_all<-inner_join(x75_all,xstation,by=c("Station"="Site"))

ddata<-x75_all%>%select(site=Station,
                        year=Year,
                        date=Sample_date,
                        week=SamplingWeek,
                        density=Density,
                        species=ScientificName,
                        standard_sample=StandardSample,
                        standard_site=Standard.Site)
ddata$date<-as.Date(ddata$date,format="%m/%d/%Y")
ddata$daynum<-format(ddata$date, "%j") # convert to Julian date

# arrange a bit and consider only standard sites with finite density value
ddata<-ddata%>%select(site,year,date,daynum,density,
                      species,standard_sample,standard_site)%>%
                      filter(!is.na(density)&density!=0)%>%
                      filter(standard_site=="yes")

# choosing sites with at least 20 years of data
c<-ddata%>%group_by(site)%>%summarize(nyr=n_distinct(year))%>%ungroup()%>%filter(nyr>=20)
ddata<-ddata%>%filter(site%in%c$site)

# Exclusion of samples outside of the first of May to 31st of August period
ddata<-ddata%>%filter(daynum> 121 & daynum < 243)
c1<-ddata%>%group_by(site)%>%summarize(nsd=n_distinct(date))%>%ungroup() # more or less even sampling effort at each site
c2<-ddata%>%group_by(year)%>%summarize(nsd=n_distinct(date))%>%ungroup() # 1978,1982 year less sampled

# NOTE: the 3 sites in c1 are very closely located (within 1 by 1 degree lat-lon cell: see info from xstation)
#       so, we are going to merge all this sites data into single site and will consider them as single site
# technically, excluding 1978, 1982 would be better to maintain even sampling effort but then timeseries
# will be 19 yrs long.

# now for phytoplankton data - we want to get the occurence data for all the species 
# and we then subset a set of common target species (Blake will help in this)
df<-ddata%>%group_by(species)%>%summarise(total_density=sum(density),
                                          sampled_yr=length(unique(ddata$year)),
                                          presentyr=n_distinct(year),
                                          sampled_date=length(unique(ddata$date)),
                                          present_date=n_distinct(date))%>%ungroup()%>%mutate(included=0)

write.csv(ddata,paste(resloc,dataset_id,"_ddata_1975to1995.csv",sep=""),row.names = F)
write.csv(df,paste(resloc,dataset_id,"_grouped_phytoplankton_list_1975to1995.csv",sep=""),row.names = F)


#---------- for 1996-onwards -----------------

x96<-read.csv(file='../../DATA/for_BioTIMEx/raw_data/oneida_phytopl_1975/data/cbfs.134.3-PhytoData_from_1996.csv')
x96_meta<-read.csv(file = "../../DATA/for_BioTIMEx/raw_data/oneida_phytopl_1975/data/cbfs.133.7-TaxaID_From_1996.csv")
x96_all<-inner_join(x96,x96_meta,by=c("TaxaID_From1996"="Taxa_id_PhycoTech"))
xstation<-read.csv(file = "../../DATA/for_BioTIMEx/raw_data/oneida_phytopl_1975/data/cbfs.33.10-Stations.csv")
x96_all<-inner_join(x96_all,xstation,by=c("Station"="Site"))

ddata<-x96_all%>%mutate(year=str_sub(Sample_date,-4,-1))
ddata<-ddata%>%select(site=Station,
                        year,
                        date=Sample_date,
                        density=algal_cell_concentration_cells_per_ml,
                        species=TaxaName,
                        standard_site=Standard.Site)
ddata$date<-as.Date(ddata$date,format="%m/%d/%Y")
ddata$daynum<-format(ddata$date, "%j") # convert to Julian date

# arrange a bit and consider only standard sites with finite density value
ddata<-ddata%>%select(site,year,date,daynum,density,
                      species,standard_site)%>%
  filter(!is.na(density)&density!=0)%>%
  filter(standard_site=="yes")%>%select(-standard_site)

# 1996-2013 data only
nyr<-length(unique(ddata$year))
# now here we know only 16 years of data present from 1996 onwards
# Now, I wanted to clean data and merge with 1975-1995 series, but I don't know 
# whether is it okay to assume the density column from 1975 phyto data same as of 
# algal_cell_concentration_cells_per_ml in 1996 phyto data.
# Is naming of some phytoplankton changed from 1996 onwards? 
# also more frequently sampled per year in 1975-1995 

# if we assume yes, then proceed as follows:

c<-ddata%>%group_by(site)%>%summarize(nyr=n_distinct(year))%>%ungroup()
ddata<-ddata%>%filter(site%in%c$site)

# Exclusion of samples outside of the first of May to 31st of August period
ddata<-ddata%>%filter(daynum> 121 & daynum < 243)
c1<-ddata%>%group_by(site)%>%summarize(nsd=n_distinct(date))%>%ungroup() # uneven sampling: exclude Shackelton Point
c2<-ddata%>%group_by(year)%>%summarize(nsd=n_distinct(date))%>%ungroup() # more/less even sampling effort
ddata<-ddata%>%filter(site%in%"Pooled Stations")

# now for phytoplankton data - we want to get the occurence data for all the species 
# and we then subset a set of common target species (Blake will help in this)
df<-ddata%>%group_by(species)%>%summarise(total_density=sum(density),
                                          sampled_yr=length(unique(ddata$year)),
                                          presentyr=n_distinct(year),
                                          sampled_date=length(unique(ddata$date)),
                                          present_date=n_distinct(date))%>%ungroup()%>%mutate(included=0)

write.csv(ddata,paste(resloc,dataset_id,"_ddata_1996to2013.csv",sep=""),row.names = F)
write.csv(df,paste(resloc,dataset_id,"_grouped_phytoplankton_list_1996to2013.csv",sep=""),row.names = F)

# merge both time periods now
df_75<-read.csv(paste(resloc,dataset_id,"_grouped_phytoplankton_list_1975to1995.csv",sep=""))
df_all<-full_join(df_75,df,by="species")
df_all[is.na(df_all)]<-0

s1<-df_75$species
s2<-df$species
sb<-intersect(s1,s2) # 30 sp. present in both time period

df_all<-df_all%>%mutate(total_density=total_density.x+total_density.y,
                        sampled_yr=sampled_yr.x+sampled_yr.y,
                        present_yr=presentyr.x+presentyr.y,
                        sampled_date=sampled_date.x+sampled_date.y,
                        present_date=present_date.x+present_date.y)%>%mutate(included=0)
write.csv(df_all,paste(resloc,dataset_id,"_grouped_phytoplankton_list_1975to2013.csv",sep=""),row.names = F)


