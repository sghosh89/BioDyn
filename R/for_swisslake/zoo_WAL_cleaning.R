library(tidyverse)
`%notin%` <- Negate(`%in%`)
#-----------------------------------------------------------------------------
resloc_z<-"../../DATA/for_swisslake/wrangled_data/zooplankton/"
if(!dir.exists(resloc_z)){
  dir.create(resloc_z)
}
#----------------------------------------------------------------------------
# read zooplankton data

path<-"../../DATA/for_swisslake/raw_data/SwissLakes_data_FP11dec2020/raw_data/zooplankton/"
xm<-read.csv(paste(path,"zooplankton_MD.csv",sep=""),sep=";")
xm<-xm%>%select(id_CH,empire,kingdom,phylum,
                class,order,family,genus,species,stage,
                affiliation,synonyms,comments,size_category)

# lake walen
l1z_wal<-read.csv(paste(path,"zoo_WAL_eawag.csv",sep=""),sep=";")

# clean data: 

#------------------ lake walen: 16 years of data (1972-1987) ------------------
unique(l1z_wal$site) # 1 site only

l1z_wal<-l1z_wal%>%separate(date, c("year","month","day"), "-")

# consider the months: June to Sept
l1z_wal<-l1z_wal%>%filter(month%in%c("06","07","08","09"))

# is every year uniformly sampled? yes (11 for 1987 only)
c<-l1z_wal%>%group_by(year)%>%summarize(nm=n_distinct(month))%>%ungroup()

# is every month uniformly sampled? yes (1-3 times)
c1<-l1z_wal%>%group_by(year,month)%>%summarize(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# lake walen max depth 151m @wikipaedia

# I am assuming the lake walen has depth then is reported in cm
# I have no info about unit of the reported value
# I am assuming values are density = number of individual/m^2 as we want a common unit
# for lake walen I am doing nothing

l1z_wal_full<-inner_join(l1z_wal,xm,by=c("taxon"="id_CH"))
(sp<-unique(l1z_wal$taxon))
length(sp)
sp_m<-xm%>%filter(id_CH%in%sp) # these sp sampled in lake walen
nrow(sp_m)

sp_m$sampled_yr<-length(unique(l1z_wal_full$year)) # 16 year sampled: 1972-1987

blake_z_wal<-l1z_wal_full%>%group_by(taxon)%>%summarize(present_yr=n_distinct(year))%>%ungroup()
sp_m<-inner_join(sp_m,blake_z_wal,by=c("id_CH"="taxon"))
write.csv(sp_m,paste(resloc_z,"splist_z_l1wal.csv",sep=""),row.names = F)

# we will exclude lake 1 as it has only 16 years of data
