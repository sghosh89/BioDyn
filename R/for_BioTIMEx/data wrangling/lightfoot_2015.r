# lightfoot_2015: Long-Term Core Site Grasshopper Dynamics for the Sevilleta National Wildlife Refuge, New Mexico (1992-2013)
# source: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sev&identifier=106&revision=214968
library(tidyverse)

dataset_id <- 'lightfoot_2015'

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

x<-read.delim2(file='../../DATA/for_BioTIMEx/raw_data/lightfoot_2015/sev106_hopperdynamics_20150826.txt',sep=",")
x<-x%>%mutate(SAMPLING_TIME=str_sub(PER,-1),
              YEAR=str_sub(PER,1,4))
x<-x%>%select(YEAR,SAMPLING_TIME,DATE,SITE,TRANSECT=TRN,SPECIES,COUNT=CNT,BURNED)

# function to get data
get_data_EarlyLate<-function(x,dataset_id='lightfoot_2015',resloc,samplingtime){
 
   xE<-x%>%filter(SAMPLING_TIME==samplingtime)%>%
    filter(BURNED!="B") #exclude burned site
  
  # count number of years sampled per site and consider sites which only have all 21 years sampled
  c1<-xE%>%group_by(SITE)%>%summarise(nyr=n_distinct(YEAR))%>%ungroup()
  
  xE<-xE%>%filter(SITE%in%c("BOER","LATR"))
  
  # equal sampling effort: once annually 6 transects sampled for each year for each sites 
  c<-xE%>%group_by(SITE,YEAR)%>%summarise(nsd=n_distinct(TRANSECT))%>%ungroup() 
  
  # finite counts only
  xE<-xE%>%filter(!is.na(COUNT) & COUNT>0)
  
  ddata<-xE%>%group_by(SITE,YEAR,SPECIES)%>%summarise(COUNT=sum(COUNT))%>%ungroup()
  
  # for site BOER
  ddata_BOER<-ddata%>%filter(SITE=="BOER")%>%
    select(YEAR,SPECIES,COUNT)%>%
    spread(SPECIES,COUNT)
  ddata_BOER<-as.data.frame(ddata_BOER)
  rownames(ddata_BOER)<-ddata_BOER$YEAR
  ddata_BOER<-ddata_BOER%>%select(-YEAR)
  ddata_BOER[is.na(ddata_BOER)]<-0
  
  selsp<-apply(ddata_BOER,MARGIN=2,FUN=function(x){sum(x!=0)})
  presentyr<-0.7*nrow(ddata_BOER)
  commonsp<-which(selsp>=presentyr) 
  raresp<-which(selsp<presentyr)
  raresp<-ddata_BOER[,raresp]
  commonsp<-ddata_BOER[,commonsp]
  commonsp$raresp<-apply(raresp,MARGIN=1,FUN=sum)
  
  write.csv2(commonsp,paste(resloc,dataset_id,"_sampling_time_",samplingtime,"_site_BOER_inputmatrix_tailanal.csv",sep=""),row.names = T)
  saveRDS(commonsp,paste(resloc,dataset_id,"_sampling_time_",samplingtime,"_site_BOER_inputmatrix_tailanal.RDS",sep=""))
  
  # for site LATR
  ddata_LATR<-ddata%>%filter(SITE=="LATR")%>%
    select(YEAR,SPECIES,COUNT)%>%
    spread(SPECIES,COUNT)
  ddata_LATR<-as.data.frame(ddata_LATR)
  rownames(ddata_LATR)<-ddata_LATR$YEAR
  ddata_LATR<-ddata_LATR%>%select(-YEAR)
  ddata_LATR[is.na(ddata_LATR)]<-0
  
  selsp<-apply(ddata_LATR,MARGIN=2,FUN=function(x){sum(x!=0)})
  presentyr<-0.7*nrow(ddata_LATR)
  commonsp<-which(selsp>=presentyr) 
  raresp<-which(selsp<presentyr)
  raresp<-ddata_LATR[,raresp]
  commonsp<-ddata_LATR[,commonsp]
  commonsp$raresp<-apply(raresp,MARGIN=1,FUN=sum)
  
  write.csv2(commonsp,paste(resloc,dataset_id,"_sampling_time_",samplingtime,"_site_LATR_inputmatrix_tailanal.csv",sep=""),row.names = T)
  saveRDS(commonsp,paste(resloc,dataset_id,"_sampling_time_",samplingtime,"_site_LATR_inputmatrix_tailanal.RDS",sep=""))
}

# call the above function
# first for data sampled on early (summer)
#get_data_EarlyLate(x = x, resloc = resloc, samplingtime = "E")
get_data_EarlyLate(x = x, resloc = resloc, samplingtime = "L") # only consider late sampling time

sitelist<-c("BOER","LATR")
saveRDS(sitelist,paste(resloc,"sitelist.RDS",sep=""))