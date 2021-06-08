# Phytoplankton data in cumbrian lake given by Alban (J. Chase group)
library(tidyverse)

`%notin%` <- Negate(`%in%`)

dataset_id <- 'cumbrian_phyto'

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

#--------------------------------------------------
# read data
allrawdata<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/allrawdata.csv")
# abundance type = mean count
abun<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/abundance.csv")
# data source
contact<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/contacts.csv")
# curation = summary
curation<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/curation.csv")
# metadatasets
datasets<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/datasets.csv")
# methods summary: "phytoplankton sampled at the deepest point of the lake, 
# weekly or bi-weekly in 3 lakes (Windermere sampled in two basins)."
methods<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/methods.csv")
# sample 
samp<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/sample.csv")
# sites 
sites<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/sites.csv")

unique(allrawdata$YEAR) # 28 years
unique(allrawdata$ID_SPECIES)
unique(allrawdata$DEPTH)
allrawdata$latlon<-paste(allrawdata$LATITUDE,"_",allrawdata$LONGITUDE,sep="")
unique(allrawdata$latlon)
site<-sort(unique(allrawdata$SITE))
# Blelham Tarn = BLEL, 
# Esthwaite Water = ESTH, 
# two sampling points from windermere lake (based on latlon)
# SBAS = southbasin
# NBAS = northbasin
# whatever it would be 3 lakes in 2 basins as per summary methods: we have to figure out!

for(i in 1:length(site)){
  x<-allrawdata%>%filter(MONTH%in%c(5:8) & SITE==site[i]) # May to August
  print(unique(x$DEPTH))
  tt<-x%>%group_by(YEAR)%>%summarize(nm=n_distinct(MONTH))%>%ungroup()# unique depth
  # for i =1, 2001 should be excluded as only month 8 sampled out of 5 to 8 months 
  badyr<-tt$YEAR[which(tt$nm<=2)] # bad year means when less than or equal to 50% sampling months
  if(length(badyr)>0){
    x<-x%>%filter(YEAR%notin%badyr)
  }
  
  spmat<-x%>%group_by(ID_SPECIES,YEAR)%>%summarize(ABUNDANCE=mean(ABUNDANCE))%>%ungroup()
  spmat<-spmat%>%spread(ID_SPECIES,ABUNDANCE,fill=0)%>%as.data.frame()
  rownames(spmat)<-spmat$YEAR
  spmat<-spmat[,-1]
  
  countnon_0<-apply(spmat,MARGIN=2,FUN=function(y){sum(y>0)})
  # common sp present atleast 70% of sampling years, rest of the species are considered rare and aggregated into single one
  presentyr<-0.7*nrow(spmat) 
  commonsp<-which(countnon_0>=presentyr)
  commonsp<-spmat[,commonsp]
  rare_sp<-which(countnon_0<presentyr)
  if(length(rare_sp)!=0){
    raresp<-spmat[,rare_sp]
    raresp<-apply(raresp,MARGIN=1,FUN=sum)
    commonsp$raresp<-raresp
  }
  
  saveRDS(commonsp,paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",dataset_id,"_",site[i],"_inputmatrix_tailanal.RDS",sep=""))
  
}
saveRDS(site,paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/sitelist.RDS",sep=""))








