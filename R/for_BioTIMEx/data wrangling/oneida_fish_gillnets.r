# fish net data in cumbrian lake given by Alban (J. Chase group)
library(tidyverse)

`%notin%` <- Negate(`%in%`)

dataset_id <- 'oneida_fish_gillnets'

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

#--------------------------------------------------
# read data
allrawdata<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/allrawdata.csv")
# abundance type = mean count
abun<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/abundance.csv")
# data source
contact<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/contacts.csv")
# curation = summary
curation<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/curation.csv")
# metadatasets
datasets<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/datasets.csv")
# methods summary: "zooplankton sampled at the deepest point of the lake, 
# weekly or bi-weekly in 3 lakes (Windermere sampled in two basins)."
methods<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/methods.csv")
# sample 
samp<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/sample.csv")
# sites 
sites<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/sites.csv")

sort(unique(allrawdata$YEAR)) # 62 years
unique(allrawdata$ID_SPECIES) # exclude: Esox masquinongy x Esox lucius
allrawdata<-allrawdata%>%filter(ID_SPECIES%notin%c("Esox masquinongy x Esox lucius"))
unique(allrawdata$DEPTH)
allrawdata$latlon<-paste(allrawdata$LATITUDE,"_",allrawdata$LONGITUDE,sep="")
unique(allrawdata$latlon)
site<-sort(unique(allrawdata$SITE))
# 15 sites

tempo<-allrawdata%>%group_by(SITE)%>%summarise(ny=n_distinct(YEAR))%>%ungroup()
# all are good sites

for(i in 1:length(site)){
  x<-allrawdata%>%filter(SITE==site[i]) # May to August
  print(unique(x$DEPTH)) # each site sampled from unique depth
  tt<-x%>%group_by(YEAR)%>%summarise(nm=n_distinct(MONTH))%>%ungroup()# unique depth
  # equally sampled: once in a year
  
  #print(length(unique(x$YEAR)))
  spmat<-x%>%group_by(ID_SPECIES,YEAR)%>%summarise(ABUNDANCE=mean(ABUNDANCE))%>%ungroup()
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
  cat("i = ", i, "\n")
}
saveRDS(site,paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/sitelist.RDS",sep=""))







