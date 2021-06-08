# fish net data in cumbrian lake given by Alban (J. Chase group)
library(tidyverse)

`%notin%` <- Negate(`%in%`)

dataset_id <- 'oneida_fish_trawl'

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

#--------------------------------------------------
# read data
allrawdata<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/allrawdata.csv")
# abundance type = mean count
abun<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/abundance.csv")
# data source
contact<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/contacts.csv")
# curation = summary
curation<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/curation.csv")
# metadatasets
datasets<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/datasets.csv")
# methods summary: "zooplankton sampled at the deepest point of the lake, 
# weekly or bi-weekly in 3 lakes (Windermere sampled in two basins)."
methods<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/methods.csv")
# sample 
samp<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/sample.csv")
# sites 
sites<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/oneida_fish_trawl/sites.csv")

sort(unique(allrawdata$YEAR)) # 62 years
unique(allrawdata$ID_SPECIES) 
unique(allrawdata$DEPTH)
allrawdata$latlon<-paste(allrawdata$LATITUDE,"_",allrawdata$LONGITUDE,sep="")
unique(allrawdata$latlon)
allrawdata$SITE<-tolower(allrawdata$SITE)
allrawdata$SAMPLE_DESC<-tolower(allrawdata$SAMPLE_DESC)
site<-sort(unique(allrawdata$SITE))
# 21 sites

xx<-allrawdata%>%filter(SITE%in%c(""))
sort(unique(xx$SAMPLE_DESC))
xx$SAMPLE_DESC<-substr(xx$SAMPLE_DESC,1,nchar(xx$SAMPLE_DESC)-9)

id<-which(xx$SAMPLE_DESC%in%c("bernhards_bay","bernhards_bay_","bernhards_bay_1"))
xx$SAMPLE_DESC[id]<-"bernhards_bay"
id<-which(xx$SAMPLE_DESC%in%c("billington_bay","billington_bay_","billington_bay_2"))
xx$SAMPLE_DESC[id]<-"billington_bay"
id<-which(xx$SAMPLE_DESC%in%c("buoy_117","buoy_117_","buoy_117_2"))
xx$SAMPLE_DESC[id]<-"buoy_117"
id<-which(xx$SAMPLE_DESC%in%c("buoy_125","buoy_125_","buoy_125_2"))
xx$SAMPLE_DESC[id]<-"buoy_125"
id<-which(xx$SAMPLE_DESC%in%c("buoy_125_north","buoy_125_north_","buoy_125_north_2"))
xx$SAMPLE_DESC[id]<-"buoy_125_north"
id<-which(xx$SAMPLE_DESC%in%c("buoy_133","buoy_133_","buoy_133_2"))
xx$SAMPLE_DESC[id]<-"buoy_133"
id<-which(xx$SAMPLE_DESC%in%c("delmarter_bay","delmarter_bay_","delmarter_bay_2" ))
xx$SAMPLE_DESC[id]<-"delmarter_bay"
id<-which(xx$SAMPLE_DESC%in%c("shackelton_point_deep","shackelton_point_deep_","shackelton_point_deep_2"))
xx$SAMPLE_DESC[id]<-"shackelton_point_deep"
id<-which(xx$SAMPLE_DESC%in%c("shackelton_point_shallow","shackelton_point_shallow_","shackelton_point_shallow_1","shackelton_point_shallow_2"))
xx$SAMPLE_DESC[id]<-"shackelton_point_shallow"
id<-which(xx$SAMPLE_DESC%in%c("three_mile_bay","three_mile_bay_","three_mile_bay_2"))
xx$SAMPLE_DESC[id]<-"three_mile_bay"
xx$SITE<-xx$SAMPLE_DESC

allrawdata<-allrawdata%>%filter(SITE%notin%c(""))
sort(unique(allrawdata$SITE))
sort(unique(xx$SAMPLE_DESC))

allrawdata<-rbind(allrawdata,xx)

tempo<-allrawdata%>%group_by(SITE)%>%summarise(ny=n_distinct(YEAR))%>%ungroup()
goodsite<-tempo$SITE[which(tempo$ny>=20)]
# 10 good sites

allrawdata<-allrawdata%>%filter(SITE%in%goodsite)

for(i in 1:length(goodsite)){
  x<-allrawdata%>%filter(SITE==goodsite[i]) # May to August
  print(unique(x$DEPTH)) # each site sampled from unique depth
  tt<-x%>%group_by(YEAR)%>%summarize(nm=n_distinct(MONTH))%>%ungroup()# unique depth
  # equally sampled: once in a year
  
  #print(length(unique(x$YEAR)))
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
  
  saveRDS(commonsp,paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",dataset_id,"_",goodsite[i],"_inputmatrix_tailanal.RDS",sep=""))
  cat("i = ", i, "\n")
}
saveRDS(goodsite,paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/sitelist.RDS",sep=""))







