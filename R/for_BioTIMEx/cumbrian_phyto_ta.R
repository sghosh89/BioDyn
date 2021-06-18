# This script will do the copula analysis, will generate necessary plots, will show how a species 
# tail dep. fluctuates with the rest of species in the phyto-plankton community

source("tail_analysis.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)

resloc<-'../../Results/for_BioTIMEx/'
if(!dir.exists(resloc)){
  dir.create(resloc)
}

dataset_id<-"cumbrian_phyto"
sites<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/sitelist.RDS",sep=""))

if(!dir.exists(paste(resloc,dataset_id,"/",sep=""))){
  dir.create(paste(resloc,dataset_id,"/",sep=""))
}

for(i in 1:length(sites)){
  if(!dir.exists(paste(resloc,dataset_id,"/",sites[i],"/",sep=""))){
    dir.create(paste(resloc,dataset_id,"/",sites[i],"/",sep=""))
  }
}

for(i in 1:length(sites)){
  spmat<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",dataset_id,"_",sites[i],"_inputmatrix_tailanal.RDS",sep=""))
  myresloc<-paste(resloc,dataset_id,"/",sites[i],"/",sep="")
  tail_analysis(mat=spmat,resloc=myresloc,nbin=2)
}

#=================================================================
# make species list 
sp1<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/cumbrian_phyto_BLEL_inputmatrix_tailanal.RDS")
sp1<-colnames(sp1)
sp2<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/cumbrian_phyto_ESTH_inputmatrix_tailanal.RDS")
sp2<-colnames(sp2)
sp3<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/cumbrian_phyto_NBAS_inputmatrix_tailanal.RDS")
sp3<-colnames(sp3)
sp4<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/cumbrian_phyto_SBAS_inputmatrix_tailanal.RDS")
sp4<-colnames(sp4)

splist<-c(sp1,sp2,sp3,sp4)
splist<-sort(unique(splist))
splist<-data.frame(code=splist,sp=NA)
write.csv(splist,"../../DATA/for_BioTIMEx/wrangled_data/cumbrian_phyto/splist.csv",row.names = F)
# now manually fill in the 2nd column and saved as splist_edited.csv








