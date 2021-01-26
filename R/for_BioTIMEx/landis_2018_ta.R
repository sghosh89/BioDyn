# This script will do the copula analysis, will generate necessary plots, will show how a species 
# tail dep. fluctuates with the rest of species in the phyto-plankton community

source("tail_analysis.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)

resloc<-'../../Results/for_BioTIMEx/'
if(!dir.exists(resloc)){
  dir.create(resloc)
}

dataset_id<-"landis_2018"
spmat<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",
                     dataset_id,"_inputmatrix_tailanal_poplarT5.RDS",sep=""))

myresloc<-paste(resloc,dataset_id,"/poplarT5/",sep="")
if(!dir.exists(myresloc)){
  dir.create(myresloc)
}

tail_analysis(mat=spmat,resloc=myresloc,nbin=2)

