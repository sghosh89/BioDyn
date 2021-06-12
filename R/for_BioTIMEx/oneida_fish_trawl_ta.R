# This script will do the copula analysis, will generate necessary plots

source("tail_analysis.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)

resloc<-'../../Results/for_BioTIMEx/'
if(!dir.exists(resloc)){
  dir.create(resloc)
}

dataset_id<-"oneida_fish_trawl"
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

