#----------------------------------------------------------------
# this file will clean zoop2014 clean and save input matrix 
# for tail analysis  
#----------------------------------------------------------------
rm(list=ls())
library(readxl)
library(tidyverse)
`%notin%` <- Negate(`%in%`)
#-------------------------------------------------------------
# read files
xmeta<-read_excel("../../DATA/for_zoop_2014/LakeNameMasterand coords.xlsx")
colnames(xmeta)[5:6]<-c("lat","lon")

listF <- list.files('../../DATA/for_zoop_2014/Zoop Data/', pattern = ".csv", full.names = TRUE)
length(listF) # 28 csv files

spinfo<-data.frame(matrix(NA,nrow=50,ncol=28))
for(i in 1:28){
  x<-read.csv(listF[i])
  colnames(spinfo)[i]<-paste("LakeID_",x$LakeID[1],sep="")
  spname<-colnames(x)
  spname<-spname[-c(1:2)]
  spinfo[,i]<-c(spname,NA*numeric(nrow(spinfo)-length(spname)))
}

write.csv(spinfo,"../../DATA/for_zoop_2014/wrangled_data/spinfo.csv",row.names = F)
#==========================================================================================================
x$JulDate # x is any one of the csv files David sent us
as.Date(x$JulDate, origin=as.Date("1900-01-01"))
#----------------------------------------------------------------
# get unique species entries from the data frame spinfo
usp<-sort(as.character(unique(unlist(spinfo))))
usp
speciespool<-data.frame(sp_pool=usp)
speciespool$species<-NA
key<-read_excel("../../DATA/for_zoop_2014/wrangled_data/key.xlsx")
speciespool<-left_join(speciespool,key,by=c("sp_pool"="Code"))
speciespool<-speciespool%>%select(-species)

taxainfo<-read_excel("../../DATA/for_zoop_2014/ZooplanktonTaxa.xlsx")

# I filled in the species name from taxainfo file Jim sent us
speciespool$Species[speciespool$sp_pool=="Acantl.curvi"]<-"Acantholeberis curvirostris"
speciespool$Species[speciespool$sp_pool=="Bytho.long"]<-"Bythotrephes longimanus"
speciespool$Species[speciespool$sp_pool=="Cal.copepodite"]<-"Calanoid copepodid"
speciespool$Species[speciespool$sp_pool=="Calanoida"]<-"Calanoida"
speciespool$Species[speciespool$sp_pool=="Cyc.copepodite"]<-"Cyclopoid copepodid"
speciespool$Species[speciespool$sp_pool=="Cyclopoida"]<-"Cyclopoida"
speciespool$Species[speciespool$sp_pool=="Daphnia.sp."]<-"Daphnia.sp."
speciespool$Species[speciespool$sp_pool=="Diacyclops.sp"]<-"Diacyclops.sp"
speciespool$Species[speciespool$sp_pool=="Diapt.copep"]<-"Diaptomus copepodid"
speciespool$Species[speciespool$sp_pool=="Episc.lacus"]<-"Epischura lacustris"
speciespool$Species[speciespool$sp_pool=="Eucyclops.sp"]<-"Eucyclops.sp"
speciespool$Species[speciespool$sp_pool=="Harpacticoida"]<-"Harpacticoid.sp"
speciespool$Species[speciespool$sp_pool=="Latona.sp"]<-"Latona.sp"
speciespool$Species[speciespool$sp_pool=="Leydi.acant"]<-"Leydigia acanthocercoides" # from wiki
speciespool$Species[speciespool$sp_pool=="Leydigia.sp"]<-"Leydigia.sp"
speciespool$Species[speciespool$sp_pool=="Ortho.modes"]<-"Orthocyclops Modestus"
speciespool$Species[speciespool$sp_pool=="Orthocyclops.sp"]<-"Orthocyclops.sp"
speciespool$Species[speciespool$sp_pool=="Rotifer"]<-"Rotifer"
speciespool$Species[speciespool$sp_pool=="Senec.calan"]<-"Senecella calanoides"
speciespool$Species[speciespool$sp_pool=="Simocephalus.sp"]<-"Simocephalus.sp"
speciespool$Species[speciespool$sp_pool=="Skistodiaptomus.sp"]<-"Skistodiaptomus.sp"
speciespool$Species[speciespool$sp_pool=="Unkno.clado"]<-"Unknown"

write.csv(speciespool,"../../DATA/for_zoop_2014/wrangled_data/speciespool.csv",row.names = F)

# Blake filled in the blanks in speciespool
speciespool_BM<-read.csv("../../DATA/for_zoop_2014/wrangled_data/speciespool_BM.csv")

# good species for grouping (use the last column for grouping)
speciespool_good<-speciespool_BM%>%filter(is.na(Exclude))
colnames(speciespool_good)[9]<-"group"
table(speciespool_good$group)

#######################################################################
# screen lakes with >= 20 years of data
# get input spmat for each lake (screen for June-Sept month only)
good_listF<-c()
good_LakeID<-c()
for(i in 1:28){
  m<-read.csv(listF[i])
  m$date<-as.Date(m$JulDate, origin=as.Date("1900-01-01"))
  m<-m%>%separate(date, c("year","month","day"), "-")
  m<-m%>%filter(month%in%c("06","07","08","09"))
  nyr<-length(unique(m$year))
  cat("i= ",i,", nyr=",nyr,"\n")
  if(nyr>=20){
    good_LakeID<-c(good_LakeID,m$LakeID[1])
    good_listF<-c(good_listF,listF[i])
    mydr<-paste("../../DATA/for_zoop_2014/wrangled_data/",m$LakeID[1],"/",sep="")
    if(!dir.exists(mydr)){
      dir.create(mydr)
    }
    write.csv(m,paste(mydr,"lakedata_raw.csv",sep=""),row.names = F)
  }
}
saveRDS(good_LakeID,"../../DATA/for_zoop_2014/wrangled_data/good_LakeID.RDS")
# 15 lakes having >=20 years of data

#################################################################################
# clean 15 lake data (consider consistent sampling: each year all 4 months sampled?)
# we need to check each lake carefully

# This function reads raw file and check necessary consistent sampling
get_t1_t2_sampling<-function(i){
  m<-read.csv(paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/lakedata_raw.csv",sep=""))
  m<-m[,-c(1:2)]
  grsp<-colnames(m)
  grsp<-setdiff(grsp,tail(grsp,3))
  id_goodsp<-which(grsp%in%speciespool_good$sp_pool)
  grsp<-grsp[id_goodsp]
  myr<-m[,c("year","month","day")]
  m<-m[,grsp]
  grsp_group<-speciespool_good$group[match(grsp,speciespool_good$sp_pool)]
  colnames(m)<-grsp_group
  
  # aggregate into unique column with same colnames
  m<-as.matrix(m)
  mtrim<-m %*% sapply(unique(grsp_group),"==", grsp_group)
  mtrim<-cbind(myr,mtrim)
  
  t1<-mtrim%>%group_by(year)%>%summarise(num=length(unique(month)))%>%ungroup()
  t2<-mtrim%>%group_by(year,month)%>%summarise(nud=length(unique(day)))%>%ungroup()
  
  return(list(mtrim=mtrim,t1=t1,t2=t2))
}

# function to get input mat for tail analysis
# Input:
# mtrim: matrix (output from previous function)
# resloc: location to save the matrix
get_inputmat_for_tailanal<-function(mtrim,resloc){
  
  mtrim<-mtrim%>%select(-day)
  
  # aggregate: for each year taking mean for each sampling months over multiple sampling days
  mtrim<-aggregate(. ~ year+month, data=mtrim, FUN=mean)
  
  # aggregate: for each year take total abun of each group
  mtrim<-aggregate(. ~ year, data=mtrim, FUN=sum)
  
  # see if any sp-group is rare or not
  rownames(mtrim)<-mtrim$year
  mtrim<-mtrim[,-c(1:2)]
  
  saveRDS(mtrim,paste(resloc,"allsp_timeseries.RDS",sep=""))
  
  presentyr<-apply(mtrim,MARGIN = 2,FUN = function(x){sum(x!=0)})
  commonsp<-which(presentyr>=0.7*nrow(mtrim)) # commonsp present 70% of sampling year 
  raresp<-which(presentyr<0.7*nrow(mtrim))
  
  commonspmat<-mtrim[,commonsp]
  
  if(length(raresp!=0)){
    rarespmat<-mtrim[,raresp]
    if(any(rarespmat!=0)){
      rarespmat<-as.matrix(rarespmat) # needed for only one columnvalue
      commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
    }
  }
  
  saveRDS(commonspmat,paste(resloc,"inputmat_for_tailanal.RDS",sep=""))
  return(commonspmat)
}

##################################
# call the function, total 15 good_LakeID
# we consider atleast 2 among (Jun-Sept: 4 months) sampling in each year
#--------------------------------------
shortyr_LakeID<-c()
#---------------------------------------
i<-1
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
# after seeing t2, we should consider year from 1981 onwards
# 1980 year has more sampling days
mtrim<-mtrim%>%filter(year>=1981)

badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#--------------------------------------
i<-2
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # equal sampling for all 4months in a year
range(t2$nud)

badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-3
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)

badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}

#-----------------------------------------
i<-4
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}

#-----------------------------------------
i<-5
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}

#-----------------------------------------
i<-6
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-7
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-8
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-9
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-10
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
# after seeing t1, we should consider year from 1973 onwards
mtrim<-mtrim%>%filter(year>=1973)
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-11
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-12
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
# after seeing t1, we should consider year from 1976 onwards
mtrim<-mtrim%>%filter(year>=1976)
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-13
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # all 4months sampled in a year?
# after seeing t1, we should consider year from 1973 onwards
mtrim<-mtrim%>%filter(year>=1973)
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-14
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}
#-----------------------------------------
i<-15
ans<-get_t1_t2_sampling(i=i)
mtrim<-ans$mtrim
t1<-ans$t1
t2<-ans$t2
unique(t1$num) # more or less equal sampling for all 4months in a year
range(t2$nud)
badyr<-t1$year[which(t1$num<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort
mtrim<-mtrim%>%filter(year%notin%badyr)

if(length(unique(mtrim$year))>=20){
  resloc<-paste("../../DATA/for_zoop_2014/wrangled_data/",good_LakeID[i],"/",sep="")
  res<-get_inputmat_for_tailanal(mtrim=mtrim,resloc=resloc)
}else{
  shortyr_LakeID<-c(shortyr_LakeID,good_LakeID[i])
}

good_LakeID<-setdiff(good_LakeID,shortyr_LakeID)
#overwrite good_LakeID: min 20 years data with consistent sampling: each 4 months sampled
saveRDS(good_LakeID,"../../DATA/for_zoop_2014/wrangled_data/good_LakeID.RDS")









