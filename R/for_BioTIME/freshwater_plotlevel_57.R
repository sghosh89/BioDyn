rm(list=ls())
library(tidyverse)
`%notin%` <- Negate(`%in%`)
xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
grid_freshw<-readRDS("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/bt_freshw_min20yr_rawdata.RDS")
df<-readRDS("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map.RDS")
df<-df%>%filter(site==57)
df$newsite<-df$site # this is the same as there is single site
#----------- create result folder for wrangle ddata -------------------------
resloc<-"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/57/"
if(!dir.exists(resloc)){
    dir.create(resloc)
}
saveRDS(df,"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/57/wrangledtable.RDS")
#================= filter data only for this site  =========================
site<-df$site
x<-grid_freshw%>%filter(STUDY_ID==site)
# df ensures there is only one single site with each month sampling once in a given year
unique(x$MONTH) # No month info available
#==================== saving input spmat  ====================
sort(unique(x$Species))

# do not consider these unknown sp into analysis
x<-x%>%filter(Species%notin%c("unspecifiable ","Unknown","Unknown rotifer", "Unknown rotifer2", "unknown ","Unknown "))

x<-x%>%select(YEAR,Species,Value=Abundance)
x<-x%>%group_by(Species,YEAR)%>%
  dplyr::summarise(Value=median(Value))%>%ungroup()

c1<-x%>%group_by(Species)%>%summarise(n_distinct(YEAR))%>%ungroup() 
# As all species are not found each year, we need to fill in the missing values with 0.
x_c<-x %>% 
  complete(Species, 
           nesting(YEAR), 
           fill = list(Value = 0))
xmat<-x_c%>%spread(Species, Value)
year<-xmat$YEAR
xmat<-as.matrix(xmat[,-1])
rownames(xmat)<-year

xmeta<-xxm%>%filter(STUDY_ID==site)

input_sp<-list(spmat=xmat,meta=xmeta)
saveRDS(input_sp,paste(resloc,"spmat.RDS",sep=""))

#==================== saving input spmat for tailanal for 57 ====================
m<-readRDS(paste(resloc,"spmat.RDS",sep=""))
# first we aggregated the rare sp (present even less than 30% of sampled years) into a pseudo sp 
presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
presentyr<-unname(presentyr)
commonspid<-which(presentyr>=0.7*nrow(m$spmat)) # consider species with >70% present yr
rareid<-which(presentyr<0.7*nrow(m$spmat)) 
ncol(m$spmat)==length(rareid) # that means not all sp are rare

if(length(rareid)!=0){
  raresp<-m$spmat[,rareid]
  raresp<-as.matrix(raresp) # this line is for when you have only one rare sp
  raresp<-apply(X=raresp,MARGIN=1,FUN=sum)
  m1<-m$spmat[,commonspid]
  m1<-cbind(m1,raresp=raresp)
  m1<-as.data.frame(m1)
  input_tailanal<-m1
}else{
  m1<-m$spmat
  input_tailanal<-m1
}
saveRDS(input_tailanal,paste(resloc,"input_tailanal.RDS",sep=""))

#==================== now do tail analysis ====================
source("tail_analysis.R")

resloc2<-"../../Results/for_BioTIME/Freshwater_plotlevel/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}
df<-input_tailanal # dataframe with species timeseries along column

#----------- analysis with covary sp ----------------
resloc<-paste(resloc2,site,"/",sep="")
if(!dir.exists(resloc)){
  dir.create(resloc)
}
res<-tail_analysis(mat = df, resloc = resloc, nbin = 2)
saveRDS(site,"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/57/newsite.RDS")
















