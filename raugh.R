source("./get_input_spmat.R")
library(tidyverse)
#----------------------------------------
# read the data
xx<-read.csv("./Data/accessed18Nov2020/BioTIMEQuery02_04_2018.csv") # a dataframe

# read the meta data
xxm<-read.csv("./Data/accessed18Nov2020/BioTIMEMetadata_02_04_2018.csv") # a dataframe

# read the citation data
xc<-read.csv("./Data/accessed18Nov2020/BioTIMECitations_02_04_2018.csv")


#--------------- choose study sites based on min year sampling threshold --------------------

minyr<-30
xxm_long<-xxm%>%filter(DATA_POINTS>=minyr) # 38 observations with minimum 30 years of data

unique(xxm_long$REALM)
unique(xxm_long$CLIMATE)

xxm_freshw<-xxm_long%>%filter(REALM=="Freshwater")
nrow(xxm_freshw) # 8 sites 

#-------------- create results folder for each study sites -------------------

freshw_study_id<-xxm_freshw$STUDY_ID

resloc<-"./Results/Freshwater/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

for(i in 1:length(freshw_study_id)){
  k<-paste(resloc,freshw_study_id[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#--------------- saving input spmat for each study id -------------------------

for(i in 1:length(freshw_study_id)){
  x<-xx%>%filter(STUDY_ID==freshw_study_id[i])
  xmeta<-xxm%>%filter(STUDY_ID==freshw_study_id[i])
  input_sp<-get_input_spmat(x=x,xmeta=xmeta)
  resloc2<-paste(resloc,freshw_study_id[i],sep="")
  saveRDS(input_sp,paste(resloc2,"/spmat_and_list.RDS",sep=""))
}

#----------------- get a map for selecting freshwater sites --------------------

library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=xxm_freshw,aes(y=CENT_LAT,x=CENT_LONG,col=factor(TAXA)),alpha=0.4)+
    theme(legend.position = "bottom",legend.title = element_blank())+ggtitle("Freshwater timeseries: min 30 years")
g1
ggsave("./Results/Freshwater/Freshwater_min30yrs.pdf", width = 20, height = 10, units = "cm")
#------------------- now do the tail association analysis ---------------

# first save the input for tail analysis
for(i in 1:length(freshw_study_id)){
  siteid<-freshw_study_id[i]
  m<-readRDS(paste("./Results/Freshwater/",siteid,"/spmat_and_list.RDS",sep=""))
  
  # first we aggregated the rare sp (present even less than 10% of sampled years) into a pseudo sp 
  presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
  presentyr<-unname(presentyr)
  rareid<-which(presentyr<=0.1*nrow(m$spmat)) # rare sp = present less than 10% of sampled year
  if(length(rareid)!=0){
    raresp<-m$spmat[,rareid]
    raresp<-apply(X=raresp,MARGIN=1,FUN=sum)
    m1<-m$spmat[,-rareid]
    m1<-cbind(m1,pseudosp=raresp)
    m1<-as.data.frame(m1)
    ms1<-m$splist[-rareid]
    ms1$pseudosp<-data.frame(YEAR=ms1[[1]]$YEAR,mean_estimate=m1$pseudosp)
    input_tailanal<-list(m_df=m1,mlist=ms1)
  }else{
    input_tailanal<-list(m_df=m$spmat,mlist=m$splist)
  }
  
  saveRDS(input_tailanal,paste("./Results/Freshwater/",siteid,"/input_tailanal.RDS",sep=""))
  
}

















