source("./get_input_spmat.R")
library(tidyverse)
#================================== read the raw data ================================

xx<-read.csv("./Data/accessed18Nov2020/BioTIMEQuery02_04_2018.csv") # a dataframe

# read the meta data
xxm<-read.csv("./Data/accessed18Nov2020/BioTIMEMetadata_02_04_2018.csv") # a dataframe

#===================== generate results folder for freshwater ===============

resloc<-"./Results/Freshwater/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

#================ choose study sites based on min year sampling threshold ==============

xxm_freshw<-xxm%>%filter(REALM=="Freshwater")
nrow(xxm_freshw) # 28 freshwater sites 

minyr<-20

pdf("./Results/Freshwater/freshwater_sites_datapoints.pdf", height=5, width=8)
hist(xxm_freshw$DATA_POINTS, breaks=50, xlab="No. of years", main="Freshwater sites", xlim=c(0,40))
abline(v=minyr,col="red")
dev.off()

xxm_long_freshw<-xxm_freshw%>%filter(DATA_POINTS>=minyr) # 8 sites with minimum 30 years of data

unique(xxm_long_freshw$CLIMATE)

#=================== create results folder for each study sites ==================

freshw_study_id<-xxm_long_freshw$STUDY_ID

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

#==================== saving input spmat for each study id ====================

for(i in 1:length(freshw_study_id)){
  x<-xx%>%filter(STUDY_ID==freshw_study_id[i])
  xmeta<-xxm%>%filter(STUDY_ID==freshw_study_id[i])
  input_sp<-get_input_spmat(x=x,xmeta=xmeta)
  resloc2<-paste(resloc,freshw_study_id[i],sep="")
  saveRDS(input_sp,paste(resloc2,"/spmat_and_list.RDS",sep=""))
}

#================ get a map for selecting freshwater sites ==========================

library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=xxm_long_freshw,aes(y=CENT_LAT,x=CENT_LONG,col=factor(TAXA)),alpha=0.4)+
  theme(legend.position = "bottom",legend.title = element_blank())+ggtitle("Freshwater timeseries: min 30 years")
g1
ggsave(paste("./Results/Freshwater/Freshwater_min",minyr,"yrs.pdf",sep =""),
       width = 20, height = 10, units = "cm")

#====================== now do the tail association analysis ===================

#----------- first save the input for tail analysis ---------------
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
    
    #------- exclude ties having more than 80% of same values ----------
    Ties<-apply(MARGIN=2,X=m1,FUN=function(x){length(x) - length(unique(x))})
    excludeTies<-which(Ties>=0.8*nrow(m1)) # more than 80% ties are excluded
    if(length(excludeTies)!=0){
      m1<-m1[,-excludeTies]
      ms1<-ms1[-excludeTies]
    }
    #--------------------------------------------------
    input_tailanal<-list(m_df=m1,mlist=ms1)
    
  }else{
    m1<-m$spmat
    ms1<-m$splist
    #------- exclude ties having more than 50% of same values ----------
    Ties<-apply(MARGIN=2,X=m1,FUN=function(x){length(x) - length(unique(x))})
    excludeTies<-which(Ties>=0.5*nrow(m1)) # more than 50% ties are excluded
    if(length(excludeTies)!=0){
      m1<-m1[,-excludeTies]
      ms1<-ms1[-excludeTies]
    }
    input_tailanal<-list(m_df=m1,mlist=ms1)
  }
  
  saveRDS(input_tailanal,paste("./Results/Freshwater/",siteid,"/input_tailanal.RDS",sep=""))
  
}

#------------ Now compute and plot the tail stats ---------------------
source("./NonParamStat.R")
source("./NonParamStat_matrixplot.R")
for(i in 1:length(freshw_study_id)){
  siteid<-freshw_study_id[i]
  resloc<-paste("./Results/Freshwater/",siteid,"/",sep="")
  d<-readRDS(paste(resloc,"input_tailanal.RDS",sep=""))
  d_allsp<-d$mlist
  z<-multcall(d_allsp = d_allsp,resloc=resloc,nbin=2,include_indep = T)
  saveRDS(z,paste(resloc,"NonParamStat.RDS",sep=""))
  NonParamStat_matrixplot(data=z,resloc=resloc,tl.cex=1.2,cl.cex=2,line=1)
}

#--------------- Do a summary stats for freshwater sites ------------------
summary_table<-c()
for (i in c(1:length(freshw_study_id))){
  resloc<-paste("./Results/Freshwater/",freshw_study_id[i],"/",sep="")
  x<-readRDS(paste(resloc,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,x)
}
summary_table<-cbind(siteid=freshw_study_id,summary_table)
saveRDS(summary_table,"./Results/Freshwater/summary_table.RDS")


summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

df<-summary_table%>%select(siteid,f_nind,f_nL,f_nU,f_nneg)
dat<-t(df)
colnames(dat)<-dat[1,]
dat<-dat[-1,]

pdf("./Results/Freshwater/summary_plot.pdf",width=15,height=5)
op<-par(mar=c(5,5,5,1))
x<-barplot(dat,main = paste("Freshwater dynamics: min ",minyr," yrs",sep=""),
        xlab = "Site id",ylab="Freq. of pairwise interaction",ylim=c(0,1.4),
        cex.lab=2,cex.main=2,
        col = c("yellow","red","blue","green"))
text(x = x, y = 1, label = summary_table$nsp, pos = 3, cex = 1.5, col = "purple")
legend("top",horiz=T,bty="n",cex=1.4,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory"),
       fill = c("yellow","red","blue","green"))
par(op)
dev.off()

