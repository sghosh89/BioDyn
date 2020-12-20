rm(list=ls())

#----------------------------------
library(tidyverse)
#================================== read the raw data ================================

# read the data
bt_rarefied<-readRDS("./Results/bt_rarefied_data_pt_thrs_20.RDS")

# read the meta data
xxm<-read.csv("./Data/accessed18Nov2020/BioTIMEMetadata_02_04_2018.csv") # a dataframe
xxm_marine<-xxm%>%filter(REALM=="Marine")
nrow(xxm_marine) # 152 Marine sites 

#===================== generate results folder for marine ===============

resloc<-"./Results/Marine/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

#================ choose study sites based on min year sampling threshold ==============

data_pt_thrs<-20

pdf("./Results/Marine/marine_sites_datapoints.pdf", height=5, width=8)
hist(xxm_marine$DATA_POINTS, breaks=50, xlab="No. of years", main="Marine sites", xlim=c(0,54))
abline(v=data_pt_thrs,col="red")
dev.off()

#=================== create results folder for each study sites ==================

marine_study_id<-xxm_marine$STUDY_ID # all marine sites
marine_study_id<-marine_study_id[which(marine_study_id%in%bt_rarefied$STUDY_ID)] # sites with a 
# certain number of threshold

resloc<-"./Results/Marine/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

for(i in 1:length(marine_study_id)){
  k<-paste(resloc,marine_study_id[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#================= filter data only for marine sites =========================
bt_rarefied_marine<-bt_rarefied%>%filter(STUDY_ID%in%marine_study_id)

#==================== saving input spmat for each study id ====================

for(i in 1:length(marine_study_id)){
  
  x<-bt_rarefied_marine%>%filter(STUDY_ID==marine_study_id[i])
  xmat<-x%>%spread(Species, Value)%>%select(-STUDY_ID)
  year<-xmat$YEAR
  xmat<-as.matrix(xmat[,-1])
  rownames(xmat)<-year
  #xlist<-split(x,x$Species)
  #xlist<-purrr::map(xlist,~ (.x %>% select(YEAR,Value)))
  
  xmeta<-xxm%>%filter(STUDY_ID==marine_study_id[i])
  
  input_sp<-list(spmat=xmat,meta=xmeta)
  
  resloc2<-paste(resloc,marine_study_id[i],sep="")
  saveRDS(input_sp,paste(resloc2,"/spmat.RDS",sep=""))
}

#====================== now do the tail association analysis ===================

all_raresp_site_id<-c() # initiate to store bad sites with all rare sp.
#----------- first save the input for tail analysis ---------------
for(i in 1:length(marine_study_id)){
  siteid<-marine_study_id[i]
  m<-readRDS(paste("./Results/Marine/",siteid,"/spmat.RDS",sep=""))
  
  # first we aggregated the rare sp (present even less than 30% of sampled years) into a pseudo sp 
  presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
  presentyr<-unname(presentyr)
  rareid<-which(presentyr<=0.3*nrow(m$spmat)) # rare sp = present less than 30% of sampled year
  
  if(ncol(m$spmat)==length(rareid)){
    all_raresp_site_id<-c(all_raresp_site_id,siteid) # this site has all sp rare throughout the years
  }else{
    if(length(rareid)!=0){
      raresp<-m$spmat[,rareid]
      raresp<-as.matrix(raresp) # this line is for when you have only one rare sp
      raresp<-apply(X=raresp,MARGIN=1,FUN=sum)
      m1<-m$spmat[,-rareid]
      m1<-cbind(m1,raresp=raresp)
      m1<-as.data.frame(m1)
      
      #------- exclude ties having more than 80% of same values ----------
      #Ties<-apply(MARGIN=2,X=m1,FUN=function(x){length(x) - length(unique(x))})
      #excludeTies<-which(Ties>=0.8*nrow(m1)) # more than 80% ties are excluded
      #spTies<-m1[,excludeTies]
      #if(length(excludeTies)!=0){
      #  m1<-m1[,-excludeTies]
      #  ms1<-ms1[-excludeTies]
      #}
      #--------------------------------------------------
      input_tailanal<-m1
      
    }else{
      m1<-m$spmat
      #------- exclude ties having more than 50% of same values ----------
      #Ties<-apply(MARGIN=2,X=m1,FUN=function(x){length(x) - length(unique(x))})
      #excludeTies<-which(Ties>=0.8*nrow(m1)) # more than 80% ties are excluded
      #if(length(excludeTies)!=0){
      #  m1<-m1[,-excludeTies]
      # ms1<-ms1[-excludeTies]
      #}
      input_tailanal<-m1
    }
    
    saveRDS(input_tailanal,paste("./Results/Marine/",siteid,"/input_tailanal.RDS",sep=""))
  }
  
}
# update marine_study_id: goodsites where some common target sp. are present
marine_study_id<-setdiff(marine_study_id,all_raresp_site_id) 

#------------ Now compute and plot the tail stats ---------------------
source("./tail_analysis.R")

for(i in 1:length(marine_study_id)){
  siteid<-marine_study_id[i]
  resloc<-paste("./Results/Marine/",siteid,"/",sep="")
  df<-readRDS(paste(resloc,"input_tailanal.RDS",sep="")) # dataframe with species timeseries along column
  
  #----------- analysis with covary sp ----------------
  res<-tail_analysis(mat = df, resloc = resloc, nbin = 2)
}


#================ get a map for selecting marine sites ==========================

meta_marine<-xxm%>%filter(STUDY_ID%in%marine_study_id)
saveRDS(meta_marine,"./Results/Marine/meta_marine.RDS")

pdf("./Results/Marine/histogram_site_area.pdf",width=5,height=3)
hist(meta_marine$AREA_SQ_KM,breaks=100,xlab="Area(Sq. Km.)",ylab="No. of marine sites", 
     main=paste(nrow(meta_marine)," sites",sep=""))
dev.off()


library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=meta_marine,aes(y=CENT_LAT,x=CENT_LONG,col=factor(TAXA)),alpha=0.4)+
  theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("Marine timeseries: min ",data_pt_thrs," years",sep=""))
g1
ggsave(paste("./Results/Marine/Marine_min",data_pt_thrs,"yrs.pdf",sep =""),
       width = 20, height = 10, units = "cm")

#--------------- Do a summary stats for marine sites ------------------
summary_table<-c()
for (i in c(1:length(marine_study_id))){
  resloc<-paste("./Results/Marine/",marine_study_id[i],"/",sep="")
  x<-readRDS(paste(resloc,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,x)
}
summary_table<-cbind(siteid=marine_study_id,summary_table)
saveRDS(summary_table,"./Results/Marine/summary_table.RDS")


summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

df<-summary_table%>%select(siteid,nsp,f_nind,f_nL,f_nU,f_nneg)
df$Taxa<-meta_marine$TAXA
df <-df[order(df$Taxa),]
dat<-t(df)
colnames(dat)<-dat[1,]
dat<-dat[-1,]
nsp<-dat[1,]
dat<-dat[-1,]

pdf("./Results/Marine/summary_plot.pdf",width=22,height=10)
op<-par(mar=c(12,5,5,1))
x<-barplot(dat,main = paste("Marine dynamics: min ",data_pt_thrs," yrs",sep=""),
           xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.4),
           cex.lab=2,cex.main=2,names.arg = dat[5,],las=2,
           col = c("yellow","red","blue","green"))
text(x = x, y = 1, label = paste(colnames(dat),"(",nsp,")",sep=""), pos = 3, cex = 1, col = "purple")
#text(x = x, y = 1, label = colnames(dat), pos = 1, cex = 1.5, col = "purple")
legend("top",horiz=T,bty="n",cex=1.2,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
       fill = c("yellow","red","blue","green","purple"))
par(op)
dev.off()

##########################################################################





