rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
library(readxl)
#---------------------------------------
resloc<-"../../Results/for_zoop_2014/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#-----------------------------------------
# plot the sampling sites
xmeta<-read_excel("../../DATA/for_zoop_2014/LakeNameMasterand coords.xlsx")
colnames(xmeta)[5:6]<-c("lat","lon")
good_LakeID<-readRDS("../../DATA/for_zoop_2014/wrangled_data/good_LakeID.RDS")
xmeta<-xmeta%>%filter(`LakeID to Use`%in%good_LakeID | `old ID`%in%good_LakeID) # RCM = RC
xmeta$good_LakeID<-good_LakeID 

library(maps)
wd<-map_data("world")
wd<-wd%>%filter(long<-5 & lat>4)
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=xmeta,aes(y=lat,x=lon),color="blue",alpha=0.1)+
  ggtitle(paste("zoop2014: ",nrow(xmeta)," sites: min 20 years",sep=""))
g1
ggsave(paste(resloc,"sites_on_map.pdf",sep =""),
       width = 20, height = 10, units = "cm")


#=================== create results folder for each study sites ==================

for(i in 1:length(good_LakeID)){
  k<-paste(resloc,good_LakeID[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#------------ Now compute and plot the tail stats ---------------------

for(i in 1:length(good_LakeID)){
  siteid<-good_LakeID[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../../DATA/for_zoop_2014/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"inputmat_for_tailanal.RDS",sep="")) # dataframe with species-group timeseries along column
  
  #----------- analysis with covary sp ----------------
  res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 2)
  cat("---------- i= ",i," siteid=",siteid," ----------\n")
}


#--------------- Do a summary stats for all good sites ------------------
summary_table<-c()
for (i in c(1:length(good_LakeID))){
  siteid<-good_LakeID[i]
  resloc_input<-paste(resloc,siteid,"/",sep="")
  x<-readRDS(paste(resloc_input,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,x)
}
summary_table<-cbind(siteid=good_LakeID,summary_table)
summary_table$initR<-NA
for(i in 1:nrow(summary_table)){
  bigM<-readRDS(paste("../../DATA/for_zoop_2014/wrangled_data/",summary_table$siteid[i],"/allsp_timeseries.RDS",sep=""))
  summary_table$initR[i]<-ncol(bigM)
}
# reorganize
summary_table<-summary_table%>%dplyr::select(siteid,initR,nsp,nint,nind,npos,nL,nU,nneg,L,U)

summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

# to get pairwise Spearman correlation
summary_table$tot_spear_sig<-NA # sum of all significant positive and negative correlation

for(i in 1:nrow(summary_table)){
  nsp<-summary_table$nsp[i]
  siteid<-good_LakeID[i]
  resloc_input<-paste(resloc,siteid,"/",sep="")
  x<-readRDS(paste(resloc_input,"NonParamStat.RDS",sep=""))
  spx<-x$corval # actual positive and negative spearman correlation
  
  posnn<-x$posn_notneeded
  #posN_ind<-which(x$posnN==1, arr.ind = T)
  posI_ind<-which(x$posnI==1, arr.ind = T)
  
  spx[posI_ind]<-NA # only exclude indep. interaction
  spx[posnn]<-NA
  
  spx<-spx[1:nsp,1:nsp]
  
  summary_table$tot_spear_sig[i]<-sum(spx, na.rm=T) # you have to normalize it by dividing with nsp*(nsp-1)/2
}


saveRDS(summary_table,"../../Results/for_zoop_2014/summary_table.RDS")

pdf(paste("../../Results/for_zoop_2014/hist_targetsp.pdf",sep=""),width=10,height=6)
op<-par(mar=c(5,5,5,2))
hist(summary_table$nsp, breaks=10, xlim=c(0,10), xlab="Number of target sp. group",
     ylab="Frequency (sites)", col="skyblue", 
     main=paste("zoop2014: ",nrow(summary_table)," sites", sep=""))
par(op)
dev.off()


df<-summary_table%>%dplyr::select(siteid,nsp,f_nind,f_nL,f_nU,f_nneg)
dat<-t(df)
colnames(dat)<-dat[1,]
dat<-dat[-1,]
nsp<-dat[1,]
dat<-dat[-1,]


pdf(paste("../../Results/for_zoop_2014/summary_plot_zoop2014.pdf",sep=""),width=10,height=5)
op<-par(mar=c(3,5,5,1))
x<-barplot(dat,width=4,border = "black", col=c("yellow", "red", "skyblue","green"),cex.axis = 1.5, ylim=c(0,1.4), 
           ylab="Frequency", cex.lab=2)
legend(x=0,y=1.3,horiz=T,bty="n",cex=0.8,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory"),
       fill = c("yellow","red","skyblue","green"))
text(x = x, y = 1, label = nsp, pos = 3, cex = 1, col = "purple")
par(op)
dev.off()

#################################################################################################
my_summary_boxplot<-function(summary_table,nametag,myresloc){
  
  # for how many sites +ve corr were dominant?
  nP<-sum((summary_table$f_nL+summary_table$f_nU)>summary_table$f_nneg)
  shorttab<-summary_table[which(summary_table$f_nL+summary_table$f_nU>summary_table$f_nneg),]
  
  # out of those sites: for how many sites LT asymmetry were dominant?
  nLT<-sum(shorttab$f_nL>shorttab$f_nU)
  
  # for how many sites UT asymmetry were dominant?
  nUT<-sum(shorttab$f_nL<shorttab$f_nU)
  
  # for how many sites no asymmetry were dominant?
  nSym<-sum(shorttab$f_nL==shorttab$f_nU)
  
  # for how many sites -ve corr were dominant?
  nC<-sum((summary_table$f_nL+summary_table$f_nU)<summary_table$f_nneg)
  
  # for how many sites syn==comp?
  nEqSynComp<-sum((summary_table$f_nL+summary_table$f_nU)==summary_table$f_nneg)
  
  z<-summary_table%>%dplyr::select(f_nind,f_nL,f_nU,f_nneg)
  colnames(z)<-c("Independent","Synchrony(rare)","Synchrony(abundant)","Compensatory")
  y <- gather(z, Pairwise.Interaction, Frequency) 
  boxplot(Frequency~Pairwise.Interaction,y,ylim=c(0,1),
          col=c("green","yellow","skyblue","red"),
          main=paste(nametag,", #sites: ",nrow(z),", #sites(more syn.): ",nP,", #sites(more comp.): ",nC))
  
  dtable<-data.frame(nSyn=nP,nLT=nLT,nUT=nUT,nSym=nSym,nComp=nC,nEqSynComp=nEqSynComp)
  rownames(dtable)<-nametag
  print(dtable)
  saveRDS(dtable,paste(myresloc,"summary_dtable_from_boxplot_",str_replace(nametag,"/","_"),".RDS",sep=""))
}
###################################################################################################

pdf("../../Results/for_zoop_2014/summary_boxplot_zoop2014.pdf",width=14,height=6)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
my_summary_boxplot(summary_table = summary_table,nametag = "zoop2014",myresloc = "../../Results/for_zoop_2014/")
par(op)
dev.off()







