rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#--------- read data ---------------------------------
xm<-read.csv("../../DATA/for_insectRoel/20yrFreshwater_Metadata.csv")
x<-readRDS("../../DATA/for_insectRoel/20yrFreshwaterData.rds")
#x<-x%>%filter(Number>0)

#---------------------------------------
resloc<-"../../Results/for_insectRoel/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

Datasource_ID<-setdiff(sort(unique(x$Datasource_ID)),63)
for(i in 1:length(Datasource_ID)){
  did<-paste(resloc,Datasource_ID[i],"/",sep="")
  if(!dir.exists(did)){
    dir.create(did)
  }
  pidlist<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/Plot_ID_list.RDS",sep=""))
  badpidlist<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/bad_pidlist.RDS",sep=""))
  goodpidlist<-setdiff(pidlist,badpidlist)
  saveRDS(goodpidlist,paste("../../Results/for_insectRoel/",Datasource_ID[i],"/goodpidlist.RDS",sep=""))
  for(j in 1:length(goodpidlist)){
    pid<-paste(did,goodpidlist[j],"/",sep="")
    if(!dir.exists(pid)){
      dir.create(pid)
    }
  }
}

#----------- Now compute and plot the tail stats ---------------------
  
for(i in 1:length(Datasource_ID)){
    did<-Datasource_ID[i]
    goodpidlist<-readRDS(paste("../../Results/for_insectRoel/",did,"/goodpidlist.RDS",sep=""))
    for(j in 1:length(goodpidlist)){
      pid<-goodpidlist[j]
      resloc_output<-paste(resloc,did,"/",pid,"/",sep="")
      resloc_input<-paste("../../DATA/for_insectRoel/wrangled_data/",did,"/",pid,"/",sep="")
      
      df<-readRDS(paste(resloc_input,"inputmat_for_tailanal.RDS",sep="")) # dataframe with species timeseries along column
      #----------- analysis with covary sp ----------------
      if(ncol(df)>=2){
        res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 2)
        cat("------- i= ",i," j=",j," did =", did, " pid = ",pid," ----------\n")
      }else{
        cat("------- i= ",i," j=",j," did =", did, " pid = ",pid," not enough sp present ----------\n")
      }
      
    }
}

#--------------- Do a summary stats for all ------------------
summary_table<-c()
didlist<-c()
pidlist<-c()
for(i in 1:length(Datasource_ID)){
  did<-Datasource_ID[i]
  goodpidlist<-readRDS(paste("../../Results/for_insectRoel/",did,"/goodpidlist.RDS",sep=""))
  for(j in 1:length(goodpidlist)){
  pid<-goodpidlist[j]
  didlist<-c(didlist,did)
  pidlist<-c(pidlist,pid)
  resloc_input<-paste("../../Results/for_insectRoel/",did,"/",pid,"/",sep="")
  st<-readRDS(paste(resloc_input,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,st)
  }
}
summary_table<-cbind(STUDY_ID=didlist,newsite=pidlist,summary_table)
saveRDS(summary_table,"../../Results/for_insectRoel/summary_table.RDS")

summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)
metadata<-xm%>%select(Plot_ID,REALM=Realm,TAXA=Taxonomic_scope,ORGANISMS=Taxonomic_scope,Latitude,Longitude)
length(unique(xm$Plot_ID))==nrow(xm)
summary_table<-inner_join(summary_table,metadata,by=c("newsite"="Plot_ID"))
summary_table$TAXA<-"Freshwater invertebrates"
saveRDS(summary_table,"../../Results/for_insectRoel/summary_table_detail_version.RDS")

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
  nEqSynComp<-sum((summary_table$f_nL+summary_table$f_nU)==summary_table$f_nneg & 
                    summary_table$f_nind!=1)
  
  # This sites are fully indep.
  #findep<-sum((summary_table$f_nL+summary_table$f_nU)==summary_table$f_nneg & 
  #                          summary_table$f_nind!=1)
  
  # nrow(summary_table)==nLT+nUT+nC+nSym+nEqSynComp+findep
  
  z<-summary_table%>%select(f_nind,f_nL,f_nU,f_nneg)
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


pdf("../../Results/for_insectRoel/summary_boxplot.pdf",width=14,height=6)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.sub=1.5)

my_summary_boxplot(summary_table = summary_table,nametag = "insect",myresloc="../../Results/for_insectRoel/")

par(op)
dev.off()











