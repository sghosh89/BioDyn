rm(list=ls())

#----------------------------------
library(tidyverse)
#---------------------------------
# read metadata
xm<-read.csv("../../DATA/for_BioTIME/raw_data/accessed18Nov2020/BioTIMEMetadata_02_04_2018.csv") 

# read summary results from freshwater, marine, terrestrial
r_frs<-readRDS("../../Results/for_BioTIME/Freshwater/summary_table.RDS")
r_mar<-readRDS("../../Results/for_BioTIME/Marine/summary_table.RDS")
r_ter<-readRDS("../../Results/for_BioTIME/Terrestrial/summary_table.RDS")
r_frs_mar_ter<-rbind(r_frs,r_mar,r_ter)

summary_table<-r_frs_mar_ter%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)
summary_table<-inner_join(summary_table,xm,by=c("siteid"="STUDY_ID"))%>%
                select(siteid,nsp,nind,npos,nL,nU,nneg,f_nind,f_nL,f_nU,f_nneg,
                       REALM,
                       CLIMATE,HABITAT,BIOME_MAP,TAXA,ORGANISMS,CENT_LAT,CENT_LONG,COMMENTS)

# we are going to exclude results for siteid 195: results from a small subset of BBS data
summary_table<-summary_table[-which(summary_table$siteid==195),]

saveRDS(summary_table,"../../Results/for_BioTIME/summary_table.RDS")

#----------------------------------------------------------------------------
# plot sites on map

library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=summary_table,aes(y=CENT_LAT,x=CENT_LONG,shape=factor(REALM),col=factor(TAXA)),alpha=0.4,size=2)+
  theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("BioTIME: min 20 years",sep=""))
g1
ggsave("../../Results/for_BioTIME/sites_on_map.pdf",
       width = 28, height = 10, units = "cm")


#---------------------------------------------------------------------------------------------------
# boxplot
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
####################################

# boxplot by REALM
sv<-split(summary_table,f=summary_table$REALM)
pdf("../../Results/for_BioTIME/summary_boxplot_by_REALM.pdf",width=25,height=10)
op<-par(mar=c(5,7,5,1),mgp=c(4,1,0),mfrow=c(2,2),cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2)
for(i in c(1:length(sv))){
  my_summary_boxplot(summary_table = sv[[i]],nametag = names(sv)[i],myresloc = "../../Results/for_BioTIME/")
}
par(op)
dev.off()

# boxplot by TAXA
sv<-split(summary_table,f=summary_table$TAXA)
pdf("../../Results/for_BioTIME/summary_boxplot_by_TAXA.pdf",width=42,height=20)
op<-par(mar=c(5,7,5,1),mgp=c(4,1,0),mfrow=c(4,3),cex.axis=2.5, cex.lab=2.5, cex.main=3, cex.sub=2)
for(i in c(1:length(sv))){
  my_summary_boxplot(summary_table = sv[[i]],nametag = names(sv)[i],myresloc = "../../Results/for_BioTIME/")
}
par(op)
dev.off()

