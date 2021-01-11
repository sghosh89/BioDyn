rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_RivFishTIME/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#-----------------------------------------
# plot the sampling sites
good_TimeSeriesID_q3q4<-readRDS("../../DATA/for_RivFishTIME/wrangled_data/good_TimeSeriesID_q3q4.RDS")
x<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_SurveyTable.csv") # a dataframe
x_meta<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_TimeseriesTable.csv")
z<-x %>% distinct(TimeSeriesID, .keep_all = TRUE)%>%select(TimeSeriesID,UnitAbundance)
x_meta<-inner_join(z,x_meta,by="TimeSeriesID")

x_meta<-x_meta%>%filter(TimeSeriesID%in%good_TimeSeriesID_q3q4)

library(maps)
wd<-map_data("world")
wd<-wd%>%filter(long<50 & lat>-50)
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=x_meta,aes(y=Latitude,x=Longitude),color="blue",alpha=0.1)+
  ggtitle(paste("RivFishTIME: ",nrow(x_meta)," sites: min 20 years",sep=""))
g1
ggsave(paste(resloc,"sites_on_map.pdf",sep =""),
       width = 20, height = 10, units = "cm")


#=================== create results folder for each study sites ==================

for(i in 1:length(good_TimeSeriesID_q3q4)){
  k<-paste(resloc,good_TimeSeriesID_q3q4[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#------------ Now compute and plot the tail stats ---------------------

for(i in 1:length(good_TimeSeriesID_q3q4)){
  siteid<-good_TimeSeriesID_q3q4[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../../DATA/for_RivFishTIME/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"commonspecies_timeseries.RDS",sep="")) # dataframe with species timeseries along column
  
  #----------- analysis with covary sp ----------------
  res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 2)
  cat("---------- i= ",i," siteid=",siteid," ----------\n")
}


#--------------- Do a summary stats for all good sites ------------------
summary_table<-c()
for (i in c(1:length(good_TimeSeriesID_q3q4))){
  siteid<-good_TimeSeriesID_q3q4[i]
  resloc_input<-paste(resloc,siteid,"/",sep="")
  x<-readRDS(paste(resloc_input,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,x)
}
summary_table<-cbind(siteid=good_TimeSeriesID_q3q4,summary_table)
saveRDS(summary_table,"../../Results/for_RivFishTIME/summary_table.RDS")

summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

pdf(paste("../../Results/for_RivFishTIME/hist_targetsp.pdf",sep=""),width=10,height=6)
op<-par(mar=c(5,5,5,2))
hist(summary_table$nsp, breaks=100, xlim=c(0,40), xlab="Number of target sp.",
     ylab="Frequency (sites)", col="skyblue", 
     main=paste("RivFishTIME: ",nrow(summary_table)," sites", sep=""))
par(op)
dev.off()

summary_table<-inner_join(summary_table,x_meta,by=c("siteid"="TimeSeriesID"))
saveRDS(summary_table,"../../Results/for_RivFishTIME/summary_table_detail_version.RDS")

df<-summary_table%>%select(siteid,nsp,f_nind,f_nL,f_nU,f_nneg)
dat<-t(df)
colnames(dat)<-dat[1,]
dat<-dat[-1,]
nsp<-dat[1,]
dat<-dat[-1,]
dat<-as.data.frame(dat)

# now plot this long dataframe by splitting in multiple pdfs 
z<-tapply(as.list(dat), gl(ncol(dat)/20, 20), as.data.frame)
z_nsp<-split(nsp, ceiling(seq_along(nsp)/20))

for(i in 1:length(z)){
  pdf(paste("../../Results/for_RivFishTIME/summary_plot_",i,".pdf",sep=""),width=25,height=10)
  op<-par(mar=c(12,5,5,1))
  z1<-as.matrix(z[[i]])
  x<-barplot(z1,main = "RivFishTIME dynamics: min 20 yrs",
             xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.4),
             cex.lab=2,cex.main=2,las=2,
             col = c("yellow","red","skyblue","green"))
  text(x = x, y = 1, label = paste(colnames(z1),"(",z_nsp[[i]],")",sep=""), pos = 3, cex = 1, col = "purple")
  legend("top",horiz=T,bty="n",cex=1.2,
         c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
         fill = c("yellow","red","skyblue","green","purple"))
  par(op)
  dev.off()
}

# second summary plot: how many sites showed indep/ LT/ RT/ -ve dominant?
d.ind<-as.numeric(dat[1,])
d.L<-as.numeric(dat[2,])
d.U<-as.numeric(dat[3,])
d.neg<-as.numeric(dat[4,])


d<-rbind(d.ind,d.L,d.U,d.neg)

pdf(paste("../../Results/for_RivFishTIME/summary_plot_full.pdf",sep=""),width=80,height=10)
op<-par(mar=c(10,15,10,10))
barplot(d,width=4,border = "black", col=c("yellow", "red", "skyblue","green"),cex.axis = 3, ylim=c(0,1.4), ylab="Frequency", cex.lab=3)
legend(x=0,y=1.3,horiz=T,bty="n",cex=4,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory"),
       fill = c("yellow","red","skyblue","green"))
par(op)
dev.off()
#######################################################################################

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
  saveRDS(dtable,paste(myresloc,"summary_dtable_from_boxplot_",nametag,".RDS",sep=""))
}
###################################################################################################
# call the boxplot by biorealm

sv<-split(summary_table,f=summary_table$BioRealm)
pdf("../../Results/for_RivFishTIME/summary_boxplot_by_biorealm.pdf",width=28,height=10)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),mfrow=c(2,2),cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2)
for(i in 1:length(sv)){
  cat("i=",i,"\n")
  my_summary_boxplot(summary_table = sv[[i]],nametag = names(sv)[i],myresloc ="../../Results/for_RivFishTIME/" )
}
par(op)
dev.off()

# call the boxplot by country

# summary by North USA, North & South EU

dt1<-summary_table%>%filter(Country=="USA")
dt2<-summary_table%>%filter(Country%in%c("FIN","SWE"))# north EU
dt3<-summary_table%>%filter(Country%in%c("GBR","FRA","BEL","ESP")) # south EU

pdf("../../Results/for_RivFishTIME/summary_boxplot_by_region.pdf",width=28,height=10)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),mfrow=c(2,2),cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2)
my_summary_boxplot(summary_table = dt1,nametag = "North America",myresloc ="../../Results/for_RivFishTIME/")
my_summary_boxplot(summary_table = dt2,nametag = "North Europe",myresloc ="../../Results/for_RivFishTIME/")
my_summary_boxplot(summary_table = dt3,nametag = "South Europe",myresloc ="../../Results/for_RivFishTIME/")
par(op)
dev.off()







pdf("../../Results/for_RivFishTIME/summary_boxplot.pdf",width=14,height=6)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.sub=1.5)
my_summary_boxplot(summary_table = summary_table,nametag = "RivFishTIME",myresloc ="../../Results/for_RivFishTIME/")
par(op)
dev.off()

#---------------------------- on map summary plot -------------------------------------------------
df<-summary_table
df$asym<-NA

# more LT
id<-which(df$f_nL>df$f_nU)
df$asym[id]<-"Syn.(rare)"

# more UT
id<-which(df$f_nL<df$f_nU)
df$asym[id]<-"Syn.(abundant)"

# LT==UT
id<-which(df$f_nL==df$f_nU)
df$asym[id]<-"Synchrony" 

# comp>syn
# more LT
id<-which(df$f_nneg>df$f_npos)
df$negcor<-"more synchronous"
df$negcor[id]<-"more compensatory" 

routeL<-sum(df$asym=="Syn.(rare)") # syn: LT dep.
routeU<-sum(df$asym=="Syn.(abundant)") # syn: UT dep.
routeS<-sum(df$asym%in%c("Syn.(rare)","Syn.(abundant)","Syn.")) # Syn: no taildep.
routeC<-sum(df$negcor=="more compensatory") # Comp.

df_s<-df%>%filter(negcor=="more synchronous")
df_c<-df%>%filter(negcor=="more compensatory")

# for synchrony
library(maps)
wd<-map_data("world")
wd<-wd%>%filter(long<50 & lat>-50)
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=df_s,aes(y=Latitude,x=Longitude,col=factor(asym)),alpha=0.2,cex=0.2)+
  ggtitle(paste("RivFishTIME: ",nrow(df_s)," synchronous sites",sep=""))+ 
  theme(plot.title = element_text(size = 5),legend.position = "right",
        legend.title = element_blank(),
        legend.text=element_text(size=4))
g1
ggsave(paste(resloc,"sites_on_map_details_syn.pdf",sep =""),
       width = 8, height = 4, units = "cm")

# for compensatory
library(maps)
wd<-map_data("world")
wd<-wd%>%filter(long<50 & lat>-50)
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=df_c,aes(y=Latitude,x=Longitude,col=factor(BioRealm)),alpha=0.2,cex=0.2)+
  ggtitle(paste("RivFishTIME: ",nrow(df_c)," compensatory sites",sep=""))+ 
  theme(plot.title = element_text(size = 5),legend.position = "right",
        legend.title = element_blank(),
        legend.text=element_text(size=3))+guides(colour=guide_legend(nrow=7))
g1
ggsave(paste(resloc,"sites_on_map_details_comp.pdf",sep =""),
       width = 8, height = 5, units = "cm")






