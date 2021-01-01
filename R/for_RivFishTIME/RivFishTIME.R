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

x_meta<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_TimeseriesTable.csv")
x_meta<-x_meta%>%filter(TimeSeriesID%in%good_TimeSeriesID_q3q4)

library(maps)
wd<-map_data("world")
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
             col = c("yellow","red","blue","green"))
  text(x = x, y = 1, label = paste(colnames(z1),"(",z_nsp[[i]],")",sep=""), pos = 3, cex = 1, col = "purple")
  legend("top",horiz=T,bty="n",cex=1.2,
         c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
         fill = c("yellow","red","blue","green","purple"))
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
barplot(d,width=4,border = "black", col=c("yellow", "red", "blue","green"),cex.axis = 3, ylim=c(0,1.4), ylab="Frequency", cex.lab=3)
legend(x=0,y=1.3,horiz=T,bty="n",cex=4,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory"),
       fill = c("yellow","red","blue","green"))
par(op)
dev.off()

# total number of sites
ncol(d)

# for how many sites indep. interaction were dominant?
sum(d[1,]>d[2,] & d[1,]>d[3,] & d[1,]>d[4,])

# for how many sites LT asymmetry were dominant?
sum(d[2,]>d[3,])

# for how many sites UT asymmetry were dominant?
sum(d[3,]>d[2,])

# for how many sites +ve corr were dominant?
sum((d[2,]+d[3,])>d[4,])

# for how many sites -ve corr were dominant?
sum((d[2,]+d[3,])<d[4,])






