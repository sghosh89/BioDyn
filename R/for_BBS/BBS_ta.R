rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_BBS/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#-----------------------------------------
# prepare the metadata
fshort_list<-readRDS("../../DATA/for_BBS/wrangled_data/sourcefile_list.RDS")
uroutes<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all.RDS")
uroutes<-data.frame(Country_State_Route=uroutes)
x_meta<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/routes.csv")
x_meta<-x_meta%>%unite("Country_State_Route",CountryNum,StateNum,Route,sep="_")
metadata<-inner_join(uroutes,x_meta,by="Country_State_Route")%>%
                            rename(Stratum_code=Stratum)

bbs_strata1<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/BBS_physiographic_strataname.csv")
bbs_strata1<-bbs_strata1%>%select(Stratum_code=Stratum,Stratum_name=Name,Stratum_area=Area.Km2)
#bbs_strata2<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/BBS_physiographic_strataname_statemap.csv")
metadata<-inner_join(metadata,bbs_strata1,by="Stratum_code")
saveRDS(metadata,"../../DATA/for_BBS/wrangled_data/unique_routes_all_metadata.RDS")

#-------------- plot the sampling routes on map -----------------------
library(maps)
wd<-map_data("world")
wd<-wd%>%filter(region%in%c("USA","Canada"))%>%filter(long<0)
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=metadata,aes(y=Latitude,x=Longitude),col="red",alpha=0.1,cex=0.1)+
  ggtitle(paste("BBS: ",nrow(metadata)," routes: min 20 years",sep=""))+ 
  theme(plot.title = element_text(size = 8))
g1
ggsave(paste(resloc,"routes_on_map.pdf",sep =""),
       width = 10, height = 5, units = "cm")

#=================== create results folder for each study sites/routes ==================

for(i in 1:nrow(uroutes)){
  k<-paste(resloc,uroutes$Country_State_Route[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#------------ Now compute and plot the tail stats ---------------------

for(i in 1:nrow(uroutes)){
  siteid<-uroutes$Country_State_Route[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../../DATA/for_BBS/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"input_mat_for_tailanal.RDS",sep="")) # dataframe with species timeseries along column
  
  #----------- analysis with covary sp ----------------
  res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 2)
  cat("---------- i= ",i," routeid=",siteid," ----------\n")
}

#--------------- Do a summary stats for all routes ------------------
summary_table<-c()
for(i in 1:nrow(uroutes)){
  siteid<-uroutes$Country_State_Route[i]
  resloc_input<-paste(resloc,siteid,"/",sep="")
  x<-readRDS(paste(resloc_input,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,x)
}
summary_table<-cbind(siteid=uroutes$Country_State_Route,summary_table)
saveRDS(summary_table,"../../Results/for_BBS/summary_table.RDS")

summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)


pdf("../../Results/for_BBS/hist_targetsp.pdf",width=10,height=6)
op<-par(mar=c(5,5,5,2))
hist(summary_table$nsp, breaks=100, xlim=c(0,100), xlab="Number of target sp.",
     ylab="Frequency (sites)", col="skyblue", 
     main=paste("BBS: ",nrow(summary_table)," routes", sep=""))
par(op)
dev.off()

summary_table<-inner_join(summary_table,metadata,by=c("siteid"="Country_State_Route"))
saveRDS(summary_table,"../../Results/for_BBS/summary_table_detail_version.RDS")

sv<-split(summary_table,f=summary_table$Stratum_name)

pdf("../../Results/for_BBS/summary_hist_by_stratumregion.pdf",width=20,height=5)
op<-par(mar=c(10,10,5,1),mgp=c(5,1,0))

for(i in 1:length(sv)){
 
    xb<-sv[[i]]
    df<-xb%>%select(siteid,nsp,f_nind,f_nL,f_nU,f_nneg)
    dat<-t(df)
    colnames(dat)<-xb$RouteName
    dat<-dat[-1,]
    nsp<-dat[1,]
    dat<-dat[-1,]
    dat<-as.matrix(dat)
    
    x<-barplot(dat,
               main = paste("Stratum= ",xb$Stratum_name[1],": #routes =",ncol(dat),
                            sep=""),
               xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.3),
               cex.lab=2,cex.main=2,las=2,cex.axis = 2,
               col = c("yellow","red","skyblue","green"))
   
    legend("top",horiz=T,bty="n",cex=1,
           c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
           fill = c("yellow","red","skyblue","green","purple"))
 
  text(x = x, y = 1, label = paste("(",xb$nsp,")",sep=""), 
       pos = 3, cex = 1, 
       col = "purple")
}

par(op)
dev.off()


#   summary histogram

#pie chart
#x<-summary_table%>%select(f_nind,f_nL,f_nU,f_nneg)
#x<-as.matrix(x)
#pie(x[1,],labels=colnames(x),col=c("yellow","red","blue","green"))

#head(mtcars)
#boxplot(mpg~cyl,y)

#################################################################################################
my_summary_boxplot<-function(summary_table,nametag){
  # for how many sites LT asymmetry were dominant?
  nLT<-sum(summary_table$f_nL>summary_table$f_nU)
  
  # for how many sites +ve corr were dominant?
  nP<-sum((summary_table$f_nL+summary_table$f_nU)>summary_table$f_nneg)
  
  # for how many sites +ve corr were dominant?
  nC<-sum((summary_table$f_nL+summary_table$f_nU)<summary_table$f_nneg)
  
  z<-summary_table%>%select(f_nind,f_nL,f_nU,f_nneg)
  colnames(z)<-c("Independent","Synchrony(rare)","Synchrony(abundant)","Compensatory")
  y <- gather(z, Pairwise.Interaction, Frequency) 
  boxplot(Frequency~Pairwise.Interaction,y,ylim=c(0,1),
          col=c("green","yellow","skyblue","red"),
          main=paste(nametag,", #sites: ",nrow(z),", #sites(more syn.): ",nP,", #sites(more comp.): ",nC))
}
###################################################################################################

pdf("../../Results/for_BBS/summary_boxplot_by_stratumregion.pdf",width=90,height=40)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),mfrow=c(8,9),cex.axis=2, cex.lab=3, cex.main=4, cex.sub=2)
for(i in 1:length(sv)){
  my_summary_boxplot(summary_table = sv[[i]],nametag = names(sv)[i])
}
par(op)
dev.off()

pdf("../../Results/for_BBS/summary_boxplot.pdf",width=14,height=6)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.sub=1.5)
  
  my_summary_boxplot(summary_table = summary_table,nametag = "BBS")

par(op)
dev.off()











