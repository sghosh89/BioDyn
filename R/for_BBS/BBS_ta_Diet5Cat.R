rm(list=ls())
library(dplyr)
`%notin%` <- Negate(`%in%`)

# function to compute stats based on 5 diet category

BBS_ta_Diet5Cat<-function(data, resloc){
  
  # read data
  zall<-read.csv("../../DATA/for_BBS/wrangled_data/species_with_taxainfo_diet_edited.csv")                                       
  sum(is.na(zall$Diet.5Cat)) # 5% not identified diet
  zall$Diet.5Cat[is.na(zall$Diet.5Cat)]<-"Unidentified"
  #table(zall$Diet.5Cat)
  zall<-zall%>%as.data.frame()%>%dplyr::select(AOU,Diet.5Cat)
  
  
  tempo<-data$Corl - data$Coru
  indI<-data$posnI
  indI<-which(indI==1,arr.ind = T)
  tempo[indI]<-NA
  diag(tempo)<-NA
  colnames(tempo)<-rownames(tempo)
  target_sp<-which(colnames(tempo)%notin%c("raresp","covsp"))
  
  z1<-tempo[target_sp,target_sp] #corl-coru matrix (no info for)
  
  #-------------- first, for neg correlated (competition) species pair ---------------------------------------
  z1posnN<-data$posnN[target_sp,target_sp]
  z1_posnN_ind<-which(z1posnN==1, arr.ind=T)
  z1_posnN_ind<-as.data.frame(z1_posnN_ind)
  idr<-z1_posnN_ind$row
  z1_posnN_ind$rowname<-rownames(z1)[idr]
  idc<-z1_posnN_ind$col
  z1_posnN_ind$colname<-rownames(z1)[idc]
  
  z1_posnN_ind$rownamediet<-zall$Diet.5Cat[match(z1_posnN_ind$rowname,zall$AOU)]
  z1_posnN_ind$colnamediet<-zall$Diet.5Cat[match(z1_posnN_ind$colname,zall$AOU)]
  
  z1_posnN_ind$samediet<-ifelse(z1_posnN_ind$rownamediet==z1_posnN_ind$colnamediet,1,0)
  
  # summary for competition: diet-cat contribution?
  summary_diet_competition<-data.frame(type="competition",
                                       samedietcat=NA,
                                       diffdietcat=NA,
                                       FruiNect=NA,
                                       Invertebrate=NA,
                                       Omnivore=NA,
                                       PlantSeed=NA,
                                       Unidentified=NA,
                                       VertFishScav=NA)
  summary_diet_competition$samedietcat<-sum(z1_posnN_ind$samediet==1)
  summary_diet_competition$diffdietcat<-sum(z1_posnN_ind$samediet==0)
  tempo_tbl<-subset(z1_posnN_ind,z1_posnN_ind$samediet==1)
  summary_diet_competition$FruiNect<-sum(tempo_tbl$rownamediet=="FruiNect")
  summary_diet_competition$Invertebrate<-sum(tempo_tbl$rownamediet=="Invertebrate")
  summary_diet_competition$Omnivore<-sum(tempo_tbl$rownamediet=="Omnivore")
  summary_diet_competition$PlantSeed<-sum(tempo_tbl$rownamediet=="PlantSeed")
  summary_diet_competition$Unidentified<-sum(tempo_tbl$rownamediet=="Unidentified")
  summary_diet_competition$VertFishScav<-sum(tempo_tbl$rownamediet=="VertFishScav")
  
  #----------------- summary for synchrony: diet-cat contribution? ---------------
  tempo<-data$Corl - data$Coru
  indI<-data$posnI
  indI<-which(indI==1,arr.ind = T)
  tempo[indI]<-NA
  diag(tempo)<-NA
  indN<-data$posnN
  indN<-which(indN==1,arr.ind = T)
  tempo[indN]<-NA
  colnames(tempo)<-rownames(tempo)
  target_sp<-which(colnames(tempo)%notin%c("raresp","covsp"))
  
  z1syn<-tempo[target_sp,target_sp]
  z1syn<-as.matrix(z1syn)
  z1posn_syn<-which(!is.na(z1syn), arr.ind=T)
  LmU<-z1syn[z1posn_syn]
  z1posn_syn<-as.data.frame(z1posn_syn)
  z1posn_syn$LmU<-LmU
  z1posn_syn$LU<-ifelse(z1posn_syn$LmU>0,"LT","UT")
  idr<-z1posn_syn$row
  z1posn_syn$rowname<-rownames(z1syn)[idr]
  idc<-z1posn_syn$col
  z1posn_syn$colname<-rownames(z1syn)[idc]
  
  z1posn_syn$rownamediet<-zall$Diet.5Cat[match(z1posn_syn$rowname,zall$AOU)]
  z1posn_syn$colnamediet<-zall$Diet.5Cat[match(z1posn_syn$colname,zall$AOU)]
  
  z1posn_syn$samediet<-ifelse(z1posn_syn$rownamediet==z1posn_syn$colnamediet,1,0)
  
  # summary for synchrony: diet-cat contribution?
  summary_diet_syn<-data.frame(type="synchrony",
                               samedietcat=NA,
                               diffdietcat=NA,
                               FruiNect=NA,
                               Invertebrate=NA,
                               Omnivore=NA,
                               PlantSeed=NA,
                               Unidentified=NA,
                               VertFishScav=NA)
  summary_diet_syn$samedietcat<-sum(z1posn_syn$samediet==1)
  summary_diet_syn$diffdietcat<-sum(z1posn_syn$samediet==0)
  tempo_tbl<-subset(z1posn_syn,z1posn_syn$samediet==1)
  summary_diet_syn$FruiNect<-sum(tempo_tbl$rownamediet=="FruiNect")
  summary_diet_syn$Invertebrate<-sum(tempo_tbl$rownamediet=="Invertebrate")
  summary_diet_syn$Omnivore<-sum(tempo_tbl$rownamediet=="Omnivore")
  summary_diet_syn$PlantSeed<-sum(tempo_tbl$rownamediet=="PlantSeed")
  summary_diet_syn$Unidentified<-sum(tempo_tbl$rownamediet=="Unidentified")
  summary_diet_syn$VertFishScav<-sum(tempo_tbl$rownamediet=="VertFishScav")
  
  #----------------------------------------------------------
  spid_aou<-setdiff(rownames(tempo),c("raresp","covsp"))
  targetsp<-zall[match(spid_aou,zall$AOU),]
  sptablediet<-table(targetsp$Diet.5Cat) # the count of target species into diet category
  
  res<-list(sptablediet=sptablediet,
            summary_diet_syn=summary_diet_syn,
            summary_diet_competition=summary_diet_competition)
  
  saveRDS(res,paste(resloc,"output_from_BBS_ta_Diet5Cat.RDS",sep=""))
  return(res)
}


#-------------------------------------------------------------------------------
# now, call the function for all routes

uroutes<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all.RDS")

#initiate to store results
summary_dietcat_allroutes_sptablediet<-c()
summary_dietcat_allroutes_summary_diet_syn<-c()
summary_dietcat_allroutes_summary_diet_competition<-c()

for(i in 1:length(uroutes)){
  route<-uroutes[i]
  resloc<-paste("../../Results/for_BBS/",route,"/",sep="")
  data<-readRDS(paste(resloc,"NonParamStat.RDS",sep=""))
  res<-BBS_ta_Diet5Cat(data,resloc)
  summary_dietcat_allroutes_sptablediet<-rbind(summary_dietcat_allroutes_sptablediet,res$sptablediet)
  summary_dietcat_allroutes_summary_diet_competition<-rbind(summary_dietcat_allroutes_summary_diet_competition,
                                                            res$summary_diet_competition)
  summary_dietcat_allroutes_summary_diet_syn<-rbind(summary_dietcat_allroutes_summary_diet_syn,
                                                    res$summary_diet_syn)
}

rownames(summary_dietcat_allroutes_sptablediet)<-uroutes
rownames(summary_dietcat_allroutes_summary_diet_competition)<-uroutes
rownames(summary_dietcat_allroutes_summary_diet_syn)<-uroutes

summary_BBSta_dietcat<-list(summary_dietcat_allroutes_sptablediet=summary_dietcat_allroutes_sptablediet,
                            summary_dietcat_allroutes_summary_diet_competition=summary_dietcat_allroutes_summary_diet_competition,
                            summary_dietcat_allroutes_summary_diet_syn=summary_dietcat_allroutes_summary_diet_syn)
saveRDS(summary_BBSta_dietcat,"../../Results/for_BBS/summary_BBSta_dietcat.RDS")

pdf("../../Results/for_BBS/summary_dietcat_allroutes.pdf", height=8, width=20)
op<-par(mfrow=c(3,2),mar=c(5,7,5,3),mgp=c(3,1,0))

x<-summary_dietcat_allroutes_summary_diet_syn
x<-x[,2:3]
range(x)
plot(1:2,x[1,],ylim=c(0,600),type="p",xaxt="n",xlab="Diet",ylab="Pairwise interactions\n (all routes)",
     cex.lab=2,cex.axis=2,pch=19,col=rgb(1,0,0,0.3))
axis(1, at=1:2, labels=c("same", "different"),cex.lab=2,cex.axis=2)
legend("top",legend="Synchrony",cex=2,bty="n")
for(i in 2:nrow(x)){
  points(1:2,x[i,],ylim=c(0,50),pch=19,col=rgb(0,0,0,0.3))
}

x<-summary_dietcat_allroutes_summary_diet_syn
x<-x[,-c(1:3)]
range(x)
plot(1:6,x[1,],ylim=c(0,320),type="p",xaxt="n",xlab="Diet",ylab="Target sp. interactions\n (same diet pair)",
     cex.lab=2,cex.axis=2,pch=19,col=rgb(1,0,0,0.3))
axis(1, at=1:6, labels=colnames(x),cex.lab=2,cex.axis=2)
legend("top",legend="Synchrony",cex=2,bty="n")
for(i in 2:nrow(x)){
  points(1:6,x[i,],ylim=c(0,50),pch=19,col=rgb(1,0,0,0.3))
}

x<-summary_dietcat_allroutes_summary_diet_competition
x<-x[,2:3]
range(x)
plot(1:2,x[1,],ylim=c(0,160),type="p",xaxt="n",xlab="Diet",ylab="Pairwise interactions\n (all routes)",
     cex.lab=2,cex.axis=2,pch=19,col=rgb(0,0,1,0.3))
axis(1, at=1:2, labels=c("same", "different"),cex.lab=2,cex.axis=2)
legend("top",legend="Competition",cex=2,bty="n")
for(i in 2:nrow(x)){
  points(1:2,x[i,],ylim=c(0,50),pch=19,col=rgb(0,0,1,0.3))
}

x<-summary_dietcat_allroutes_summary_diet_competition
x<-x[,-c(1:3)]
range(x)
plot(1:6,x[1,],ylim=c(0,80),type="p",xaxt="n",xlab="Diet",ylab="Target sp. interactions\n (same diet pair)",
     cex.lab=2,cex.axis=2,pch=19,col=rgb(0,0,1,0.3))
axis(1, at=1:6, labels=colnames(x),cex.lab=2,cex.axis=2)
legend("top",legend="Competition",cex=2,bty="n")
for(i in 2:nrow(x)){
  points(1:6,x[i,],ylim=c(0,50),pch=19,col=rgb(0,0,1,0.3))
}

x<-summary_dietcat_allroutes_sptablediet
range(x)
plot(1:6,x[1,],ylim=c(0,50),type="p",xaxt="n",xlab="Diet",ylab="Target sp. count\n (all routes)",
     cex.lab=2,cex.axis=2,pch=19,col=rgb(0,0,0,0.3))
axis(1, at=1:6, labels=colnames(x),cex.lab=2,cex.axis=2)
for(i in 2:nrow(x)){
  points(1:6,x[i,],ylim=c(0,50),pch=19,col=rgb(0,0,0,0.3))
}
par(op)
dev.off()













