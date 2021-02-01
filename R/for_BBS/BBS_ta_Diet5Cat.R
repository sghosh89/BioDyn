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
  
  z1<-tempo[target_sp,target_sp] #corl-coru matrix (indep excluded)
  
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
                                       # within same guild competition
                                       FruiNect=NA, 
                                       Invertebrate=NA,
                                       Omnivore=NA,
                                       PlantSeed=NA,
                                       Unidentified=NA,
                                       VertFishScav=NA,
                                       # within different guild competition
                                       FruiNect_Invertebrate=NA,
                                       FruiNect_Omnivore=NA,
                                       FruiNect_PlantSeed=NA,
                                       FruiNect_VertFishScav=NA,
                                       Invertebrate_Omnivore=NA,
                                       Invertebrate_PlantSeed=NA,
                                       Invertebrate_VertFishScav=NA,
                                       Omnivore_PlantSeed=NA,
                                       Omnivore_VertFishScav=NA,
                                       PlantSeed_VertFishScav=NA)
  
  summary_diet_competition$samedietcat<-sum(z1_posnN_ind$samediet==1)
  summary_diet_competition$diffdietcat<-sum(z1_posnN_ind$samediet==0)
  
  # table for same diet: competition
  tempo_tbl_same<-subset(z1_posnN_ind,z1_posnN_ind$samediet==1)
  summary_diet_competition$FruiNect<-sum(tempo_tbl_same$rownamediet=="FruiNect")
  summary_diet_competition$Invertebrate<-sum(tempo_tbl_same$rownamediet=="Invertebrate")
  summary_diet_competition$Omnivore<-sum(tempo_tbl_same$rownamediet=="Omnivore")
  summary_diet_competition$PlantSeed<-sum(tempo_tbl_same$rownamediet=="PlantSeed")
  summary_diet_competition$Unidentified<-sum(tempo_tbl_same$rownamediet=="Unidentified")
  summary_diet_competition$VertFishScav<-sum(tempo_tbl_same$rownamediet=="VertFishScav")
  
  # table for different diet: competition
  tempo_tbl_diff<-subset(z1_posnN_ind,z1_posnN_ind$samediet==0)
  tempo_tbl_diff$row_coldiet<-paste(tempo_tbl_diff$rownamediet,tempo_tbl_diff$colnamediet,sep="_")
  
  summary_diet_competition$FruiNect_Invertebrate<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_Invertebrate","Invertebrate_FruiNect"))
  summary_diet_competition$FruiNect_Omnivore<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_Omnivore","Omnivore_FruiNect"))
  summary_diet_competition$FruiNect_PlantSeed<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_PlantSeed","PlantSeed_FruiNect")) 
  summary_diet_competition$FruiNect_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_VertFishScav","VertFishScav_FruiNect")) 
  
  summary_diet_competition$Invertebrate_Omnivore<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Invertebrate_Omnivore","Omnivore_Invertebrate"))
  summary_diet_competition$Invertebrate_PlantSeed<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Invertebrate_PlantSeed","PlantSeed_Invertebrate"))
  summary_diet_competition$Invertebrate_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Invertebrate_VertFishScav","VertFishScav_Invertebrate"))
  
  summary_diet_competition$Omnivore_PlantSeed<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Omnivore_PlantSeed","PlantSeed_Omnivore"))
  summary_diet_competition$Omnivore_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Omnivore_VertFishScav","VertFishScav_Omnivore"))
  
  summary_diet_competition$PlantSeed_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("PlantSeed_VertFishScav","VertFishScav_PlantSeed"))
  
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
                               # within same guild competition
                               FruiNect=NA, 
                               Invertebrate=NA,
                               Omnivore=NA,
                               PlantSeed=NA,
                               Unidentified=NA,
                               VertFishScav=NA,
                               # within different guild competition
                               FruiNect_Invertebrate=NA,
                               FruiNect_Omnivore=NA,
                               FruiNect_PlantSeed=NA,
                               FruiNect_VertFishScav=NA,
                               Invertebrate_Omnivore=NA,
                               Invertebrate_PlantSeed=NA,
                               Invertebrate_VertFishScav=NA,
                               Omnivore_PlantSeed=NA,
                               Omnivore_VertFishScav=NA,
                               PlantSeed_VertFishScav=NA)
  
  summary_diet_syn$samedietcat<-sum(z1posn_syn$samediet==1)
  summary_diet_syn$diffdietcat<-sum(z1posn_syn$samediet==0)
  
  # table for same diet: synchrony
  tempo_tbl_same<-subset(z1posn_syn,z1posn_syn$samediet==1)
  summary_diet_syn$FruiNect<-sum(tempo_tbl_same$rownamediet=="FruiNect")
  summary_diet_syn$Invertebrate<-sum(tempo_tbl_same$rownamediet=="Invertebrate")
  summary_diet_syn$Omnivore<-sum(tempo_tbl_same$rownamediet=="Omnivore")
  summary_diet_syn$PlantSeed<-sum(tempo_tbl_same$rownamediet=="PlantSeed")
  summary_diet_syn$Unidentified<-sum(tempo_tbl_same$rownamediet=="Unidentified")
  summary_diet_syn$VertFishScav<-sum(tempo_tbl_same$rownamediet=="VertFishScav")
  
  # table for different diet: synchrony
  tempo_tbl_diff<-subset(z1_posnN_ind,z1_posnN_ind$samediet==0)
  tempo_tbl_diff$row_coldiet<-paste(tempo_tbl_diff$rownamediet,tempo_tbl_diff$colnamediet,sep="_")
  
  summary_diet_syn$FruiNect_Invertebrate<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_Invertebrate","Invertebrate_FruiNect"))
  summary_diet_syn$FruiNect_Omnivore<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_Omnivore","Omnivore_FruiNect"))
  summary_diet_syn$FruiNect_PlantSeed<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_PlantSeed","PlantSeed_FruiNect")) 
  summary_diet_syn$FruiNect_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("FruiNect_VertFishScav","VertFishScav_FruiNect")) 
  
  summary_diet_syn$Invertebrate_Omnivore<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Invertebrate_Omnivore","Omnivore_Invertebrate"))
  summary_diet_syn$Invertebrate_PlantSeed<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Invertebrate_PlantSeed","PlantSeed_Invertebrate"))
  summary_diet_syn$Invertebrate_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Invertebrate_VertFishScav","VertFishScav_Invertebrate"))
  
  summary_diet_syn$Omnivore_PlantSeed<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Omnivore_PlantSeed","PlantSeed_Omnivore"))
  summary_diet_syn$Omnivore_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("Omnivore_VertFishScav","VertFishScav_Omnivore"))
  
  summary_diet_syn$PlantSeed_VertFishScav<-
    sum(tempo_tbl_diff$row_coldiet%in%c("PlantSeed_VertFishScav","VertFishScav_PlantSeed"))
  #----------------------------------------------------------
  spid_aou<-setdiff(rownames(tempo),c("raresp","covsp"))
  targetsp<-zall[match(spid_aou,zall$AOU),]
  sptablediet<-table(targetsp$Diet.5Cat) # the count of target species into diet category
  
  #possible pairwise (syn or comp. would be equal) combination table with the species diet for each route
  
  possible_pairwise<-data.frame(# within same guild 
                               FruiNect=NA, 
                               Invertebrate=NA,
                               Omnivore=NA,
                               PlantSeed=NA,
                               VertFishScav=NA,
                               # within different guild 
                               FruiNect_Invertebrate=NA,
                               FruiNect_Omnivore=NA,
                               FruiNect_PlantSeed=NA,
                               FruiNect_VertFishScav=NA,
                               Invertebrate_Omnivore=NA,
                               Invertebrate_PlantSeed=NA,
                               Invertebrate_VertFishScav=NA,
                               Omnivore_PlantSeed=NA,
                               Omnivore_VertFishScav=NA,
                               PlantSeed_VertFishScav=NA)
  
  # same diet group combo
  possible_pairwise$FruiNect<-unname(sptablediet[1])*(unname(sptablediet[1])-1)*0.5
  possible_pairwise$Invertebrate<-unname(sptablediet[2])*(unname(sptablediet[2])-1)*0.5
  possible_pairwise$Omnivore<-unname(sptablediet[3])*(unname(sptablediet[3])-1)*0.5
  possible_pairwise$PlantSeed<-unname(sptablediet[4])*(unname(sptablediet[4])-1)*0.5
  possible_pairwise$VertFishScav<-unname(sptablediet[6])*(unname(sptablediet[6])-1)*0.5
  
  # different diet group combo
  possible_pairwise$FruiNect_Invertebrate<-unname(sptablediet[1])*(unname(sptablediet[2]))
  possible_pairwise$FruiNect_Omnivore<-unname(sptablediet[1])*(unname(sptablediet[3]))
  possible_pairwise$FruiNect_PlantSeed<-unname(sptablediet[1])*(unname(sptablediet[4]))
  possible_pairwise$FruiNect_VertFishScav<-unname(sptablediet[1])*(unname(sptablediet[6]))
  possible_pairwise$Invertebrate_Omnivore<-unname(sptablediet[2])*(unname(sptablediet[3]))
  possible_pairwise$Invertebrate_PlantSeed<-unname(sptablediet[2])*(unname(sptablediet[4]))
  possible_pairwise$Invertebrate_VertFishScav<-unname(sptablediet[2])*(unname(sptablediet[6]))
  possible_pairwise$Omnivore_PlantSeed<-unname(sptablediet[3])*(unname(sptablediet[4]))
  possible_pairwise$Omnivore_VertFishScav<-unname(sptablediet[3])*(unname(sptablediet[6]))
  possible_pairwise$PlantSeed_VertFishScav<-unname(sptablediet[4])*(unname(sptablediet[6]))
  
  res<-list(sptablediet=sptablediet,
            possible_pairwise=possible_pairwise,
            observed_syn_pairwise=summary_diet_syn,
            observed_comp_pairwise=summary_diet_competition)
  
  saveRDS(res,paste(resloc,"output_from_BBS_ta_Diet5Cat.RDS",sep=""))
  return(res)
}


#-------------------------------------------------------------------------------
# now, call the function for all routes

uroutes<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all.RDS")

#initiate to store results
summary_dietcat_allroutes_sptablediet<-c()
summary_allroutes_obs_diet_comp<-c()
summary_allroutes_obs_diet_syn<-c()
summary_allroutes_possible_pairwise<-c()

for(i in 1:length(uroutes)){
  route<-uroutes[i]
  resloc<-paste("../../Results/for_BBS/",route,"/",sep="")
  data<-readRDS(paste(resloc,"NonParamStat.RDS",sep=""))
  res<-BBS_ta_Diet5Cat(data,resloc)
  summary_dietcat_allroutes_sptablediet<-rbind(summary_dietcat_allroutes_sptablediet,res$sptablediet)
  summary_allroutes_obs_diet_comp<-rbind(summary_allroutes_obs_diet_comp,res$observed_comp_pairwise)
  summary_allroutes_obs_diet_syn<-rbind(summary_allroutes_obs_diet_syn,res$observed_syn_pairwise)
  summary_allroutes_possible_pairwise<-rbind(summary_allroutes_possible_pairwise,res$possible_pairwise)
}

rownames(summary_dietcat_allroutes_sptablediet)<-uroutes
rownames(summary_allroutes_obs_diet_comp)<-uroutes
rownames(summary_allroutes_obs_diet_syn)<-uroutes
rownames(summary_allroutes_possible_pairwise)<-uroutes

summary_BBSta_dietcat<-list(summary_dietcat_allroutes_sptablediet=summary_dietcat_allroutes_sptablediet,
                            summary_allroutes_obs_diet_comp=summary_allroutes_obs_diet_comp,
                            summary_allroutes_obs_diet_syn=summary_allroutes_obs_diet_syn,
                            summary_allroutes_possible_pairwise=summary_allroutes_possible_pairwise)
saveRDS(summary_BBSta_dietcat,"../../Results/for_BBS/summary_BBSta_dietcat.RDS")

########################################################################################################################

# Now ask some question:














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













