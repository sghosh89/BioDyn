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
  
  # the count of target species into diet category
  sptablediet<-data.frame(nsp_FruiNect=NA,
                          nsp_Invertebrate=NA,
                          nsp_Omnivore=NA,
                          nsp_PlantSeed=NA,
                          nsp_VertFishScav=NA)
  sptablediet$nsp_FruiNect<-sum(targetsp$Diet.5Cat=="FruiNect")
  sptablediet$nsp_Invertebrate<-sum(targetsp$Diet.5Cat=="Invertebrate")
  sptablediet$nsp_Omnivore<-sum(targetsp$Diet.5Cat=="Omnivore")
  sptablediet$nsp_PlantSeed<-sum(targetsp$Diet.5Cat=="PlantSeed")
  sptablediet$nsp_VertFishScav<-sum(targetsp$Diet.5Cat=="VertFishScav")
  
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
  possible_pairwise$FruiNect<-sptablediet$nsp_FruiNect*(sptablediet$nsp_FruiNect-1)*0.5
  possible_pairwise$Invertebrate<-sptablediet$nsp_Invertebrate*(sptablediet$nsp_Invertebrate-1)*0.5
  possible_pairwise$Omnivore<-sptablediet$nsp_Omnivore*(sptablediet$nsp_Omnivore-1)*0.5
  possible_pairwise$PlantSeed<-sptablediet$nsp_PlantSeed*(sptablediet$nsp_PlantSeed-1)*0.5
  possible_pairwise$VertFishScav<-sptablediet$nsp_VertFishScav*(sptablediet$nsp_VertFishScav-1)*0.5
  
  # different diet group combo
  possible_pairwise$FruiNect_Invertebrate<-sptablediet$nsp_FruiNect*sptablediet$nsp_Invertebrate
  possible_pairwise$FruiNect_Omnivore<-sptablediet$nsp_FruiNect*sptablediet$nsp_Omnivore
  possible_pairwise$FruiNect_PlantSeed<-sptablediet$nsp_FruiNect*sptablediet$nsp_PlantSeed
  possible_pairwise$FruiNect_VertFishScav<-sptablediet$nsp_FruiNect*sptablediet$nsp_VertFishScav
  possible_pairwise$Invertebrate_Omnivore<-sptablediet$nsp_Invertebrate*sptablediet$nsp_Omnivore
  possible_pairwise$Invertebrate_PlantSeed<-sptablediet$nsp_Invertebrate*sptablediet$nsp_PlantSeed
  possible_pairwise$Invertebrate_VertFishScav<-sptablediet$nsp_Invertebrate*sptablediet$nsp_VertFishScav
  possible_pairwise$Omnivore_PlantSeed<-sptablediet$nsp_Omnivore*sptablediet$nsp_PlantSeed
  possible_pairwise$Omnivore_VertFishScav<-sptablediet$nsp_Omnivore*sptablediet$nsp_VertFishScav
  possible_pairwise$PlantSeed_VertFishScav<-sptablediet$nsp_PlantSeed*sptablediet$nsp_VertFishScav
  
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

#================= whether guild "Invertebrate" has more or less synchrony/competition than other guilds? =======================
ysp<-summary_BBSta_dietcat$summary_dietcat_allroutes_sptablediet # number of species into each guild
x_obs_syn<-summary_BBSta_dietcat$summary_allroutes_obs_diet_syn
x_obs_syn<-x_obs_syn[,c(4:7,9)] # within same guild pairwise syn observered
x_poss<-summary_BBSta_dietcat$summary_allroutes_possible_pairwise
x_poss<-x_poss[,1:5] # within same guild pairwise syn possible
x_expected_syn<-x_obs_syn/x_poss # the ratio

x_obs_comp<-summary_BBSta_dietcat$summary_allroutes_obs_diet_comp
x_obs_comp<-x_obs_comp[,c(4:7,9)] # within same guild pairwise syn observered
x_poss<-summary_BBSta_dietcat$summary_allroutes_possible_pairwise
x_poss<-x_poss[,1:5] # within same guild pairwise competition possible
x_expected_comp<-x_obs_comp/x_poss # the ratio

#x_expected_nonNA<-na.omit(x_expected) # 67 common routes with all finite diet categories expectation
#boxplot(x_expected_nonNA,main=paste(nrow(x_expected_nonNA)," routes",sep=""),
#        ylab="observed/possible pairwise synchrony within same guild")

#-----------------------------------------------------------
pdf("../../Results/for_BBS/Invertebrate_vs_other_withinguild_pairwiseinteraction.pdf", 
    height=10, width=7)
op<-par(mfrow=c(4,2),mar=c(2,5,3,3),mgp=c(3,1,0))

tempo<-x_expected_syn[,c(2,1)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="observed/possible pairwise interaction 
        within same guild", xlab="")
legend("top",legend="synchrony",bty="n")

tempo<-x_expected_comp[,c(2,1)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="")
legend("top",legend="competition",bty="n")


tempo<-x_expected_syn[,c(2,3)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="observed/possible pairwise interaction 
        within same guild", xlab="")
legend("top",legend="synchrony",bty="n")

tempo<-x_expected_comp[,c(2,3)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="")
legend("top",legend="competition",bty="n")


tempo<-x_expected_syn[,c(2,4)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="observed/possible pairwise interaction 
        within same guild", xlab="")
legend("top",legend="synchrony",bty="n")

tempo<-x_expected_comp[,c(2,4)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="")
legend("top",legend="competition",bty="n")


tempo<-x_expected_syn[,c(2,5)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="observed/possible pairwise interaction 
        within same guild", xlab="Guild based on diet")
legend("top",legend="synchrony",bty="n")

tempo<-x_expected_comp[,c(2,5)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="Guild based on diet")
legend("top",legend="competition",bty="n")

par(op)
dev.off()

#===== whether guild "Invertebrate" has show more or less synchrony/competition within itself than cross-guild?=======================

ysp<-summary_BBSta_dietcat$summary_dietcat_allroutes_sptablediet # number of species into each guild
x_obs_syn<-summary_BBSta_dietcat$summary_allroutes_obs_diet_syn
x_obs_syn<-x_obs_syn[,c("Invertebrate",
                        "FruiNect_Invertebrate",
                        "Invertebrate_Omnivore",
                        "Invertebrate_PlantSeed",
                        "Invertebrate_VertFishScav")] # within same guild pairwise syn observered
x_poss<-summary_BBSta_dietcat$summary_allroutes_possible_pairwise
x_poss<-x_poss[,c("Invertebrate",
                  "FruiNect_Invertebrate",
                  "Invertebrate_Omnivore",
                  "Invertebrate_PlantSeed",
                  "Invertebrate_VertFishScav")] # within same guild pairwise syn possible
x_expected_syn<-x_obs_syn/x_poss # the ratio

x_obs_comp<-summary_BBSta_dietcat$summary_allroutes_obs_diet_comp
x_obs_comp<-x_obs_comp[,c("Invertebrate",
                        "FruiNect_Invertebrate",
                        "Invertebrate_Omnivore",
                        "Invertebrate_PlantSeed",
                        "Invertebrate_VertFishScav")] # within same guild pairwise comp. observered
x_expected_comp<-x_obs_comp/x_poss # the ratio

#---------------------------------------
# for common routes (limited number)
#x_expected_syn<-na.omit(x_expected_syn)
#x_expected_comp<-na.omit(x_expected_comp)
#---------------------------------------


pdf("../../Results/for_BBS/Invertebrate_vs_other_crossguild_pairwiseinteraction.pdf", 
    height=8, width=15)
op<-par(mfrow=c(2,4),mar=c(5,6,3,2),mgp=c(3,1,0))

tempo<-x_expected_syn[,c(1,2)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="observed/possible pairwise interaction\n (synchrony)", xlab="")
#legend("top",legend="synchrony",bty="n")

tempo<-x_expected_syn[,c(1,3)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="")
#legend("top",legend="synchrony",bty="n")

tempo<-x_expected_syn[,c(1,4)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="")
#legend("top",legend="synchrony",bty="n")

tempo<-x_expected_syn[,c(1,5)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="")
#legend("top",legend="synchrony",bty="n")


tempo<-x_expected_comp[,c(1,2)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="observed/possible pairwise interaction\n (competition)", xlab="Guild based on diet")
#legend("top",legend="competition",bty="n")

tempo<-x_expected_comp[,c(1,3)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="Guild based on diet")
#legend("top",legend="competition",bty="n")

tempo<-x_expected_comp[,c(1,4)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="Guild based on diet")
#legend("top",legend="competition",bty="n")

tempo<-x_expected_comp[,c(1,5)]
tempo<-na.omit(tempo)
boxplot(tempo,main=paste(nrow(tempo)," routes",sep=""),
        ylab="", xlab="Guild based on diet")
#legend("top",legend="competition",bty="n")

par(op)
dev.off()



