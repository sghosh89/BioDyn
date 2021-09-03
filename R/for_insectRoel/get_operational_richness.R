# to get total number of species before applying 70% threshold but after aggregation 
# generally, we aggregated from family level downwards until genus level

rm(list=ls())
library(tidyverse)

x<-readRDS("../../DATA/for_insectRoel/20yrFreshwaterData 202106.rds")
xtbl<-x%>%dplyr::distinct(Rank,.keep_all=T)
xtbl<-xtbl[order(xtbl$Rank),]
xtbl<-xtbl%>%dplyr::select(Rank,Level)
xtbl$Rank<-as.factor(xtbl$Rank)

summary_table<-readRDS("../../Results/for_insectRoel/summary_table.RDS")

for(i in 1:nrow(summary_table)){
  cc<-x%>%filter(Datasource_ID==summary_table$STUDY_ID[i] & Plot_ID==summary_table$newsite[i])
  
  cc<-apply(X=cc,MARGIN=2,FUN=tolower)
  cc<-apply(X=cc,MARGIN=2,FUN=trimws)
  cc<-as.data.frame(cc)
  cc$Phylum[cc$Phylum==""]<-"NA.phylum"
  cc$Class[cc$Class==""]<-"NA.class"
  cc$Subclass[cc$Subclass==""]<-"NA.subclass"
  cc$Order[cc$Order==""]<-"NA.order"
  cc$Suborder[cc$Suborder==""]<-"NA.suborder"
  cc$Family[cc$Family==""]<-"NA.family"
  cc$Subfamily[cc$Subfamily==""]<-"NA.subfamily"
  cc$Genus[cc$Genus%in%c("","1")]<-"NA.genus"
  cc$Species[cc$Species%in%c("",".")]<-"NA.species"
  
  cc_table<-as.data.frame(table(cc$Rank))
  colnames(cc_table)[1]<-"Rank"
  
  xtbl2<-left_join(xtbl,cc_table,by="Rank")
  xtbl2$Freq[is.na(xtbl2$Freq)]<-0
  
  write.csv(xtbl2,paste("../../DATA/for_insectRoel/wrangled_data/",
                       summary_table$STUDY_ID[i],"/",summary_table$newsite[i],
                       "/count_on_species_rank_table.csv",sep=""),row.names=F)
  
  #------------------------------------------------------------------------------
  cc_9<-cc%>%filter(Rank==9) # species level identified
  cc_9$gen_sp<-paste(cc_9$Genus,cc_9$Species, sep="_")
  nsp_9<-length(unique(cc_9$gen_sp)) # unique species level identified
  
  cc_n9<-cc%>%filter(Rank!=9)
  # set unique identifier
  cc_n9$uid0<-paste(cc_n9$Phylum,cc_n9$Class,cc_n9$Subclass,cc_n9$Order,cc_n9$Suborder,cc_n9$Family,sep="_") # uid upto family level
  cc_n9$uid1<-paste(cc_n9$uid0,cc_n9$Subfamily,sep="_") # sequential addition of subfamily
  cc_n9$uid2<-paste(cc_n9$uid1,cc_n9$Genus,sep="_") # then add genus
  
  cc_8<-cc_n9%>%filter(Rank==8)
  nsp_8<-length(unique(cc_8$uid2)) # unique genus level identified
  
  cc_7<-cc_n9%>%filter(Rank==7)
  nsp_7<-length(unique(cc_7$uid1)) # unique subfamily level identified
  
  cc_6<-cc_n9%>%filter(Rank==6)
  nsp_6<-length(unique(cc_6$uid0)) # unique family level identified
  
  #------------------------------------------------------------------------------
  # rest of entries in cc_n9 with Rank <=5 were not counted
  
  spconsidered<-sum(xtbl2$Freq[6:9])/sum(xtbl2$Freq) # unique subfamily level identified
  #print(spconsidered)
  
  orich<-nsp_9+nsp_8+nsp_7+nsp_6
  print(orich)
  saveRDS(orich,paste("../../DATA/for_insectRoel/wrangled_data/",
                      summary_table$STUDY_ID[i],"/",summary_table$newsite[i],
                      "/initial_richness.RDS",sep=""))
}

