# Here we want to do a scatter plot using all community data
# stability=mean/sd of total community abundance or biomass on y axis
# and mean(community biomass) as x axis
rm(list=ls())
library(tidyverse)
library(ggpubr)
library(BiodiversityR)

#========== first, for BioTIME data ====================
r_frs<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")
r_ter<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")
#r_ter<-r_ter%>%filter(f_nind!=1) # remove community with all independent interactions

r_BioTIME<-rbind(r_frs,r_ter)

xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
xxm<-xxm%>%dplyr::select(STUDY_ID,REALM)
r_BioTIME<-left_join(r_BioTIME,xxm,"STUDY_ID")

r_BioTIME$iCV_allsp<-NA # mean/sd for all sp
r_BioTIME$iCV<-NA # mean/sd only for dominant sp
r_BioTIME$mean.cb_allsp<-NA # mean community biomass, all sp
r_BioTIME$mean.cb<-NA # mean community biomass, only for dominant sp

for(i in 1:nrow(r_BioTIME)){
  
  STUDY_ID<-r_BioTIME$STUDY_ID[i]
  newsite<-r_BioTIME$newsite[i]
  realm<-r_BioTIME$REALM[i]
  if(STUDY_ID==newsite){
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"_plotlevel/",STUDY_ID,"/",sep="")
  }else{
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"_plotlevel/",STUDY_ID,"/",newsite,"/",sep="")
  }
  
  
  m<-readRDS(paste(mypath,"input_tailanal.RDS",sep=""))
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_BioTIME$iCV_allsp[i]<-iCV_allsp
  r_BioTIME$mean.cb_allsp[i]<-mean(tot_quantity)
  
  #========= now remove rare sp ===========
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_BioTIME$iCV[i]<-iCV
  
  r_BioTIME$mean.cb[i]<-mean(tot_quantity)
}

#========== now, for BioTIMEx data ====================
r_combo<-readRDS("../../Results/for_BioTIMEx/summary_table.RDS")
inputmatfile_list<-readRDS("../../Results/for_BioTIMEx/inputloc_table.RDS")
mytable<-inner_join(r_combo,inputmatfile_list,by=c("STUDY_ID","newsite"))
inputmatfile_list<-mytable$inputloc
r_combo$iCV_allsp<-NA
r_combo$iCV<-NA
r_combo$mean.cb_allsp<-NA
r_combo$mean.cb<-NA

for(i in 1:nrow(r_combo)){
  
  m<-readRDS(inputmatfile_list[i])
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_combo$iCV_allsp[i]<-iCV_allsp
  r_combo$mean.cb_allsp[i]<-mean(tot_quantity)
  
  #========= now remove rare sp ===========
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_combo$iCV[i]<-iCV
  
  r_combo$mean.cb[i]<-mean(tot_quantity)
}

#========== now, for RivFishTIME data ====================

r_fish<-readRDS("../../Results/for_RivFishTIME/summary_table_detail_version.RDS")
r_fish$iCV_allsp<-NA
r_fish$iCV<-NA
r_fish$mean.cb_allsp<-NA
r_fish$mean.cb<-NA

for(i in 1:nrow(r_fish)){
  siteid<-r_fish$siteid[i]
  
  m<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",siteid,"/allspecies_timeseries.RDS",sep=""))
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_fish$iCV_allsp[i]<-iCV_allsp
  r_fish$mean.cb_allsp[i]<-mean(tot_quantity)
  
  # now for dominant sp only
  m<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",siteid,"/commonspecies_timeseries.RDS",sep=""))
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_fish$iCV[i]<-iCV
  r_fish$mean.cb[i]<-mean(tot_quantity)
}

#========== now, for BBS data ====================

r_BBS<-readRDS("../../Results/for_BBS/summary_table_detail_version.RDS")
r_BBS$iCV_allsp<-NA
r_BBS$iCV<-NA
r_BBS$mean.cb_allsp<-NA
r_BBS$mean.cb<-NA

for(i in 1:nrow(r_BBS)){
  mypath<-paste("../../DATA/for_BBS/wrangled_data/",r_BBS$siteid[i],"/",sep="")
  m<-readRDS(paste(mypath,"input_mat_for_tailanal.RDS",sep=""))
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_BBS$iCV_allsp[i]<-iCV_allsp
  r_BBS$mean.cb_allsp[i]<-mean(tot_quantity)
  
  # now for dominant sp only
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_BBS$iCV[i]<-iCV
  r_BBS$mean.cb[i]<-mean(tot_quantity)
}

#========== now, for insectRoel data ====================
r_insect<-readRDS("../../Results/for_insectRoel/summary_table_detail_version.RDS")
r_insect$iCV_allsp<-NA
r_insect$iCV<-NA
r_insect$mean.cb_allsp<-NA
r_insect$mean.cb<-NA

for(i in 1:nrow(r_insect)){
  mypath<-paste("../../DATA/for_insectRoel/wrangled_data/",r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/",sep="")
  m<-readRDS(paste(mypath,"inputmat_for_tailanal.RDS",sep=""))
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_insect$iCV_allsp[i]<-r_insect$iCV[i]<-iCV_allsp
  r_insect$mean.cb_allsp[i]<-r_insect$mean.cb[i]<-mean(tot_quantity)
}
# Note: for insect data we aggregated on species level, genus level, etc. 
# so we cannot choose rare vs common species

#========== now, for swissLake, phytopl data ====================
r_phyto<-readRDS("../../Results/for_swisslake/summary_table_phytoplankton.RDS")
r_phyto$iCV_allsp<-r_phyto$iCV<-NA
r_phyto$mean.cb_allsp<-r_phyto$mean.cb<-NA

for(i in 1:nrow(r_phyto)){
  siteid<-r_phyto$siteid[i]
  m<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",siteid,".RDS",sep=""))

  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_phyto$iCV_allsp[i]<-iCV_allsp
  r_phyto$mean.cb_allsp[i]<-mean(tot_quantity)
  
  #========= now remove rare sp ===========
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_phyto$iCV[i]<-iCV
  
  r_phyto$mean.cb[i]<-mean(tot_quantity)
  
}

#========== now, for swissLake, zoopl data ====================
r_zoo<-readRDS("../../Results/for_swisslake/summary_table_zooplankton.RDS")
r_zoo$iCV_allsp<-r_zoo$iCV<-NA
r_zoo$mean.cb_allsp<-r_zoo$mean.cb<-NA

for(i in 1:nrow(r_zoo)){
  siteid<-r_zoo$siteid[i]
  if(siteid=="LU"){
    siteid<-"LU_site3A01"
  }
  m<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_",siteid,".RDS",sep=""))

  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_zoo$iCV_allsp[i]<-iCV_allsp
  r_zoo$mean.cb_allsp[i]<-mean(tot_quantity)
  
  #========= now remove rare sp ===========
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_zoo$iCV[i]<-iCV
  
  r_zoo$mean.cb[i]<-mean(tot_quantity)
}

#========== now, for zoop2014 data ====================

r_zoop<-readRDS("../../Results/for_zoop_2014/summary_table.RDS")
r_zoop$iCV_allsp<-r_zoop$iCV<-NA
r_zoop$mean.cb_allsp<-r_zoop$mean.cb<-NA

for(i in 1:nrow(r_zoop)){
  mypath<-paste("../../DATA/for_zoop_2014/wrangled_data/",r_zoop$siteid[i],"/",sep="")
  m<-readRDS(paste(mypath,"inputmat_for_tailanal.RDS",sep=""))
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV_allsp<-mean(tot_quantity)/sd(tot_quantity)
  r_zoop$iCV_allsp[i]<-iCV_allsp
  r_zoop$mean.cb_allsp[i]<-mean(tot_quantity)
  
  #========= now remove rare sp ===========
  id<-which(colnames(m)%in%c("raresp","covsp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  iCV<-mean(tot_quantity)/sd(tot_quantity)
  r_zoop$iCV[i]<-iCV
  
  r_zoop$mean.cb[i]<-mean(tot_quantity)
}

#================== now gather all data ==============
r_BioTIME<-r_BioTIME%>%dplyr::select(c(STUDY_ID,newsite,
                                       iCV,iCV_allsp,
                                       mean.cb,mean.cb_allsp))

r_BioTIMEx<-r_combo%>%dplyr::select(c(STUDY_ID,newsite,
                                       iCV,iCV_allsp,
                                       mean.cb,mean.cb_allsp))

r_BBS<-r_BBS%>%dplyr::select(c(STUDY_ID = Stratum_name,newsite = siteid,
                               iCV,iCV_allsp,
                               mean.cb,mean.cb_allsp))

r_fish<-r_fish%>%dplyr::select(c(STUDY_ID = HydroBasin,newsite = siteid,
                               iCV,iCV_allsp,
                               mean.cb,mean.cb_allsp))

r_phyto$STUDY_ID<-c("lake walensee","lake zurich","lake luzern","lake zurich",
                    "lake sempach","lake hallwil","lake baldegg","lake greifensee")
r_phyto<-r_phyto%>%dplyr::select(c(STUDY_ID,newsite=siteid,
                                   iCV,iCV_allsp,
                                   mean.cb,mean.cb_allsp))

r_zoo$STUDY_ID<-c("lake zurich","lake luzern","lake sempach",
                  "lake hallwil","lake greifensee","lake baldegg")
r_zoo<-r_zoo%>%dplyr::select(c(STUDY_ID,newsite=siteid,
                                 iCV,iCV_allsp,
                                 mean.cb,mean.cb_allsp))

r_zoop<-r_zoop%>%dplyr::select(c(STUDY_ID=siteid,newsite=siteid,
                               iCV,iCV_allsp,
                               mean.cb,mean.cb_allsp))

r_insect<-r_insect%>%dplyr::select(c(STUDY_ID,newsite,
                                     iCV,iCV_allsp,
                                     mean.cb,mean.cb_allsp))
r_insect<-r_insect[-1,]# remove duplicate 478 site

r_all<-rbind(r_BioTIME,r_BioTIMEx,r_BBS,r_fish,r_phyto,r_zoo,r_zoop,r_insect)

saveRDS(r_all,"../../Results/gather_res/stability_considering_allsp.RDS")
############################

# stability plot considering all sp vs common sp (>70% threshold)
g1<-ggplot(r_all, aes(x=iCV,y=iCV_allsp),add="reg.line")+
  geom_point()+
  geom_smooth(method="lm")+theme_bw()+
  xlab("Stability for common species")+
  ylab("Stability for all species")+
  stat_cor(aes(label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0, label.y = 20,col="black")+
  stat_regline_equation(label.x = 0, label.y = 18)+theme_bw(base_size = 18)

pdf("../../Results/gather_res/plot_stability_for_allsp_vs_commonsp.pdf", height=5, width=5)
g1
dev.off()






