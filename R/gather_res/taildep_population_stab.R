rm(list=ls())
library(tidyverse)
library(gridExtra)
source("./fn_tacont_vs_iCVsp.R")
#====================
# from 2,668 community data make summary 
# for regressed relationship between x=CV_sp, 
#                                   y=# or Corl-Coru value of times that sp involved in tail-dep.
#======================
#df<-read.csv("../../Results/gather_res/data_summary.csv") # whole data summary
#hist(df$f_nneg/(df$f_nL+df$f_nU))

#========== first, for BioTIME data ====================
r_frs<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")
r_ter<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")

r_BioTIME<-rbind(r_frs,r_ter)

xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
xxm<-xxm%>%dplyr::select(STUDY_ID,REALM)
r_BioTIME<-left_join(r_BioTIME,xxm,"STUDY_ID")

for(i in 1:nrow(r_BioTIME)){
  
  STUDY_ID<-r_BioTIME$STUDY_ID[i]
  newsite<-r_BioTIME$newsite[i]
  realm<-r_BioTIME$REALM[i]
  
  #============== path to read species timeseries matrix =============
  if(STUDY_ID==newsite){
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"_plotlevel/",STUDY_ID,"/",sep="")
  }else{
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"_plotlevel/",STUDY_ID,"/",newsite,"/",sep="")
  }
  givenspmat<-readRDS(paste(mypath,"input_tailanal.RDS",sep=""))
  #============== read nonparam stat results and path to save output files =============
  if(STUDY_ID==newsite){
    mypath2<-paste("../../Results/for_BioTIME/",realm,"_plotlevel/",STUDY_ID,"/",sep="")
  }else{
    mypath2<-paste("../../Results/for_BioTIME/",realm,"_plotlevel/",STUDY_ID,"/",newsite,"/",sep="")
  }
  
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  print(i)
  
}

for(i in 1:nrow(r_BioTIME)){
  STUDY_ID<-r_BioTIME$STUDY_ID[i]
  newsite<-r_BioTIME$newsite[i]
  realm<-r_BioTIME$REALM[i]
  
  if(STUDY_ID==newsite){
    mypath2<-paste("../../Results/for_BioTIME/",realm,"_plotlevel/",STUDY_ID,"/",sep="")
  }else{
    mypath2<-paste("../../Results/for_BioTIME/",realm,"_plotlevel/",STUDY_ID,"/",newsite,"/",sep="")
  }
  
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_BioTIME$pearsoncor[i]<-cortab$pearsoncor
  r_BioTIME$pval[i]<-cortab$pval
  r_BioTIME$is.sig[i]<-cortab$is.sig
  r_BioTIME$nsp[i]<-cortab$nsp
}
r_BioTIME<-r_BioTIME%>%dplyr::select(STUDY_ID,newsite,pearsoncor,pval,is.sig,nsp)
saveRDS(r_BioTIME, "../../Results/for_BioTIME/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for BioTIMEx data ====================
r_combo<-readRDS("../../Results/for_BioTIMEx/summary_table.RDS")
inputmatfile_list<-readRDS("../../Results/for_BioTIMEx/inputloc_table.RDS")
mytable<-inner_join(r_combo,inputmatfile_list,by=c("STUDY_ID","newsite"))

for(i in 1:nrow(mytable)){
  
  givenspmat<-readRDS(mytable$inputloc[i])
  mypath2<-mytable$resloc[i]
    
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  print(i)
  
}

for(i in 1:nrow(mytable)){
  mypath2<-mytable$resloc[i]
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  mytable$pearsoncor[i]<-cortab$pearsoncor
  mytable$pval[i]<-cortab$pval
  mytable$is.sig[i]<-cortab$is.sig
  mytable$nsp[i]<-cortab$nsp
}

r_BioTIMEx<-mytable%>%dplyr::select(STUDY_ID,newsite,pearsoncor,pval,is.sig,nsp)
saveRDS(r_BioTIMEx, "../../Results/for_BioTIMEx/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for RivFishTIME data ====================

r_fish<-readRDS("../../Results/for_RivFishTIME/summary_table_detail_version.RDS")

for(i in 1:nrow(r_fish)){
  siteid<-r_fish$siteid[i]
  
  # now for dominant sp only
  givenspmat<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",siteid,"/commonspecies_timeseries.RDS",sep=""))
  
  mypath2<-paste("../../Results/for_RivFishTIME/",siteid,"/",sep="")
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  print(i)
}

for(i in 1:nrow(r_fish)){
  mypath2<-paste("../../Results/for_RivFishTIME/",r_fish$siteid[i],"/",sep="")
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_fish$pearsoncor[i]<-cortab$pearsoncor
  r_fish$pval[i]<-cortab$pval
  r_fish$is.sig[i]<-cortab$is.sig
  r_fish$nsp[i]<-cortab$nsp
}
r_fish<-r_fish%>%dplyr::select(STUDY_ID = HydroBasin, newsite = siteid,
                               pearsoncor,pval,is.sig,nsp)
saveRDS(r_fish, "../../Results/for_RivFishTIME/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for BBS data ====================

r_BBS<-readRDS("../../Results/for_BBS/summary_table_detail_version.RDS")

for(i in 1:nrow(r_BBS)){
  
  mypath<-paste("../../DATA/for_BBS/wrangled_data/",r_BBS$siteid[i],"/",sep="")
  givenspmat<-readRDS(paste(mypath,"input_mat_for_tailanal.RDS",sep=""))
  
  mypath2<-paste("../../Results/for_BBS/",r_BBS$siteid[i],"/",sep="")
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  
  print(i)
}

for(i in 1:nrow(r_BBS)){
  mypath2<-paste("../../Results/for_BBS/",r_BBS$siteid[i],"/",sep="")
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_BBS$pearsoncor[i]<-cortab$pearsoncor
  r_BBS$pval[i]<-cortab$pval
  r_BBS$is.sig[i]<-cortab$is.sig
  r_BBS$nsp[i]<-cortab$nsp
}

r_BBS<-r_BBS%>%dplyr::select(STUDY_ID = Stratum_name,newsite = siteid,
                               pearsoncor,pval,is.sig,nsp)
saveRDS(r_BBS, "../../Results/for_BBS/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for insectRoel data ====================
r_insect<-readRDS("../../Results/for_insectRoel/summary_table_detail_version.RDS")

for(i in 1:nrow(r_insect)){
  mypath<-paste("../../DATA/for_insectRoel/wrangled_data/",r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/",sep="")
  givenspmat<-readRDS(paste(mypath,"inputmat_for_tailanal.RDS",sep=""))
  
  mypath2<-paste("../../Results/for_insectRoel/",r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/",sep="")
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  
  print(i)
}

for(i in 1:nrow(r_insect)){
  mypath2<-paste("../../Results/for_insectRoel/",r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/",sep="")
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_insect$pearsoncor[i]<-cortab$pearsoncor
  r_insect$pval[i]<-cortab$pval
  r_insect$is.sig[i]<-cortab$is.sig
  r_insect$nsp[i]<-cortab$nsp
}

r_insect<-r_insect%>%dplyr::select(STUDY_ID,newsite,
                             pearsoncor,pval,is.sig,nsp)
r_insect<-r_insect[-1,]# drop the duplicate site 478
saveRDS(r_insect, "../../Results/for_insectRoel/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for swissLake, phytopl data ====================
r_phyto<-readRDS("../../Results/for_swisslake/summary_table_phytoplankton.RDS")

for(i in 1:nrow(r_phyto)){
  siteid<-r_phyto$siteid[i]
  givenspmat<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",siteid,".RDS",sep=""))
  
  mypath2<-paste("../../Results/for_swisslake/",siteid,"/",sep="")
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  
  print(i)
}

for(i in 1:nrow(r_phyto)){
  siteid<-r_phyto$siteid[i]
  mypath2<-paste("../../Results/for_swisslake/",siteid,"/",sep="")
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_phyto$pearsoncor[i]<-cortab$pearsoncor
  r_phyto$pval[i]<-cortab$pval
  r_phyto$is.sig[i]<-cortab$is.sig
  r_phyto$nsp[i]<-cortab$nsp
}

r_phyto<-r_phyto%>%dplyr::select(STUDY_ID=siteid,newsite=siteid,
                                   pearsoncor,pval,is.sig,nsp)
saveRDS(r_phyto, "../../Results/for_swisslake/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for swissLake, zoopl data ====================
r_zoo<-readRDS("../../Results/for_swisslake/summary_table_zooplankton.RDS")

for(i in 1:nrow(r_zoo)){
  siteid<-r_zoo$siteid[i]
  if(siteid=="LU"){
    siteid<-"LU_site3A01"
  }
  givenspmat<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_",siteid,".RDS",sep=""))
  
  mypath2<-paste("../../Results/for_swisslake/zooplankton/zoo_",r_zoo$siteid[i],"/",sep="")
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  
  print(i)
  
}

for(i in 1:nrow(r_zoo)){
  mypath2<-paste("../../Results/for_swisslake/zooplankton/zoo_",r_zoo$siteid[i],"/",sep="")
  
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_zoo$pearsoncor[i]<-cortab$pearsoncor
  r_zoo$pval[i]<-cortab$pval
  r_zoo$is.sig[i]<-cortab$is.sig
  r_zoo$nsp[i]<-cortab$nsp
}

r_zoo<-r_zoo%>%dplyr::select(STUDY_ID=siteid,newsite=siteid,
                                 pearsoncor,pval,is.sig,nsp)
saveRDS(r_zoo, "../../Results/for_swisslake/zooplankton/summary_for_cont_ta_vs_iCVeachsp.RDS")

#========== now, for zoop2014 data ====================

r_zoop<-readRDS("../../Results/for_zoop_2014/summary_table.RDS")

for(i in 1:nrow(r_zoop)){
  mypath<-paste("../../DATA/for_zoop_2014/wrangled_data/",r_zoop$siteid[i],"/",sep="")
  givenspmat<-readRDS(paste(mypath,"inputmat_for_tailanal.RDS",sep=""))
  
  mypath2<-paste("../../Results/for_zoop_2014/",r_zoop$siteid[i],"/",sep="")
  fn_tacont_vs_iCVsp(givenspmat=givenspmat, mypath2=mypath2)
  
  print(i)
}  

for(i in 1:nrow(r_zoop)){
  mypath2<-paste("../../Results/for_zoop_2014/",r_zoop$siteid[i],"/",sep="")
  cortab<-readRDS(paste(mypath2,"species_TA_vs_iCV_each_sp_corsummary.RDS",sep=""))
  r_zoop$pearsoncor[i]<-cortab$pearsoncor
  r_zoop$pval[i]<-cortab$pval
  r_zoop$is.sig[i]<-cortab$is.sig
  r_zoop$nsp[i]<-cortab$nsp
}

r_zoop<-r_zoop%>%dplyr::select(STUDY_ID=siteid,newsite=siteid,
                             pearsoncor,pval,is.sig,nsp)
saveRDS(r_zoop, "../../Results/for_zoop_2014/summary_for_cont_ta_vs_iCVeachsp.RDS")

# ============== gather all data ===================
r_BioTIME<-readRDS("../../Results/for_BioTIME/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_BioTIMEx<-readRDS("../../Results/for_BioTIMEx/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_BBS<-readRDS("../../Results/for_BBS/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_fish<-readRDS("../../Results/for_RivFishTIME/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_phyto<-readRDS("../../Results/for_swisslake/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_zoo<-readRDS("../../Results/for_swisslake/zooplankton/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_zoop<-readRDS("../../Results/for_zoop_2014/summary_for_cont_ta_vs_iCVeachsp.RDS")
r_insect<-readRDS("../../Results/for_insectRoel/summary_for_cont_ta_vs_iCVeachsp.RDS")

r_all<-rbind(r_BioTIME,r_BioTIMEx,r_BBS,r_fish,r_phyto,r_zoo,r_zoop,r_insect)

sum(r_all$is.sig, na.rm=T)/nrow(r_all)
# only 6.5% data showed sig. linear relationship

# consider communities with atleast 10 sp.
r_allnsp10<-r_all%>%filter(nsp>=10)
sum(r_allnsp10$is.sig)/nrow(r_allnsp10)
# only 8.7% data showed sig. linear relationship

r_allnsp10sig<-r_allnsp10%>%filter(is.sig==1)
hist(r_allnsp10sig$pearsoncor)
sum(r_allnsp10sig$pearsoncor>0)# 85
sum(r_allnsp10sig$pearsoncor<0) #74


