library(tidyverse)
#=====================================================================
# gather stability metric summary for all data you have analyzed

#----------------------------- for BioTIME -----------------------------------------------------
sm_BioTIME<-readRDS("../../Results/for_BioTIME/stability_metric_plotlevel.RDS")
sm_BioTIME<-sm_BioTIME%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                  initR,nsp, nind,npos,nL,nU,nneg,
                                  L,U,f_nind,f_nL,f_nU,f_nneg,
                                  cvsq_real,cvsq_indep,phi,phi_LdM,
                                  skw_real,skw_indep,phi_skw,
                                  iCV,iCValt))
sm_BioTIME$source<-"BioTIME"

#----------------------------------- for_BioTIMEx -------------------------------------------
sm_BioTIMEx<-readRDS("../../Results/for_BioTIMEx/stability_metric.RDS")
sm_BioTIMEx<-sm_BioTIMEx%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                    initR,nsp, nind,npos,nL,nU,nneg,
                                    L,U,f_nind,f_nL,f_nU,f_nneg,
                                    cvsq_real,cvsq_indep,phi,phi_LdM,
                                    skw_real,skw_indep,phi_skw,
                                    iCV,iCValt))
sm_BioTIMEx$source<-"BioTIMEx"

#------------------------------------- for BBS -----------------------------------------------
sm_BBS<-readRDS("../../Results/for_BBS/stability_metric.RDS")
sm_BBS$TAXA <-"Birds"
sm_BBS$ORGANISMS <-"Birds"
sm_BBS<-rename(sm_BBS, newsite = siteid) # each siteid is renamed as newsite to be nested within the stratum 
sm_BBS<-rename(sm_BBS, STUDY_ID = Stratum_name) # stratum name renamed as STUDY_ID
sm_BBS<-sm_BBS%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                          initR,nsp, nind,npos,nL,nU,nneg,
                          L,U,f_nind,f_nL,f_nU,f_nneg,
                          cvsq_real,cvsq_indep,phi,phi_LdM,
                          skw_real,skw_indep,phi_skw,
                          iCV,iCValt))
sm_BBS$source<-"BBS"

#-------------------------------- for RivFishTIME ---------------------------------------------
sm_RF<-readRDS("../../Results/for_RivFishTIME/stability_metric.RDS")
sm_RF$TAXA <-"Fish"
sm_RF$ORGANISMS <-"Fish"
sm_RF<-rename(sm_RF, newsite = siteid) # each siteid is renamed as newsite to be nested within the hydrobasin 
sm_RF<-rename(sm_RF, STUDY_ID = HydroBasin) # Hydrobasin renamed as STUDY_ID
sm_RF<-sm_RF%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                        initR,nsp, nind,npos,nL,nU,nneg,
                          L,U,f_nind,f_nL,f_nU,f_nneg,
                          cvsq_real,cvsq_indep,phi,phi_LdM,
                          skw_real,skw_indep,phi_skw,
                          iCV,iCValt))
sm_RF$source<-"RivFishTIME"

#--------------------------------------- for swisslake phyto -----------------------------------------------------
sm_swisslake_phyto<-readRDS("../../Results/for_swisslake/stability_metric_for_phytoplankton.RDS")
sm_swisslake_phyto$TAXA<-"Freshwater plants" # phytoplanktons are tagged as invertebrates in BioTIME?
sm_swisslake_phyto$ORGANISMS<-"Phytoplankton"
sm_swisslake_phyto$newsite<-sm_swisslake_phyto$siteid
sm_swisslake_phyto$STUDY_ID<-c("lake walensee","lake zurich","lake luzern","lake zurich",
                               "lake sempach","lake hallwil","lake baldegg","lake greifensee")
  
sm_swisslake_phyto<-sm_swisslake_phyto%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                                  initR,nsp, nind,npos,nL,nU,nneg,
                                                  L,U,f_nind,f_nL,f_nU,f_nneg,
                                                  cvsq_real,cvsq_indep,phi,phi_LdM,
                                                  skw_real,skw_indep,phi_skw,
                                                  iCV,iCValt))
sm_swisslake_phyto$source<-"SwissLakePhyto"

#--------------------------------------- for swisslake zoo -----------------------------------------------------
sm_swisslake_zoo<-readRDS("../../Results/for_swisslake/stability_metric_for_zooplankton.RDS")
sm_swisslake_zoo$TAXA<-"Freshwater invertebrates"
sm_swisslake_zoo$ORGANISMS<-"Zooplankton"
sm_swisslake_zoo$newsite<-sm_swisslake_zoo$siteid
sm_swisslake_zoo$STUDY_ID<-c("lake zurich","lake luzern","lake sempach",
                             "lake hallwil","lake greifensee","lake baldegg")
sm_swisslake_zoo<-sm_swisslake_zoo%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                              initR,nsp, nind,npos,nL,nU,nneg,
                                              L,U,f_nind,f_nL,f_nU,f_nneg,
                                              cvsq_real,cvsq_indep,phi,phi_LdM,
                                              skw_real,skw_indep,phi_skw,
                                              iCV,iCValt))
sm_swisslake_zoo$source<-"SwissLakeZoo"

#------------------------------- for zoop2014 ----------------------------------------------
sm_zoop<-readRDS("../../Results/for_zoop_2014/stability_metric.RDS")
sm_zoop$TAXA<-"Freshwater invertebrates"
sm_zoop$ORGANISMS<-"Zooplankton"
sm_zoop$STUDY_ID<-sm_zoop$newsite<-sm_zoop$siteid
sm_zoop<-sm_zoop%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                            initR,nsp, nind,npos,nL,nU,nneg,
                            L,U,f_nind,f_nL,f_nU,f_nneg,
                            cvsq_real,cvsq_indep,phi,phi_LdM,
                            skw_real,skw_indep,phi_skw,
                            iCV,iCValt))
sm_zoop$source<-"Zooplankton2014"

#------------------------------- for insectRoel ----------------------------------------------
sm_insect<-readRDS("../../Results/for_insectRoel/stability_metric.RDS")
sm_insect<-sm_insect%>%dplyr::select(c(STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                              initR,nsp, nind,npos,nL,nU,nneg,
                            L,U,f_nind,f_nL,f_nU,f_nneg,
                            cvsq_real,cvsq_indep,phi,phi_LdM,
                            skw_real,skw_indep,phi_skw,
                            iCV,iCValt))
sm_insect$source<-"InsectRoel"

#================================================================================================
sm_all<-rbind(sm_BioTIME,sm_BioTIMEx,sm_BBS,sm_RF,sm_swisslake_phyto,sm_swisslake_zoo,sm_zoop,sm_insect)
sm_all$TAXA<-tolower(sm_all$TAXA)
sm_all$ORGANISMS<-tolower(sm_all$ORGANISMS)
saveRDS(sm_all,"../../Results/gather_res/stability_metric_all.RDS")

#------------------ plot f_nL vs f_nU for both realms ------------------------------

tempo<-sm_all%>%mutate(f_syn=f_nL+f_nU,f_asyn=f_nneg)%>%dplyr::select(f_syn,f_asyn,REALM)
tempo2<-tempo%>%dplyr::select(f=f_syn,REALM)%>%mutate(Type="f_syn")
tempo3<-tempo%>%dplyr::select(f=f_asyn,REALM)%>%mutate(Type="f_asyn")

tempo4<-rbind(tempo2,tempo3)
gp<-ggplot(tempo4, aes(x=REALM, y=f, fill=Type)) +
  geom_boxplot()+
  scale_fill_manual(values=alpha(c("lawngreen","plum2"),0.7))+theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  ylab("Synchrony and Asynchrony")
gp

pdf("../../Results/gather_res/syn_and_asyn_ratio_for_eachrealm.pdf",height=5,width=8)
gp
dev.off()

#------------------ plot f_nL vs f_nU for both realms ------------------------------

sm_all_2<-sm_all%>%dplyr::select(f=f_nL,REALM)%>%mutate(Type="LT")
sm_all_3<-sm_all%>%dplyr::select(f=f_nU,REALM)%>%mutate(Type="UT")
sm_all_4<-rbind(sm_all_2,sm_all_3)
gp<-ggplot(sm_all_4, aes(x=REALM, y=f, fill=Type)) +
  geom_boxplot()+
  scale_fill_manual(values=alpha(c("red","blue"),0.7))+theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  ylab("Fraction in synchrony")
gp

pdf("../../Results/gather_res/f_nL_f_nU_ratio_for_eachrealm.pdf",height=5,width=8)
gp
dev.off()

sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
sm_all$domrich<-(sm_all$nsp/sm_all$initR)*100
ggplot(sm_all,aes(x=domrich,y=iCValt,col=REALM))+geom_point()
cor.test(sm_all$domrich,sm_all$iCValt)












