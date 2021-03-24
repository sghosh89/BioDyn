library(tidyverse)
#=====================================================================
# gather stability metric summary for all data you have analyzed

# for BioTIME
sm_BioTIME<-readRDS("../../Results/for_BioTIME/stability_metric.RDS")
sm_BioTIME<-sm_BioTIME%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                                  L,U,f_nind,f_nL,f_nU,f_nneg,
                                  cvsq_real,cvsq_indep,phi,phi_LdM,
                                  skw_real,skw_indep,phi_skw,
                                  iCV,
                                  REALM))
sm_BioTIME$source<-"BioTIME"

# for_BioTIMEx
sm_BioTIMEx<-readRDS("../../Results/for_BioTIMEx/stability_metric.RDS")
sm_BioTIMEx<-sm_BioTIMEx%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                                    L,U,f_nind,f_nL,f_nU,f_nneg,
                                    cvsq_real,cvsq_indep,phi,phi_LdM,
                                    skw_real,skw_indep,phi_skw,
                                    iCV,
                                    REALM))
sm_BioTIMEx$source<-"BioTIMEx"

# for BBS
sm_BBS<-readRDS("../../Results/for_BBS/stability_metric.RDS")
sm_BBS<-sm_BBS%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                          L,U,f_nind,f_nL,f_nU,f_nneg,
                          cvsq_real,cvsq_indep,phi,phi_LdM,
                          skw_real,skw_indep,phi_skw,
                          iCV,
                          REALM))
sm_BBS$source<-"BBS"

# for RivFishTIME
sm_RF<-readRDS("../../Results/for_RivFishTIME/stability_metric.RDS")
sm_RF<-sm_RF%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                        L,U,f_nind,f_nL,f_nU,f_nneg,
                        cvsq_real,cvsq_indep,phi,phi_LdM,
                        skw_real,skw_indep,phi_skw,
                        iCV,
                        REALM))
sm_RF$source<-"RivFishTIME"

# for swisslake
sm_swisslake_phyto<-readRDS("../../Results/for_swisslake/stability_metric_for_phytoplankton.RDS")
sm_swisslake_phyto<-sm_swisslake_phyto%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                                                  L,U,f_nind,f_nL,f_nU,f_nneg,
                                                  cvsq_real,cvsq_indep,phi,phi_LdM,
                                                  skw_real,skw_indep,phi_skw,
                                                  iCV,
                                                  REALM))
sm_swisslake_phyto$source<-"SwissLakePhyto"

sm_swisslake_zoo<-readRDS("../../Results/for_swisslake/stability_metric_for_zooplankton.RDS")
sm_swisslake_zoo<-sm_swisslake_zoo%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                                              L,U,f_nind,f_nL,f_nU,f_nneg,
                                              cvsq_real,cvsq_indep,phi,phi_LdM,
                                              skw_real,skw_indep,phi_skw,
                                              iCV,
                                              REALM))
sm_swisslake_zoo$source<-"SwissLakeZoo"

# for zoop2014
sm_zoop<-readRDS("../../Results/for_zoop_2014/stability_metric.RDS")
sm_zoop<-sm_zoop%>%select(c(siteid,nsp,ens,nind,npos,nL,nU,nneg,
                            L,U,f_nind,f_nL,f_nU,f_nneg,
                            cvsq_real,cvsq_indep,phi,phi_LdM,
                            skw_real,skw_indep,phi_skw,
                            iCV,
                            REALM))
sm_zoop$source<-"Zooplankton2014"

sm_all<-rbind(sm_BioTIME,sm_BioTIMEx,sm_BBS,sm_RF,sm_swisslake_phyto,sm_swisslake_zoo,sm_zoop)
saveRDS(sm_all,"../../Results/gather_res/stability_metric_all.RDS")
