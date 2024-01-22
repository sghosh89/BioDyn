rm(list=ls())
library(brms)
library(rethinking)
library(ggplot2)
library(gridExtra)

# abbreviation for overall synchrony (variance ratio modified version by Loreau-de Mazancourt: VR_LdM or LM)


pred_func <- function(post, R, VR_LdM, A, REALM){
  res <-  with(post, b_Intercept + b_R * R + b_VR_LdM*VR_LdM + b_A* A + 
                 b_REALMTerrestrial* REALM +
                 `b_R:REALMTerrestrial` * REALM*R + `b_VR_LdM:REALMTerrestrial` * REALM*VR_LdM +
                 `b_A:REALMTerrestrial` * REALM*A )
  
  res<-res*(sd(res))+mean(res) # bring back raw var from scaled var
  
  return(res)
  
}

conditionalplot_100runs_traditional_stability<-function(tag,stability,givenR,givenVR,givenA,xvar){
  
  tab_terres<-as.data.frame(matrix(NA,nrow=length(xvar),ncol=100))
  colnames(tab_terres)<-paste("run_",1:100,sep="")
  tab_freshw<-tab_terres
  
  for(i in 1:100){
    if(stability=="yes"){
      # stability
      full_model<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm_traditional_stability/full_model.RDS",sep=""))
    }
    post <- as_draws_df(full_model) # posterior distribution
    
    if(tag=="varyR"){
      # Terrestrial
      T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = xvar[i],A = givenA,
                                                            VR_LdM = givenVR, REALM = 1))
      meanT <- apply(T.pred, 2, mean)
      tab_terres[,i]<-meanT
      
      #Freshwater
      F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = xvar[i],A = givenA,
                                                            VR_LdM = givenVR, REALM = 0))
      meanF <- apply(F.pred, 2, mean)
      tab_freshw[,i]<-meanF
    }else if(tag=="varyVR"){
      # Terrestrial
      T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = givenR,A = givenA,
                                                            VR_LdM = xvar[i], REALM = 1))
      meanT <- apply(T.pred, 2, mean)
      tab_terres[,i]<-meanT
      
      #Freshwater
      F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = givenR,A = givenA,
                                                            VR_LdM = xvar[i], REALM = 0))
      meanF <- apply(F.pred, 2, mean)
      tab_freshw[,i]<-meanF
    }else{
      # Terrestrial
      T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = givenR,A = xvar[i],
                                                            VR_LdM = givenVR, REALM = 1))
      meanT <- apply(T.pred, 2, mean)
      tab_terres[,i]<-meanT
      
      #Freshwater
      F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = givenR,A = xvar[i],
                                                            VR_LdM = givenVR, REALM = 0))
      meanF <- apply(F.pred, 2, mean)
      tab_freshw[,i]<-meanF
    }
    
  }
  return(list(tab_terres=tab_terres,
              tab_freshw=tab_freshw))
}

# call for richness effect, low level of synchrony, VR=0.1, A=0.1
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.1,givenA=0.1,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.1_A0.1.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.1_A0.1.RDS")

# call for richness effect, both high level of A=15, VR=0.1
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.9,givenA=15,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.9_A15.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.9_A15.RDS")

# call for richness effect, high level of only overall synchrony, VR=0.9, A=0.1
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.9,givenA=0.1,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.9_A0.1.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.9_A0.1.RDS")

# call for richness effect, high level of only taildep synchrony, VR=0.1, A=15
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.1,givenA=15,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.1_A15.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.1_A15.RDS")


#---------richness effect:  first plot for terrestrial ---------------
tab_R_terres1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.1_A0.1.RDS")
meanT<-apply(tab_R_terres1[-1],1,FUN=mean)
tab_R_terres1<-data.frame(R=tab_R_terres1[,1],Pred_Stab=meanT,Label="LM_0.1_TA_0.1")

tab_R_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.9_A15.RDS")
meanT<-apply(tab_R_terres2[-1],1,FUN=mean)
tab_R_terres2<-data.frame(R=tab_R_terres2[,1],Pred_Stab=meanT,Label="LM_0.9_TA_15")

tab_R_terres3<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.9_A0.1.RDS")
meanT<-apply(tab_R_terres3[-1],1,FUN=mean)
tab_R_terres3<-data.frame(R=tab_R_terres3[,1],Pred_Stab=meanT,Label="LM_0.9_TA_0.1")

tab_R_terres4<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_terres_at_LM0.1_A15.RDS")
meanT<-apply(tab_R_terres4[-1],1,FUN=mean)
tab_R_terres4<-data.frame(R=tab_R_terres4[,1],Pred_Stab=meanT,Label="LM_0.1_TA_15")

tab_R_terres<-rbind(tab_R_terres1,tab_R_terres2,tab_R_terres3,tab_R_terres4)

g1_terres_R<-ggplot(tab_R_terres,aes(x=R,y=Pred_Stab,col=as.factor(Label)))+
  geom_line(aes(linetype=as.factor(Label)))+xlab("Richness, R")+ylab("Stability")+
  scale_linetype_manual(values=c("solid","twodash", "dotted","dashed"))+
  scale_color_manual(values=c("skyblue","red","black","blue"))+
  theme_bw()+ theme(legend.position="top")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = c(0.3, 0.7),legend.title = element_blank())

g1_terres_R

# now plot for freshwater
tab_R_freshw1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.1_A0.1.RDS")
meanF<-apply(tab_R_freshw1[-1],1,FUN=mean)
tab_R_freshw1<-data.frame(R=tab_R_freshw1[,1],Pred_Stab=meanF,Label="LM_0.1_TA_0.1")

tab_R_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.9_A15.RDS")
meanF<-apply(tab_R_freshw2[-1],1,FUN=mean)
tab_R_freshw2<-data.frame(R=tab_R_freshw2[,1],Pred_Stab=meanF,Label="LM_0.9_TA_15")

tab_R_freshw3<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.9_A0.1.RDS")
meanF<-apply(tab_R_freshw3[-1],1,FUN=mean)
tab_R_freshw3<-data.frame(R=tab_R_freshw3[,1],Pred_Stab=meanF,Label="LM_0.9_TA_0.1")

tab_R_freshw4<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_richness_100runs_freshw_at_LM0.1_A15.RDS")
meanF<-apply(tab_R_freshw4[-1],1,FUN=mean)
tab_R_freshw4<-data.frame(R=tab_R_freshw4[,1],Pred_Stab=meanF,Label="LM_0.1_TA_15")

tab_R_freshw<-rbind(tab_R_freshw1,tab_R_freshw2,tab_R_freshw3,tab_R_freshw4)

g1_freshw_R<-ggplot(tab_R_freshw,aes(x=R,y=Pred_Stab,col=as.factor(Label)))+
  geom_line(aes(linetype=as.factor(Label)))+xlab("Richness, R")+ylab("Stability")+
  scale_linetype_manual(values=c("solid","twodash", "dotted","dashed"))+
  scale_color_manual(values=c("skyblue","red","black","blue"))+
  theme_bw()+ theme(legend.position="top")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.3, 0.3),legend.title = element_blank())

g1_freshw_R

# ================= call for VR effect ======================

# call for VR effect, both at low values: R=20, TA=15
xvar<-seq(from=0, to=1, by = 0.1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyVR",stability="yes",
                             givenR=20,givenVR=NA,givenA=15,xvar=xvar)
tab_VR_terres<-cbind(VR=xvar,ans$tab_terres)
tab_VR_freshw<-cbind(VR=xvar,ans$tab_freshw)
saveRDS(tab_VR_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_terres_at_R20_A15.RDS")
saveRDS(tab_VR_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_freshw_at_R20_A15.RDS")

# call for VR effect, at values: R=20, TA=0.1
xvar<-seq(from=0, to=1, by = 0.1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyVR",stability="yes",
                             givenR=20,givenVR=NA,givenA=0.1,xvar=xvar)
tab_VR_terres<-cbind(VR=xvar,ans$tab_terres)
tab_VR_freshw<-cbind(VR=xvar,ans$tab_freshw)
saveRDS(tab_VR_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_terres_at_R20_A0.1.RDS")
saveRDS(tab_VR_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_freshw_at_R20_A0.1.RDS")

#--------- overall synchrony effect:  first plot for terrestrial ------
tab_LM_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_terres_at_R20_A15.RDS")
meanT<-apply(tab_LM_terres2[-1],1,FUN=mean)
tab_LM_terres2<-data.frame(LM=tab_LM_terres2[,1],Pred_Stab=meanT,Label="R_20_TA_15")

tab_LM_terres3<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_terres_at_R20_A0.1.RDS")
meanT<-apply(tab_LM_terres3[-1],1,FUN=mean)
tab_LM_terres3<-data.frame(LM=tab_LM_terres3[,1],Pred_Stab=meanT,Label="R_20_TA_0.1")

tab_LM_terres<-rbind(tab_LM_terres2,tab_LM_terres3)

g1_terres_LM<-ggplot(tab_LM_terres,aes(x=LM,y=Pred_Stab,col=as.factor(Label)))+
  geom_line(aes(linetype=as.factor(Label)))+xlab("Overall synchrony, LMS")+ylab("Stability")+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_manual(values=c("red","black"))+
  theme_bw()+ theme(legend.position="top")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.5, 0.7),legend.title = element_blank())

g1_terres_LM

# now plot for freshwater
tab_LM_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_freshw_at_R20_A15.RDS")
meanF<-apply(tab_LM_freshw2[-1],1,FUN=mean)
tab_LM_freshw2<-data.frame(LM=tab_LM_freshw2[,1],Pred_Stab=meanF,Label="R_20_TA_15")

tab_LM_freshw3<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_LM_100runs_freshw_at_R20_A0.1.RDS")
meanF<-apply(tab_LM_freshw3[-1],1,FUN=mean)
tab_LM_freshw3<-data.frame(LM=tab_LM_freshw3[,1],Pred_Stab=meanF,Label="R_20_TA_0.1")

tab_LM_freshw<-rbind(tab_LM_freshw2,tab_LM_freshw3)

g1_freshw_LM<-ggplot(tab_LM_freshw,aes(x=LM,y=Pred_Stab,col=as.factor(Label)))+
  geom_line(aes(linetype=as.factor(Label)))+xlab("Overall Synchrony, LMS")+ylab("Stability")+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_manual(values=c("red","black"))+
  theme_bw()+ theme(legend.position="top")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.7, 0.8),
        legend.title = element_blank())

g1_freshw_LM

#======================
# call for A effect, at high R, LM
xvar = seq(from=0, to=15, by = 1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyA",stability="yes",
                             givenR=20,givenVR=0.9,givenA=NA,xvar=xvar)
tab_A_terres<-cbind(A=xvar,ans$tab_terres)
tab_A_freshw<-cbind(A=xvar,ans$tab_freshw)
saveRDS(tab_A_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_terres_at_R20_LM0.9.RDS")
saveRDS(tab_A_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_freshw_at_R20_LM0.9.RDS")

# call for A effect, at high R, low LM
xvar = seq(from=0, to=15, by = 1)
ans<-conditionalplot_100runs_traditional_stability(tag="varyA",stability="yes",
                             givenR=20,givenVR=0.1,givenA=NA,xvar=xvar)
tab_A_terres<-cbind(A=xvar,ans$tab_terres)
tab_A_freshw<-cbind(A=xvar,ans$tab_freshw)
saveRDS(tab_A_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_terres_at_R20_LM0.1.RDS")
saveRDS(tab_A_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_freshw_at_R20_LM0.1.RDS")

#----------------
# first plot for terrestrial
tab_A_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_terres_at_R20_LM0.9.RDS")
meanT<-apply(tab_A_terres2[-1],1,FUN=mean)
tab_A_terres2<-data.frame(A=tab_A_terres2[,1],Pred_Stab=meanT,Label="R_20_LM_0.9")

tab_A_terres3<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_terres_at_R20_LM0.1.RDS")
meanT<-apply(tab_A_terres3[-1],1,FUN=mean)
tab_A_terres3<-data.frame(A=tab_A_terres3[,1],Pred_Stab=meanT,Label="R_20_LM_0.1")

tab_A_terres<-rbind(tab_A_terres2,tab_A_terres3)

g1_terres_A<-ggplot(tab_A_terres,aes(x=A,y=Pred_Stab,col=as.factor(Label)))+
  geom_line(aes(linetype=as.factor(Label)))+xlab("Synchrony at the extremes, TA")+ylab("Stability")+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_manual(values=c("red","black"))+
  theme_bw()+ theme(legend.position="top")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.75, 0.8),
        legend.title = element_blank())

g1_terres_A

# now plot for freshwater
tab_A_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_freshw_at_R20_LM0.9.RDS")
meanT<-apply(tab_A_freshw2[-1],1,FUN=mean)
tab_A_freshw2<-data.frame(A=tab_A_freshw2[,1],Pred_Stab=meanT,Label="R_20_LM_0.9")

tab_A_freshw3<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditional_stability_A_100runs_freshw_at_R20_LM0.1.RDS")
meanT<-apply(tab_A_freshw3[-1],1,FUN=mean)
tab_A_freshw3<-data.frame(A=tab_A_freshw3[,1],Pred_Stab=meanT,Label="R_20_LM_0.1")

tab_A_freshw<-rbind(tab_A_freshw2,tab_A_freshw3)

g1_freshw_A<-ggplot(tab_A_freshw,aes(x=A,y=Pred_Stab,col=as.factor(Label)))+
  geom_line(aes(linetype=as.factor(Label)))+xlab("Synchrony at the extremes, TA")+ylab("Stability")+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_manual(values=c("red","black"))+
  theme_bw()+ theme(legend.position="top")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.3, 0.7),
        legend.title = element_blank())

g1_freshw_A

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditionalplot_100runs_stability_richness.pdf", height=3, width=3)
g1_terres_R
dev.off()

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditionalplot_100runs_stability_details.pdf", height=3, width=7)
grid.arrange(g1_terres_A, g1_freshw_LM,
             ncol=2)
dev.off()

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm_traditional_stability/conditionalplot_100runs_stability_extra.pdf", height=6, width=7)
grid.arrange(g1_terres_R, g1_freshw_R,
             g1_terres_LM, g1_freshw_LM,
             g1_terres_A,g1_freshw_A,
             ncol=2)
dev.off()

##################################
#extra stuff








