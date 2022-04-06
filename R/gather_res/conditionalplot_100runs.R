rm(list=ls())
library(brms)
library(rethinking)

pred_func <- function(post, R, VR_LdM, A, REALM){
  res <-  with(post, b_Intercept + b_R * R + b_VR_LdM*VR_LdM + b_A* A + 
                 b_REALMTerrestrial* REALM +
                 `b_R:REALMTerrestrial` * REALM*R + `b_VR_LdM:REALMTerrestrial` * REALM*VR_LdM +
                 `b_A:REALMTerrestrial` * REALM*A )
  
  res<-res*(sd(res))+mean(res) # bring back raw var from scaled var
  
  return(res)
  
}

conditionalplot_100runs<-function(tag,stability,givenR,givenVR,givenA,xvar){
  
  tab_terres<-as.data.frame(matrix(NA,nrow=length(xvar),ncol=100))
  colnames(tab_terres)<-paste("run_",1:100,sep="")
  tab_freshw<-tab_terres
  
  for(i in 1:100){
    if(stability=="yes"){
      # stability
      full_model<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm/full_model.RDS",sep=""))
    }else{
      # CV
      full_model<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm/CV_full_model.RDS",sep=""))
    }
    post <- posterior_samples(full_model) # posterior distribution
    
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

# call for richness effect, low level of synchrony, VR=0.1
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.1,givenA=0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A0.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A0.RDS")

ans<-conditionalplot_100runs(tag="varyR",stability="no",
                             givenR=NA,givenVR=0.1,givenA=0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_terres_at_VR0.1_A0.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_freshw_at_VR0.1_A0.RDS")

# call for richness effect, high level of synchrony, VR=0.9
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.9,givenA=0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.9_A0.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.9_A0.RDS")

ans<-conditionalplot_100runs(tag="varyR",stability="no",
                             givenR=NA,givenVR=0.9,givenA=0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_terres_at_VR0.9_A0.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_freshw_at_VR0.9_A0.RDS")

# call for richness effect, low level of A=1, VR=0.1
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.1,givenA=1.0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A1.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A1.RDS")

ans<-conditionalplot_100runs(tag="varyR",stability="no",
                             givenR=NA,givenVR=0.1,givenA=1.0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_terres_at_VR0.1_A1.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_freshw_at_VR0.1_A1.RDS")

# call for richness effect, high level of A=15, VR=0.1
xvar<-seq(from=2, to=40, by = 1)
ans<-conditionalplot_100runs(tag="varyR",stability="yes",
                             givenR=NA,givenVR=0.1,givenA=15.0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A15.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A15.RDS")

ans<-conditionalplot_100runs(tag="varyR",stability="no",
                             givenR=NA,givenVR=0.1,givenA=15.0,xvar=xvar)
tab_R_terres<-cbind(R=xvar,ans$tab_terres)
tab_R_freshw<-cbind(R=xvar,ans$tab_freshw)
saveRDS(tab_R_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_terres_at_VR0.1_A15.RDS")
saveRDS(tab_R_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_freshw_at_VR0.1_A15.RDS")

# call for VR effect, at low R
xvar<-seq(from=0, to=1, by = 0.1)
ans<-conditionalplot_100runs(tag="varyVR",stability="yes",
                             givenR=4,givenVR=0,givenA=0,xvar=xvar)
tab_VR_terres<-cbind(VR=xvar,ans$tab_terres)
tab_VR_freshw<-cbind(VR=xvar,ans$tab_freshw)
saveRDS(tab_VR_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_terres_at_R4_A0.RDS")
saveRDS(tab_VR_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_freshw_at_R4_A0.RDS")

ans<-conditionalplot_100runs(tag="varyVR",stability="no",
                             givenR=4,givenVR=0,givenA=0,xvar=xvar)
tab_VR_terres<-cbind(VR=xvar,ans$tab_terres)
tab_VR_freshw<-cbind(VR=xvar,ans$tab_freshw)
saveRDS(tab_VR_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_VR_100runs_terres_at_R4_A0.RDS")
saveRDS(tab_VR_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_VR_100runs_freshw_at_R4_A0.RDS")

# call for VR effect, at high R
xvar<-seq(from=0, to=1, by = 0.1)
ans<-conditionalplot_100runs(tag="varyVR",stability="yes",
                             givenR=20,givenVR=0,givenA=0,xvar=xvar)
tab_VR_terres<-cbind(VR=xvar,ans$tab_terres)
tab_VR_freshw<-cbind(VR=xvar,ans$tab_freshw)
saveRDS(tab_VR_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_terres_at_R20_A0.RDS")
saveRDS(tab_VR_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_freshw_at_R20_A0.RDS")

ans<-conditionalplot_100runs(tag="varyVR",stability="no",
                             givenR=20,givenVR=0,givenA=0,xvar=xvar)
tab_VR_terres<-cbind(VR=xvar,ans$tab_terres)
tab_VR_freshw<-cbind(VR=xvar,ans$tab_freshw)
saveRDS(tab_VR_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_VR_100runs_terres_at_R20_A0.RDS")
saveRDS(tab_VR_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_VR_100runs_freshw_at_R20_A0.RDS")

# call for A effect, at low R
xvar = seq(from=0, to=20, by = 1)
ans<-conditionalplot_100runs(tag="varyA",stability="yes",
                             givenR=4,givenVR=0,givenA=0,xvar=xvar)
tab_A_terres<-cbind(A=xvar,ans$tab_terres)
tab_A_freshw<-cbind(A=xvar,ans$tab_freshw)
saveRDS(tab_A_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_terres_at_R4_VR0.RDS")
saveRDS(tab_A_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_freshw_at_R4_VR0.RDS")

ans<-conditionalplot_100runs(tag="varyA",stability="no",
                             givenR=4,givenVR=0,givenA=0,xvar=xvar)
tab_A_terres<-cbind(A=xvar,ans$tab_terres)
tab_A_freshw<-cbind(A=xvar,ans$tab_freshw)
saveRDS(tab_A_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_A_100runs_terres_at_R4_VR0.RDS")
saveRDS(tab_A_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_A_100runs_freshw_at_R4_VR0.RDS")

# call for A effect, at high R
xvar = seq(from=0, to=20, by = 1)
ans<-conditionalplot_100runs(tag="varyA",stability="yes",
                             givenR=20,givenVR=0,givenA=0,xvar=xvar)
tab_A_terres<-cbind(A=xvar,ans$tab_terres)
tab_A_freshw<-cbind(A=xvar,ans$tab_freshw)
saveRDS(tab_A_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_terres_at_R20_VR0.RDS")
saveRDS(tab_A_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_freshw_at_R20_VR0.RDS")

ans<-conditionalplot_100runs(tag="varyA",stability="no",
                             givenR=20,givenVR=0,givenA=0,xvar=xvar)
tab_A_terres<-cbind(A=xvar,ans$tab_terres)
tab_A_freshw<-cbind(A=xvar,ans$tab_freshw)
saveRDS(tab_A_terres,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_A_100runs_terres_at_R20_VR0.RDS")
saveRDS(tab_A_freshw,"../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_A_100runs_freshw_at_R20_VR0.RDS")

plotter_conditionalplot_100runs<-function(tab_terres,tab_freshw,xlab,ylab,ylim){
  xvar<-tab_terres[,1]
  tab_t<-tab_terres[,-1]
  tab_f<-tab_freshw[,-1]
  meanT<-apply(tab_t,1,FUN=mean)
  meanF<-apply(tab_f,1,FUN=mean)
  ciT<-apply(tab_t,1, PI, prob= .95)
  ciF<-apply(tab_f,1, PI, prob= .95)
  
  plot(xvar,meanT, pch="", ylim = ylim, xlab=xlab, ylab=ylab)
  abline(h=0,lty=2)
  lines(xvar,meanT, col='green')
  shade(ciT,xvar, col=alpha("green", 0.2))
  lines(xvar,meanF, col='skyblue')
  shade(ciF,xvar, col=alpha("skyblue", 0.2))
}

# Now plot the result
pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditionalplot_100runs_stability.pdf",height=6,width=6)
op<-par(mfrow=c(4,2),mar=c(4,4,1,1),mgp=c(2.5,1,0))

# richness effect, lowVR
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A0.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A0.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="Richness",ylab="Stability",ylim=c(-200,200))

#tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_terres_at_VR0.1_A0.RDS")
#tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_CV_richness_100runs_freshw_at_VR0.1_A0.RDS")
#plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
#                                xlab="Richness",ylab="CV",ylim=c(-200,200))
# richness effect, highVR
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.9_A0.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.9_A0.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="Richness",ylab="Stability",ylim=c(-200,200))

# richness effect, lowA
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A1.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A1.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="Richness",ylab="Stability",ylim=c(-200,200))

# richness effect, highA
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A15.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A15.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="Richness",ylab="Stability",ylim=c(-200,200))

# VR effect, lowR
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_terres_at_R4_A0.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_freshw_at_R4_A0.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="VR",ylab="Stability",ylim=c(-20,40))

# VR effect, highR
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_terres_at_R20_A0.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_freshw_at_R20_A0.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="VR",ylab="Stability",ylim=c(-200,400))

# A effect, lowR
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_terres_at_R4_VR0.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_freshw_at_R4_VR0.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="A",ylab="Stability",ylim=c(-100,40))

# A effect, highR
tab_terres<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_terres_at_R20_VR0.RDS")
tab_freshw<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_freshw_at_R20_VR0.RDS")
plotter_conditionalplot_100runs(tab_terres = tab_terres,tab_freshw=tab_freshw,
                                xlab="A",ylab="Stability",ylim=c(-100,400))
par(op)
dev.off()

#############################

# alternative version of plotting
plotter_conditionalplot_100runsavg<-function(tab_terres1,tab_freshw1,
                                             tab_terres2,tab_freshw2,
                                             xlab,ylab,ylim,legend_text="NA"){
  xvar<-tab_terres1[,1]
  tab_t1<-tab_terres1[,-1]
  tab_f1<-tab_freshw1[,-1]
  meanT1<-apply(tab_t1,1,FUN=mean)
  meanF1<-apply(tab_f1,1,FUN=mean)
  
  tab_t2<-tab_terres2[,-1]
  tab_f2<-tab_freshw2[,-1]
  meanT2<-apply(tab_t2,1,FUN=mean)
  meanF2<-apply(tab_f2,1,FUN=mean)
  
  plot(xvar,meanT1, pch="", ylim = ylim, xlab=xlab, ylab=ylab)
  abline(h=0,lty=3)
  lines(xvar,meanT1, col='green',lty=1)
  lines(xvar,meanF1, col='skyblue',lty=1)
  lines(xvar,meanT2, col='green4', lty=2)
  lines(xvar,meanF2, col='blue', lty=2)
  legend(1, 300, legend=legend_text,
         col=c("green", "skyblue","green4","blue"), 
         lty=c(1,1,2,2), 
         box.lty=0)
}

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditionalplot_100runsavg_stability.pdf",height=5.5,width=6)
op<-par(mfrow=c(2,2),mar=c(4,4,1,1),mgp=c(2.5,1,0))

# stability-diversity relationship: indep. of overall synchrony
tab_terres1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A0.RDS")
tab_freshw1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A0.RDS")
tab_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.9_A0.RDS")
tab_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.9_A0.RDS")
plotter_conditionalplot_100runsavg(tab_terres1 = tab_terres1,tab_freshw1=tab_freshw1,
                                tab_terres2 = tab_terres2,tab_freshw2=tab_freshw2,
                                xlab="Richness",ylab="Stability",ylim=c(-100,400),
                                legend_text = c("VR=0.1, TA=0","VR=0.1, TA=0","VR=0.9, TA=0","VR=0.9, TA=0"))

# stability-diversity relationship: sensitive to tail-dependent synchrony
tab_terres1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A1.RDS")
tab_freshw1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A1.RDS")
tab_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_terres_at_VR0.1_A15.RDS")
tab_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_richness_100runs_freshw_at_VR0.1_A15.RDS")
plotter_conditionalplot_100runsavg(tab_terres1 = tab_terres1,tab_freshw1=tab_freshw1,
                                   tab_terres2 = tab_terres2,tab_freshw2=tab_freshw2,
                                   xlab="Richness",ylab="Stability",ylim=c(-100,400),
                                   legend_text = c("VR=0.1, TA=1","VR=0.1, TA=1","VR=0.1, TA=15","VR=0.1, TA=15"))

# stability-overall synchrony relationship at different richness level
tab_terres1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_terres_at_R4_A0.RDS")
tab_freshw1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_freshw_at_R4_A0.RDS")
tab_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_terres_at_R20_A0.RDS")
tab_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_VR_100runs_freshw_at_R20_A0.RDS")
plotter_conditionalplot_100runsavg(tab_terres1 = tab_terres1,tab_freshw1=tab_freshw1,
                                   tab_terres2 = tab_terres2,tab_freshw2=tab_freshw2,
                                   xlab="Overall synchrony",ylab="Stability",ylim=c(-70,200))


# stability-tail-dep. synchrony relationship at different richness level
tab_terres1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_terres_at_R4_VR0.RDS")
tab_freshw1<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_freshw_at_R4_VR0.RDS")
tab_terres2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_terres_at_R20_VR0.RDS")
tab_freshw2<-readRDS("../../Results/gather_res/res_taxa15/summary_fixed_realm/conditional_stability_A_100runs_freshw_at_R20_VR0.RDS")
plotter_conditionalplot_100runsavg(tab_terres1 = tab_terres1,tab_freshw1=tab_freshw1,
                                   tab_terres2 = tab_terres2,tab_freshw2=tab_freshw2,
                                   xlab="Tail-dependent synchrony",ylab="Stability",ylim=c(-70,200))


par(op)
dev.off()










