rm(list=ls())
library(VineCopula)
#=====================================
Corbds<-function(vi,vj,lb,ub){
  #get mean and variance
  vi_mean<-mean(vi)
  vj_mean<-mean(vj)
  var_vi<-var(vi)
  var_vj<-var(vj)
  
  #compute the indices of the points between the bounds
  inds<-which(vi+vj>2*lb & vi+vj<2*ub)
  
  #get the portion of the Spearman
  res<-sum((vi[inds]-vi_mean)*(vj[inds]-vj_mean))/((length(vi)-1)*sqrt(var_vi*var_vj))
  
  return(res)  
}

CorlCoru<-function(vi,vj,nbin){
  
  lb<-0
  ub<-1/nbin
  
  Corl<-Corbds(vi,vj,lb=lb,ub=ub)
  Coru<-Corbds(vi,vj,lb=1-ub,ub=1-lb)
  
  return(c(Corl,Coru))
}
#==================================
#generate data from a seed copula


get_cop<-function(d){
  # case I: insufficient sampling for higher values
  #censor it - randomly delete many of the points that are in the upper right 
  #triangle of the copula plot
  inds<-which(d[,2]> 1-d[,1])
  inds0.8<-inds[sample(1:length(inds),size=floor(0.8*length(inds)),replace=FALSE)]
  dc_u_0.8<-d[-inds0.8,]
  inds0.4<-inds[sample(1:length(inds),size=floor(0.4*length(inds)),replace=FALSE)]
  dc_u_0.4<-d[-inds0.4,]
  inds0.2<-inds[sample(1:length(inds),size=floor(0.2*length(inds)),replace=FALSE)]
  dc_u_0.2<-d[-inds0.2,]
  
  #censor it - randomly delete many of the points that are in the lower left 
  #triangle of the copula plot
  inds2<-which(d[,2]< 1-d[,1])
  inds2_0.8<-inds2[sample(1:length(inds2),size=floor(0.8*length(inds2)),replace=FALSE)]
  dc_l_0.8<-d[-inds2_0.8,]
  inds2_0.4<-inds2[sample(1:length(inds2),size=floor(0.4*length(inds2)),replace=FALSE)]
  dc_l_0.4<-d[-inds2_0.4,]
  inds2_0.2<-inds2[sample(1:length(inds2),size=floor(0.2*length(inds2)),replace=FALSE)]
  dc_l_0.2<-d[-inds2_0.2,]
  
  
  #reperform the ranking
  dc_u_0.8<-pobs(dc_u_0.8)
  dc_u_0.4<-pobs(dc_u_0.4)
  dc_u_0.2<-pobs(dc_u_0.2)
  
  dc_l_0.8<-pobs(dc_l_0.8)
  dc_l_0.4<-pobs(dc_l_0.4)
  dc_l_0.2<-pobs(dc_l_0.2)
  
  res<-list(dc_u_0.8=dc_u_0.8,
            dc_u_0.4=dc_u_0.4,
            dc_u_0.2=dc_u_0.2,
            dc_l_0.8=dc_l_0.8,
            dc_l_0.4=dc_l_0.4,
            dc_l_0.2=dc_l_0.2)
  return(res)
}


pdf("../../Results/sampling_effort_fig.pdf",height=11,width=22)
op<-par(mfrow=c(3,7))

#================ for frank copula ============
set.seed(101)
fc<-BiCopSim(N=500,family=5, par=10) # frank copula

tl<-CorlCoru(vi=fc[,1],vj=fc[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(fc[,1],fc[,2], xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

res_fc<-get_cop(d=fc)
tl<-CorlCoru(vi=res_fc$dc_u_0.8[,1],vj=res_fc$dc_u_0.8[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_fc$dc_u_0.8[,1],res_fc$dc_u_0.8[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_fc$dc_u_0.4[,1],vj=res_fc$dc_u_0.4[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_fc$dc_u_0.4[,1],res_fc$dc_u_0.4[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_fc$dc_u_0.2[,1],vj=res_fc$dc_u_0.2[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_fc$dc_u_0.2[,1],res_fc$dc_u_0.2[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_fc$dc_l_0.8[,1],vj=res_fc$dc_l_0.8[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_fc$dc_l_0.8[,1],res_fc$dc_l_0.8[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_fc$dc_l_0.4[,1],vj=res_fc$dc_l_0.4[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_fc$dc_l_0.4[,1],res_fc$dc_l_0.4[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_fc$dc_l_0.2[,1],vj=res_fc$dc_l_0.2[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_fc$dc_l_0.2[,1],res_fc$dc_l_0.2[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

#============= for clayton copula ==============
#set.seed(101)
cc<-BiCopSim(N=500,family=3, par=10) # clayton copula

tl<-CorlCoru(vi=cc[,1],vj=cc[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(cc[,1],cc[,2], xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

res_cc<-get_cop(d=cc)
tl<-CorlCoru(vi=res_cc$dc_u_0.8[,1],vj=res_cc$dc_u_0.8[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_cc$dc_u_0.8[,1],res_cc$dc_u_0.8[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_cc$dc_u_0.4[,1],vj=res_cc$dc_u_0.4[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_cc$dc_u_0.4[,1],res_cc$dc_u_0.4[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_cc$dc_u_0.2[,1],vj=res_cc$dc_u_0.2[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_cc$dc_u_0.2[,1],res_cc$dc_u_0.2[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_cc$dc_l_0.8[,1],vj=res_cc$dc_l_0.8[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_cc$dc_l_0.8[,1],res_cc$dc_l_0.8[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_cc$dc_l_0.4[,1],vj=res_cc$dc_l_0.4[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_cc$dc_l_0.4[,1],res_cc$dc_l_0.4[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_cc$dc_l_0.2[,1],vj=res_cc$dc_l_0.2[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_cc$dc_l_0.2[,1],res_cc$dc_l_0.2[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

#============= for inverted clayton copula ==============
#set.seed(101)
gc<-1-cc#BiCopSim(N=500,family=13, par=10) # inverted clayton copula

tl<-CorlCoru(vi=gc[,1],vj=gc[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(gc[,1],gc[,2], xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

res_gc<-get_cop(d=gc)
tl<-CorlCoru(vi=res_gc$dc_u_0.8[,1],vj=res_gc$dc_u_0.8[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_gc$dc_u_0.8[,1],res_gc$dc_u_0.8[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_gc$dc_u_0.4[,1],vj=res_gc$dc_u_0.4[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_gc$dc_u_0.4[,1],res_gc$dc_u_0.4[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_gc$dc_u_0.2[,1],vj=res_gc$dc_u_0.2[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_gc$dc_u_0.2[,1],res_gc$dc_u_0.2[,2],col="blue", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_gc$dc_l_0.8[,1],vj=res_gc$dc_l_0.8[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_gc$dc_l_0.8[,1],res_gc$dc_l_0.8[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_gc$dc_l_0.4[,1],vj=res_gc$dc_l_0.4[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_gc$dc_l_0.4[,1],res_gc$dc_l_0.4[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 

tl<-CorlCoru(vi=res_gc$dc_l_0.2[,1],vj=res_gc$dc_l_0.2[,2],nbin=2)
CorlmCoru<-tl[1]-tl[2]
plot(res_gc$dc_l_0.2[,1],res_gc$dc_l_0.2[,2],col="red", xlab="",ylab="",cex.axis=1.6,
     main=paste("Corl= ",round(tl[1],2),", Coru= ",round(tl[2],2),"\n Corl-Coru= ",round(CorlmCoru, 2),sep=""),cex.main=2) 


par(op)
dev.off()







