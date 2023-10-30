rm(list=ls())
library(VineCopula)
library(copula)
library(tidyverse)
library(ggExtra)
library(gridExtra)
#====================
#======compute partial Spearman correlation==========
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

# below is the function to compute "partial Spearman correlation"
CorlCoru<-function(vi,vj,nbin){
  
  lb<-0
  ub<-1/nbin
  
  Corl<-Corbds(vi,vj,lb=lb,ub=ub)
  Coru<-Corbds(vi,vj,lb=1-ub,ub=1-lb)
  
  return(c(Corl,Coru))
}
#==============================
#=======================
set.seed(seed=101)
rotc<-BiCopSim(N=50,family=33, par=-7)
plot(rotc[,1],rotc[,2],xlim=c(0,1),ylim=c(0,1),col="green4",
     xlab="Normalized rank, Sp. 1", 
     ylab= "Normalized rank, Sp. 2")
#BiCopPar2TailDep(family=3, par=7)

plot(rotc[,2],rotc[,1],xlim=c(0,1),ylim=c(0,1),col="green4",
     xlab="Normalized rank, Sp. 2", 
     ylab= "Normalized rank, Sp. 1")


rotc2<-rotc
rotc2[,1]<-rotc2[,2]
rotc2[,2]<-rotc[,1]
plot(rotc2[,1],rotc2[,2],xlim=c(0,1),ylim=c(0,1),col="green4",
     xlab="Normalized rank, Sp. 1", 
     ylab= "Normalized rank, Sp. 2")
#BiCopPar2TailDep(family=3, par=7)

posc<-rotc
posc[,2]<-1-posc[,2]
plot(posc[,1],posc[,2],xlim=c(0,1),ylim=c(0,1), 
     xlab="Normalized rank, Sp. 1", 
     ylab= "Normalized rank, Sp. 2")

poscx<-rotc
poscx[,1]<-1-poscx[,1]
plot(poscx[,1],poscx[,2],xlim=c(0,1),ylim=c(0,1), 
     xlab="Normalized rank, Sp. 1", 
     ylab= "Normalized rank, Sp. 2", col="purple")

# page 61, https://cran.r-project.org/web/packages/VineCopula/VineCopula.pdf

round(CorlCoru(vi=posc[,1],vj=posc[,2],nbin=2),3)
round(CorlCoru(vi=poscx[,1],vj=poscx[,2],nbin=2),3)







