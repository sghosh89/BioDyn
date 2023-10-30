rm(list=ls())
library(VineCopula)
library(copula)
library(tidyverse)
library(ggExtra)
library(gridExtra)
set.seed(seed=101)
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

d<-2
n<-50
ic<-indepCopula(dim=d)
rc<-rCopula(n=n, ic)


#cc<-claytonCopula(dim=d, param=8)
#nc<-normalCopula(dim=d,param=0.8)

ac<-rc
ac[,1]<-19.5+ac[,1]
ac[,2]<-19+ac[,2]

# independent
i_ac<-ac
c_i_ac<-pobs(i_ac)
matplot(c_i_ac,type="l",lty=1, ylab="Independent species")
plot(c_i_ac[,1],c_i_ac[,2])# indep


# synchrony at high tail because of external oscillation
ind<-sample(1:n,size=6)# randomly drawn value would have extreme high synchrony
ac_u<-ac
ac_u[ind,]<-rnorm(length(ind)*d, mean=23, sd=0.5)
ac_u<-as.data.frame(ac_u)
# create scatter plot using ggplot() function
plot <- ggplot(ac_u, aes(x=V1, y=V2))+
  geom_point()+theme_bw()+xlab("Abundance, Sp.1")+ylab("Abundance, Sp.2")+
  theme(legend.position="none")
# marginal histogram
plot1 <- ggMarginal(plot, type="histogram",yparams = list(fill = "green4"))
plot1
cor.test(ac_u[,1],ac_u[,2],method="spearman")

plot11<-ggplot(ac_u, aes(x=1:n, y=V1))+
  geom_line()+
  geom_line(data=ac_u, aes(x=1:n, y=V2), col="green4")+
  theme_bw()+xlab("Years")+ylab("Abundance")+
  theme(legend.position="none")
plot11

cop_ac_u<-pobs(ac_u)
cop_ac_u<-as.data.frame(cop_ac_u)
# create scatter plot using ggplot() function
plot <- ggplot(cop_ac_u, aes(x=V1, y=V2))+
  geom_point()+theme_bw()+xlab("Normalized rank, Sp.1")+
  ylab("Normalized rank, Sp.2")+
  theme(legend.position="none")+geom_abline(slope = -1, intercept = 1)
# marginal histogram
plot2 <- ggMarginal(plot, type="histogram",yparams = list(fill = "green4"))
plot2

plot21<-ggplot(cop_ac_u, aes(x=1:n, y=V1))+
  geom_line()+
  geom_line(data=cop_ac_u, aes(x=1:n, y=V2), col="green4")+
  theme_bw()+xlab("Years")+ylab("Normalized rank")+
  theme(legend.position="none")
plot21


grid.arrange( plot11, plot1, plot21, plot2, ncol=2)

CorlCoru(vi=cop_ac_u[,1],vj=cop_ac_u[,2],nbin=2)
sum(CorlCoru(vi=cop_ac_u[,1],vj=cop_ac_u[,2],nbin=2))
cor.test(cop_ac_u[,1],cop_ac_u[,2],method="spearman")


