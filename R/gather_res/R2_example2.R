library(VineCopula)
library(copula)
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
pdf("../../Results/R2_example2.pdf", width=8, height=6)
op<-par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0))

# first, generate two indep timeseries
bc<-BiCopSim(N=50, family=0, par=1)# independent 
bc[,1]<-5+bc[,1]
bc[,2]<-5.5+bc[,2]

plot(1:50, bc[,1], col="red", type="l", ylim=c(5,7), ylab="Abundances", xlab="Years")
lines(1:50, bc[,2], col="blue", type="l" )
text(20,6.8, "Independent timeseries")
text(48,6.8, "(a)", cex=2)



plot(bc[,1],bc[,2], ylab="Abundance, Sp.2", xlab="Abundance, Sp.1")
#R<-cor.test(x=bc[,1],y=bc[,2], method="pearson")# no significant correlation, as p>0.05
S<-cor.test(x=bc[,1],y=bc[,2], method="spearman")# no significant correlation, as p>0.05
#text(5.1,6.2, paste("R=",round(R$estimate,2)))
text(5.2,6.1, paste("S=",round(S$estimate,2),", p=", round(S$p.value, 3)))
text(5.4,6.4, "(b)", cex=2)


ind<-sample(1:50,size=5)# randomly 5 value would have extreme high synchrony
bc[ind,1]<-c(30,31,31.5,32,32.5)
bc[ind,2]<-c(30,31.5,31.4,32.8,32)
plot(1:50, bc[,1], col="red", type="l", ylim=c(4,32), ylab="Abundances", xlab="Years")
abline(v=ind, lty=2, col="gray")
lines(1:50, bc[,2], col="blue", type="l" )
text(26,20, "Correlated \n timeseries")
text(25,30, "(c)", cex=2)



# Now, this would introduce some significant correlation because of those 5 similar values
cor.test(x=bc[,1],y=bc[,2])# correlated

# tansform to copula
cc<-pobs(bc)
plot(cc[,1],cc[,2], xlab="Normalized rank, Sp.1", 
     ylab="Normalized rank, Sp.2", xlim=c(0,1), ylim=c(0,1))
abline(a=1,b=-1)
S<-cor.test(x=cc[,1],y=cc[,2], method="spearman")# correlated
text(0.4,0.95, paste("S=",round(S$estimate,2),", p=", round(S$p.value, 3)))


(ps<-CorlCoru(vi=cc[,1],vj=cc[,2],nbin=2))
# 0.09654262 0.28477791 # see here most of the correlation contribution comes from high ranks or upper tail
sum(ps)# sum of partial Spearman Correlation: lower tail + upper tail
sum(ps)==S$estimate # and total spearman correlation would be equal to sum of partial Spearman correlation from lower and upper tail 

text(0.5,0.1, paste("Corl=",round(ps[1],2)), col="red")
text(0.8,0.4, paste("Coru=",round(ps[2],2)), col="blue")
text(0.1,0.6, "(d)", cex=2)

ps[1]-ps[2]

par(op)
dev.off()










