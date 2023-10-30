rm(list=ls())
library(VineCopula)
library(copula)
set.seed(seed=101)
#============================
d<-4
n<-50# no of years
ic<-indepCopula(dim=d)
rc<-rCopula(n=n, ic)
cc<-claytonCopula(dim=d, param=8)
nc<-normalCopula(dim=d,param=0.8)

ac<-rc
ac[,1]<-20+ac[,1]
ac[,2]<-17+ac[,2]
ac[,3]<-18+ac[,3]
ac[,4]<-19+ac[,4]

# independent
i_ac<-ac
c_i_ac<-pobs(i_ac)

op<-par(mar=c(4,6,1,1),mgp=c(2,0.5,0))
matplot(i_ac,type="l",lty=1, ylab="Independent species' \n biomass", 
        xlab="Years", cex.lab=2)
par(op)

# synchrony at high tail because of external driver
ind<-sample(1:n,size=6)# randomly 10 value would have extreme high synchrony
ac_u<-ac
ac_u[ind,]<-rnorm(length(ind)*d, mean=25, sd=0.5)

op<-par(mar=c(4,6,1,1),mgp=c(2,0.5,0))
matplot(ac_u,type="l",lty=1, 
        ylab="Biomass synchrony \n at high values", 
        xlab="Years", cex.lab=2)
par(op)

cop_ac_u<-pobs(ac_u)
op<-par(mar=c(4,4,2,1),mgp=c(2,0.5,0))
plot(cop_ac_u[,1],cop_ac_u[,2], xlim=c(0,1),ylim=c(0,1),
     xlab="Normalized rank, Sp.1",
     ylab="Normalized rank, Sp.2", cex.lab=1.8)# synchrony at upper tail
par(op)


# synchrony at low tail because of external decadal oscillation (bad climate)
ind<-c(10,20,34,40,44)#
ac_l<-ac
ac_l[ind,]<-c(15,16,13,12, 15)

op<-par(mar=c(4,6,1,1),mgp=c(2,0.5,0))
matplot(ac_l,type="l",lty=1, 
        ylab="Biomass synchrony \n at low values", 
        xlab="Years", cex.lab=2)
par(op)

cop_ac_l<-pobs(ac_l)
op<-par(mar=c(4,4,2,1),mgp=c(2,0.5,0))
plot(cop_ac_l[,1],cop_ac_l[,2], xlim=c(0,1),ylim=c(0,1),
     xlab="Normalized rank, Sp.1",
     ylab="Normalized rank, Sp.2",cex.lab=1.8)# synchrony at upper tail
par(op)

# state change
init<-ac
matplot(init,type="l",lty=1, ylab="Indep species", 
        xlab="Years",cex.lab=2)
tempo<-rCopula(n=15,nc)
init[36:50,1]<-21+tempo[,1]
init[36:50,2]<-20+tempo[,2]
init[36:50,3]<-20.8+tempo[,3]
init[36:50,4]<-22+tempo[,4]

op<-par(mar=c(4,6,1,1),mgp=c(2,0.5,0))
matplot(init,type="l",lty=1, ylab="Synchronous \n species", 
        xlab="Years",cex.lab=2)
par(op)

cop_ac_init<-pobs(init)

op<-par(mar=c(4,4,2,1),mgp=c(2,0.5,0))
plot(cop_ac_init[,1],cop_ac_init[,2], xlim=c(0,1),ylim=c(0,1),
     xlab="Normalized rank, Sp.1",
     ylab="Normalized rank, Sp.2",cex.lab=1.8)
par(op)

init<-ac
matplot(init,type="l",lty=1, ylab="Indep species")
tempo<-rCopula(n=15,nc)
init[36:50,1]<-15+tempo[,1]
init[36:50,2]<-10+tempo[,2]
init[36:50,3]<-14+tempo[,3]
init[36:50,4]<-17+tempo[,4]

op<-par(mar=c(4,6,1,1),mgp=c(2,0.5,0))
matplot(init,type="l",lty=1, ylab="Synchronous \n species", 
        xlab="Years",cex.lab=2)
par(op)

cop_ac_init<-pobs(init)

op<-par(mar=c(4,4,2,1),mgp=c(2,0.5,0))
plot(cop_ac_init[,1],cop_ac_init[,2], xlim=c(0,1),ylim=c(0,1),
     xlab="Normalized rank, Sp.1",
     ylab="Normalized rank, Sp.2",cex.lab=1.8)
par(op)