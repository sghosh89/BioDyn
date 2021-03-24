# ref: https://jslefche.github.io/sem_book/
# https://www.youtube.com/watch?v=ezT7VgPZJdk&list=RDCMUCMdihazndR0f9XBoSXWqnYg&index=4

# this is using sem function from lavaan package


library(tidyverse)
library(lavaan)
library(semPlot)

sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")


mydat<-sm_all[,c("iCV","phi","REALM","phi_skw","nsp","L","U")]
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U
mydat$AL<-mydat$L/mydat$A # fraction of L in total Asym
mydat$AU<-abs(mydat$U)/mydat$A
mydat$community<-ifelse((mydat$uniA) >0,"LT","UT")
#mydat$AL<-mydat$L/mydat$A # degree of LT in A: =-1 means full UT dep, = +1 means full LT dep
#mydat$AU<-abs(mydat$U)/mydat$A # degree of LT in A: =-1 means full UT dep, = +1 means full LT dep
myrealm<-"Terrestrial"
mydat<-mydat[mydat$REALM==myrealm,]
mydat<-mydat%>%rename(
       stability=iCV,
       VR=phi,
       SR=phi_skw,
       R=nsp)
#mydat<-mydat%>%filter(SR>0)

# convert to log scale: provided > 0
#mydat$stability<-log(mydat$stability) # stability
#mydat$VR<-log(mydat$VR) # variance ratio
#mydat$SR<-log(mydat$SR) # skewness ratio
#mydat$R<-log(mydat$R) # richness
#mydat$A<-log(mydat$A) # total asymmetry
#mydat$AL<-log(mydat$AL) # Asym in LT
#mydat$AU<-log(mydat$AU) # Asym in UT
#mydat<-mydat[-c(8,34),]

modelh0<-"
stability ~ R + VR 
VR ~ R
"

modelh1<-"
stability ~ R + VR + SR + A +uniA
VR ~ R
SR ~ R + A + uniA 
"
#==================== run the model for h0 =========================================
path_h0<-sem(model=modelh0, data=mydat, estimator="ML", likelihood = "wishart")
# ML= max likelihood estimator for unbiased estimate should specify the likelihood="wishart"
# ref: https://lavaan.ugent.be/tutorial/est.html

# view the output
summary(path_h0,standardized=T,fit.measures=T,rsquare=T,
        #modindices=T
)
fit_h0<-summary(path_h0,standardized=T,fit.measures=T,rsquare=T,
        #modindices=T
        )
fit_h0$FIT["aic"]
# visualize the output
semPaths(path_h0,"std",residuals=F,sizeMan = 4, layout="circle2",
         sizeMan2 = 4,edge.label.cex = 1.2,label.cex=2.5,fade=F,intercepts = F)


#
#semPaths(path, whatLabels = "std", fade=F, layout = "spring",intercepts=F,
#         sizeMan = 4, sizeMan2 = 4,edge.color = "black",
#         edge.label.cex = 1.2,color="gray", label.cex=2.5,
#        residuals=F, posCol=c("blue","red"))
text(-1, 0.1, labels = myrealm)

#==================================================================

#==================== run the model for h0 =========================================
path_h1<-sem(model=modelh1, data=mydat, estimator="ML", likelihood = "wishart")
# ML= max likelihood estimator for unbiased estimate should specify the likelihood="wishart"
# ref: https://lavaan.ugent.be/tutorial/est.html

# view the output
summary(path_h1,standardized=T,fit.measures=T,rsquare=T,
        #modindices=T
)
fit_h1<-summary(path_h1,standardized=T,fit.measures=T,rsquare=T,
                #modindices=T
)
fit_h1$FIT["aic"]
# visualize the output
semPaths(path_h1,"std",residuals=F,sizeMan = 4, layout="circle2",
         sizeMan2 = 4,edge.label.cex = 1.2,label.cex=2.5,fade=F,intercepts = F)

text(-1, 0.1, labels = myrealm)






