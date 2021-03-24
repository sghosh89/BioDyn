rm(list=ls())
library(piecewiseSEM)

# function for piecewise nested sem
# Input:
#  mydat = data
#  myrealm = REALM
get_res_psem<-function(mydat,myrealm){
  mydat<-mydat[mydat$REALM==myrealm,]
  mydat<-mydat%>%rename(
    stability=iCV,
    VR=phi,
    SR=phi_skw,
    R=nsp)
  
  # full model
  model_full<-psem(
    lm(stability ~ R + VR +SR +A +uniA, mydat),
    lm(VR ~ R, mydat),
    lm(SR ~ R + A + uniA, mydat)
  )
  
  #basic model
  model_basic1<-psem(
    lm(stability ~ R + VR, mydat),
    lm(VR ~ R, mydat),
    lm(SR ~ R+A+uniA, mydat)
  )
  
  #model_basic2<-psem(
  #  lm(stability ~ R + VR, mydat),
  #  lm(VR ~ R, mydat),
  #  lm(SR ~ 1, mydat),
  #  lm(A~1,mydat),
  #  lm(uniA~1,mydat)
  #)
  
  # comparing better fit
  aic_full_basic<-AIC(model_full,model_basic1) # full model has lower AIC: so a better fit
  #AIC(model_full,model_basic2) # full model has lower AIC: so a better fit
  
  summary_model_full<-summary(model_full,.progressBar=T,standardize = "scale",test.type="III")
  summary_model_basic<-summary(model_basic1,.progressBar=T,standardize = "scale",test.type="III")
  #summary(model_basic2,.progressBar=T,standardize = "scale",test.type="III") # there is a problem
  # ref: https://www.rdocumentation.org/packages/piecewiseSEM/versions/2.1.2/topics/summary.psem
  
  plot_model_full<-plot(model_full,layout="circle2",show = "std",ns_dashed = T)
  plot_model_basic<-plot(model_basic1,layout="circle2",show = "std",ns_dashed = T)
  
  res<-list(model_full=model_full,
            model_basic=model_basic1,
            summary_model_full=summary_model_full,
            summary_model_basic=summary_model_basic,
            plot_model_full=plot_model_full,
            plot_model_basic=plot_model_basic,
            aic_full_basic=aic_full_basic)
  
}


#####################################################################
sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")


mydat<-sm_all[,c("iCV","phi","REALM","phi_skw","nsp","L","U")]
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U
#mydat$AL<-mydat$L/mydat$A # fraction of L in total Asym
#mydat$AU<-abs(mydat$U)/mydat$A
mydat$community<-ifelse((mydat$uniA) >0,"LT","UT") # LT==UT wrongly predicted as UT here
#mydat$AL<-mydat$L/mydat$A # degree of LT in A: =-1 means full UT dep, = +1 means full LT dep
#mydat$AU<-abs(mydat$U)/mydat$A # degree of LT in A: =-1 means full UT dep, = +1 means full LT dep


# call function for Terrestrial
myrealm<-"Terrestrial"
res_terres<-get_res_psem(mydat=mydat,myrealm=myrealm)
res_terres$summary_model_basic
res_terres$summary_model_full
res_terres$aic_full_basic
res_terres$plot_model_basic
res_terres$plot_model_full


# call function for Freshwater
myrealm<-"Freshwater"
res_freshw<-get_res_psem(mydat=mydat,myrealm=myrealm)
res_freshw$summary_model_full
res_freshw$aic_full_basic
res_freshw$plot_model_basic
res_freshw$plot_model_full




