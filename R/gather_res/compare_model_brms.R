# This script will compare relationships between response (stability) 
# and its predictors (richness, var ratio, skew ratio, asymmetry, dir of asymmetry)
# between two realms: freshwater, terrestrial
# basic Bayesian modeling is used with constant intercept
#===========================================================
rm(list=ls())
library(brms)
library(tidyverse)
#========================================================================================
sink("./console_brms_univar_model.txt", append=TRUE, split=TRUE)
#====================================================================================================
sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")

# exclude marine realm
sm_all$REALM<-as.character(sm_all$REALM)
sm_all<-sm_all%>%filter(REALM%in%c("Freshwater","Terrestrial"))
sm_all$REALM<-as.factor(sm_all$REALM)

mydat<-sm_all[,c("siteid","iCV","phi","REALM","phi_skw","nsp","L","U")]
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U
mydat$community<-"NT" #Symmetric
mydat$community[which(mydat$L>abs(mydat$U))]<-"LT"
mydat$community[which(mydat$L<abs(mydat$U))]<-"UT"
#mydat<-mydat%>%filter(community%in%c("LT","UT"))

mydat<-mydat%>%rename(
  stability=iCV,
  VR=phi,
  SR=phi_skw,
  R=nsp)
mydat<-na.omit(mydat) # one community G8554 has dominant target sp. varying indep with each other, so 
# SR was not calculated - VR showing compensatory dynamics (<1) due to other common sp not considered
# in tail-analysis.

#==============================================================================
# first, we should scale the variables we are going to consider
# scaled quantity = (actual quantity -mean(actual quantity))/sd(actual quantity)
mydat<-mydat%>%select(stability,R,VR,SR,A,uniA,REALM)
mydat_scaled<-mydat
mydat_scaled$stability<-scale(mydat_scaled$stability)
mydat_scaled$R<-scale(mydat_scaled$R)
mydat_scaled$VR<- scale(mydat_scaled$VR) 
mydat_scaled$SR<- scale(mydat_scaled$SR)
mydat_scaled$A<- scale(mydat_scaled$A)
mydat_scaled$uniA<- scale(mydat_scaled$uniA)
#===============================================================================

#----------- is there any difference between null and full model, and also between habitats --------
model1<-lm(stability ~ (R + VR)*REALM, mydat_scaled) # R:REALM + VR:REALM +REALM
model2<-lm(stability ~ (R + VR + SR + A + uniA)*REALM, mydat_scaled)
anova(model1,model2)
AIC(model1,model2)
#car::vif(stability ~ (R + VR), mydat_scaled) testing multicolinearity 
#car::vif(stability ~ (R + VR+ SR + A + uniA), mydat_scaled) testing multicolinearity 

#OK, model 2 including all vars is the best one

#======================================================================================================

#===============================================================================================
cat(paste("========= starting null_model Bayesian at ",Sys.time()," ============= \n"))
#===============================================================================================
bf_stability0<-bf(stability ~ (R + VR )* REALM) 
bform0 <- bf_stability0 

null_model<-brm(bform0,
                data=mydat_scaled,
                chains=4,cores=4,iter=10000,
                control = list(adapt_delta = 0.99, max_treedepth = 20),
                save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model),digits = 3)
saveRDS(null_model,"../../Results/gather_res/null_model.RDS")

cat(paste("========= starting a basic full_model Bayesian at ",Sys.time()," ============= \n"))

#bf_stability<-bf(stability ~ (R + VR + SR + A + uniA )* REALM) 
#bform <- bf_stability 

#full_model<-brm(bform,
#                data=mydat_scaled,
#                chains=4,cores=4,iter=10000,
#                control = list(adapt_delta = 0.99, max_treedepth = 20),
#                save_pars = save_pars(all = TRUE),seed=123)

full_model <- update(null_model, stability ~ (R + VR + SR + A + uniA )* REALM, 
                     newdata = mydat_scaled)
saveRDS(full_model,"../../Results/gather_res/full_model.RDS")
print(summary(full_model),digits = 3)
#plot(full_model)
#plot(conditional_effects(full_model), points = T, ask=F)
# fixef(full_model) #population level effect
#full_model<-readRDS("../../Results/gather_res/full_model.RDS")
Sys.time()

#===============================================================================================
cat(paste("========= finished model Bayesian at ",Sys.time()," ============= \n"))
#===============================================================================================
# compare stats: which model is better?
cat("==== loo for null model =====","\n")
(loo_null<-loo(null_model,moment_match = T,reloo = T))
saveRDS(loo_null,"../../Results/gather_res/loo_null.RDS")
cat("==== loo for full model =====","\n")
(loo_full<-loo(full_model,moment_match = T,reloo = T))
saveRDS(loo_full,"../../Results/gather_res/loo_full.RDS")

cat("==== compare loo for both model =====","\n")
loo_compare(loo_null,loo_full)
loo_model_weights(null_model,full_model)

cat("==== R2 for null model =====","\n")
loo_R2(null_model)

cat("==== R2 for full model =====","\n")
loo_R2(full_model)
sink()

#========================================================
plot(full_model)

x<-conditional_effects(full_model
                       #,method = "posterior_predict"
                       )

#plot conditional effects
pdf("./plot.pdf",height=12,width=16)
op<-par(mfrow=c(3,2),mar=c(3,3,3,3))

p1<-plot(x,points=F,plot=F)[[1]]+theme_classic()

p2<-plot(x,points=F,plot=F)[[2]]+theme_classic()

p3<-plot(x,points=F,plot=F)[[3]]+theme_classic()

p4<-plot(x,points=F,plot=F)[[4]]+theme_classic()

p5<-plot(x,points=F,plot=F)[[5]]+theme_classic()

p6<-plot(x,points=F,plot=F)[[6]]+theme_classic()+
    scale_color_manual(values=c("skyblue","green"))+
    scale_fill_manual(values=c("skyblue","green"))

p7<-plot(x,points=F,plot=F)[[7]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p8<-plot(x,points=F,plot=F)[[8]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p9<-plot(x,points=F,plot=F)[[9]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p10<-plot(x,points=F,plot=F)[[10]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p11<-plot(x,points=F,plot=F)[[11]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, nrow =3, ncol=4)
par(op)
dev.off()

#============================
post <- posterior_samples(full_model) # posterior distribution

# level of support
LOS <- function(x=NULL){
  out =   (length(which(x > 0)) / length(x))*100
  return(round(out, 3))
}
apply(post, 2, LOS)

#============================

#posterior_summary(full_model)
#print(summary(full_model),digits=3)
# conditional effects mean interaction between two var where other remains fixed(=0)


