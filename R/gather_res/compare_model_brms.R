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

mydat<-sm_all[,c("siteid","iCV","iCValt","phi","REALM","phi_skw","nsp","L","U")]
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U
mydat$community<-"NT" #Symmetric
mydat$community[which(mydat$L>abs(mydat$U))]<-"LT"
mydat$community[which(mydat$L<abs(mydat$U))]<-"UT"
#mydat<-mydat%>%filter(community%in%c("LT","UT"))

mydat<-mydat%>%rename(
  stability=iCV,
  stability_skw=iCValt,
  VR=phi,
  SR=phi_skw,
  R=nsp)
mydat<-na.omit(mydat) # one community G8554 has dominant target sp. varying indep with each other, so 
# SR was not calculated - VR showing compensatory dynamics (<1) due to other common sp not considered
# in tail-analysis.

#==============================================================================
# first, we should scale the variables we are going to consider
# scaled quantity = (actual quantity -mean(actual quantity))/sd(actual quantity)
mydat<-mydat%>%select(stability,stability_skw,R,VR,SR,A,uniA,REALM,community)
mydat_scaled<-mydat
mydat_scaled$stability<-scale(mydat_scaled$stability)
mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
mydat_scaled$R<-scale(mydat_scaled$R)
mydat_scaled$VR<- scale(mydat_scaled$VR) 
mydat_scaled$SR<- scale(mydat_scaled$SR)
mydat_scaled$A<- scale(mydat_scaled$A)
mydat_scaled$uniA<- scale(mydat_scaled$uniA)
#===============================================================================

#----------- is there any difference between null and full model, and also between habitats --------
model1<-lm(stability ~ (R + VR)*REALM, mydat_scaled) # R:REALM + VR:REALM +REALM
model2<-lm(stability ~ (R + VR + A)*REALM, mydat_scaled) 
model3<-lm(stability ~ (R + VR + A + uniA)*REALM, mydat_scaled) 
model4<-lm(stability ~ (R + VR + A + uniA + SR)*REALM, mydat_scaled) 
model5<-lm(stability ~ (R + VR + SR)*REALM, mydat_scaled)
anova(model1,model2,model3,model4,model5)
AIC(model1,model2,model3,model4,model5)

cat(" ------ adjusted Rsq value for lm(stability ~ (R + VR)*REALM, mydat_scaled)---------", "\n")
summary(model1)$adj.r.squared
cat(" ------ adjusted Rsq value for lm(stability ~ (R + VR +A)*REALM, mydat_scaled)---------","\n")
summary(model2)$adj.r.squared
cat(" ------ adjusted Rsq value for lm(stability ~ (R + VR +A +uniA)*REALM, mydat_scaled)---------", "\n")
summary(model3)$adj.r.squared
cat(" ------ adjusted Rsq value for lm(stability ~ (R + VR +A +uniA +SR)*REALM, mydat_scaled)---------", "\n")
summary(model4)$adj.r.squared
cat(" ------ adjusted Rsq value for lm(stability ~ (R + VR +SR)*REALM, mydat_scaled)---------", "\n")
summary(model5)$adj.r.squared

#car::vif(lm(stability ~ (R + VR), mydat_scaled)) testing multicolinearity 
#car::vif(lm(stability ~ (R + VR+A+uniA+SR+REALM), mydat_scaled)) testing multicolinearity 

# summary(model1) # to see R2 value
# summary(model2)

#======================================================================================================

# first, we should explore which variables are important in explaining variation in stability
# if we don't consider the realm effect
model_00<-brm(bf(stability ~ (R + VR )),
              data=mydat_scaled,
              chains=4,cores=4,iter=10000,
              control = list(adapt_delta = 0.99, max_treedepth = 20),
              save_pars = save_pars(all = TRUE),sample_prior = T,seed=123)
model_00<-add_criterion(model_00,
                        criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                        moment_match = T,
                        reloo = T)

cat(paste("========= summary for model_00",Sys.time()," ============= \n"))
print(summary(model_00),digits = 3)
#(loo_00<-loo(model_00,moment_match = T,reloo = T))

cat(paste("========= summary for model_01 ",Sys.time()," ============= \n"))
model_01 <- update(model_00, stability ~ (R + VR + A ), 
                   newdata = mydat_scaled)
model_01<-add_criterion(model_01,
                        criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                        moment_match = T,
                        reloo = T)

print(summary(model_01),digits = 3)

cat(paste("========= summary for model_02 ",Sys.time()," ============= \n"))
model_02 <- update(model_00, stability ~ (R + VR + A + uniA), 
                   newdata = mydat_scaled)
model_02<-add_criterion(model_02,
                        criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                        moment_match = T,
                        reloo = T)

print(summary(model_02),digits = 3)

cat(paste("========= summary for model_03 ",Sys.time()," ============= \n"))
model_03 <- update(model_00, stability ~ (R + VR + A + uniA +SR), 
                   newdata = mydat_scaled)
model_03<-add_criterion(model_03,
                        criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                        moment_match = T,
                        reloo = T)

print(summary(model_03),digits = 3)

cat(paste("========= summary for model_04 ",Sys.time()," ============= \n"))
model_04 <- update(model_00, stability ~ (R + VR + SR), 
                   newdata = mydat_scaled)
model_04<-add_criterion(model_04,
                        criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                        moment_match = T,
                        reloo = T)

print(summary(model_04),digits = 3)


#=============== Next, we should explore the realm effect ========================
model_00R<-update(model_00, stability ~ (R + VR + REALM), 
                  newdata = mydat_scaled)
model_00R<-add_criterion(model_00R,
                         criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                         moment_match = T,
                         reloo = T)

cat(paste("========= summary for model_00R ",Sys.time()," ============= \n"))
print(summary(model_00R),digits = 3)

cat(paste("========= summary for model_01R ",Sys.time()," ============= \n"))
model_01R <- update(model_00, stability ~ (R + VR + REALM + A ), 
                    newdata = mydat_scaled)
model_01R<-add_criterion(model_01R,
                         criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                         moment_match = T,
                         reloo = T)

print(summary(model_01R),digits = 3)

cat(paste("========= summary for model_02R ",Sys.time()," ============= \n"))
model_02R <- update(model_00, stability ~ (R + VR+REALM + A + uniA), 
                    newdata = mydat_scaled)
model_02R<-add_criterion(model_02R,
                         criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                         moment_match = T,
                         reloo = T)

print(summary(model_02R),digits = 3)

cat(paste("========= summary for model_03R ",Sys.time()," ============= \n"))
model_03R <- update(model_00, stability ~ (R + VR +REALM+ A + uniA +SR), 
                    newdata = mydat_scaled)
model_03R<-add_criterion(model_03R,
                         criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                         moment_match = T,
                         reloo = T)

print(summary(model_03R),digits = 3)

cat(paste("========= summary for model_04R ",Sys.time()," ============= \n"))
model_04R<-update(model_00, stability ~ (R + VR +REALM + SR), 
                  newdata = mydat_scaled)
model_04R<-add_criterion(model_04R,
                         criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                         moment_match = T,
                         reloo = T)

print(summary(model_04R),digits = 3)

cat(paste("========= comparing between models and loo stats",Sys.time()," ============= \n"))
loo_00<-loo(model_00)
loo_01<-loo(model_01)
loo_02<-loo(model_02)
loo_03<-loo(model_03)
loo_04<-loo(model_04)
loo_00R<-loo(model_00R)
loo_01R<-loo(model_01R)
loo_02R<-loo(model_02R)
loo_03R<-loo(model_03R)
loo_04R<-loo(model_04R)

loo_compare(loo_00, loo_01, loo_02, loo_03, loo_04,loo_00R, loo_01R, loo_02R, loo_03R, loo_04R)
loo_model_weights(loo_00, loo_01, loo_02, loo_03, loo_04,loo_00R, loo_01R, loo_02R, loo_03R, loo_04R)

cat(paste("========= comparing between models loo_R2: with and without REALM",Sys.time()," ============= \n"))
loo_R2(model_00)
loo_R2(model_01)
loo_R2(model_02)
loo_R2(model_03)
loo_R2(model_04)

loo_R2(model_00R)
loo_R2(model_01R)
loo_R2(model_02R)
loo_R2(model_03R)
loo_R2(model_04R)

cat(paste("========= comparing between models bayes_R2: with and without REALM",Sys.time()," ============= \n"))
bayes_R2(model_00)
bayes_R2(model_01)
bayes_R2(model_02)
bayes_R2(model_03)
bayes_R2(model_04)

bayes_R2(model_00R)
bayes_R2(model_01R)
bayes_R2(model_02R)
bayes_R2(model_03R)
bayes_R2(model_04R)
#===============================================================================================
cat(paste("========= starting null_model Bayesian at ",Sys.time()," ============= \n"))
#===============================================================================================
bf_stability0<-bf(stability ~ (R + VR )* REALM) 
bform0 <- bf_stability0 

null_model<-brm(bform0,
                data=mydat_scaled,
                chains=4,cores=4,iter=10000,
                control = list(adapt_delta = 0.99, max_treedepth = 20),
                save_pars = save_pars(all = TRUE),sample_prior = T,seed=123)
null_model<-add_criterion(null_model,
                          criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                          moment_match = T,
                          reloo = T)

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
full_model<-add_criterion(full_model,
                          criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                          moment_match = T,
                          reloo = T)

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

cat("==== compare loo for both model =====","\n")
loo_null<-loo(null_model)
loo_full<-loo(full_model)
loo_compare(loo_null,loo_full)
loo_model_weights(null_model,full_model)

cat("==== R2 for null model: loo =====","\n")
loo_R2(null_model)

cat("==== R2 for full model: loo =====","\n")
loo_R2(full_model)

cat("==== R2 for null model: bayes =====","\n")
bayes_R2(null_model)

cat("==== R2 for full model: bayes =====","\n")
bayes_R2(full_model)
sink()

#========================================================
#plot(full_model)

x<-conditional_effects(full_model
                       #,method = "posterior_predict"
)

#plot conditional effects
pdf("../../Results/gather_res/conditional_effects_brms_univar.pdf",height=12,width=16)
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
#post <- posterior_samples(full_model) # posterior distribution

# level of support
#LOS <- function(x=NULL){
#  out =   (length(which(x > 0)) / length(x))*100
#  return(round(out, 3))
#}
#apply(post, 2, LOS)

#============================

#posterior_summary(full_model)
#print(summary(full_model),digits=3)
# conditional effects mean interaction between two var where other remains fixed(=0)


