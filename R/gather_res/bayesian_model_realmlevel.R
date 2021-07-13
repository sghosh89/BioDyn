rm(list=ls())
library(brms)
library(tidyverse)
library(performance)

if(!dir.exists("../../Results/gather_res/test")){
  dir.create("../../Results/gather_res/test")
}


#========================================================================================
sink("../../Results/gather_res/test/console_practice_hierarchicalmodel_test.txt", append=TRUE, split=TRUE)
#====================================================================================================


#===========================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_skw","nsp","L","U","f_nL","f_nU")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$f_nL+mydat$f_nU # total asymmetry
mydat$uniA<-mydat$f_nL-mydat$f_nU # net asymmetry
#mydat$community<-"NT" #Symmetric
#mydat$community[which(mydat$L>abs(mydat$U))]<-"LT"
#mydat$community[which(mydat$L<abs(mydat$U))]<-"UT"

mydat<-mydat%>%rename(
  stability=iCV,
  stability_skw=iCValt,
  VR=phi,
  SR=phi_skw,
  R=nsp)
#============================================================================
table(mydat$TAXA) # mammals for terrestrial, terrestrial data are 
                  # always large in number than freshwater

mydat$TAXA<-as.factor(mydat$TAXA)
mydat$UID<-as.factor(mydat$UID)
#============================================================================
# first, we should scale the variables we are going to consider
# scaled quantity = (actual quantity -mean(actual quantity))/sd(actual quantity)
mydat_scaled<-mydat
mydat_scaled$stability<-scale(mydat_scaled$stability)
mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
mydat_scaled$R<-scale(mydat_scaled$R)
mydat_scaled$VR<- scale(mydat_scaled$VR) 
mydat_scaled$SR<- scale(mydat_scaled$SR)
mydat_scaled$A<- scale(mydat_scaled$A)
mydat_scaled$uniA<- scale(mydat_scaled$uniA)

class(mydat_scaled$REALM)
#=====================================================

# (R+VR)*REALM = R + VR + REALM + R:REALM + VR:REALM

cat("------- Hierarchical structure in the data -------------- \n ")
xx<-mydat_scaled%>%group_by(REALM,TAXA,UID)%>%count()%>%ungroup()
#sum(xx$n)==nrow(mydat)

ggplot(data=mydat_scaled,aes(y=stability_skw,x=R,col=TAXA,shape=REALM))+
  geom_point(size=1.2,alpha=0.8,position="jitter")+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+
  theme(legend.position = "bottom")

dt<-mydat_scaled%>%filter(TAXA=="fish")
ggplot(data=dt,aes(y=stability_skw,x=R,col=UID))+
  geom_point(size=1.2,alpha=0.8,position="jitter")+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+
  theme(legend.position = "none")

ggplot(data=dt,aes(y=stability_skw,x=VR,col=UID))+
  geom_point(size=1.2,alpha=0.8,position="jitter")+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+
  theme(legend.position = "none")


#mydat_scaled<-mydat_scaled%>%filter(TAXA%in%c("birds","fish",
#                                              "freshwater invertebrates","terrestrial invertebrates",
#                                              "freshwater plants", "terrestrial plants"))


#(1 + REALM |STUDY_ID) = The effect of REALM will vary between STUDY_ID.
#Random intercepts for REALM, 
#random slopes for STUDY_IDs influenced by REALM.

# write the formula  
#bf_stability0<-bf(stability_skw ~ (R+VR)*REALM+
#                    (1||REALM)+(1||REALM:TAXA)+(1||REALM:TAXA:UID))

cat(paste("------- brms null model without REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0_wo_REALM<-bf(stability_skw ~ (R+VR) + (1|TAXA/UID))
null_model_wo_REALM<-brm(bf_stability0_wo_REALM,
                data=mydat_scaled,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=8000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model_wo_REALM),digits = 3)
saveRDS(null_model_wo_REALM,"../../Results/gather_res/test/nullmodel_wo_REALM.RDS")

cat(paste("------- brms null model with REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0_w_REALM<-bf(stability_skw ~ (R+VR)*REALM + (1|TAXA/UID))
null_model_w_REALM<-brm(bf_stability0_w_REALM,
                         data=mydat_scaled,
                         family = gaussian(),
                         chains=4,cores=4,iter=16000,
                         warmup=8000,init="0",thin=4,
                         control = list(adapt_delta = 0.99, max_treedepth = 15),
                         save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model_w_REALM),digits = 3)
saveRDS(null_model_w_REALM,"../../Results/gather_res/test/nullmodel_w_REALM.RDS")

cat(paste("------- brms full model starting at time: ", Sys.time()," -------------- \n "))
bf_stability<-bf(stability_skw ~ (R+VR+A+uniA+SR)*REALM + (1|TAXA/UID))

full_model<-brm(bf_stability,
                data=mydat_scaled,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=8000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)

print(summary(full_model),digits = 3)
saveRDS(full_model,"../../Results/gather_res/test/fullmodel.RDS")


#null_model_c<-add_criterion(null_model,
#                            criterion=c("loo", "loo_R2"),
#                            moment_match = T,
#                            reloo = T)
#saveRDS(null_model_c,"../../Results/gather_res/pratice_hierarchical_nullmodel_withcrit.RDS")


cat(paste("------- compare loo for all models: ", Sys.time()," -------------- \n "))
loo_null_wo_REALM<-loo(null_model_wo_REALM,moment_match=T,reloo=T)
loo_null_w_REALM<-loo(null_model_w_REALM,moment_match=T,reloo=T)
loo_full<-loo(full_model,moment_match=T,reloo=T)
lc<-loo_compare(loo_null_wo_REALM,loo_null_w_REALM,loo_full)
lmw<-loo_model_weights(null_model_wo_REALM,null_model_w_REALM,full_model)
lc_list<-list(loo_null_wo_REALM=loo_null_wo_REALM,
              loo_null_w_REALM=loo_null_w_REALM,
              loo_full=loo_full,
              lc=lc,
              lmw=lmw)
saveRDS(lc_list,"../../Results/gather_res/test/lc_list.RDS")
print(lc)
print(lmw)

# to get marginal (variance explained by fixed effects only) R2
#    and conditional (variance explained by fixed+random effects) R2 
#    for bayesian mixed effect models


cat(paste("------- compare R2 for all models: ", Sys.time()," -------------- \n "))
cat("==== R2 for null model without REALM: bayes =====","\n")
r2_null_wo_REALM<-r2_bayes(null_model_wo_REALM,ci=0.95)
r2_null_wo_REALM
cat("==== R2 for null model with REALM: bayes =====","\n")
r2_null_w_REALM<-r2_bayes(null_model_w_REALM,ci=0.95)
r2_null_w_REALM
cat("==== R2 for full model: bayes =====","\n")
r2_full<-r2_bayes(full_model,ci=0.95)
r2_full

r2_list<-list(r2_null_wo_REALM=r2_null_wo_REALM,
              r2_null_w_REALM=r2_null_w_REALM,
              r2_full=r2_full)
saveRDS(r2_list,"../../Results/gather_res/test/r2_list.RDS")

cat(paste("------- completed: ", Sys.time()," -------------- \n "))

sink()

#################################
null_model_wo_REALM<-readRDS("../../Results/gather_res/test/nullmodel_wo_REALM.RDS")
null_model_w_REALM<-readRDS("../../Results/gather_res/test/nullmodel_w_REALM.RDS")
full_model<-readRDS("../../Results/gather_res/test/fullmodel.RDS")

# is it a reasonable fit?
#brms::pp_check(null_model_wo_REALM,nsamples = 1000)
#brms::pp_check(null_model_w_REALM,nsamples = 1000)
#brms::pp_check(full_model,nsamples = 1000)

conditional_effects(null_model_wo_REALM)
conditional_effects(null_model_w_REALM)
conditional_effects(full_model)

# to see estimates
fixef(full_model,probs = c(0.025, 0.125, 0.875, 0.975))
ranobj<-ranef(full_model)
obj<-coef(full_model) # this is the combined effect of fixed and random effect
  
# to see estimates from random effects
names(ranobj)
ranobj$TAXA
ranobj$`TAXA:UID`


# test case
objf<-fixef(null_model_w_REALM)
objr<-ranef(null_model_w_REALM)
objr$TAXA
objc<-coef(null_model_w_REALM)
objc$TAXA[,,"Intercept"]

# fixed effect + random effect == coef effect
objf["Intercept","Estimate"]+objr$TAXA[,,"Intercept"][1,1]==objc$TAXA[,,"Intercept"][1,1]

## question: what are r_ terms in posterior_samples(full_model)?





