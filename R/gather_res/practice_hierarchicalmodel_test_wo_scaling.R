rm(list=ls())
library(brms)
library(tidyverse)
library(performance)

if(!dir.exists("../../Results/gather_res/test")){
  dir.create("../../Results/gather_res/test")
}

if(!dir.exists("../../Results/gather_res/test/wo_scaling")){
  dir.create("../../Results/gather_res/test/wo_scaling")
}

#========================================================================================
sink("../../Results/gather_res/test/wo_scaling/console_practice_hierarchicalmodel_test_wo_scaling.txt", append=TRUE, split=TRUE)
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


#(1 + REALM |STUDY_ID) = The effect of REALM will vary between STUDY_ID.
#Random intercepts for REALM, 
#random slopes for STUDY_IDs influenced by REALM.

# write the formula  
#bf_stability0<-bf(stability_skw ~ (R+VR)*REALM+
#                    (1||REALM)+(1||REALM:TAXA)+(1||REALM:TAXA:UID))

cat(paste("------- brms null model without REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0_wo_REALM<-bf(stability_skw ~ (R+VR) + (1|TAXA/UID))
null_model_wo_REALM<-brm(bf_stability0_wo_REALM,
                data=mydat,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=8000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model_wo_REALM),digits = 3)
saveRDS(null_model_wo_REALM,"../../Results/gather_res/test/wo_scaling/nullmodel_wo_REALM.RDS")

cat(paste("------- brms null model with REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0_w_REALM<-bf(stability_skw ~ (R+VR)*REALM + (1|TAXA/UID))
null_model_w_REALM<-brm(bf_stability0_w_REALM,
                         data=mydat,
                         family = gaussian(),
                         chains=4,cores=4,iter=16000,
                         warmup=8000,init="0",thin=4,
                         control = list(adapt_delta = 0.99, max_treedepth = 15),
                         save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model_w_REALM),digits = 3)
saveRDS(null_model_w_REALM,"../../Results/gather_res/test/wo_scaling/nullmodel_w_REALM.RDS")

cat(paste("------- brms full model starting at time: ", Sys.time()," -------------- \n "))
bf_stability<-bf(stability_skw ~ (R+VR+A+uniA+SR)*REALM + (1|TAXA/UID))

full_model<-brm(bf_stability,
                data=mydat,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=8000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)

print(summary(full_model),digits = 3)
saveRDS(full_model,"../../Results/gather_res/test/wo_scaling/fullmodel.RDS")


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
saveRDS(lc_list,"../../Results/gather_res/test/wo_scaling/lc_list.RDS")
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
saveRDS(r2_list,"../../Results/gather_res/test/wo_scaling/r2_list.RDS")

cat(paste("------- completed: ", Sys.time()," -------------- \n "))

sink()

#################################
