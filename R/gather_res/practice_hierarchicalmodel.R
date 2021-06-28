rm(list=ls())
library(brms)
library(tidyverse)

#========================================================================================
sink("./console_practice_hierarchicalmodel.txt", append=TRUE, split=TRUE)
#====================================================================================================


#===========================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_skw","nsp","L","U","f_nL","f_nU")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$f_nL+mydat$f_nU # total asymmetry
mydat$uniA<-mydat$f_nL-mydat$f_nU # net asymmetry
mydat$community<-"NT" #Symmetric
mydat$community[which(mydat$L>abs(mydat$U))]<-"LT"
mydat$community[which(mydat$L<abs(mydat$U))]<-"UT"

mydat<-mydat%>%rename(
  stability=iCV,
  stability_skw=iCValt,
  VR=phi,
  SR=phi_skw,
  R=nsp)
mydat<-na.omit(mydat) # one RivFishTIME data point omitted as SR=NaN
# one community G8554 has dominant target sp. varying indep with each other, so 
# SR was not calculated - VR showing compensatory dynamics (<1) due to other common sp not considered
# in tail-analysis.
#============================================================================
table(mydat$TAXA) # mammals for terrestrial, terrestrial data are 
                  # always large in number than freshwater
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
#=====================================================

# (R+VR)*REALM = R + VR + REALM + R:REALM + VR:REALM
# (REALM|STUDY_ID/Protocol) = (REALM|STUDY_ID) + (REALM|STUDY_ID:Protocol)

# mydat_scaled$Protocol[is.na(mydat_scaled$Protocol)]<-"same"

# 237 newsites are arranged in the following hierarchical structure
#xx<-mydat_scaled%>%group_by(REALM,STUDY_ID,Protocol)%>%count()%>%ungroup()
xx<-mydat_scaled%>%group_by(REALM,TAXA,UID)%>%count()%>%ungroup()
sum(xx$n)==nrow(mydat)

ggplot(data=mydat_scaled,aes(y=stability_skw,x=R,col=UID,shape=REALM))+
  geom_point(size=1.2,alpha=0.8,position="jitter")+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+
  theme(legend.position = "none")


cat(paste("------- brms null model starting at time: ", Sys.time()," -------------- \n "))

#(1 + REALM |STUDY_ID) = The effect of REALM will vary between STUDY_ID.
#Random intercepts for REALM, 
#random slopes for STUDY_IDs influenced by REALM.

# write the formula  
bf_stability0<-bf(stability_skw ~ (R+VR)*REALM+
                    (1+ (R+VR)*REALM |TAXA/UID))

null_model<-brm(bf_stability0,
                data=mydat_scaled,
                #family = skew_normal(),
                chains=4,cores=4,iter=10000,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model),digits = 3)
saveRDS(null_model,"../../Results/gather_res/pratice_hierarchical_nullmodel.RDS")

null_model_c<-add_criterion(null_model,
                          criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                          moment_match = T,
                          reloo = T)
saveRDS(null_model_c,"../../Results/gather_res/pratice_hierarchical_nullmodel_withcrit.RDS")


cat(paste("------- brms full model starting at time: ", Sys.time()," -------------- \n "))

bf_stability<-bf(stability_skw ~ (R+VR+A+uniA+SR)*REALM+ 
                   (1+(R+VR+A+uniA+SR)*REALM|TAXA/UID))

full_model<-brm(bf_stability,
                data=mydat_scaled,
                #family = skew_normal(),
                chains=4,cores=4,iter=10000,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)

print(summary(full_model),digits = 3)
saveRDS(full_model,"../../Results/gather_res/pratice_hierarchical_fullmodel.RDS")

full_model_c<-add_criterion(full_model,
                          criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                          moment_match = T,
                          reloo = T)
saveRDS(full_model_c,"../../Results/gather_res/pratice_hierarchical_fullmodel_withcrit.RDS")

cat("==== compare loo for both model =====","\n")
loo_null<-loo(null_model)
loo_full<-loo(full_model)
loo_compare(loo_null,loo_full)
loo_model_weights(null_model,full_model)

#cat("==== R2 for null model: loo =====","\n")
#loo_R2(null_model)

#cat("==== R2 for full model: loo =====","\n")
#loo_R2(full_model)

#cat("==== R2 for null model: bayes =====","\n")
#bayes_R2(null_model)

#cat("==== R2 for full model: bayes =====","\n")
#bayes_R2(full_model)

# to get marginal (variance explained by fixed effects only) R2
#    and conditional (variance explained by fixed+random effects) R2 
#    for bayesian mixed effect models
library(performance)
cat("==== R2 for null model: bayes =====","\n")
r2_bayes(null_model,ci=0.95)
cat("==== R2 for full model: bayes =====","\n")
r2_bayes(full_model,ci=0.95)

sink()

#################################

# to see estimates from random effects
#xr<-ranef(null_model)
#xr1<-xr$STUDY_ID
#names(xr1)
#xr11<-xr1[,,1]
#xr12<-xr1[,,2]
# is it a reasonable fit?
#pp_check(null_model,nsamples = 100)
#pp_check(full_model,nsamples = 10)
#conditional_effects(null_model)
#conditional_effects(full_model)
#null_model<-readRDS("../../Results/gather_res/pratice_hierarchical_nullmodel_skwnormal.RDS")
#full_model<-readRDS("../../Results/gather_res/pratice_hierarchical_fullmodel_skwnormal.RDS")

#brms::pp_check(null_model,nsamples = 100)
#brms::pp_check(full_model,nsamples = 100)
#brms::conditional_effects(full_model)







