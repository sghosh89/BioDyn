#=============================================================
# Here we already know there are different effects on realm, so
# we want to build a model with TAXA as fixed effect
#================================================================

rm(list=ls())
library(brms)
library(tidyverse)
library(performance)

if(!dir.exists("../../Results/gather_res/taxa_fixedeffect")){
  dir.create("../../Results/gather_res/taxa_fixedeffect")
}


#========================================================================================
sink("../../Results/gather_res/taxa_fixedeffect/console_practice_hierarchicalmodel.txt", append=TRUE, split=TRUE)
#====================================================================================================


#===========================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_skw","nsp","L","U","f_nL","f_nU")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$f_nL+mydat$f_nU # total asymmetry
mydat$uniA<-mydat$f_nL-mydat$f_nU # net asymmetry

mydat<-mydat%>%rename(
  stability=iCV,
  stability_skw=iCValt,
  VR=phi,
  SR=phi_skw,
  R=nsp)
#============================================================================
table(mydat$TAXA) # mammals for terrestrial, terrestrial data are 
# always large in number than freshwater
class(mydat_scaled$REALM)
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
#=====================================================

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

cat(paste("------- brms null model starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability_skw ~ (R+VR)*TAXA + (1|UID))
null_model<-brm(bf_stability0,
                data=mydat_scaled,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=8000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)
print(summary(null_model),digits = 3)
saveRDS(null_model,"../../Results/gather_res/taxa_fixedeffect/nullmodel.RDS")


cat(paste("------- brms full model starting at time: ", Sys.time()," -------------- \n "))
bf_stability<-bf(stability_skw ~ (R+VR+A+uniA+SR)*TAXA + (1|UID))

full_model<-brm(bf_stability,
                data=mydat_scaled,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=8000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)
print(summary(full_model),digits = 3)
saveRDS(full_model,"../../Results/gather_res/taxa_fixedeffect/fullmodel.RDS")

cat(paste("------- compare loo for all models: ", Sys.time()," -------------- \n "))
loo_null<-loo(null_model,moment_match=T,reloo=T)
loo_full<-loo(full_model,moment_match=T,reloo=T)
lc<-loo_compare(loo_null,loo_full)
lmw<-loo_model_weights(null_model,full_model)
lc_list<-list(loo_null=loo_null,
              loo_full=loo_full,
              lc=lc,
              lmw=lmw)
saveRDS(lc_list,"../../Results/gather_res/taxa_fixedeffect/lc_list.RDS")
print(lc)
print(lmw)

# to get marginal (variance explained by fixed effects only) R2
#    and conditional (variance explained by fixed+random effects) R2 
#    for bayesian mixed effect models


cat(paste("------- compare R2 for all models: ", Sys.time()," -------------- \n "))
cat("==== R2 for null model: bayes =====","\n")
r2_null<-r2_bayes(null_model,ci=0.95)
r2_null
cat("==== R2 for full model: bayes =====","\n")
r2_full<-r2_bayes(full_model,ci=0.95)
r2_full

r2_list<-list(r2_null=r2_null,
              r2_full=r2_full)
saveRDS(r2_list,"../../Results/gather_res/taxa_fixedeffect/r2_list.RDS")

cat(paste("------- completed: ", Sys.time()," -------------- \n "))

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







