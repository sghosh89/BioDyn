rm(list=ls())
library(brms)
library(tidyverse)
library(performance)

if(!dir.exists("../../Results/gather_res/simple")){
  dir.create("../../Results/gather_res/simple")
}

#========================================================================================
sink("../../Results/gather_res/simple/console_bayesian_model_simple.txt", append=TRUE, split=TRUE)
#====================================================================================================


#===========================
df<-readRDS("../../Results/gather_res/stability_metric_all_subset_birds_21.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_skw","nsp","L","U","f_nL","f_nU","f_nneg")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U # net asymmetry

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

cat(paste("------- brms basic model with R starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability_skw ~ R + (1|TAXA/UID))
basic_model_w_R<-brm(bf_stability0,
                 data=mydat_scaled,
                 family = gaussian(),
                 chains=4,cores=4,iter=16000,
                 warmup=12000,init="0",thin=4,
                 control = list(adapt_delta = 0.99, max_treedepth = 15),
                 save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R),digits = 3)
saveRDS(basic_model_w_R,"../../Results/gather_res/simple/basic_model_w_R.RDS")


cat(paste("------- brms basic model with R and REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability_skw ~ R*REALM + (1|TAXA/UID))
basic_model_w_R_REALM<-brm(bf_stability0,
                         data=mydat_scaled,
                         family = gaussian(),
                         chains=4,cores=4,iter=16000,
                         warmup=12000,init="0",thin=4,
                         control = list(adapt_delta = 0.99, max_treedepth = 15),
                         save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_REALM),digits = 3)
saveRDS(basic_model_w_R_REALM,"../../Results/gather_res/simple/basic_model_w_R_REALM.RDS")

cat(paste("------- brms basic model with R, VR starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability_skw ~ (R+VR) + (1|TAXA/UID))
basic_model_w_R_VR<-brm(bf_stability0,
                        data=mydat_scaled,
                        family = gaussian(),
                        chains=4,cores=4,iter=16000,
                        warmup=12000,init="0",thin=4,
                        control = list(adapt_delta = 0.99, max_treedepth = 15),
                        save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_VR),digits = 3)
saveRDS(basic_model_w_R_VR,"../../Results/gather_res/simple/basic_model_w_R_VR.RDS")

cat(paste("------- brms basic model with R, VR and REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability_skw ~ (R+VR)*REALM + (1|TAXA/UID))
basic_model_w_R_VR_REALM<-brm(bf_stability0,
                           data=mydat_scaled,
                           family = gaussian(),
                           chains=4,cores=4,iter=16000,
                           warmup=12000,init="0",thin=4,
                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                           save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_VR_REALM),digits = 3)
saveRDS(basic_model_w_R_VR_REALM,"../../Results/gather_res/simple/basic_model_w_R_VR_REALM.RDS")

cat(paste("------- brms basic_model_w R, A starting at time: ", Sys.time()," -------------- \n "))
bf_stability0<-bf(stability_skw ~ (R+A) + (1|TAXA/UID))

basic_model_w_R_A<-brm(bf_stability0,
                             data=mydat_scaled,
                             family = gaussian(),
                             chains=4,cores=4,iter=16000,
                             warmup=12000,init="0",thin=4,
                             control = list(adapt_delta = 0.99, max_treedepth = 15),
                             save_pars = save_pars(all = TRUE),seed=123)

print(summary(basic_model_w_R_A),digits = 3)
saveRDS(basic_model_w_R_A,"../../Results/gather_res/simple/basic_model_w_R_A.RDS")

cat(paste("------- brms basic_model_w R, A, REALM starting at time: ", Sys.time()," -------------- \n "))
bf_stability0<-bf(stability_skw ~ (R+A)*REALM + (1|TAXA/UID))

basic_model_w_R_A_REALM<-brm(bf_stability0,
                     data=mydat_scaled,
                     family = gaussian(),
                     chains=4,cores=4,iter=16000,
                     warmup=12000,init="0",thin=4,
                     control = list(adapt_delta = 0.99, max_treedepth = 15),
                     save_pars = save_pars(all = TRUE),seed=123)

print(summary(basic_model_w_R_A_REALM),digits = 3)
saveRDS(basic_model_w_R_A_REALM,"../../Results/gather_res/simple/basic_model_w_R_A_REALM.RDS")

cat(paste("------- brms model with R, A, VR starting at time: ", Sys.time()," -------------- \n "))
bf_stability0<-bf(stability_skw ~ (R+A+VR) + (1|TAXA/UID))

basic_model_w_R_A_VR<-brm(bf_stability0,
                data=mydat_scaled,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=12000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)

print(summary(basic_model_w_R_A_VR),digits = 3)
saveRDS(basic_model_w_R_A_VR,"../../Results/gather_res/simple/basic_model_w_R_A_VR.RDS")

cat(paste("------- brms model with R, A, VR, REALM starting at time: ", Sys.time()," -------------- \n "))
bf_stability<-bf(stability_skw ~ (R+A+VR)*REALM + (1|TAXA/UID))

full_model<-brm(bf_stability,
                data=mydat_scaled,
                family = gaussian(),
                chains=4,cores=4,iter=16000,
                warmup=12000,init="0",thin=4,
                control = list(adapt_delta = 0.99, max_treedepth = 15),
                save_pars = save_pars(all = TRUE),seed=123)

print(summary(full_model),digits = 3)
saveRDS(full_model,"../../Results/gather_res/simple/fullmodel.RDS")


cat(paste("------- compare loo for all models: ", Sys.time()," -------------- \n "))
loo_basic_w_R<-loo(basic_model_w_R,moment_match=T,reloo=T)
loo_basic_w_R_REALM<-loo(basic_model_w_R_REALM,moment_match=T,reloo=T)

loo_basic_w_R_VR<-loo(basic_model_w_R_VR,moment_match=T,reloo=T)
loo_basic_w_R_VR_REALM<-loo(basic_model_w_R_VR_REALM,moment_match=T,reloo=T)

loo_basic_w_R_A<-loo(basic_model_w_R_A,moment_match=T,reloo=T)
loo_basic_w_R_A_REALM<-loo(basic_model_w_R_A_REALM,moment_match=T,reloo=T)

loo_basic_w_R_A_VR<-loo(basic_model_w_R_A_VR,moment_match=T,reloo=T)
loo_full<-loo(full_model,moment_match=T,reloo=T)

lc<-loo_compare(loo_basic_w_R,loo_basic_w_R_REALM,
                loo_basic_w_R_VR,loo_basic_w_R_VR_REALM,
                loo_basic_w_R_A,loo_basic_w_R_A_REALM,
                loo_basic_w_R_A_VR,loo_full)

lmw<-loo_model_weights(basic_model_w_R,basic_model_w_R_REALM,
                       basic_model_w_R_VR,basic_model_w_R_VR_REALM,
                       basic_model_w_R_A,loo_basic_w_R_A_REALM,
                       basic_model_w_R_A_VR,full_model)

lc_list<-list(loo_basic_w_R=loo_basic_w_R,
              loo_basic_w_R_REALM=loo_basic_w_R_REALM,
              loo_basic_w_R_VR=loo_basic_w_R_VR,
              loo_basic_w_R_VR_REALM=loo_basic_w_R_VR_REALM,
              loo_basic_w_R_A=loo_basic_w_R_A,
              loo_basic_w_R_A_REALM=loo_basic_w_R_A_REALM,
              loo_basic_w_R_A_VR=loo_basic_w_R_A_VR,
              loo_full=loo_full,
              lc=lc,
              lmw=lmw)
saveRDS(lc_list,"../../Results/gather_res/simple/lc_list.RDS")
print(lc)
print(lmw)

# to get marginal (variance explained by fixed effects only) R2
#    and conditional (variance explained by fixed+random effects) R2 
#    for bayesian mixed effect models


cat(paste("------- compare R2 for all models: ", Sys.time()," -------------- \n "))
cat("==== R2 for basic model with R: bayes =====","\n")
r2_basic_w_R<-r2_bayes(basic_model_w_R,ci=0.95)
r2_basic_w_R
cat("==== R2 for basic model with R and REALM: bayes =====","\n")
r2_basic_w_R_REALM<-r2_bayes(basic_model_w_R_REALM,ci=0.95)
r2_basic_w_R_REALM

cat("==== R2 for basic model with R, VR: bayes =====","\n")
r2_basic_w_R_VR<-r2_bayes(basic_model_w_R_VR,ci=0.95)
r2_basic_w_R_VR
cat("==== R2 for basic model with R, VR, REALM: bayes =====","\n")
r2_basic_w_R_VR_REALM<-r2_bayes(basic_model_w_R_VR_REALM,ci=0.95)
r2_basic_w_R_VR_REALM

cat("==== R2 for basic model with R, A: bayes =====","\n")
r2_basic_w_R_A<-r2_bayes(basic_model_w_R_A,ci=0.95)
r2_basic_w_R_A
cat("==== R2 for basic model with R, A, REALM: bayes =====","\n")
r2_basic_w_R_A_REALM<-r2_bayes(basic_model_w_R_A_REALM,ci=0.95)
r2_basic_w_R_A_REALM

cat("==== R2 for basic model with R, A, VR: bayes =====","\n")
r2_basic_w_R_A_VR<-r2_bayes(basic_model_w_R_A_VR,ci=0.95)
r2_basic_w_R_A_VR
cat("==== R2 for full model: bayes =====","\n")
r2_full<-r2_bayes(full_model,ci=0.95)
r2_full

r2_list<-list(r2_basic_w_R=r2_basic_w_R,
              r2_basic_w_R_REALM=r2_basic_w_R_REALM,
              r2_basic_w_R_VR=r2_basic_w_R_VR,
              r2_basic_w_R_VR_REALM=r2_basic_w_R_VR_REALM,
              r2_basic_w_R_A=r2_basic_w_R_A,
              r2_basic_w_R_A_REALM=r2_basic_w_R_A_REALM,
              r2_basic_w_R_A_VR=r2_basic_w_R_A_VR,
              r2_full=r2_full)
saveRDS(r2_list,"../../Results/gather_res/simple/r2_list.RDS")

cat(paste("------- completed: ", Sys.time()," -------------- \n "))


cat("------- compare performance with all simple models -------------- \n ")
cp<-compare_performance(basic_model_w_R, basic_model_w_R_REALM,
                        basic_model_w_R_VR,basic_model_w_R_VR_REALM,
                        basic_model_w_R_A,basic_model_w_R_A_REALM,
                        basic_model_w_R_A_VR,full_model, rank=T, metrics="common")
cp
saveRDS(cp,"../../Results/gather_res/simple/cp.RDS")
sink()

#################################

