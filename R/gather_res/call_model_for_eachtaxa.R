library(tidyverse)
library(brms)
library(performance)
# mydat: full data
# taxa: specific taxa you want to select

call_model_for_eachtaxa<-function(mydat, taxa, resloc){
  
  mydat_raw<-mydat%>%filter(TAXA==taxa)
  mydat_scaled<-mydat_raw
  mydat_scaled$stability<-scale(mydat_scaled$stability)
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR<- scale(mydat_scaled$VR) 
  mydat_scaled$SR<- scale(mydat_scaled$SR)
  mydat_scaled$A<- scale(mydat_scaled$A)
  mydat_scaled$uniA<- scale(mydat_scaled$uniA)
  
  myresloc<-paste(resloc,"/modelres_",taxa,"/",sep="")
  
  if(!dir.exists(myresloc)){
    dir.create(myresloc)
  }
  
  usedat<-list(mydat_raw=mydat_raw,mydat_scaled=mydat_scaled)
  saveRDS(usedat,paste(myresloc,"usedat.RDS",sep=""))
  
  cat(paste("------- brms null model starting for ", taxa, " at time: ", Sys.time()," -------------- \n "))
  
  bf_stability0<-bf(stability_skw ~ (R+VR)+ (1|UID))
  
  null_model<-brm(bf_stability0,
                  data=mydat_scaled,
                  family = gaussian(),
                  chains=4,cores=4,iter=16000,
                  warmup=8000,init="0",thin=4,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  save_pars = save_pars(all = TRUE),seed=123)
  
  saveRDS(null_model,paste(myresloc,"nullmodel.RDS",sep=""))

  print(summary(null_model),digits = 3)
  
  cat(paste("------- brms full model starting for ", taxa, " at time: ", Sys.time()," -------------- \n "))
  
  bf_stability<-bf(stability_skw ~ (R+VR+A+uniA+SR)+(1|UID))
  
  full_model<-brm(bf_stability,
                    data=mydat_scaled,
                    family = gaussian(),
                    chains=4,cores=4,iter=16000,
                    warmup=8000,init="0",thin=4,
                    control = list(adapt_delta = 0.99, max_treedepth = 15),
                    save_pars = save_pars(all = TRUE),seed=123)
  
  
  print(summary(full_model),digits = 3)
  
  saveRDS(full_model,paste(myresloc,"fullmodel.RDS",sep=""))
  
  cat(paste("------- brms compare model starting for ", taxa, " at time: ", Sys.time()," -------------- \n "))
  
  loo_null<-loo(null_model,moment_match=T,reloo=T)
  loo_full<-loo(full_model,moment_match=T,reloo=T)
  lc<-loo_compare(loo_null,loo_full)
  lmw<-loo_model_weights(null_model,full_model)
  lc_list<-list(loo_null=loo_null,
                loo_full=loo_full,
                lc=lc,
                lmw=lmw)
  
  saveRDS(lc_list,paste(myresloc,"lc_list.RDS",sep=""))
  
  cat(paste("------- brms R2 starting for ", taxa, " at time: ", Sys.time()," -------------- \n "))
  
  cat("==== R2 for null model: bayes =====","\n")
  r2_null<-r2_bayes(null_model,ci=0.95)
  r2_null
  cat("==== R2 for full model: bayes =====","\n")
  r2_full<-r2_bayes(full_model,ci=0.95)
  r2_full
  
  r2_list<-list(r2_null=r2_null,
                r2_full=r2_full)
  saveRDS(r2_list,paste(myresloc,"r2_list.RDS",sep=""))
  
}

