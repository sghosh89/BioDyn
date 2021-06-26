library(brms)
# mydat: full data
# taxa: specific taxa you want to select

call_model_for_eachtaxa<-function(mydat, taxa){
  
  mydat_raw<-mydat%>%filter(TAXA==taxa)
  mydat_scaled<-mydat_raw
  mydat_scaled$stability<-scale(mydat_scaled$stability)
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR<- scale(mydat_scaled$VR) 
  mydat_scaled$SR<- scale(mydat_scaled$SR)
  mydat_scaled$A<- scale(mydat_scaled$A)
  mydat_scaled$uniA<- scale(mydat_scaled$uniA)
  
  myresloc<-paste("../../Results/gather_res/modelres_",taxa,"/",sep="")
  
  if(!dir.exists(myresloc)){
    dir.create(myresloc)
  }
  
  usedat<-list(mydat_raw=mydat_raw,mydat_scaled=mydat_scaled)
  saveRDS(usedat,paste("../../Results/gather_res/modelres_",taxa,"/usedat.RDS",sep=""))
  
  cat(paste("------- brms null model starting for ", taxa, " at time: ", Sys.time()," -------------- \n "))
  
  bf_stability0<-bf(stability_skw ~ (R+VR)+ (1+ (R+VR)|UID))
  
  null_model<-brm(bf_stability0,
                  data=mydat_scaled,
                  chains=4,cores=4,iter=10000,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  save_pars = save_pars(all = TRUE),seed=123)
  
  saveRDS(null_model,paste("../../Results/gather_res/modelres_",taxa,"/nullmodel.RDS",sep=""))
  
  cat(paste("------- brms full model starting at time: ", Sys.time()," -------------- \n "))
  
  bf_stability<-bf(stability_skw ~ (R+VR+A+uniA+SR)+(1+(R+VR+A+uniA+SR)|UID))
  
  #full_model<-update(null_model, bf_stability, 
  #                   newdata = mydat_scaled)
  full_model<-brm(bf_stability,
                  data=mydat_scaled,
                  chains=4,cores=4,iter=10000,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  save_pars = save_pars(all = TRUE),seed=123)
  
  saveRDS(full_model,paste("../../Results/gather_res/modelres_",taxa,"/fullmodel.RDS",sep=""))
  
  null_model_c<-add_criterion(null_model,
                            criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                            moment_match = T,
                            reloo = T)
  saveRDS(null_model_c,paste("../../Results/gather_res/modelres_",taxa,"/nullmodel_withcrit.RDS",sep=""))
  
  full_model_c<-add_criterion(full_model,
                            criterion=c("loo", "waic", "loo_R2", "bayes_R2"),
                            moment_match = T,
                            reloo = T)
  saveRDS(full_model_c,paste("../../Results/gather_res/modelres_",taxa,"/fullmodel_withcrit.RDS",sep=""))

}

