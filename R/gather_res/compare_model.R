compare_model<-function(null_model,full_model,resloc){

  null_model_c<-add_criterion(null_model,
                              criterion=c("loo", "waic", "loo_R2"),
                              moment_match = T,
                              reloo = T)
  saveRDS(null_model_c,paste(resloc,"/nullmodel_withcrit.RDS",sep=""))
  
  full_model_c<-add_criterion(full_model,
                              criterion=c("loo", "waic", "loo_R2"),
                              moment_match = T,
                              reloo = T)
  saveRDS(full_model_c,paste(resloc,"/fullmodel_withcrit.RDS",sep=""))
  #saveRDS(full_model_c,paste("../../Results/gather_res/modelres_",taxa,"/fullmodel_withcrit.RDS",sep=""))
}