library(brms)

reslocn<-paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric")
if(!dir.exists(reslocn)){
  dir.create(reslocn)
}

call_multires_multigroup<-function(i){
  
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15_with_newmetric.RDS",sep=""))
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$LM<- scale(mydat_scaled$VR_LdM)# LM synchrony: modified variance ratio 
  mydat_scaled$A<- scale(mydat_scaled$A)
  mydat_scaled$newcormetric<- scale(mydat_scaled$newcormetric)
  
  #=====================================================
  
  # (R+VR)*REALM = R + newcormetric + REALM + R:REALM + VR:REALM
  
  cat("------- Hierarchical structure in the data -------------- \n ")
  
  resloc<-paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric/run_",i,sep="")
  if(!dir.exists(resloc)){
    dir.create(resloc)
  }
  
  sink(paste(resloc,"/console_bayesian_model_subset_birds_fixed_realm.txt",sep=""),
       append=TRUE, split=TRUE)
  
  cat(paste("------- brms model with R, A, newcormetric, REALM starting at time: ", Sys.time()," -------------- \n "))
  bf_stability<-bf(stability_skw ~ (R+A+newcormetric)*REALM + (1|TAXA/UID))
  bf_LM<-bf(newcormetric ~ R*REALM+(1|TAXA/UID))
  bf_A<-bf(A  ~ R*REALM+(1|TAXA/UID))
  
  bf_tot<-bf_stability+bf_LM+bf_A+set_rescor(rescor=T)
  
  full_model<-brm(bf_tot,
                  data=mydat_scaled,
                  family = gaussian(),
                  chains=4,cores=4,iter=16000,
                  warmup=12000,init="random",thin=4,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  save_pars = save_pars(all = TRUE),seed=123)
  
  print(summary(full_model),digits = 3)
  saveRDS(full_model,paste(resloc,"/full_model.RDS",sep=""))
  
  sink()
}
for(i in 1:100){
  call_multires_multigroup(i=i)  
}

#=====================================================================
rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)

for(i in c(1:100)){
  resloc<-paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric/run_",i,sep="")
  
  bm<-readRDS(paste(resloc,"/full_model.RDS",sep=""))
  
  post<-as_draws_df(bm)
  
  #------------------------------------------------------------
  
  post<-post%>%dplyr::select(b_stabilityskw_Intercept,
                             b_stabilityskw_R,b_stabilityskw_A,b_stabilityskw_newcormetric,
                             b_stabilityskw_REALMTerrestrial,
                             `b_stabilityskw_R:REALMTerrestrial`, `b_stabilityskw_A:REALMTerrestrial`, `b_stabilityskw_newcormetric:REALMTerrestrial`,
                             b_newcormetric_Intercept,b_A_Intercept,
                             b_newcormetric_REALMTerrestrial,b_A_REALMTerrestrial,
                             b_newcormetric_R,`b_newcormetric_R:REALMTerrestrial`,
                             b_A_R,`b_A_R:REALMTerrestrial`)
  
  
  df<-data.frame(Median=NA*numeric(16),
                 LowCI0.95=NA*numeric(16),UpCI0.95=NA*numeric(16),
                 LowCI0.75=NA*numeric(16),UpCI0.75=NA*numeric(16))
  
  rownames(df)<-c("Intercept_stability_Freshw",
                  "Slope_stability_R_Freshw","Slope_stability_A_Freshw","Slope_stability_newcormetric_Freshw",
                  "Intercept_A_Freshw",
                  "Slope_A_R_Freshw",
                  "Intercept_newcormetric_Freshw",
                  "Slope_newcormetric_R_Freshw",
                  "Intercept_stability_Terres",
                  "Slope_stability_R_Terres","Slope_stability_A_Terres","Slope_stability_newcormetric_Terres",
                  "Intercept_A_Terres",
                  "Slope_A_R_Terres",
                  "Intercept_newcormetric_Terres",
                  "Slope_newcormetric_R_Terres")
  
  df1<-post %>%dplyr::select(b_stabilityskw_Intercept)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[1]<-df1$b_stabilityskw_Intercept[1]
  df$LowCI0.95[1]<-df1$.lower[1]
  df$LowCI0.75[1]<-df1$.lower[2]
  df$UpCI0.95[1]<-df1$.upper[1]
  df$UpCI0.75[1]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_stabilityskw_R)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[2]<-df1$b_stabilityskw_R[1]
  df$LowCI0.95[2]<-df1$.lower[1]
  df$LowCI0.75[2]<-df1$.lower[2]
  df$UpCI0.95[2]<-df1$.upper[1]
  df$UpCI0.75[2]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_stabilityskw_A)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[3]<-df1$b_stabilityskw_A[1]
  df$LowCI0.95[3]<-df1$.lower[1]
  df$LowCI0.75[3]<-df1$.lower[2]
  df$UpCI0.95[3]<-df1$.upper[1]
  df$UpCI0.75[3]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_stabilityskw_newcormetric)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[4]<-df1$b_stabilityskw_newcormetric[1]
  df$LowCI0.95[4]<-df1$.lower[1]
  df$LowCI0.75[4]<-df1$.lower[2]
  df$UpCI0.95[4]<-df1$.upper[1]
  df$UpCI0.75[4]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_A_Intercept)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[5]<-df1$b_A_Intercept[1]
  df$LowCI0.95[5]<-df1$.lower[1]
  df$LowCI0.75[5]<-df1$.lower[2]
  df$UpCI0.95[5]<-df1$.upper[1]
  df$UpCI0.75[5]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_A_R)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[6]<-df1$b_A_R[1]
  df$LowCI0.95[6]<-df1$.lower[1]
  df$LowCI0.75[6]<-df1$.lower[2]
  df$UpCI0.95[6]<-df1$.upper[1]
  df$UpCI0.75[6]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_newcormetric_Intercept)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[7]<-df1$b_newcormetric_Intercept[1]
  df$LowCI0.95[7]<-df1$.lower[1]
  df$LowCI0.75[7]<-df1$.lower[2]
  df$UpCI0.95[7]<-df1$.upper[1]
  df$UpCI0.75[7]<-df1$.upper[2]
  
  df1<-post %>%dplyr::select(b_newcormetric_R)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[8]<-df1$b_newcormetric_R[1]
  df$LowCI0.95[8]<-df1$.lower[1]
  df$LowCI0.75[8]<-df1$.lower[2]
  df$UpCI0.95[8]<-df1$.upper[1]
  df$UpCI0.75[8]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_stabilityskw_Intercept_terres=b_stabilityskw_Intercept+b_stabilityskw_REALMTerrestrial)%>% select(b_stabilityskw_Intercept_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[9]<-df1$b_stabilityskw_Intercept_terres[1]
  df$LowCI0.95[9]<-df1$.lower[1]
  df$LowCI0.75[9]<-df1$.lower[2]
  df$UpCI0.95[9]<-df1$.upper[1]
  df$UpCI0.75[9]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_stabilityskw_R_terres=b_stabilityskw_R+`b_stabilityskw_R:REALMTerrestrial`)%>% select(b_stabilityskw_R_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[10]<-df1$b_stabilityskw_R_terres[1]
  df$LowCI0.95[10]<-df1$.lower[1]
  df$LowCI0.75[10]<-df1$.lower[2]
  df$UpCI0.95[10]<-df1$.upper[1]
  df$UpCI0.75[10]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_stabilityskw_A_terres=b_stabilityskw_A+`b_stabilityskw_A:REALMTerrestrial`)%>% select(b_stabilityskw_A_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[11]<-df1$b_stabilityskw_A_terres[1]
  df$LowCI0.95[11]<-df1$.lower[1]
  df$LowCI0.75[11]<-df1$.lower[2]
  df$UpCI0.95[11]<-df1$.upper[1]
  df$UpCI0.75[11]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_stabilityskw_newcormetric_terres=b_stabilityskw_newcormetric+`b_stabilityskw_newcormetric:REALMTerrestrial`)%>% select(b_stabilityskw_newcormetric_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[12]<-df1$b_stabilityskw_newcormetric_terres[1]
  df$LowCI0.95[12]<-df1$.lower[1]
  df$LowCI0.75[12]<-df1$.lower[2]
  df$UpCI0.95[12]<-df1$.upper[1]
  df$UpCI0.75[12]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_A_Intercept_terres=b_A_Intercept+b_A_REALMTerrestrial)%>% select(b_A_Intercept_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[13]<-df1$b_A_Intercept_terres[1]
  df$LowCI0.95[13]<-df1$.lower[1]
  df$LowCI0.75[13]<-df1$.lower[2]
  df$UpCI0.95[13]<-df1$.upper[1]
  df$UpCI0.75[13]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_A_R_terres=b_A_R+`b_A_R:REALMTerrestrial`)%>% select(b_A_R_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[14]<-df1$b_A_R_terres[1]
  df$LowCI0.95[14]<-df1$.lower[1]
  df$LowCI0.75[14]<-df1$.lower[2]
  df$UpCI0.95[14]<-df1$.upper[1]
  df$UpCI0.75[14]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_newcormetric_Intercept_terres=b_newcormetric_Intercept+b_newcormetric_REALMTerrestrial)%>% select(b_newcormetric_Intercept_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[15]<-df1$b_newcormetric_Intercept_terres[1]
  df$LowCI0.95[15]<-df1$.lower[1]
  df$LowCI0.75[15]<-df1$.lower[2]
  df$UpCI0.95[15]<-df1$.upper[1]
  df$UpCI0.75[15]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_newcormetric_R_terres=b_newcormetric_R+`b_newcormetric_R:REALMTerrestrial`)%>% select(b_newcormetric_R_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[16]<-df1$b_newcormetric_R_terres[1]
  df$LowCI0.95[16]<-df1$.lower[1]
  df$LowCI0.75[16]<-df1$.lower[2]
  df$UpCI0.95[16]<-df1$.upper[1]
  df$UpCI0.75[16]<-df1$.upper[2]
  
  # rearrange df
  df<-df[c("Intercept_stability_Freshw","Intercept_stability_Terres",
           "Slope_stability_R_Freshw","Slope_stability_R_Terres",
           "Slope_stability_A_Freshw","Slope_stability_A_Terres",
           "Slope_stability_newcormetric_Freshw","Slope_stability_newcormetric_Terres",
           "Intercept_A_Freshw","Intercept_A_Terres",
           "Slope_A_R_Freshw","Slope_A_R_Terres",
           "Intercept_newcormetric_Freshw","Intercept_newcormetric_Terres",
           "Slope_newcormetric_R_Freshw","Slope_newcormetric_R_Terres"),]
  
  dfbm<-mutate(df, across(where(is.numeric), round, 3))
  row.names(dfbm)<-rownames(df)
  write.csv(dfbm,paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  cat(paste("------- i = ", i ," -------done ------- \n "))
}

fixedeffect<-c()
for(i in 1:100){
  resloc<-paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric/run_",i,sep="")
  dfbm<-read.csv(paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  dfbm$run<-i
  fixedeffect<-rbind(fixedeffect,dfbm)
}
write.csv(fixedeffect,paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric/fixedeffectestimate_summary_fixed_realm.csv",sep=""),row.names = F)

#----------------Now, make table for Summary estimates from 100 runs--------------------------
fixedeffect<-read.csv(paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric/fixedeffectestimate_summary_fixed_realm.csv",sep=""))
fixedeffect$sig95<-ifelse((fixedeffect$LowCI0.95/fixedeffect$UpCI0.95)>0,1,0) #significance based on 95%CI
fixedeffect$sig75<-ifelse((fixedeffect$LowCI0.75/fixedeffect$UpCI0.75)>0,1,0) #significance based on 75%CI

si<-fixedeffect%>%select(X, Median, sig95, sig75, run)

# counting how many times out of 100 runs the median estimate was sig based on 95% and 75% CI
si1<-si%>%group_by(X)%>%summarise(sign95=sum(sig95),
                                  sign75=sum(sig75))%>%ungroup()

# Getting avg estimate with CI of all the median for 100 runs
si2<-si%>%group_by(X)%>%select(Median)%>%
  mean_qi(.width=c(0.95,0.75))%>%ungroup()

si3<-left_join(si2,si1,by="X")

sink(paste("../../Results/gather_res/res_taxa15/multires_multigroup_w_newcormetric/100runs_fixedeffects_estimates_table.txt",sep=""))
print(si3,digits=3,n=32)
sink()
