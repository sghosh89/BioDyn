library(brms)

reslocn<-"../../Results/gather_res/res_taxa15/LT_and_UT_sep/"
if(!dir.exists(reslocn)){
  dir.create(reslocn)
}


call_toymodel_fixed_realm_LT_and_UT_sep<-function(i){
  
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR_LdM<- scale(mydat_scaled$VR_LdM) 
  mydat_scaled$A<- scale(mydat_scaled$A)
  mydat_scaled$L<- scale(mydat_scaled$L)
  mydat_scaled$U<- scale(abs(mydat_scaled$U))
  #=====================================================
  
  resloc<-paste("../../Results/gather_res/res_taxa15/LT_and_UT_sep/run_",i,sep="")
  if(!dir.exists(resloc)){
    dir.create(resloc)
  }
  
  sink(paste(resloc,"/console_bayesian_model_subset_birds_fixed_realm.txt",sep=""),
       append=TRUE, split=TRUE)
  
  cat(paste("------- brms model with R, L, |U|, VR_LdM, REALM starting at time: ", Sys.time()," -------------- \n "))
  bf_stability<-bf(stability_skw ~ (R+L+U+VR_LdM)*REALM + (1|TAXA/UID))
  
  full_model<-brm(bf_stability,
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
  call_toymodel_fixed_realm_LT_and_UT_sep(i=i)
}
#==================
rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)
#=========================
taxaeffect_fish<-data.frame(taxa="fish",
                            Median=NA*numeric(100),
                            LowCI0.95=NA*numeric(100),
                            UpCI0.95=NA*numeric(100),
                            LowCI0.75=NA*numeric(100),UpCI0.75=NA*numeric(100),
                            run=NA*numeric(100))
taxaeffect_freshwater.inv<-taxaeffect_freshwater.plants<-taxaeffect_birds<-
  taxaeffect_terrestrial.inv<-taxaeffect_terrestrial.plants<-taxaeffect_mammals<-
  taxaeffect_fish
taxaeffect_freshwater.inv$taxa<-"freshwater.inv"
taxaeffect_freshwater.plants$taxa<-"freshwater.plants"
taxaeffect_birds$taxa<-"birds"
taxaeffect_terrestrial.inv$taxa<-"terrestrial.inv"
taxaeffect_terrestrial.plants$taxa<-"terrestrial.plants"
taxaeffect_mammals$taxa<-"mammals"

# see summary from basic model with R, LT or |UT| , VR, REALM
for(i in c(1:100)){
  resloc<-paste("../../Results/gather_res/res_taxa15/LT_and_UT_sep/run_",i,sep="")
  
  
  bm<-readRDS(paste(resloc,"/full_model.RDS",sep=""))
 
  post<-as_draws_df(bm)
  #----------------------------------taxa effect data -------
  tx<-post%>%select(b_Intercept,
                    `r_TAXA[fish,Intercept]`,
                    `r_TAXA[freshwater.invertebrates,Intercept]`,
                    `r_TAXA[freshwater.plants,Intercept]`,
                    `r_TAXA[birds,Intercept]`,
                    `r_TAXA[terrestrial.invertebrates,Intercept]`,
                    `r_TAXA[terrestrial.plants,Intercept]`,
                    `r_TAXA[mammals,Intercept]`
  )
  
  # following columns are the original source for objc$TAXA[,,"Intercept"]
  tx$fish<-tx$b_Intercept+tx$`r_TAXA[fish,Intercept]`
  tx$freshwater.inv<-tx$b_Intercept+tx$`r_TAXA[freshwater.invertebrates,Intercept]`
  tx$freshwater.plants<-tx$b_Intercept+tx$`r_TAXA[freshwater.plants,Intercept]`
  tx$birds<-tx$b_Intercept+tx$`r_TAXA[birds,Intercept]`
  tx$terrestrial.inv<-tx$b_Intercept+tx$`r_TAXA[terrestrial.invertebrates,Intercept]`
  tx$terrestrial.plants<-tx$b_Intercept+tx$`r_TAXA[terrestrial.plants,Intercept]`
  tx$mammals<-tx$b_Intercept+tx$`r_TAXA[mammals,Intercept]`
  
  tx<-tx %>% 
    select(fish,freshwater.inv,freshwater.plants,
           birds,terrestrial.inv,terrestrial.plants,mammals)
  
  tx1<-tx%>%select(fish)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_fish$run[i]<-i
  taxaeffect_fish[i,2:6]<-tx1[1,]
  
  tx1<-tx%>%select(freshwater.inv)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_freshwater.inv$run[i]<-i
  taxaeffect_freshwater.inv[i,2:6]<-tx1[1,]
  
  tx1<-tx%>%select(freshwater.plants)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_freshwater.plants$run[i]<-i
  taxaeffect_freshwater.plants[i,2:6]<-tx1[1,]
  
  tx1<-tx%>%select(birds)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_birds$run[i]<-i
  taxaeffect_birds[i,2:6]<-tx1[1,]
  
  tx1<-tx%>%select(terrestrial.inv)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_terrestrial.inv$run[i]<-i
  taxaeffect_terrestrial.inv[i,2:6]<-tx1[1,]
  
  tx1<-tx%>%select(terrestrial.plants)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_terrestrial.plants$run[i]<-i
  taxaeffect_terrestrial.plants[i,2:6]<-tx1[1,]
  
  tx1<-tx%>%select(mammals)%>%median_qi(.width = c(.95,0.75))
  tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
  taxaeffect_mammals$run[i]<-i
  taxaeffect_mammals[i,2:6]<-tx1[1,]
  
  #------------------------------------------------------------

    post<-post%>%select(b_Intercept,b_R,b_L,b_U,b_VR_LdM,
                        b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_L:REALMTerrestrial`,`b_U:REALMTerrestrial`,`b_VR_LdM:REALMTerrestrial`,
                        sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
    
  
  
  df<-data.frame(Median=NA*numeric(12),
                 LowCI0.95=NA*numeric(12),UpCI0.95=NA*numeric(12),
                 LowCI0.75=NA*numeric(12),UpCI0.75=NA*numeric(12))
  
  
    rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_L_Freshw","Slope_U_Freshw","Slope_VR_LdM_Freshw",
                    "Intercept_Terres","Slope_R_Terres","Slope_L_Terres","Slope_U_Terres","Slope_VR_LdM_Terres",
                    "sd_Taxa_Intercept","sd_TAXA:UID_Intercept")
    
 
  df1<-post %>%select(b_Intercept)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[1]<-df1$b_Intercept[1]
  df$LowCI0.95[1]<-df1$.lower[1]
  df$LowCI0.75[1]<-df1$.lower[2]
  df$UpCI0.95[1]<-df1$.upper[1]
  df$UpCI0.75[1]<-df1$.upper[2]
  
  df1<-post %>%select(b_R)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[2]<-df1$b_R[1]
  df$LowCI0.95[2]<-df1$.lower[1]
  df$LowCI0.75[2]<-df1$.lower[2]
  df$UpCI0.95[2]<-df1$.upper[1]
  df$UpCI0.75[2]<-df1$.upper[2]
  
  
    df1<-post %>%select(b_L)%>% 
      tidybayes::median_qi(.width = c(.95,0.75))
    df$Median[3]<-df1$b_L[1]
  df$LowCI0.95[3]<-df1$.lower[1]
  df$LowCI0.75[3]<-df1$.lower[2]
  df$UpCI0.95[3]<-df1$.upper[1]
  df$UpCI0.75[3]<-df1$.upper[2]
  
  df1<-post %>%select(b_U)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[4]<-df1$b_U[1]
  df$LowCI0.95[4]<-df1$.lower[1]
  df$LowCI0.75[4]<-df1$.lower[2]
  df$UpCI0.95[4]<-df1$.upper[1]
  df$UpCI0.75[4]<-df1$.upper[2]
  
  df1<-post %>%select(b_VR_LdM)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[5]<-df1$b_VR_LdM[1]
  df$LowCI0.95[5]<-df1$.lower[1]
  df$LowCI0.75[5]<-df1$.lower[2]
  df$UpCI0.95[5]<-df1$.upper[1]
  df$UpCI0.75[5]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_Intercept_terres=b_Intercept+b_REALMTerrestrial)%>% select(b_Intercept_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[6]<-df1$b_Intercept_terres[1]
  df$LowCI0.95[6]<-df1$.lower[1]
  df$LowCI0.75[6]<-df1$.lower[2]
  df$UpCI0.95[6]<-df1$.upper[1]
  df$UpCI0.75[6]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_R_terres=b_R+`b_R:REALMTerrestrial`)%>% select(b_R_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[7]<-df1$b_R_terres[1]
  df$LowCI0.95[7]<-df1$.lower[1]
  df$LowCI0.75[7]<-df1$.lower[2]
  df$UpCI0.95[7]<-df1$.upper[1]
  df$UpCI0.75[7]<-df1$.upper[2]
  
 
    df1<-post %>%mutate(b_L_terres=b_L+`b_L:REALMTerrestrial`)%>% select(b_L_terres)%>% 
      tidybayes::median_qi(.width = c(.95,0.75))
    df$Median[8]<-df1$b_L_terres[1]
  df$LowCI0.95[8]<-df1$.lower[1]
  df$LowCI0.75[8]<-df1$.lower[2]
  df$UpCI0.95[8]<-df1$.upper[1]
  df$UpCI0.75[8]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_U_terres=b_U+`b_U:REALMTerrestrial`)%>% select(b_U_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[9]<-df1$b_U_terres[1]
  df$LowCI0.95[9]<-df1$.lower[1]
  df$LowCI0.75[9]<-df1$.lower[2]
  df$UpCI0.95[9]<-df1$.upper[1]
  df$UpCI0.75[9]<-df1$.upper[2]
  
  
  df1<-post %>%mutate(b_VR_LdM_terres=b_VR_LdM+`b_VR_LdM:REALMTerrestrial`)%>% select(b_VR_LdM_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[10]<-df1$b_VR_LdM_terres[1]
  df$LowCI0.95[10]<-df1$.lower[1]
  df$LowCI0.75[10]<-df1$.lower[2]
  df$UpCI0.95[10]<-df1$.upper[1]
  df$UpCI0.75[10]<-df1$.upper[2]
  
  df1<-post %>%select(sd_TAXA__Intercept)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[11]<-df1$sd_TAXA__Intercept[1]
  df$LowCI0.95[11]<-df1$.lower[1]
  df$LowCI0.75[11]<-df1$.lower[2]
  df$UpCI0.95[11]<-df1$.upper[1]
  df$UpCI0.75[11]<-df1$.upper[2]
  
  df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[12]<-df1$`sd_TAXA:UID__Intercept`[1]
  df$LowCI0.95[12]<-df1$.lower[1]
  df$LowCI0.75[12]<-df1$.lower[2]
  df$UpCI0.95[12]<-df1$.upper[1]
  df$UpCI0.75[12]<-df1$.upper[2]
  
  # rearrange df
  
    df<-df[c("Intercept_Freshw","Intercept_Terres",
             "Slope_R_Freshw","Slope_R_Terres",
             "Slope_L_Freshw","Slope_L_Terres",
             "Slope_U_Freshw","Slope_U_Terres",
             "Slope_VR_LdM_Freshw","Slope_VR_LdM_Terres",
             "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]
  
  
  dfbm<-mutate(df, across(where(is.numeric), round, 3))
  row.names(dfbm)<-rownames(df)
  write.csv(dfbm,paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  cat(paste("------- i = ", i ," -------done ------- \n "))
}

taxaeffect<-rbind(taxaeffect_fish,taxaeffect_freshwater.inv,taxaeffect_freshwater.plants,
                  taxaeffect_birds,taxaeffect_terrestrial.inv,taxaeffect_terrestrial.plants,
                  taxaeffect_mammals)

write.csv(taxaeffect,paste("../../Results/gather_res/res_taxa15/LT_and_UT_sep/randomtaxaeffect_summary_fixed_realm.csv",sep=""),row.names = F)

fixedeffect<-c()
for(i in 1:100){
  resloc<-paste("../../Results/gather_res/res_taxa15/LT_and_UT_sep/run_",i,sep="")
  dfbm<-read.csv(paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  dfbm$run<-i
  fixedeffect<-rbind(fixedeffect,dfbm)
}
write.csv(fixedeffect,"../../Results/gather_res/res_taxa15/LT_and_UT_sep/fixedeffectestimate_summary_fixed_realm.csv",row.names = F)

#----------------Now, make table for Summary estimates from 100 runs--------------------------
fixedeffect<-read.csv("../../Results/gather_res/res_taxa15/LT_and_UT_sep/fixedeffectestimate_summary_fixed_realm.csv")
fixedeffect$sig95<-ifelse((fixedeffect$LowCI0.95/fixedeffect$UpCI0.95)>0,1,0) #significance based on 95%CI
fixedeffect$sig75<-ifelse((fixedeffect$LowCI0.75/fixedeffect$UpCI0.75)>0,1,0) #significance based on 75%CI

tempo<-fixedeffect%>%select(X, Median, sig95, sig75, run)

# for intercept

  si<-tempo%>%filter(X%in%c("Intercept_Freshw","Intercept_Terres",
                            "Slope_R_Freshw","Slope_R_Terres",
                            "Slope_VR_LdM_Freshw","Slope_VR_LdM_Terres",
                            "Slope_L_Freshw","Slope_L_Terres",
                            "Slope_U_Freshw","Slope_U_Terres"))

# counting how many times out of 100 runs the median estimate was sig based on 95% and 75% CI
si1<-si%>%group_by(X)%>%summarise(sign95=sum(sig95),
                                  sign75=sum(sig75))%>%ungroup()

# Getting avg estimate with CI of all the median for 100 runs
si2<-si%>%group_by(X)%>%select(Median)%>%
  mean_qi(.width=c(0.95,0.75))%>%ungroup()

si3<-left_join(si2,si1,by="X")

sink("../../Results/gather_res/res_taxa15/LT_and_UT_sep/100runs_fixedeffects_estimates_table.txt")
print(si3,digits=3)
sink()






