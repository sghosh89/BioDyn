rm(list=ls())
library(tidyverse)
# first save the data with newmetric
for(i in 1:100){
  
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  mydat_scaled<-as.data.frame(mydat_scaled)
  
  # now read full data
  df<-read.csv("../../Results/gather_res/data_summary.csv")
  df<-df%>%dplyr::select(source, STUDY_ID,newsite,nsp, L, U, tot_spear_sig,iCValt)
  
  df<-df%>%mutate(UID=paste(source,STUDY_ID,sep=","))%>%
    dplyr::select(-source,-STUDY_ID)
  
  mydat_scaled$tot_spear_sig<-NA
  for(j in 1:nrow(mydat_scaled)){
    #for(j in 70:80){
    id2<-which(df$UID%in%mydat_scaled$UID[j] & df$nsp%in%mydat_scaled$R[j] & df$newsite%in%mydat_scaled$newsite[j])
    df2<-df[id2,]
    df2<-df2%>%distinct()
    id3<-which(abs(df2$iCValt-mydat_scaled$stability_skw[j])<1e-10 & 
                 abs(df2$L-mydat_scaled$L[j])<1e-10  &
                 abs(df2$U-mydat_scaled$U[j])<1e-10)
    mydat_scaled$tot_spear_sig[j]<-df2$tot_spear_sig[id3]
    #print(j)
  }
  
  mydat_scaled$nint<- 0.5*mydat_scaled$R*(mydat_scaled$R - 1)
  mydat_scaled$newcormetric <- mydat_scaled$tot_spear_sig / mydat_scaled$nint
  
  saveRDS(mydat_scaled,paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15_with_newmetric.RDS",sep=""))
  #print(i)
}

library(brms)
reslocn<-"../../Results/gather_res/res_taxa15/newcormetric_res/"
if(!dir.exists(reslocn)){
  dir.create(reslocn)
}
call_toymodel_fixed_realm_w_newcormetric<-function(i){
  
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15_with_newmetric.RDS",sep=""))
  
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR_LdM<- scale(mydat_scaled$VR_LdM) 
  mydat_scaled$A<- scale(mydat_scaled$A)
  mydat_scaled$newcormetric<- scale(mydat_scaled$newcormetric)
  #=====================================================
  
  cat("------- Hierarchical structure in the data -------------- \n ")
  
  resloc<-paste("../../Results/gather_res/res_taxa15/newcormetric_res/run_",i,sep="")
  if(!dir.exists(resloc)){
    dir.create(resloc)
  }
  
  sink(paste(resloc,"/console_bayesian_model_subset_birds_fixed_realm.txt",sep=""),
       append=TRUE, split=TRUE)
  
  
  cat(paste("------- brms model with R, A, newcormetric, REALM starting at time: ", Sys.time()," -------------- \n "))
  bf_stability<-bf(stability_skw ~ (R+A+newcormetric)*REALM + (1|TAXA/UID))
  
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
#call_toymodel_fixed_realm(i=100)
#.rs.restartR()

# fixed_realm
for(i in 1:100){
  call_toymodel_fixed_realm_w_newcormetric(i=i)
}
#==============================
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

# see summary from basic model with R, A, newcormetric, REALM
for(i in c(1:100)){
  resloc<-paste("../../Results/gather_res/res_taxa15/newcormetric_res/run_",i,sep="")
  
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
  
    post<-post%>%select(b_Intercept,b_R,b_A,b_newcormetric,
                        b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_A:REALMTerrestrial`,`b_newcormetric:REALMTerrestrial`,
                        sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
    
 
  df<-data.frame(Median=NA*numeric(10),
                 LowCI0.95=NA*numeric(10),UpCI0.95=NA*numeric(10),
                 LowCI0.75=NA*numeric(10),UpCI0.75=NA*numeric(10))
  
 rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_A_Freshw","Slope_newcormetric_Freshw",
                    "Intercept_Terres","Slope_R_Terres","Slope_A_Terres","Slope_newcormetric_Terres",
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
  
  df1<-post %>%select(b_A)%>% 
      tidybayes::median_qi(.width = c(.95,0.75))
    df$Median[3]<-df1$b_A[1]
  df$LowCI0.95[3]<-df1$.lower[1]
  df$LowCI0.75[3]<-df1$.lower[2]
  df$UpCI0.95[3]<-df1$.upper[1]
  df$UpCI0.75[3]<-df1$.upper[2]
  
  df1<-post %>%select(b_newcormetric)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[4]<-df1$b_newcormetric[1]
  df$LowCI0.95[4]<-df1$.lower[1]
  df$LowCI0.75[4]<-df1$.lower[2]
  df$UpCI0.95[4]<-df1$.upper[1]
  df$UpCI0.75[4]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_Intercept_terres=b_Intercept+b_REALMTerrestrial)%>% select(b_Intercept_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[5]<-df1$b_Intercept_terres[1]
  df$LowCI0.95[5]<-df1$.lower[1]
  df$LowCI0.75[5]<-df1$.lower[2]
  df$UpCI0.95[5]<-df1$.upper[1]
  df$UpCI0.75[5]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_R_terres=b_R+`b_R:REALMTerrestrial`)%>% select(b_R_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[6]<-df1$b_R_terres[1]
  df$LowCI0.95[6]<-df1$.lower[1]
  df$LowCI0.75[6]<-df1$.lower[2]
  df$UpCI0.95[6]<-df1$.upper[1]
  df$UpCI0.75[6]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_A_terres=b_A+`b_A:REALMTerrestrial`)%>% select(b_A_terres)%>% 
      tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[7]<-df1$b_A_terres[1]
  df$LowCI0.95[7]<-df1$.lower[1]
  df$LowCI0.75[7]<-df1$.lower[2]
  df$UpCI0.95[7]<-df1$.upper[1]
  df$UpCI0.75[7]<-df1$.upper[2]
  
  df1<-post %>%mutate(b_newcormetric_terres=b_newcormetric+`b_newcormetric:REALMTerrestrial`)%>% select(b_newcormetric_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[8]<-df1$b_newcormetric_terres[1]
  df$LowCI0.95[8]<-df1$.lower[1]
  df$LowCI0.75[8]<-df1$.lower[2]
  df$UpCI0.95[8]<-df1$.upper[1]
  df$UpCI0.75[8]<-df1$.upper[2]
  
  df1<-post %>%select(sd_TAXA__Intercept)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[9]<-df1$sd_TAXA__Intercept[1]
  df$LowCI0.95[9]<-df1$.lower[1]
  df$LowCI0.75[9]<-df1$.lower[2]
  df$UpCI0.95[9]<-df1$.upper[1]
  df$UpCI0.75[9]<-df1$.upper[2]
  
  df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[10]<-df1$`sd_TAXA:UID__Intercept`[1]
  df$LowCI0.95[10]<-df1$.lower[1]
  df$LowCI0.75[10]<-df1$.lower[2]
  df$UpCI0.95[10]<-df1$.upper[1]
  df$UpCI0.75[10]<-df1$.upper[2]
  
  # rearrange df
  df<-df[c("Intercept_Freshw","Intercept_Terres",
             "Slope_R_Freshw","Slope_R_Terres",
             "Slope_A_Freshw","Slope_A_Terres",
             "Slope_newcormetric_Freshw","Slope_newcormetric_Terres",
             "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]
  
  dfbm<-mutate(df, across(where(is.numeric), round, 3))
  row.names(dfbm)<-rownames(df)
  write.csv(dfbm,paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  cat(paste("------- i = ", i ," -------done ------- \n "))
}

taxaeffect<-rbind(taxaeffect_fish,taxaeffect_freshwater.inv,taxaeffect_freshwater.plants,
                  taxaeffect_birds,taxaeffect_terrestrial.inv,taxaeffect_terrestrial.plants,
                  taxaeffect_mammals)

write.csv(taxaeffect,paste("../../Results/gather_res/res_taxa15/newcormetric_res/randomtaxaeffect_summary_fixed_realm.csv",sep=""),row.names = F)

fixedeffect<-c()
for(i in 1:100){
  resloc<-paste("../../Results/gather_res/res_taxa15/newcormetric_res/run_",i,sep="")
  dfbm<-read.csv(paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  dfbm$run<-i
  fixedeffect<-rbind(fixedeffect,dfbm)
}
write.csv(fixedeffect,paste("../../Results/gather_res/res_taxa15/newcormetric_res/fixedeffectestimate_summary_fixed_realm.csv",sep=""),row.names = F)

#----------------Now, make table for Summary estimates from 100 runs--------------------------
fixedeffect<-read.csv(paste("../../Results/gather_res/res_taxa15/newcormetric_res/fixedeffectestimate_summary_fixed_realm.csv",sep=""))
fixedeffect$sig95<-ifelse((fixedeffect$LowCI0.95/fixedeffect$UpCI0.95)>0,1,0) #significance based on 95%CI
fixedeffect$sig75<-ifelse((fixedeffect$LowCI0.75/fixedeffect$UpCI0.75)>0,1,0) #significance based on 75%CI

tempo<-fixedeffect%>%select(X, Median, sig95, sig75, run)

# for intercept
si<-tempo%>%filter(X%in%c("Intercept_Freshw","Intercept_Terres",
                            "Slope_R_Freshw","Slope_R_Terres",
                            "Slope_newcormetric_Freshw","Slope_newcormetric_Terres",
                            "Slope_A_Freshw","Slope_A_Terres"))



# counting how many times out of 100 runs the median estimate was sig based on 95% and 75% CI
si1<-si%>%group_by(X)%>%summarise(sign95=sum(sig95),
                                  sign75=sum(sig75))%>%ungroup()

# Getting avg estimate with CI of all the median for 100 runs
si2<-si%>%group_by(X)%>%select(Median)%>%
  mean_qi(.width=c(0.95,0.75))%>%ungroup()

si3<-left_join(si2,si1,by="X")

sink(paste("../../Results/gather_res/res_taxa15/newcormetric_res/100runs_fixedeffects_estimates_table.txt",sep=""))
print(si3,digits=3)
sink()












