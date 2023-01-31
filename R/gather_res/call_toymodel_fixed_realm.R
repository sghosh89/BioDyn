library(brms)
call_toymodel_fixed_realm<-function(i){
  
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR_LdM<- scale(mydat_scaled$VR_LdM) 
  mydat_scaled$A<- scale(mydat_scaled$A)
  
  #=====================================================
  
  # (R+VR)*REALM = R + VR + REALM + R:REALM + VR:REALM
  
  cat("------- Hierarchical structure in the data -------------- \n ")
  
  resloc<-paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm",sep="")
  if(!dir.exists(resloc)){
    dir.create(resloc)
  }
  
  sink(paste(resloc,"/console_bayesian_model_subset_birds_fixed_realm.txt",sep=""),
       append=TRUE, split=TRUE)
  
  cat(paste("------- brms basic model with R starting at time: ", Sys.time()," -------------- \n "))
  
  bf_stability0<-bf(stability_skw ~ R + (1|TAXA/UID))
  basic_model_w_R<-brm(bf_stability0,
                       data=mydat_scaled,
                       family = gaussian(),
                       chains=4,cores=4,iter=16000,
                       warmup=12000,inits="random",thin=4,
                       control = list(adapt_delta = 0.99, max_treedepth = 15),
                       save_pars = save_pars(all = TRUE),seed=123)
  print(summary(basic_model_w_R),digits = 3)
  saveRDS(basic_model_w_R,paste(resloc,"/basic_model_w_R.RDS",sep=""))
  
  cat(paste("------- brms basic model with R and REALM starting at time: ", Sys.time()," -------------- \n "))
  
  bf_stability0<-bf(stability_skw ~ R*REALM + (1|TAXA/UID))
  basic_model_w_R_REALM<-brm(bf_stability0,
                             data=mydat_scaled,
                             family = gaussian(),
                             chains=4,cores=4,iter=16000,
                             warmup=12000,inits="random",thin=4,
                             control = list(adapt_delta = 0.99, max_treedepth = 15),
                             save_pars = save_pars(all = TRUE),seed=123)
  print(summary(basic_model_w_R_REALM),digits = 3)
  saveRDS(basic_model_w_R_REALM,paste(resloc,"/basic_model_w_R_REALM.RDS",sep=""))
  
  cat(paste("------- brms basic model with R, VR_LdM starting at time: ", Sys.time()," -------------- \n "))
  
  bf_stability0<-brms::bf(stability_skw ~ (R+VR_LdM) + (1|TAXA/UID))
  basic_model_w_R_VR<-brms::brm(bf_stability0,
                                data=mydat_scaled,
                                family = gaussian(),
                                chains=4,cores=4,iter=16000,
                                warmup=12000,inits="random",thin=4,
                                control = list(adapt_delta = 0.99, max_treedepth = 15),
                                save_pars = save_pars(all = TRUE),seed=123)
  print(summary(basic_model_w_R_VR),digits = 3)
  saveRDS(basic_model_w_R_VR,paste(resloc,"/basic_model_w_R_VR.RDS",sep=""))
  
  cat(paste("------- brms basic model with R, VR_LdM, REALM starting at time: ", Sys.time()," -------------- \n "))
  
  bf_stability0<-brms::bf(stability_skw ~ (R+VR_LdM)*REALM + (1|TAXA/UID))
  basic_model_w_R_VR_REALM<-brms::brm(bf_stability0,
                                data=mydat_scaled,
                                family = gaussian(),
                                chains=4,cores=4,iter=16000,
                                warmup=12000,inits="random",thin=4,
                                control = list(adapt_delta = 0.99, max_treedepth = 15),
                                save_pars = save_pars(all = TRUE),seed=123)
  print(summary(basic_model_w_R_VR_REALM),digits = 3)
  saveRDS(basic_model_w_R_VR_REALM,paste(resloc,"/basic_model_w_R_VR_REALM.RDS",sep=""))
  
  cat(paste("------- brms basic_model_w R, A starting at time: ", Sys.time()," -------------- \n "))
  bf_stability0<-brms::bf(stability_skw ~ (R+A) + (1|TAXA/UID))
  
  basic_model_w_R_A<-brms::brm(bf_stability0,
                               data=mydat_scaled,
                               family = gaussian(),
                               chains=4,cores=4,iter=16000,
                               warmup=12000,inits="random",thin=4,
                               control = list(adapt_delta = 0.99, max_treedepth = 15),
                               save_pars = save_pars(all = TRUE),seed=123)
  
  print(summary(basic_model_w_R_A),digits = 3)
  saveRDS(basic_model_w_R_A,paste(resloc,"/basic_model_w_R_A.RDS",sep=""))
  
  cat(paste("------- brms basic_model_w R, A starting at time: ", Sys.time()," -------------- \n "))
  bf_stability0<-brms::bf(stability_skw ~ (R+A)*REALM + (1|TAXA/UID))
  
  basic_model_w_R_A_REALM<-brms::brm(bf_stability0,
                               data=mydat_scaled,
                               family = gaussian(),
                               chains=4,cores=4,iter=16000,
                               warmup=12000,inits="random",thin=4,
                               control = list(adapt_delta = 0.99, max_treedepth = 15),
                               save_pars = save_pars(all = TRUE),seed=123)
  print(summary(basic_model_w_R_A_REALM),digits = 3)
  saveRDS(basic_model_w_R_A_REALM,paste(resloc,"/basic_model_w_R_A_REALM.RDS",sep=""))
  
  cat(paste("------- brms model with R, A, VR_LdM starting at time: ", Sys.time()," -------------- \n "))
  bf_stability0<-brms::bf(stability_skw ~ (R+A+VR_LdM) + (1|TAXA/UID))
  
  basic_model_w_R_A_VR<-brms::brm(bf_stability0,
                                  data=mydat_scaled,
                                  family = gaussian(),
                                  chains=4,cores=4,iter=16000,
                                  warmup=12000,inits="random",thin=4,
                                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                                  save_pars = save_pars(all = TRUE),seed=123)
  
  print(summary(basic_model_w_R_A_VR),digits = 3)
  saveRDS(basic_model_w_R_A_VR,paste(resloc,"/basic_model_w_R_A_VR.RDS",sep=""))
  
  cat(paste("------- brms model with R, A, VR_LdM, REALM starting at time: ", Sys.time()," -------------- \n "))
  bf_stability<-bf(stability_skw ~ (R+A+VR_LdM)*REALM + (1|TAXA/UID))
  
  full_model<-brm(bf_stability,
                  data=mydat_scaled,
                  family = gaussian(),
                  chains=4,cores=4,iter=16000,
                  warmup=12000,inits="random",thin=4,
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
  call_toymodel_fixed_realm(i=i)
}
#.rs.restartR() 

# We will now see a summary how consistent is the result for all runs
#################################
library(performance)

for (i in 1:100){
  # read models
  resloc<-paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm",sep="")
  
  basic_model_w_R<-readRDS(paste(resloc,"/basic_model_w_R.RDS",sep=""))
  basic_model_w_R_REALM<-readRDS(paste(resloc,"/basic_model_w_R_REALM.RDS",sep=""))
  basic_model_w_R_VR<-readRDS(paste(resloc,"/basic_model_w_R_VR.RDS",sep=""))
  basic_model_w_R_VR_REALM<-readRDS(paste(resloc,"/basic_model_w_R_VR_REALM.RDS",sep=""))
  basic_model_w_R_A<-readRDS(paste(resloc,"/basic_model_w_R_A.RDS",sep=""))
  basic_model_w_R_A_REALM<-readRDS(paste(resloc,"/basic_model_w_R_A_REALM.RDS",sep=""))
  basic_model_w_R_A_VR<-readRDS(paste(resloc,"/basic_model_w_R_A_VR.RDS",sep=""))
  full_model<-readRDS(paste(resloc,"/full_model.RDS",sep=""))
  
  
  cp<-compare_performance(basic_model_w_R, basic_model_w_R_REALM,
                          basic_model_w_R_VR,basic_model_w_R_VR_REALM,
                          basic_model_w_R_A,basic_model_w_R_A_REALM,
                          basic_model_w_R_A_VR,full_model, rank=T, metrics="common")
  
  saveRDS(cp,paste(resloc,"/cp.RDS",sep=""))
  row.names(cp)<-cp[,1]
  cp<-cp[,3:11]
  cp<-format(cp,3)
  write.csv(cp,paste(resloc,"/cp.csv",sep=""))
  cat(paste("------- i = ", i ," -------done ------- \n "))
}
###########################################################################################
#---------------------------------------- PLOT ----------------------------------------
###########################################################################################
# summary plot for performance
rm(list=ls())
library(dplyr)
library(gdata)

if(!dir.exists("../../Results/gather_res/res_taxa15/summary_fixed_realm/")){
  dir.create("../../Results/gather_res/res_taxa15/summary_fixed_realm/")
}
pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/cp_summary_fixed_realm_trajectory.pdf",height=4,width=8)
op<-par(mar=c(8,3,1,1))
plot(1, type = "n",                        # Remove all elements of plot
     xlab = "", ylab = "",xaxt="n",
     ylim = c(0,100), xlim=c(1,8))
cp_all<-c()
for(i in c(1:100)){
  resloc<-paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm",sep="")
  cp<-read.csv(paste(resloc,"/cp.csv",sep=""))
  cp$rank<-1:8
  cp<-cp[,c(1,8,10,11)]
  cp$X <- gsub("basic_model_w_", "", cp$X)
  cp$X<- gsub("R_A_REALM", "REALM_R_A", cp$X)
  cp$X<- gsub("R_VR_REALM", "REALM_R_VR", cp$X)
  cp$X<- gsub("R_REALM", "REALM_R", cp$X)
  cp$X<- gsub("full_model", "REALM_R_A_VR", cp$X)
  cp$REALM<-ifelse(startsWith(cp$X,"REALM_"),1,0)
  cp$X<-as.factor(cp$X)
  cp$Performance.Score <- gsub("%", "", cp$Performance.Score)
  cp$Performance.Score<-as.numeric(cp$Performance.Score)
  cp<-cp%>%arrange(X)
  cp_all<-rbind(cp_all,cp)
  lines(cp$X,cp$Performance.Score,type="o", col=rgb(0,0,0,0.3)) # overall performance
  #lines(cp$X,cp$R2..marg..,type="o", col=rgb(0,0,0,0.3)) # for marginal R2
}
axis(1, at = 1:8, las=2, labels=cp$X)
par(op)
dev.off()
#-------------------------------------------------
library(tidyverse)
library(PupillometryR)
cp_all$REALM<-as.factor(cp_all$REALM)
gs<-ggplot(data = cp_all, aes(y = Performance.Score, x = X, fill=REALM)) +
  scale_fill_manual(values=alpha(c("white","grey"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) +
  geom_point(aes(y = Performance.Score),  
             position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Score")+xlab("Predictors used")+
  theme_bw()+
  theme(
    panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("white","grey"), 1))
pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/cp_summary_fixed_realm.pdf",height=4,width=8)
gs
dev.off()

# see how many times model including predictor "A" get top ranks?
# and plot frequency of performance for each model
fp<-as.data.frame(matrix(NA,nrow=8, ncol=8))
colnames(fp)<-cp$X  
dummytab<-data.frame(Var1=as.factor(1:8), Freq=0*numeric(8)) # Var1 is the 1:8 model rank levels

for(i in 1:8){
  tempo <- cp_all%>%filter(X%in%colnames(fp)[i])
  bb<-as.data.frame(table(tempo$rank)/(nrow(cp_all)/8))
  
  tt<-merge(dummytab,bb,by="Var1",all=T)
  tt$Freq.y[is.na(tt$Freq.y)]<-0
  
  fp[,i]<-tt$Freq.y
}
fp<-cbind(Rank=1:8,fp)
write.csv(fp,"../../Results/gather_res/res_taxa15/summary_fixed_realm/freq_performance_summary_fixed_realm.csv",row.names = F)

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/freq_performance_summary_fixed_realm.pdf",height=5,width=8)
fp_g<-fp %>% gather(predictors, Frequency, -Rank)
fp_g$REALM<-ifelse(startsWith(fp_g$predictors,"REALM_"),1,2)
fp_g1<-ggplot(data = fp_g, aes(y = Frequency, x = Rank, col=predictors))+
  ylab("Frequency")+xlab("Performance-ranking (highest =1, lowest =8)")+
  geom_line(linetype=fp_g$REALM)+geom_point()+theme_bw()+
  theme(text=element_text(size=12),legend.key.size = unit(1.2, 'cm'))+
  guides(color = guide_legend(override.aes = list(linetype=c(2,2,2,2,1,1,1,1))))
fp_g1
dev.off()

#=============================================================
# overall difference between both realms
# summary for only full model comparisons
#---------------------------------------------
####################
rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)
####################
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

# see summary from basic model with R, A, VR, REALM
for(i in c(1:100)){
  resloc<-paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm",sep="")
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
  
  post<-post%>%select(b_Intercept,b_R,b_A,b_VR_LdM,
                      b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_A:REALMTerrestrial`,`b_VR_LdM:REALMTerrestrial`,
                      sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
  df<-data.frame(Median=NA*numeric(10),
                 LowCI0.95=NA*numeric(10),UpCI0.95=NA*numeric(10),
                 LowCI0.75=NA*numeric(10),UpCI0.75=NA*numeric(10))
  
  rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_A_Freshw","Slope_VR_LdM_Freshw",
                  "Intercept_Terres","Slope_R_Terres","Slope_A_Terres","Slope_VR_LdM_Terres",
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
  
  df1<-post %>%select(b_VR_LdM)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[4]<-df1$b_VR_LdM[1]
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
  
  df1<-post %>%mutate(b_VR_LdM_terres=b_VR_LdM+`b_VR_LdM:REALMTerrestrial`)%>% select(b_VR_LdM_terres)%>% 
    tidybayes::median_qi(.width = c(.95,0.75))
  df$Median[8]<-df1$b_VR_LdM_terres[1]
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

write.csv(taxaeffect,"../../Results/gather_res/res_taxa15/summary_fixed_realm/randomtaxaeffect_summary_fixed_realm.csv",row.names = F)

fixedeffect<-c()
for(i in 1:100){
  resloc<-paste("../../Results/gather_res/res_taxa15/run_",i,"/res_fixed_realm",sep="")
  dfbm<-read.csv(paste(resloc,"/summary_table_for_full_model.csv",sep=""))
  dfbm$run<-i
  fixedeffect<-rbind(fixedeffect,dfbm)
}
write.csv(fixedeffect,"../../Results/gather_res/res_taxa15/summary_fixed_realm/fixedeffectestimate_summary_fixed_realm.csv",row.names = F)

#------------------------------------------------
# Now plot the summary estimates for 100 runs

# fixedeffect, intercept (freshw, terrestrial)
fix_i<-fixedeffect%>%filter(X%in%c("Intercept_Freshw","Intercept_Terres"))

fix_i_p<-fix_i%>%ggplot(aes(x=run,y=Median,col=as.factor(X)))+
  geom_hline(yintercept = 0, linetype=2, color="dimgray")+
  #geom_point()+
  geom_line()+
  xlab("Run")+ylab("Intercept")+
  #geom_ribbon(aes(ymin=LowCI0.75, ymax=UpCI0.75), linetype=2, alpha=0.1)+
  geom_ribbon(aes(ymin=LowCI0.75, ymax=UpCI0.75,fill=as.factor(X)), linetype=0, alpha=0.4)+
  geom_ribbon(aes(ymin=LowCI0.95, ymax=UpCI0.95,fill=as.factor(X)), linetype=0, alpha=0.18)+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 18)+theme(#axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 legend.position = "none")
fix_i_p

# fixedeffect, slope_R (freshw, terrestrial)
fix_R<-fixedeffect%>%filter(X%in%c("Slope_R_Freshw","Slope_R_Terres"))

fix_R_p<-fix_R%>%ggplot(aes(x=run,y=Median,col=as.factor(X)))+
  geom_hline(yintercept = 0, linetype=2, color="dimgray")+
  #geom_point()+
  geom_line()+
  xlab("Run")+ylab("Slope_R")+
  geom_ribbon(aes(ymin=LowCI0.75, ymax=UpCI0.75,fill=as.factor(X)), linetype=0, alpha=0.25)+
  geom_ribbon(aes(ymin=LowCI0.95, ymax=UpCI0.95,fill=as.factor(X)), linetype=0, alpha=0.15)+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 18)+theme(#axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 legend.position = "none")
fix_R_p

# fixedeffect, slope_VR (freshw, terrestrial)
fix_VR<-fixedeffect%>%filter(X%in%c("Slope_VR_LdM_Freshw","Slope_VR_LdM_Terres"))

fix_VR_p<-fix_VR%>%ggplot(aes(x=run,y=Median,col=as.factor(X)))+
  geom_hline(yintercept = 0, linetype=2, color="dimgray")+
  #geom_point()+
  geom_line()+
  xlab("Run")+ylab("Slope_VR")+
  geom_ribbon(aes(ymin=LowCI0.75, ymax=UpCI0.75,fill=as.factor(X)), linetype=0, alpha=0.25)+
  geom_ribbon(aes(ymin=LowCI0.95, ymax=UpCI0.95,fill=as.factor(X)), linetype=0, alpha=0.15)+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 18)+theme(#axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 legend.position = "none")
fix_VR_p

# fixedeffect, slope_A (freshw, terrestrial)
fix_A<-fixedeffect%>%filter(X%in%c("Slope_A_Freshw","Slope_A_Terres"))

fix_A_p<-fix_A%>%ggplot(aes(x=run,y=Median,col=as.factor(X)))+
  geom_hline(yintercept = 0, linetype=2, color="dimgray")+
  #geom_point()+
  geom_line()+
  xlab("Run")+ylab("Slope_A")+
  geom_ribbon(aes(ymin=LowCI0.75, ymax=UpCI0.75,fill=as.factor(X)), linetype=0, alpha=0.25)+
  geom_ribbon(aes(ymin=LowCI0.95, ymax=UpCI0.95,fill=as.factor(X)), linetype=0, alpha=0.15)+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 18)+theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())
fix_A_p

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/fixedeffectestimates_summary_fixed_realm.pdf",height=6,width=12)
op<-par(mar=c(4,4,2,2))
gridExtra::grid.arrange(fix_i_p,fix_R_p,fix_VR_p,fix_A_p,nrow=2)
par(op)
dev.off()

#------------------------------------------------
# Now plot the summary estimates for 100 runs, random taxaeffect 
taxaeffect$REALM<-ifelse(taxaeffect$taxa%in%c("fish","freshwater.inv","freshwater.plants"),0,1)
tx<-taxaeffect%>%ggplot(aes(x=run,y=Median,col=as.factor(REALM)))+
  geom_hline(yintercept = 0, linetype=2, color="dimgray")+
  #geom_point()+
  geom_line()+
  xlab("Run")+ylab("Effect size due to taxa")+
  geom_ribbon(aes(ymin=LowCI0.75, ymax=UpCI0.75,fill=as.factor(REALM)), linetype=0, alpha=0.3)+
  geom_ribbon(aes(ymin=LowCI0.95, ymax=UpCI0.95,fill=as.factor(REALM)), linetype=0, alpha=0.15)+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1),guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 18)+theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+facet_wrap(~taxa)

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/randomtaxaeffect_summary_fixed_realm.pdf",height=6,width=10)
tx
dev.off()


#----------------Now, make table for Summary estimates from 100 runs--------------------------
rm(list=ls())
library(tidyverse)
library(tidybayes)
fixedeffect<-read.csv("../../Results/gather_res/res_taxa15/summary_fixed_realm/fixedeffectestimate_summary_fixed_realm.csv")
fixedeffect$sig95<-ifelse((fixedeffect$LowCI0.95/fixedeffect$UpCI0.95)>0,1,0) #significance based on 95%CI
fixedeffect$sig75<-ifelse((fixedeffect$LowCI0.75/fixedeffect$UpCI0.75)>0,1,0) #significance based on 75%CI

tempo<-fixedeffect%>%select(X, Median, sig95, sig75, run)

# for intercept
si<-tempo%>%filter(X%in%c("Intercept_Freshw","Intercept_Terres",
                          "Slope_R_Freshw","Slope_R_Terres",
                          "Slope_VR_LdM_Freshw","Slope_VR_LdM_Terres",
                          "Slope_A_Freshw","Slope_A_Terres"))

# counting how many times out of 100 runs the median estimate was sig based on 95% and 75% CI
si1<-si%>%group_by(X)%>%summarise(sign95=sum(sig95),
                                 sign75=sum(sig75))%>%ungroup()

# Getting avg estimate with CI of all the median for 100 runs
si2<-si%>%group_by(X)%>%select(Median)%>%
  mean_qi(.width=c(0.95,0.75))%>%ungroup()

si3<-left_join(si2,si1,by="X")

sink("../../Results/gather_res/res_taxa15/summary_fixed_realm/100runs_fixedeffects_estimates_table.txt")
print(si3,digits=3)
sink()
#=============================================================
# summary plot with the raw data used for each run
rm(list=ls())
library(tidyverse)
library(PupillometryR)
i<-1
dd<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
dd$Run<-i
for(i in 2:100){
  d1<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  d1$Run<-i
  dd<-rbind(dd,d1)
}

# richness by REALM
ddr<-ggplot(data = dd, aes(y = R, x = REALM, fill = REALM)) +
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = R, color=REALM), position = position_jitter(width = 0.15), size = 1, alpha = 0.3) +
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 0.3))+
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.3)+
  ylab("Richness")+xlab("REALM")+
  theme_bw()+
  theme(legend.position="none",
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    text=element_text(size=20))

# richness by taxa
myColors<- c("royalblue","skyblue","blue",
             "green3","olivedrab2","yellowgreen","seagreen1")
dd$TAXA <- factor(dd$TAXA , levels=c("fish", "freshwater invertebrates", "freshwater plants",
                                     "birds", "terrestrial invertebrates", "terrestrial plants", "mammals"))
ddt<-ggplot(data = dd, aes(y = R, x = TAXA, fill=TAXA)) +
  scale_fill_manual(values=alpha(myColors, 1))+
  geom_point(aes(y = R, color = TAXA), position = position_jitter(width = .15), size = 1, alpha = 0.3) +
  scale_color_manual(values=alpha(myColors, 0.3))+
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.3, show.legend = F)+
  ylab("Richness")+xlab("TAXA")+
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    text=element_text(size=20))+
  guides(color = guide_legend(override.aes = list(size = 3, alpha=0.9)))

pdf("../../Results/gather_res/res_taxa15/summary_fixed_realm/richness_summary_fixed_realm.pdf",height=10,width=10)
op<-par(mar=c(6,4,2,2))
gridExtra::grid.arrange(ddr, ddt,nrow=2)
par(op)
dev.off()

