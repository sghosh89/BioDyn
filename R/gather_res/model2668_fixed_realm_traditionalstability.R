library(brms)
library(tidyverse)
library(tidybayes)
library(PupillometryR)
resloc<-paste("../../Results/gather_res/res_model2668_fixed_realm_traditionalstability",sep="")
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#-------------------------------------------------------------
df<-read.csv("../../Results/gather_res/data_summary.csv")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_LdM","nsp","L","U","f_nL","f_nU","f_nneg","tot_spear_sig")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U # net asymmetry
mydat$absU<-abs(mydat$U)


mydat<-mydat%>%dplyr::rename(
  stability=iCV,
  VR_LdM=phi_LdM,
  R=nsp)
mydat$nint<-mydat$R *(mydat$R -1)*0.5
mydat<-mydat%>%select(REALM,TAXA,UID,stability,R,VR_LdM,L,U, absU,A,tot_spear_sig,nint)
mydat$newcormetric <- mydat$tot_spear_sig / mydat$nint

write.csv(mydat, paste(resloc,"/mydat_2668.csv",sep=""), row.names = F)
#====================================
mydat_scaled<-mydat
mydat_scaled$stability<-scale(mydat_scaled$stability)
mydat_scaled$R<-scale(mydat_scaled$R)
mydat_scaled$VR_LdM<- scale(mydat_scaled$VR_LdM) 
mydat_scaled$A<- scale(mydat_scaled$A)

sink(paste(resloc,"/console_bayesian_model_fixed_realm.txt",sep=""),
     append=TRUE, split=TRUE)

cat(paste("------- brms basic model with R starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability ~ R + (1|TAXA/UID))
basic_model_w_R<-brm(bf_stability0,
                     data=mydat_scaled,
                     family = gaussian(),
                     chains=4,cores=4,iter=16000,
                     warmup=12000,init="random",thin=4,
                     control = list(adapt_delta = 0.99, max_treedepth = 15),
                     save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R),digits = 3)
saveRDS(basic_model_w_R,paste(resloc,"/basic_model_w_R.RDS",sep=""))

cat(paste("------- brms basic model with R and REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-bf(stability ~ R*REALM + (1|TAXA/UID))
basic_model_w_R_REALM<-brm(bf_stability0,
                           data=mydat_scaled,
                           family = gaussian(),
                           chains=4,cores=4,iter=16000,
                           warmup=12000,init="random",thin=4,
                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                           save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_REALM),digits = 3)
saveRDS(basic_model_w_R_REALM,paste(resloc,"/basic_model_w_R_REALM.RDS",sep=""))

cat(paste("------- brms basic model with R, VR_LdM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-brms::bf(stability ~ (R+VR_LdM) + (1|TAXA/UID))
basic_model_w_R_VR<-brms::brm(bf_stability0,
                              data=mydat_scaled,
                              family = gaussian(),
                              chains=4,cores=4,iter=16000,
                              warmup=12000,init="random",thin=4,
                              control = list(adapt_delta = 0.99, max_treedepth = 15),
                              save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_VR),digits = 3)
saveRDS(basic_model_w_R_VR,paste(resloc,"/basic_model_w_R_VR.RDS",sep=""))

cat(paste("------- brms basic model with R, VR_LdM, REALM starting at time: ", Sys.time()," -------------- \n "))

bf_stability0<-brms::bf(stability ~ (R+VR_LdM)*REALM + (1|TAXA/UID))
basic_model_w_R_VR_REALM<-brms::brm(bf_stability0,
                                    data=mydat_scaled,
                                    family = gaussian(),
                                    chains=4,cores=4,iter=16000,
                                    warmup=12000,init="random",thin=4,
                                    control = list(adapt_delta = 0.99, max_treedepth = 15),
                                    save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_VR_REALM),digits = 3)
saveRDS(basic_model_w_R_VR_REALM,paste(resloc,"/basic_model_w_R_VR_REALM.RDS",sep=""))

cat(paste("------- brms basic_model_w R, A starting at time: ", Sys.time()," -------------- \n "))
bf_stability0<-brms::bf(stability ~ (R+A) + (1|TAXA/UID))

basic_model_w_R_A<-brms::brm(bf_stability0,
                             data=mydat_scaled,
                             family = gaussian(),
                             chains=4,cores=4,iter=16000,
                             warmup=12000,init="random",thin=4,
                             control = list(adapt_delta = 0.99, max_treedepth = 15),
                             save_pars = save_pars(all = TRUE),seed=123)

print(summary(basic_model_w_R_A),digits = 3)
saveRDS(basic_model_w_R_A,paste(resloc,"/basic_model_w_R_A.RDS",sep=""))

cat(paste("------- brms basic_model_w R, A starting at time: ", Sys.time()," -------------- \n "))
bf_stability0<-brms::bf(stability ~ (R+A)*REALM + (1|TAXA/UID))

basic_model_w_R_A_REALM<-brms::brm(bf_stability0,
                                   data=mydat_scaled,
                                   family = gaussian(),
                                   chains=4,cores=4,iter=16000,
                                   warmup=12000,init="random",thin=4,
                                   control = list(adapt_delta = 0.99, max_treedepth = 15),
                                   save_pars = save_pars(all = TRUE),seed=123)
print(summary(basic_model_w_R_A_REALM),digits = 3)
saveRDS(basic_model_w_R_A_REALM,paste(resloc,"/basic_model_w_R_A_REALM.RDS",sep=""))

cat(paste("------- brms model with R, A, VR_LdM starting at time: ", Sys.time()," -------------- \n "))
bf_stability0<-brms::bf(stability ~ (R+A+VR_LdM) + (1|TAXA/UID))

basic_model_w_R_A_VR<-brms::brm(bf_stability0,
                                data=mydat_scaled,
                                family = gaussian(),
                                chains=4,cores=4,iter=16000,
                                warmup=12000,init="random",thin=4,
                                control = list(adapt_delta = 0.99, max_treedepth = 15),
                                save_pars = save_pars(all = TRUE),seed=123)

print(summary(basic_model_w_R_A_VR),digits = 3)
saveRDS(basic_model_w_R_A_VR,paste(resloc,"/basic_model_w_R_A_VR.RDS",sep=""))

cat(paste("------- brms model with R, A, VR_LdM, REALM starting at time: ", Sys.time()," -------------- \n "))
bf_stability<-bf(stability ~ (R+A+VR_LdM)*REALM + (1|TAXA/UID))

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
#-----------------------------------

#============ model performance ==================

library(performance)

# read models
resloc<-paste("../../Results/gather_res/res_model2668_fixed_realm_traditionalstability/",sep="")

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

cp$pu<-c("REALM, R, TA, LMS", 
         "REALM, R, LMS", 
         "REALM, R, TA", 
         "R, TA, LMS", 
         "R, LMS", 
         "R, TA", 
         "REALM, R", 
         "R")
# plot for performance score
p<-ggplot(data=cp, aes(x=Performance_Score, y=pu, fill=pu)) +
  geom_bar(stat="identity")+xlab("Score")+ylab("Predictors' used")+
  scale_fill_manual(values=c("#A6CEE3", #1
                             "#CCCCCC", #2 light gray
                             "#FFCC99", #3 light orange
                             "#CAB2D6", #4
                             "#1F78B4", #5
                             "#666666", #6 deep gray
                             "#CC6600", #7 deep orange
                             "#6A3D9A" #8
  ))+theme_classic()+theme(legend.position = "none")
p
pdf(paste(resloc,"/cp.pdf",sep=""),height=4,width=6)
p
dev.off()# results similar to that of 100 replicates

#=============================================================
# overall difference between both realms
# summary for only full model comparisons

taxaeffect_fish<-data.frame(taxa="fish",
                            Median=NA*numeric(1),
                            LowCI0.95=NA*numeric(1),
                            UpCI0.95=NA*numeric(1),
                            LowCI0.75=NA*numeric(1),
                            UpCI0.75=NA*numeric(1),
                            run=NA*numeric(1))
taxaeffect_freshwater.inv<-taxaeffect_freshwater.plants<-taxaeffect_birds<-
  taxaeffect_terrestrial.inv<-taxaeffect_terrestrial.plants<-taxaeffect_mammals<-
  taxaeffect_fish
taxaeffect_freshwater.inv$taxa<-"freshwater.inv"
taxaeffect_freshwater.plants$taxa<-"freshwater.plants"
taxaeffect_birds$taxa<-"birds"
taxaeffect_terrestrial.inv$taxa<-"terrestrial.inv"
taxaeffect_terrestrial.plants$taxa<-"terrestrial.plants"
taxaeffect_mammals$taxa<-"mammals"


bm<-readRDS(paste(resloc,"/full_model.RDS",sep=""))
post<-as_draws_df(bm)

#----------------------------------taxa effect data -------
tx<-post%>%dplyr::select(b_Intercept,b_REALMTerrestrial,
                         `r_TAXA[fish,Intercept]`,
                         `r_TAXA[freshwater.invertebrates,Intercept]`,
                         `r_TAXA[freshwater.plants,Intercept]`,
                         `r_TAXA[birds,Intercept]`,
                         `r_TAXA[terrestrial.invertebrates,Intercept]`,
                         `r_TAXA[terrestrial.plants,Intercept]`,
                         `r_TAXA[mammals,Intercept]`)

# following columns are the original source for objc$TAXA[,,"Intercept"]
tx$fish<-tx$`r_TAXA[fish,Intercept]`#+tx$b_Intercept
tx$freshwater.inv<-tx$`r_TAXA[freshwater.invertebrates,Intercept]`#+tx$b_Intercept
tx$freshwater.plants<-tx$`r_TAXA[freshwater.plants,Intercept]`#+tx$b_Intercept

tx$birds<-tx$`r_TAXA[birds,Intercept]`#+tx$b_Intercept+tx$b_REALMTerrestrial
tx$terrestrial.inv<-tx$`r_TAXA[terrestrial.invertebrates,Intercept]`#+tx$b_Intercept+tx$b_REALMTerrestrial
tx$terrestrial.plants<-tx$`r_TAXA[terrestrial.plants,Intercept]`#+tx$b_Intercept+tx$b_REALMTerrestrial
tx$mammals<-tx$`r_TAXA[mammals,Intercept]`#+tx$b_Intercept+tx$b_REALMTerrestrial

tx<-tx %>% 
  select(fish,freshwater.inv,freshwater.plants,
         birds,terrestrial.inv,terrestrial.plants,mammals)

tx1<-tx%>%select(fish)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_fish$run[1]<-1
taxaeffect_fish[1,2:6]<-tx1[1,]

tx1<-tx%>%select(freshwater.inv)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_freshwater.inv$run[1]<-1
taxaeffect_freshwater.inv[1,2:6]<-tx1[1,]

tx1<-tx%>%select(freshwater.plants)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_freshwater.plants$run[1]<-1
taxaeffect_freshwater.plants[1,2:6]<-tx1[1,]

tx1<-tx%>%select(birds)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_birds$run[1]<-1
taxaeffect_birds[1,2:6]<-tx1[1,]

tx1<-tx%>%select(terrestrial.inv)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_terrestrial.inv$run[1]<-1
taxaeffect_terrestrial.inv[1,2:6]<-tx1[1,]

tx1<-tx%>%select(terrestrial.plants)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_terrestrial.plants$run[1]<-1
taxaeffect_terrestrial.plants[1,2:6]<-tx1[1,]

tx1<-tx%>%select(mammals)%>%median_qi(.width = c(.95,0.75))
tx1<-cbind(tx1[1,1:3],tx1[2,2:3])
taxaeffect_mammals$run[1]<-1
taxaeffect_mammals[1,2:6]<-tx1[1,]

taxaeffect<-rbind(taxaeffect_fish,taxaeffect_freshwater.inv,taxaeffect_freshwater.plants,
                  taxaeffect_birds,taxaeffect_terrestrial.inv,taxaeffect_terrestrial.plants,
                  taxaeffect_mammals)

write.csv(taxaeffect,paste(resloc,"/randomtaxaeffect_summary_fixed_realm.csv",sep=""),row.names = F)

#=================== fixed effect summary ====================
post<-post%>%dplyr::select(b_Intercept,b_R,b_A,b_VR_LdM,
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

#----------------------------------------------------------
# Now plot the summary estimates, random taxaeffect 
taxaeffect$REALM<-ifelse(taxaeffect$taxa%in%c("fish","freshwater.inv","freshwater.plants"),0,1)

taxaeffect$taxa<-factor(taxaeffect$taxa, levels=c("birds", "mammals", 
                                                  "terrestrial.inv", "terrestrial.plants",
                                                  "fish", 
                                                  "freshwater.inv", "freshwater.plants")
)
tx<-taxaeffect%>%ggplot(aes(x=taxa,y=Median,col=as.factor(REALM)))+
  geom_hline(yintercept = 0, linetype=2, color="dimgray")+
  geom_point(size=4)+
  xlab("Taxa")+ylab("Random effects due to taxa")+
  geom_crossbar(aes(ymin = LowCI0.75, ymax = UpCI0.75, fill=as.factor(REALM)), 
                width = 0.1, linetype=0, alpha=0.3)+
  geom_crossbar(aes(ymin=LowCI0.95, ymax=UpCI0.95,fill=as.factor(REALM)), 
                width = 0.1,linetype=0, alpha=0.15)+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1),
                     guide = guide_legend(reverse = TRUE) )+
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1),
                    guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 18)+theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 legend.position = "none")
tx

#----------------------------------------------------------
# Now plot the summary estimates, fixedeffect 

library(brms)
library(tidyverse)
library(tidybayes)

bm<-readRDS(paste(resloc,"/full_model.RDS",sep=""))
post<-as_draws_df(bm)

post<-post%>%dplyr::select(b_Intercept,b_R,b_A,b_VR_LdM,
                           b_REALMTerrestrial,`b_R:REALMTerrestrial`,
                           `b_A:REALMTerrestrial`,`b_VR_LdM:REALMTerrestrial`,
                           sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)

#------------------------- intercept ---------------------
gathered_post <-
  post %>% 
  select(c("b_Intercept","b_REALMTerrestrial")) %>% 
  rename(b_Intercept_Fresh=b_Intercept,
         `b_Intercept_diff_T-F`=`b_REALMTerrestrial`)%>%
  mutate("b_Intercept_Terres" = 
           b_Intercept_Fresh + `b_Intercept_diff_T-F`)%>%
  dplyr::select(b_Intercept_Fresh,b_Intercept_Terres)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_Intercept_Fresh" , 
                                      "b_Intercept_Terres"
  )))


keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-2

myColors <- c("dodgerblue", "green3")

gathered_postI<-gathered_post
g_I<-gathered_postI %>% 
  ggplot(aes(x = value, group = key,fill=as.factor(key))) +
  scale_fill_manual(values=alpha(myColors, 0.6))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(linewidth = .2)+guides(fill="none", color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_grid(~key, scales = "free")

g_I
#--------------------------- Richness ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_R","b_R:REALMTerrestrial")) %>% 
  rename(b_R_Fresh=b_R,
         `b_R_diff_T-F`=`b_R:REALMTerrestrial`)%>%
  mutate("b_R_Terres" = 
           b_R_Fresh + `b_R_diff_T-F`)%>%
  dplyr::select(b_R_Fresh,b_R_Terres)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_R_Fresh" , 
                                      "b_R_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-2

gathered_postR<-gathered_post
g_R<-gathered_postR %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(key))) +
  scale_fill_manual(values=alpha(myColors, 0.6))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(linewidth = .2, alpha=0.6)+guides(fill="none", color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_grid(~key, scales = "free")
g_R

#--------------------------- Variance ratio ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_VR_LdM","b_VR_LdM:REALMTerrestrial")) %>% 
  rename(b_VR_LdM_Fresh=b_VR_LdM,
         `b_VR_LdM_T-F`=`b_VR_LdM:REALMTerrestrial`)%>%
  mutate("b_VR_LdM_Terres" = 
           b_VR_LdM_Fresh + `b_VR_LdM_T-F`)%>%
  dplyr::select(b_VR_LdM_Fresh,b_VR_LdM_Terres)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_VR_LdM_Fresh" , 
                                      "b_VR_LdM_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-2


gathered_postVR<-gathered_post
g_VR<-gathered_postVR %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(key))) +
  scale_fill_manual(values=alpha(myColors, 0.6))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(linewidth = .2, alpha=0.6)+guides(fill="none", color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_grid(~key, scales = "free")
g_VR

#--------------------------- Total asymmetry A ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_A","b_A:REALMTerrestrial")) %>% 
  rename(b_A_Fresh=b_A,
         `b_A_diff_T-F`=`b_A:REALMTerrestrial`)%>%
  mutate("b_A_Terres" = 
           b_A_Fresh + `b_A_diff_T-F`)%>%
  dplyr::select(b_A_Fresh,b_A_Terres)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_A_Fresh" , 
                                      "b_A_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-2


gathered_postA<-gathered_post
g_A<-gathered_postA %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(key))) +
  scale_fill_manual(values=alpha(myColors, 0.6))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(linewidth = .2, alpha=0.6)+guides(fill="none", color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_grid(~key, scales = "free")
g_A

df1<-post %>% 
  select(c("sd_TAXA__Intercept","sd_TAXA:UID__Intercept")) %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("sd_TAXA__Intercept" , 
                                      "sd_TAXA:UID__Intercept"
  )))

sd_rd<-ggplot(data = df1, aes(y = value, x = key, fill = key)) +
  scale_fill_manual(values=alpha(c("purple","gold3"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .3) + 
  #coord_flip()+
  geom_point(aes(y = value, color = key), position = position_jitter(width = .15), 
             size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.3)+ 
  ylab("SD of random effects")+xlab("")+
  theme_bw()+
  scale_color_manual(values=alpha(c("purple","gold3"), 1))+ 
  theme(
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=20))#+ 
#annotate("text",  x=0.85, y = 80, label = "(a)", vjust=1.5, hjust=1.5, size=10)

pdf(paste(resloc,"/effectsize_summary.pdf",sep=""),height=12,width=13)
gridExtra::grid.arrange(g_I, g_R, g_VR, g_A, tx, 
                        layout_matrix=rbind(c(1,2),c(3,4),c(5,5)))
dev.off()# results similar to that of 100 replicates

pdf(paste(resloc,"/effectsize_summary_nestedrandom.pdf",sep=""),height=4,width=7)
sd_rd
dev.off()# results similar to that of 100 replicates

