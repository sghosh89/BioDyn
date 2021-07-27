rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)

###########################################################################
sink("../../Results/gather_res/console_model_summary_table.txt", append=TRUE, split=TRUE)

# see summary from basic model without realm
bm<-readRDS("../../Results/gather_res/test/nullmodel_wo_REALM.RDS")
post<-posterior_samples(bm)
df<-data.frame(Median=NA*numeric(5),
               LowCI0.95=NA*numeric(5),UpCI0.95=NA*numeric(5),
               LowCI0.75=NA*numeric(5),UpCI0.75=NA*numeric(5))
rownames(df)<-c("Intercept","Slope_R","Slope_VR","sd_Taxa_Intercept","sd_TAXA:UID_Intercept")
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

df1<-post %>%select(b_VR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[3]<-df1$b_VR[1]
df$LowCI0.95[3]<-df1$.lower[1]
df$LowCI0.75[3]<-df1$.lower[2]
df$UpCI0.95[3]<-df1$.upper[1]
df$UpCI0.75[3]<-df1$.upper[2]

df1<-post %>%select(sd_TAXA__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$sd_TAXA__Intercept[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[5]<-df1$`sd_TAXA:UID__Intercept`[1]
df$LowCI0.95[5]<-df1$.lower[1]
df$LowCI0.75[5]<-df1$.lower[2]
df$UpCI0.95[5]<-df1$.upper[1]
df$UpCI0.75[5]<-df1$.upper[2]

cat(paste("----------- basic model output, without realm, posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm
#################################################################################

nm<-readRDS("../../Results/gather_res/test/nullmodel_w_REALM.RDS")
post<-posterior_samples(nm)
post<-post%>%select(b_Intercept,b_R,b_VR,
                    b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_VR:REALMTerrestrial`,
                    sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
df<-data.frame(Median=NA*numeric(8),
               LowCI0.95=NA*numeric(8),UpCI0.95=NA*numeric(8),
               LowCI0.75=NA*numeric(8),UpCI0.75=NA*numeric(8))

rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_VR_Freshw",
                "Intercept_Terres","Slope_R_Terres","Slope_VR_Terres",
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

df1<-post %>%select(b_VR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[3]<-df1$b_VR[1]
df$LowCI0.95[3]<-df1$.lower[1]
df$LowCI0.75[3]<-df1$.lower[2]
df$UpCI0.95[3]<-df1$.upper[1]
df$UpCI0.75[3]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_terres=b_Intercept+b_REALMTerrestrial)%>% select(b_Intercept_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$b_Intercept_terres[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

df1<-post %>%mutate(b_R_terres=b_R+`b_R:REALMTerrestrial`)%>% select(b_R_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[5]<-df1$b_R_terres[1]
df$LowCI0.95[5]<-df1$.lower[1]
df$LowCI0.75[5]<-df1$.lower[2]
df$UpCI0.95[5]<-df1$.upper[1]
df$UpCI0.75[5]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_terres=b_VR+`b_VR:REALMTerrestrial`)%>% select(b_VR_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[6]<-df1$b_VR_terres[1]
df$LowCI0.95[6]<-df1$.lower[1]
df$LowCI0.75[6]<-df1$.lower[2]
df$UpCI0.95[6]<-df1$.upper[1]
df$UpCI0.75[6]<-df1$.upper[2]

df1<-post %>%select(sd_TAXA__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[7]<-df1$sd_TAXA__Intercept[1]
df$LowCI0.95[7]<-df1$.lower[1]
df$LowCI0.75[7]<-df1$.lower[2]
df$UpCI0.95[7]<-df1$.upper[1]
df$UpCI0.75[7]<-df1$.upper[2]

df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[8]<-df1$`sd_TAXA:UID__Intercept`[1]
df$LowCI0.95[8]<-df1$.lower[1]
df$LowCI0.75[8]<-df1$.lower[2]
df$UpCI0.95[8]<-df1$.upper[1]
df$UpCI0.75[8]<-df1$.upper[2]

# rearrange df
df<-df[c("Intercept_Freshw","Intercept_Terres",
     "Slope_R_Freshw","Slope_R_Terres",
     "Slope_VR_Freshw","Slope_VR_Terres",
     "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]

cat(paste("----------- null model output, with realm, posterior summary table -------------- \n"))

dfnm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfnm)<-rownames(df)
dfnm

#################################################################################

fm<-readRDS("../../Results/gather_res/test/fullmodel.RDS")
post<-posterior_samples(fm)
post<-post%>%select(b_Intercept,b_R,b_VR,b_A,b_uniA,b_SR,
                    b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_VR:REALMTerrestrial`,
                    `b_A:REALMTerrestrial`,`b_uniA:REALMTerrestrial`,`b_SR:REALMTerrestrial`,
                    sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
df<-data.frame(Median=NA*numeric(14),
               LowCI0.95=NA*numeric(14),UpCI0.95=NA*numeric(14),
               LowCI0.75=NA*numeric(14),UpCI0.75=NA*numeric(14))

rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_VR_Freshw","Slope_A_Freshw","Slope_uniA_Freshw","Slope_SR_Freshw",
                "Intercept_Terres","Slope_R_Terres","Slope_VR_Terres","Slope_A_Terres","Slope_uniA_Terres","Slope_SR_Terres",
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

df1<-post %>%select(b_VR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[3]<-df1$b_VR[1]
df$LowCI0.95[3]<-df1$.lower[1]
df$LowCI0.75[3]<-df1$.lower[2]
df$UpCI0.95[3]<-df1$.upper[1]
df$UpCI0.75[3]<-df1$.upper[2]

df1<-post %>%select(b_A)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$b_A[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

df1<-post %>%select(b_uniA)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[5]<-df1$b_uniA[1]
df$LowCI0.95[5]<-df1$.lower[1]
df$LowCI0.75[5]<-df1$.lower[2]
df$UpCI0.95[5]<-df1$.upper[1]
df$UpCI0.75[5]<-df1$.upper[2]

df1<-post %>%select(b_SR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[6]<-df1$b_SR[1]
df$LowCI0.95[6]<-df1$.lower[1]
df$LowCI0.75[6]<-df1$.lower[2]
df$UpCI0.95[6]<-df1$.upper[1]
df$UpCI0.75[6]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_terres=b_Intercept+b_REALMTerrestrial)%>% select(b_Intercept_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[7]<-df1$b_Intercept_terres[1]
df$LowCI0.95[7]<-df1$.lower[1]
df$LowCI0.75[7]<-df1$.lower[2]
df$UpCI0.95[7]<-df1$.upper[1]
df$UpCI0.75[7]<-df1$.upper[2]

df1<-post %>%mutate(b_R_terres=b_R+`b_R:REALMTerrestrial`)%>% select(b_R_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[8]<-df1$b_R_terres[1]
df$LowCI0.95[8]<-df1$.lower[1]
df$LowCI0.75[8]<-df1$.lower[2]
df$UpCI0.95[8]<-df1$.upper[1]
df$UpCI0.75[8]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_terres=b_VR+`b_VR:REALMTerrestrial`)%>% select(b_VR_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[9]<-df1$b_VR_terres[1]
df$LowCI0.95[9]<-df1$.lower[1]
df$LowCI0.75[9]<-df1$.lower[2]
df$UpCI0.95[9]<-df1$.upper[1]
df$UpCI0.75[9]<-df1$.upper[2]

df1<-post %>%mutate(b_A_terres=b_A+`b_A:REALMTerrestrial`)%>% select(b_A_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[10]<-df1$b_A_terres[1]
df$LowCI0.95[10]<-df1$.lower[1]
df$LowCI0.75[10]<-df1$.lower[2]
df$UpCI0.95[10]<-df1$.upper[1]
df$UpCI0.75[10]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_terres=b_uniA+`b_uniA:REALMTerrestrial`)%>% select(b_uniA_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[11]<-df1$b_uniA_terres[1]
df$LowCI0.95[11]<-df1$.lower[1]
df$LowCI0.75[11]<-df1$.lower[2]
df$UpCI0.95[11]<-df1$.upper[1]
df$UpCI0.75[11]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_terres=b_SR+`b_SR:REALMTerrestrial`)%>% select(b_SR_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[12]<-df1$b_SR_terres[1]
df$LowCI0.95[12]<-df1$.lower[1]
df$LowCI0.75[12]<-df1$.lower[2]
df$UpCI0.95[12]<-df1$.upper[1]
df$UpCI0.75[12]<-df1$.upper[2]

df1<-post %>%select(sd_TAXA__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[13]<-df1$sd_TAXA__Intercept[1]
df$LowCI0.95[13]<-df1$.lower[1]
df$LowCI0.75[13]<-df1$.lower[2]
df$UpCI0.95[13]<-df1$.upper[1]
df$UpCI0.75[13]<-df1$.upper[2]

df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[14]<-df1$`sd_TAXA:UID__Intercept`[1]
df$LowCI0.95[14]<-df1$.lower[1]
df$LowCI0.75[14]<-df1$.lower[2]
df$UpCI0.95[14]<-df1$.upper[1]
df$UpCI0.75[14]<-df1$.upper[2]

# rearrange df
df<-df[c("Intercept_Freshw","Intercept_Terres",
         "Slope_R_Freshw","Slope_R_Terres",
         "Slope_VR_Freshw","Slope_VR_Terres",
         "Slope_A_Freshw","Slope_A_Terres",
         "Slope_uniA_Freshw","Slope_uniA_Terres",
         "Slope_SR_Freshw","Slope_SR_Terres",
         "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]

cat(paste("----------- full model output, with realm and with asymmetry, posterior summary table -------------- \n"))

dffm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dffm)<-rownames(df)
dffm

#################################################################################

txm<-readRDS("../../Results/gather_res/taxa_fixedeffect/fullmodel.RDS")
post<-posterior_samples(txm)

post<-post%>%select(b_Intercept,b_TAXAterrestrialinvertebrates,b_TAXAterrestrialplants,b_TAXAmammals,
                    b_TAXAfish,b_TAXAfreshwaterinvertebrates,b_TAXAfreshwaterplants,
                    b_R,`b_R:TAXAterrestrialinvertebrates`,`b_R:TAXAterrestrialplants`,`b_R:TAXAmammals`,
                    `b_R:TAXAfish`,`b_R:TAXAfreshwaterinvertebrates`,`b_R:TAXAfreshwaterplants`,
                    b_VR,`b_VR:TAXAterrestrialinvertebrates`,`b_VR:TAXAterrestrialplants`,`b_VR:TAXAmammals`,
                    `b_VR:TAXAfish`,`b_VR:TAXAfreshwaterinvertebrates`,`b_VR:TAXAfreshwaterplants`,
                    b_A,`b_A:TAXAterrestrialinvertebrates`,`b_A:TAXAterrestrialplants`,`b_A:TAXAmammals`,
                    `b_A:TAXAfish`,`b_A:TAXAfreshwaterinvertebrates`,`b_A:TAXAfreshwaterplants`,
                    b_uniA,`b_uniA:TAXAterrestrialinvertebrates`,`b_uniA:TAXAterrestrialplants`,`b_uniA:TAXAmammals`,
                    `b_uniA:TAXAfish`,`b_uniA:TAXAfreshwaterinvertebrates`,`b_uniA:TAXAfreshwaterplants`,
                    b_SR,`b_SR:TAXAterrestrialinvertebrates`,`b_SR:TAXAterrestrialplants`,`b_SR:TAXAmammals`,
                    `b_SR:TAXAfish`,`b_SR:TAXAfreshwaterinvertebrates`,`b_SR:TAXAfreshwaterplants`,
                    sd_UID__Intercept
                    )
df<-data.frame(Median=NA*numeric(43),
               LowCI0.95=NA*numeric(43),UpCI0.95=NA*numeric(43),
               LowCI0.75=NA*numeric(43),UpCI0.75=NA*numeric(43))

rownames(df)<-c("Intercept_birds","Intercept_terres.inv","Intercept_terres.plants","Intercept_mammals",
                "Intercept_fish","Intercept_fresh.inv","Intercept_fresh.plants",
                "Slope_R_birds","Slope_R_terres.inv","Slope_R_terres.plants","Slope_R_mammals",
                "Slope_R_fish","Slope_R_fresh.inv","Slope_R_fresh.plants",
                "Slope_VR_birds","Slope_VR_terres.inv","Slope_VR_terres.plants","Slope_VR_mammals",
                "Slope_VR_fish","Slope_VR_fresh.inv","Slope_VR_fresh.plants",
                "Slope_A_birds","Slope_A_terres.inv","Slope_A_terres.plants","Slope_A_mammals",
                "Slope_A_fish","Slope_A_fresh.inv","Slope_A_fresh.plants",
                "Slope_uniA_birds","Slope_uniA_terres.inv","Slope_uniA_terres.plants","Slope_uniA_mammals",
                "Slope_uniA_fish","Slope_uniA_fresh.inv","Slope_uniA_fresh.plants",
                "Slope_SR_birds","Slope_SR_terres.inv","Slope_SR_terres.plants","Slope_SR_mammals",
                "Slope_SR_fish","Slope_SR_fresh.inv","Slope_SR_fresh.plants",
                "sd_UID_Intercept")

#------------ intercept ----------------------

df1<-post %>%select(b_Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[1]<-df1$b_Intercept[1]
df$LowCI0.95[1]<-df1$.lower[1]
df$LowCI0.75[1]<-df1$.lower[2]
df$UpCI0.95[1]<-df1$.upper[1]
df$UpCI0.75[1]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_terres.inv=b_Intercept+b_TAXAterrestrialinvertebrates)%>% select(b_Intercept_terres.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[2]<-df1$b_Intercept_terres.inv[1]
df$LowCI0.95[2]<-df1$.lower[1]
df$LowCI0.75[2]<-df1$.lower[2]
df$UpCI0.95[2]<-df1$.upper[1]
df$UpCI0.75[2]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_terres.pl=b_Intercept+b_TAXAterrestrialplants)%>% select(b_Intercept_terres.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[3]<-df1$b_Intercept_terres.pl[1]
df$LowCI0.95[3]<-df1$.lower[1]
df$LowCI0.75[3]<-df1$.lower[2]
df$UpCI0.95[3]<-df1$.upper[1]
df$UpCI0.75[3]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_mam=b_Intercept+b_TAXAmammals)%>% select(b_Intercept_mam)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$b_Intercept_mam[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_fish=b_Intercept+b_TAXAfish)%>% select(b_Intercept_fish)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[5]<-df1$b_Intercept_fish[1]
df$LowCI0.95[5]<-df1$.lower[1]
df$LowCI0.75[5]<-df1$.lower[2]
df$UpCI0.95[5]<-df1$.upper[1]
df$UpCI0.75[5]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_fresh.inv=b_Intercept+b_TAXAfreshwaterinvertebrates)%>% select(b_Intercept_fresh.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[6]<-df1$b_Intercept_fresh.inv[1]
df$LowCI0.95[6]<-df1$.lower[1]
df$LowCI0.75[6]<-df1$.lower[2]
df$UpCI0.95[6]<-df1$.upper[1]
df$UpCI0.75[6]<-df1$.upper[2]

df1<-post %>%mutate(b_Intercept_fresh.pl=b_Intercept+b_TAXAfreshwaterplants)%>% select(b_Intercept_fresh.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[7]<-df1$b_Intercept_fresh.pl[1]
df$LowCI0.95[7]<-df1$.lower[1]
df$LowCI0.75[7]<-df1$.lower[2]
df$UpCI0.95[7]<-df1$.upper[1]
df$UpCI0.75[7]<-df1$.upper[2]

#------------ richness ----------------------

df1<-post %>%select(b_R)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[8]<-df1$b_R[1]
df$LowCI0.95[8]<-df1$.lower[1]
df$LowCI0.75[8]<-df1$.lower[2]
df$UpCI0.95[8]<-df1$.upper[1]
df$UpCI0.75[8]<-df1$.upper[2]

df1<-post %>%mutate(b_R_terres.inv=b_R+`b_R:TAXAterrestrialinvertebrates`)%>% select(b_R_terres.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[9]<-df1$b_R_terres.inv[1]
df$LowCI0.95[9]<-df1$.lower[1]
df$LowCI0.75[9]<-df1$.lower[2]
df$UpCI0.95[9]<-df1$.upper[1]
df$UpCI0.75[9]<-df1$.upper[2]

df1<-post %>%mutate(b_R_terres.pl=b_R+`b_R:TAXAterrestrialplants`)%>% select(b_R_terres.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[10]<-df1$b_R_terres.pl[1]
df$LowCI0.95[10]<-df1$.lower[1]
df$LowCI0.75[10]<-df1$.lower[2]
df$UpCI0.95[10]<-df1$.upper[1]
df$UpCI0.75[10]<-df1$.upper[2]

df1<-post %>%mutate(b_R_mam=b_R+`b_R:TAXAmammals`)%>% select(b_R_mam)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[11]<-df1$b_R_mam[1]
df$LowCI0.95[11]<-df1$.lower[1]
df$LowCI0.75[11]<-df1$.lower[2]
df$UpCI0.95[11]<-df1$.upper[1]
df$UpCI0.75[11]<-df1$.upper[2]

df1<-post %>%mutate(b_R_fish=b_R+`b_R:TAXAfish`)%>% select(b_R_fish)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[12]<-df1$b_R_fish[1]
df$LowCI0.95[12]<-df1$.lower[1]
df$LowCI0.75[12]<-df1$.lower[2]
df$UpCI0.95[12]<-df1$.upper[1]
df$UpCI0.75[12]<-df1$.upper[2]

df1<-post %>%mutate(b_R_fresh.inv=b_R+`b_R:TAXAfreshwaterinvertebrates`)%>% select(b_R_fresh.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[13]<-df1$b_R_fresh.inv[1]
df$LowCI0.95[13]<-df1$.lower[1]
df$LowCI0.75[13]<-df1$.lower[2]
df$UpCI0.95[13]<-df1$.upper[1]
df$UpCI0.75[13]<-df1$.upper[2]

df1<-post %>%mutate(b_R_fresh.pl=b_R+`b_R:TAXAfreshwaterplants`)%>% select(b_R_fresh.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[14]<-df1$b_R_fresh.pl[1]
df$LowCI0.95[14]<-df1$.lower[1]
df$LowCI0.75[14]<-df1$.lower[2]
df$UpCI0.95[14]<-df1$.upper[1]
df$UpCI0.75[14]<-df1$.upper[2]

#------------ variance ratio ----------------------

df1<-post %>%select(b_VR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[15]<-df1$b_VR[1]
df$LowCI0.95[15]<-df1$.lower[1]
df$LowCI0.75[15]<-df1$.lower[2]
df$UpCI0.95[15]<-df1$.upper[1]
df$UpCI0.75[15]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_terres.inv=b_VR+`b_VR:TAXAterrestrialinvertebrates`)%>% select(b_VR_terres.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[16]<-df1$b_VR_terres.inv[1]
df$LowCI0.95[16]<-df1$.lower[1]
df$LowCI0.75[16]<-df1$.lower[2]
df$UpCI0.95[16]<-df1$.upper[1]
df$UpCI0.75[16]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_terres.pl=b_VR+`b_VR:TAXAterrestrialplants`)%>% select(b_VR_terres.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[17]<-df1$b_VR_terres.pl[1]
df$LowCI0.95[17]<-df1$.lower[1]
df$LowCI0.75[17]<-df1$.lower[2]
df$UpCI0.95[17]<-df1$.upper[1]
df$UpCI0.75[17]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_mam=b_VR+`b_VR:TAXAmammals`)%>% select(b_VR_mam)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[18]<-df1$b_VR_mam[1]
df$LowCI0.95[18]<-df1$.lower[1]
df$LowCI0.75[18]<-df1$.lower[2]
df$UpCI0.95[18]<-df1$.upper[1]
df$UpCI0.75[18]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_fish=b_VR+`b_VR:TAXAfish`)%>% select(b_VR_fish)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[19]<-df1$b_VR_fish[1]
df$LowCI0.95[19]<-df1$.lower[1]
df$LowCI0.75[19]<-df1$.lower[2]
df$UpCI0.95[19]<-df1$.upper[1]
df$UpCI0.75[19]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_fresh.inv=b_VR+`b_VR:TAXAfreshwaterinvertebrates`)%>% select(b_VR_fresh.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[20]<-df1$b_VR_fresh.inv[1]
df$LowCI0.95[20]<-df1$.lower[1]
df$LowCI0.75[20]<-df1$.lower[2]
df$UpCI0.95[20]<-df1$.upper[1]
df$UpCI0.75[20]<-df1$.upper[2]

df1<-post %>%mutate(b_VR_fresh.pl=b_VR+`b_VR:TAXAfreshwaterplants`)%>% select(b_VR_fresh.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[21]<-df1$b_VR_fresh.pl[1]
df$LowCI0.95[21]<-df1$.lower[1]
df$LowCI0.75[21]<-df1$.lower[2]
df$UpCI0.95[21]<-df1$.upper[1]
df$UpCI0.75[21]<-df1$.upper[2]

#------------ total asymmetry, A ----------------------

df1<-post %>%select(b_A)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[22]<-df1$b_A[1]
df$LowCI0.95[22]<-df1$.lower[1]
df$LowCI0.75[22]<-df1$.lower[2]
df$UpCI0.95[22]<-df1$.upper[1]
df$UpCI0.75[22]<-df1$.upper[2]

df1<-post %>%mutate(b_A_terres.inv=b_A+`b_A:TAXAterrestrialinvertebrates`)%>% select(b_A_terres.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[23]<-df1$b_A_terres.inv[1]
df$LowCI0.95[23]<-df1$.lower[1]
df$LowCI0.75[23]<-df1$.lower[2]
df$UpCI0.95[23]<-df1$.upper[1]
df$UpCI0.75[23]<-df1$.upper[2]

df1<-post %>%mutate(b_A_terres.pl=b_A+`b_A:TAXAterrestrialplants`)%>% select(b_A_terres.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[24]<-df1$b_A_terres.pl[1]
df$LowCI0.95[24]<-df1$.lower[1]
df$LowCI0.75[24]<-df1$.lower[2]
df$UpCI0.95[24]<-df1$.upper[1]
df$UpCI0.75[24]<-df1$.upper[2]

df1<-post %>%mutate(b_A_mam=b_A+`b_A:TAXAmammals`)%>% select(b_A_mam)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[25]<-df1$b_A_mam[1]
df$LowCI0.95[25]<-df1$.lower[1]
df$LowCI0.75[25]<-df1$.lower[2]
df$UpCI0.95[25]<-df1$.upper[1]
df$UpCI0.75[25]<-df1$.upper[2]

df1<-post %>%mutate(b_A_fish=b_A+`b_A:TAXAfish`)%>% select(b_A_fish)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[26]<-df1$b_A_fish[1]
df$LowCI0.95[26]<-df1$.lower[1]
df$LowCI0.75[26]<-df1$.lower[2]
df$UpCI0.95[26]<-df1$.upper[1]
df$UpCI0.75[26]<-df1$.upper[2]

df1<-post %>%mutate(b_A_fresh.inv=b_A+`b_A:TAXAfreshwaterinvertebrates`)%>% select(b_A_fresh.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[27]<-df1$b_A_fresh.inv[1]
df$LowCI0.95[27]<-df1$.lower[1]
df$LowCI0.75[27]<-df1$.lower[2]
df$UpCI0.95[27]<-df1$.upper[1]
df$UpCI0.75[27]<-df1$.upper[2]

df1<-post %>%mutate(b_A_fresh.pl=b_A+`b_A:TAXAfreshwaterplants`)%>% select(b_A_fresh.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[28]<-df1$b_A_fresh.pl[1]
df$LowCI0.95[28]<-df1$.lower[1]
df$LowCI0.75[28]<-df1$.lower[2]
df$UpCI0.95[28]<-df1$.upper[1]
df$UpCI0.75[28]<-df1$.upper[2]

#------------ net asymmetry, uniA ----------------------

df1<-post %>%select(b_uniA)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[29]<-df1$b_uniA[1]
df$LowCI0.95[29]<-df1$.lower[1]
df$LowCI0.75[29]<-df1$.lower[2]
df$UpCI0.95[29]<-df1$.upper[1]
df$UpCI0.75[29]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_terres.inv=b_uniA+`b_uniA:TAXAterrestrialinvertebrates`)%>% select(b_uniA_terres.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[30]<-df1$b_uniA_terres.inv[1]
df$LowCI0.95[30]<-df1$.lower[1]
df$LowCI0.75[30]<-df1$.lower[2]
df$UpCI0.95[30]<-df1$.upper[1]
df$UpCI0.75[30]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_terres.pl=b_uniA+`b_uniA:TAXAterrestrialplants`)%>% select(b_uniA_terres.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[31]<-df1$b_uniA_terres.pl[1]
df$LowCI0.95[31]<-df1$.lower[1]
df$LowCI0.75[31]<-df1$.lower[2]
df$UpCI0.95[31]<-df1$.upper[1]
df$UpCI0.75[31]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_mam=b_uniA+`b_uniA:TAXAmammals`)%>% select(b_uniA_mam)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[32]<-df1$b_uniA_mam[1]
df$LowCI0.95[32]<-df1$.lower[1]
df$LowCI0.75[32]<-df1$.lower[2]
df$UpCI0.95[32]<-df1$.upper[1]
df$UpCI0.75[32]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_fish=b_uniA+`b_uniA:TAXAfish`)%>% select(b_uniA_fish)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[33]<-df1$b_uniA_fish[1]
df$LowCI0.95[33]<-df1$.lower[1]
df$LowCI0.75[33]<-df1$.lower[2]
df$UpCI0.95[33]<-df1$.upper[1]
df$UpCI0.75[33]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_fresh.inv=b_uniA+`b_uniA:TAXAfreshwaterinvertebrates`)%>% select(b_uniA_fresh.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[34]<-df1$b_uniA_fresh.inv[1]
df$LowCI0.95[34]<-df1$.lower[1]
df$LowCI0.75[34]<-df1$.lower[2]
df$UpCI0.95[34]<-df1$.upper[1]
df$UpCI0.75[34]<-df1$.upper[2]

df1<-post %>%mutate(b_uniA_fresh.pl=b_uniA+`b_uniA:TAXAfreshwaterplants`)%>% select(b_uniA_fresh.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[35]<-df1$b_uniA_fresh.pl[1]
df$LowCI0.95[35]<-df1$.lower[1]
df$LowCI0.75[35]<-df1$.lower[2]
df$UpCI0.95[35]<-df1$.upper[1]
df$UpCI0.75[35]<-df1$.upper[2]

#------------ skewness ratio, SR ----------------------

df1<-post %>%select(b_SR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[36]<-df1$b_SR[1]
df$LowCI0.95[36]<-df1$.lower[1]
df$LowCI0.75[36]<-df1$.lower[2]
df$UpCI0.95[36]<-df1$.upper[1]
df$UpCI0.75[36]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_terres.inv=b_SR+`b_SR:TAXAterrestrialinvertebrates`)%>% select(b_SR_terres.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[37]<-df1$b_SR_terres.inv[1]
df$LowCI0.95[37]<-df1$.lower[1]
df$LowCI0.75[37]<-df1$.lower[2]
df$UpCI0.95[37]<-df1$.upper[1]
df$UpCI0.75[37]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_terres.pl=b_SR+`b_SR:TAXAterrestrialplants`)%>% select(b_SR_terres.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[38]<-df1$b_SR_terres.pl[1]
df$LowCI0.95[38]<-df1$.lower[1]
df$LowCI0.75[38]<-df1$.lower[2]
df$UpCI0.95[38]<-df1$.upper[1]
df$UpCI0.75[38]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_mam=b_SR+`b_SR:TAXAmammals`)%>% select(b_SR_mam)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[39]<-df1$b_SR_mam[1]
df$LowCI0.95[39]<-df1$.lower[1]
df$LowCI0.75[39]<-df1$.lower[2]
df$UpCI0.95[39]<-df1$.upper[1]
df$UpCI0.75[39]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_fish=b_SR+`b_SR:TAXAfish`)%>% select(b_SR_fish)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[40]<-df1$b_SR_fish[1]
df$LowCI0.95[40]<-df1$.lower[1]
df$LowCI0.75[40]<-df1$.lower[2]
df$UpCI0.95[40]<-df1$.upper[1]
df$UpCI0.75[40]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_fresh.inv=b_SR+`b_SR:TAXAfreshwaterinvertebrates`)%>% select(b_SR_fresh.inv)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[41]<-df1$b_SR_fresh.inv[1]
df$LowCI0.95[41]<-df1$.lower[1]
df$LowCI0.75[41]<-df1$.lower[2]
df$UpCI0.95[41]<-df1$.upper[1]
df$UpCI0.75[41]<-df1$.upper[2]

df1<-post %>%mutate(b_SR_fresh.pl=b_SR+`b_SR:TAXAfreshwaterplants`)%>% select(b_SR_fresh.pl)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[42]<-df1$b_SR_fresh.pl[1]
df$LowCI0.95[42]<-df1$.lower[1]
df$LowCI0.75[42]<-df1$.lower[2]
df$UpCI0.95[42]<-df1$.upper[1]
df$UpCI0.75[42]<-df1$.upper[2]

df1<-post %>%select(sd_UID__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[43]<-df1$sd_UID__Intercept[1]
df$LowCI0.95[43]<-df1$.lower[1]
df$LowCI0.75[43]<-df1$.lower[2]
df$UpCI0.95[43]<-df1$.upper[1]
df$UpCI0.75[43]<-df1$.upper[2]

cat(paste("----------- taxa model output, without realm, posterior summary table -------------- \n"))

dftxm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dftxm)<-rownames(df)
dftxm

sink()
