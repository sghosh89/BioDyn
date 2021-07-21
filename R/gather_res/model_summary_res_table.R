rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)

###########################################################################
sink("../../Results/gather_res/test/console_model_summary_table.txt", append=TRUE, split=TRUE)

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

sink()
