# This script will make summary tables for simple model results
# 8 model summary tables: 4 without REALM, 4 with REALM

# Models without REALM
# stability ~ R
# stability ~ R + VR
# stability ~ R + A
# stability ~ R + A + VR

# Models with REALM
# stability ~ R * REALM
# stability ~ (R + VR) * REALM
# stability ~ (R + A) * REALM
# stability ~ (R + A + VR) * REALM

###########################################################################
rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)
###########################################################################

sink("../../Results/gather_res/simple/model_summary_table.txt", append=TRUE, split=TRUE)

# see summary from basic model without realm
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R.RDS")
post<-posterior_samples(bm)
df<-data.frame(Median=NA*numeric(4),
               LowCI0.95=NA*numeric(4),UpCI0.95=NA*numeric(4),
               LowCI0.75=NA*numeric(4),UpCI0.75=NA*numeric(4))
rownames(df)<-c("Intercept","Slope_R","sd_Taxa_Intercept","sd_TAXA:UID_Intercept")
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

df1<-post %>%select(sd_TAXA__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[3]<-df1$sd_TAXA__Intercept[1]
df$LowCI0.95[3]<-df1$.lower[1]
df$LowCI0.75[3]<-df1$.lower[2]
df$UpCI0.95[3]<-df1$.upper[1]
df$UpCI0.75[3]<-df1$.upper[2]

df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$`sd_TAXA:UID__Intercept`[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

cat(paste("----------- basic model with R output, without realm, posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

################################################################

# see summary from basic model (R, VR) without realm
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R_VR.RDS")
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

cat(paste("----------- basic model with R, VR output, without realm, posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

################################################################

# see summary from basic model (R, A) without realm
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R_A.RDS")
post<-posterior_samples(bm)
df<-data.frame(Median=NA*numeric(5),
               LowCI0.95=NA*numeric(5),UpCI0.95=NA*numeric(5),
               LowCI0.75=NA*numeric(5),UpCI0.75=NA*numeric(5))
rownames(df)<-c("Intercept","Slope_R","Slope_A","sd_Taxa_Intercept","sd_TAXA:UID_Intercept")
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

cat(paste("----------- basic model with R, A output, without realm, posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

######################################################################################

# see summary from basic model (R, A, VR) without realm
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R_A_VR.RDS")
post<-posterior_samples(bm)
df<-data.frame(Median=NA*numeric(6),
               LowCI0.95=NA*numeric(6),UpCI0.95=NA*numeric(6),
               LowCI0.75=NA*numeric(6),UpCI0.75=NA*numeric(6))
rownames(df)<-c("Intercept","Slope_R","Slope_A","Slope_VR","sd_Taxa_Intercept","sd_TAXA:UID_Intercept")
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

df1<-post %>%select(b_VR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$b_VR[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

df1<-post %>%select(sd_TAXA__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[5]<-df1$sd_TAXA__Intercept[1]
df$LowCI0.95[5]<-df1$.lower[1]
df$LowCI0.75[5]<-df1$.lower[2]
df$UpCI0.95[5]<-df1$.upper[1]
df$UpCI0.75[5]<-df1$.upper[2]

df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[6]<-df1$`sd_TAXA:UID__Intercept`[1]
df$LowCI0.95[6]<-df1$.lower[1]
df$LowCI0.75[6]<-df1$.lower[2]
df$UpCI0.95[6]<-df1$.upper[1]
df$UpCI0.75[6]<-df1$.upper[2]

cat(paste("----------- basic model with R, A, VR without realm, posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

#================================ NOW WITH REALM =======================================

# see summary from basic model with realm
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R_REALM.RDS")
post<-posterior_samples(bm)
df<-data.frame(Median=NA*numeric(6),
               LowCI0.95=NA*numeric(6),UpCI0.95=NA*numeric(6),
               LowCI0.75=NA*numeric(6),UpCI0.75=NA*numeric(6))
rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw",
                "Intercept_Terres","Slope_R_Terres",
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

df1<-post %>%mutate(b_Intercept_terres=b_Intercept+b_REALMTerrestrial)%>% select(b_Intercept_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[3]<-df1$b_Intercept_terres[1]
df$LowCI0.95[3]<-df1$.lower[1]
df$LowCI0.75[3]<-df1$.lower[2]
df$UpCI0.95[3]<-df1$.upper[1]
df$UpCI0.75[3]<-df1$.upper[2]

df1<-post %>%mutate(b_R_terres=b_R+`b_R:REALMTerrestrial`)%>% select(b_R_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$b_R_terres[1]
df$LowCI0.95[4]<-df1$.lower[1]
df$LowCI0.75[4]<-df1$.lower[2]
df$UpCI0.95[4]<-df1$.upper[1]
df$UpCI0.75[4]<-df1$.upper[2]

df1<-post %>%select(sd_TAXA__Intercept)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[5]<-df1$sd_TAXA__Intercept[1]
df$LowCI0.95[5]<-df1$.lower[1]
df$LowCI0.75[5]<-df1$.lower[2]
df$UpCI0.95[5]<-df1$.upper[1]
df$UpCI0.75[5]<-df1$.upper[2]

df1<-post %>%select(`sd_TAXA:UID__Intercept`)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[6]<-df1$`sd_TAXA:UID__Intercept`[1]
df$LowCI0.95[6]<-df1$.lower[1]
df$LowCI0.75[6]<-df1$.lower[2]
df$UpCI0.95[6]<-df1$.upper[1]
df$UpCI0.75[6]<-df1$.upper[2]

# rearrange df
df<-df[c("Intercept_Freshw","Intercept_Terres",
         "Slope_R_Freshw","Slope_R_Terres",
         "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]

cat(paste("----------- basic model with (R, REALM) posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

######################################################################################

# see summary from basic model with R, VR, REALM
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R_VR_REALM.RDS")
post<-posterior_samples(bm)
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

cat(paste("----------- basic model with (R, VR, REALM) posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

######################################################################################

# see summary from basic model with R, A, REALM
bm<-readRDS("../../Results/gather_res/simple/basic_model_w_R_A_REALM.RDS")
post<-posterior_samples(bm)
post<-post%>%select(b_Intercept,b_R,b_A,
                    b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_A:REALMTerrestrial`,
                    sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
df<-data.frame(Median=NA*numeric(8),
               LowCI0.95=NA*numeric(8),UpCI0.95=NA*numeric(8),
               LowCI0.75=NA*numeric(8),UpCI0.75=NA*numeric(8))

rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_A_Freshw",
                "Intercept_Terres","Slope_R_Terres","Slope_A_Terres",
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

df1<-post %>%mutate(b_A_terres=b_A+`b_A:REALMTerrestrial`)%>% select(b_A_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[6]<-df1$b_A_terres[1]
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
         "Slope_A_Freshw","Slope_A_Terres",
         "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]

cat(paste("----------- basic model with (R, A, REALM) posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

######################################################################################

# see summary from basic model with R, A, VR, REALM
bm<-readRDS("../../Results/gather_res/simple/fullmodel.RDS")
post<-posterior_samples(bm)
post<-post%>%select(b_Intercept,b_R,b_A,b_VR,
                    b_REALMTerrestrial,`b_R:REALMTerrestrial`,`b_A:REALMTerrestrial`,`b_VR:REALMTerrestrial`,
                    sd_TAXA__Intercept,`sd_TAXA:UID__Intercept`)
df<-data.frame(Median=NA*numeric(10),
               LowCI0.95=NA*numeric(10),UpCI0.95=NA*numeric(10),
               LowCI0.75=NA*numeric(10),UpCI0.75=NA*numeric(10))

rownames(df)<-c("Intercept_Freshw","Slope_R_Freshw","Slope_A_Freshw","Slope_VR_Freshw",
                "Intercept_Terres","Slope_R_Terres","Slope_A_Terres","Slope_VR_Terres",
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

df1<-post %>%select(b_VR)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[4]<-df1$b_VR[1]
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

df1<-post %>%mutate(b_VR_terres=b_A+`b_VR:REALMTerrestrial`)%>% select(b_VR_terres)%>% 
  tidybayes::median_qi(.width = c(.95,0.75))
df$Median[8]<-df1$b_VR_terres[1]
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
         "Slope_VR_Freshw","Slope_VR_Terres",
         "sd_Taxa_Intercept","sd_TAXA:UID_Intercept"),]

cat(paste("----------- full model with (R, A, VR, REALM) posterior summary table -------------- \n"))

dfbm<-mutate(df, across(where(is.numeric), round, 3))
row.names(dfbm)<-rownames(df)
dfbm

#------------------------------------------------------------------

rm(list=ls())
library(performance)

# read models

basic_model_w_R<-readRDS("../../Results/gather_res/simple/basic_model_w_R.RDS")
basic_model_w_R_REALM<-readRDS("../../Results/gather_res/simple/basic_model_w_R_REALM.RDS")
basic_model_w_R_VR<-readRDS("../../Results/gather_res/simple/basic_model_w_R_VR.RDS")
basic_model_w_R_VR_REALM<-readRDS("../../Results/gather_res/simple/basic_model_w_R_VR_REALM.RDS")
basic_model_w_R_A<-readRDS("../../Results/gather_res/simple/basic_model_w_R_A.RDS")
basic_model_w_R_A_REALM<-readRDS("../../Results/gather_res/simple/basic_model_w_R_A_REALM.RDS")
basic_model_w_R_A_VR<-readRDS("../../Results/gather_res/simple/basic_model_w_R_A_VR.RDS")
full_model<-readRDS("../../Results/gather_res/simple/fullmodel.RDS")

cp<-compare_performance(basic_model_w_R, basic_model_w_R_REALM,
                        basic_model_w_R_VR,basic_model_w_R_VR_REALM,
                        basic_model_w_R_A,basic_model_w_R_A_REALM,
                        basic_model_w_R_A_VR,full_model, rank=T, metrics="common")
cp
saveRDS(cp,"../../Results/gather_res/simple/cp.RDS")
sink()


cp<-readRDS("../../Results/gather_res/simple/cp.RDS")
row.names(cp)<-cp[,1]
cp<-cp[,3:11]
cp<-format(cp,3)
write.csv(cp,"../../Results/gather_res/simple/cp.csv")

