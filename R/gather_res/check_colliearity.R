rm(list=ls())
library(car)
library(lmerTest)
library(tidyverse)
#With whole data: 2759 communities
df1<-read.csv("../../Results/gather_res/data_summary.csv") # whole data summary
df1<-df1%>%dplyr::select(REALM,TAXA,newsite,
                         stability_skw=iCValt,STUDY_ID,source,
                         R=nsp,VR_LdM=phi_LdM,L,U)
                         
df1$A<-df1$L+abs(df1$U)
df1$UID<-paste(df1$source,df1$STUDY_ID,sep=",")

# scale data
df1_scaled<-df1
df1_scaled$stability_skw<-scale(df1_scaled$stability_skw)
df1_scaled$R<-scale(df1_scaled$R)
df1_scaled$VR_LdM<- scale(df1_scaled$VR_LdM) 
df1_scaled$A<- scale(df1_scaled$A)

mod1<-lmer(stability_skw ~ R+ A + VR_LdM + REALM + (1|TAXA/UID), 
                              data=df1_scaled)
car::vif(mod1) # VIF was always <2
mod1wi<-lmer(stability_skw ~ (R+ A + VR_LdM)* REALM + (1|TAXA/UID), 
           data=df1_scaled)
car::vif(mod1wi) # here with interaction term, of course VIF shows high values
# But you can ignore those high VIFs as you include same variables twice, and when the interaction terms significant
# https://stats.stackexchange.com/questions/52856/vif-values-and-interactions-in-multiple-regression
# see this post: https://statisticalhorizons.com/multicollinearity/

#library("olsrr")
#ols_eigen_cindex(mod1) # condition matrix was also ok for lm model

#With subsetted data: 1791 communities
df2<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")
df2<-df2%>%dplyr::select(REALM,TAXA,newsite,
                         stability_skw=iCValt,STUDY_ID,source,
                         R=nsp,VR_LdM=phi_LdM,L,U)

df2$A<-df2$L+abs(df2$U)
df2$UID<-paste(df2$source,df2$STUDY_ID,sep=",")

# scale data
df2_scaled<-df2
df2_scaled$stability_skw<-scale(df2_scaled$stability_skw)
df2_scaled$R<-scale(df2_scaled$R)
df2_scaled$VR_LdM<- scale(df2_scaled$VR_LdM) 
df2_scaled$A<- scale(df2_scaled$A)

mod1<-lmer(stability_skw ~ R+ A + VR_LdM + REALM + (1|TAXA/UID), 
           data=df2_scaled)
car::vif(mod1) # VIF was always <2

#With further subsetted data: 105 communities in each replicates
vf_all_max<-c()
for(i in 1:100){
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  mydat_scaled$stability_skw<-scale(mydat_scaled$stability_skw)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR_LdM<- scale(mydat_scaled$VR_LdM) 
  mydat_scaled$A<- scale(mydat_scaled$A)
  
  mod1<-lmer(stability_skw ~ R+ A + VR_LdM + REALM + (1|TAXA/UID), 
             data=mydat_scaled)
  vf<-car::vif(mod1)
  print(max(vf))
  vf_all_max<-c(vf_all_max,max(vf))
  #if(any(vf>5)){
  #  print(i)
  #}
}

# out of 100 replicates, only for three data set (i=13,53,76)
# VIF was >5, ~6, otherwise it was always <5
#As a rule of thumb, a VIF exceeding 5 requires further investigation, 
#whereas VIFs above 10 indicate multicollinearity. 
hist(vf_all_max,50) # histogram showing the max of VIF values for 4 variables for 100 replicates




