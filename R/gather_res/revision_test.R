
#=======================================
rm(list=ls())
library(tidyverse)
#=======================================
# First test correlation of new correlation based metric to LM synchrony
#========================================================================
df<-read.csv("../../Results/gather_res/data_summary.csv")
df$nint<-df$nsp*0.5*(df$nsp - 1) # number of pairwise interactions possible
df$newcor<-df$tot_spear_sig/df$nint

plot(df$phi_LdM,df$newcor)
df0<-df%>%filter(newcor==0)
sum(df0$nint==df0$nind)# 270 communities all independent - so no synchrony or asynchrony
# Let's exclude these communities before compare between phi_LdM and newcor
df<-df%>%filter(nind!=nint)
plot(df$phi_LdM,df$newcor)

cor.test(df$newcor,df$phi_LdM) #0.73, p-value < 2.2e-16

#===================================================================================
# Now test the correlation between extrme syn computed based on nbin=2, and nbin=4
#===================================================================================
rm(list=ls())
library(tidyverse)
df<-read.csv("../../Results/gather_res/data_summary.csv")
df_sel<-df%>%filter(nyr>=41)# 41 observations
df_sel$nint<-df_sel$nsp*0.5*(df_sel$nsp - 1)# total number of pairwise interactions
id<-which(df_sel$nind==df_sel$nint) # 6 obs have all interactions independent
df_sel<-df_sel[-id,]# now 35 observations

df_sel$nL_nbin4<-NA
df_sel$nU_nbin4<-NA
df_sel$L_nbin4<-NA
df_sel$U_nbin4<-NA

for(i in 1:nrow(df_sel)){
  if(df_sel$source[i]=="RivFishTIME"){
    tempo<-df_sel$newsite[i]
    sdf<-readRDS(paste("../../Results/for_RivFishTIME/",tempo,"/nbin4/summary_df.RDS",sep=""))
    df_sel$nL_nbin4[i]<-sdf$nL
    df_sel$nU_nbin4[i]<-sdf$nU
    df_sel$L_nbin4[i]<-sdf$L
    df_sel$U_nbin4[i]<-sdf$U
  }
  
  if(df_sel$source[i]=="BioTIME"){
    
    if(df_sel$STUDY_ID[i]==df_sel$newsite[i]){
      tempo<-df_sel$newsite[i]
      sdf<-readRDS(paste("../../Results/for_BioTIME/Terrestrial_plotlevel/",tempo,"/nbin4/summary_df.RDS",sep=""))
    }else{
      tempo<-df_sel$newsite[i]
      sdf<-readRDS(paste("../../Results/for_BioTIME/Terrestrial_plotlevel/",df_sel$STUDY_ID[i],"/",tempo,"/nbin4/summary_df.RDS",sep=""))
    }
    
    df_sel$nL_nbin4[i]<-sdf$nL
    df_sel$nU_nbin4[i]<-sdf$nU
    df_sel$L_nbin4[i]<-sdf$L
    df_sel$U_nbin4[i]<-sdf$U
  }
  
  if(df_sel$source[i]=="BioTIMEx"){
    tempo<-df_sel$newsite[i]
    sdf<-readRDS(paste("../../Results/for_BioTIMEx/",df_sel$STUDY_ID[i],"/",tempo,"/nbin4/summary_df.RDS",sep=""))
    df_sel$nL_nbin4[i]<-sdf$nL
    df_sel$nU_nbin4[i]<-sdf$nU
    df_sel$L_nbin4[i]<-sdf$L
    df_sel$U_nbin4[i]<-sdf$U
  }
  print(i)
}

plot(df_sel$L,df_sel$L_nbin4)
plot(df_sel$U,df_sel$U_nbin4)

cor.test(df_sel$L,df_sel$L_nbin4)
cor.test(df_sel$U,df_sel$U_nbin4)

cor.test(df_sel$L,df_sel$L_nbin4, method="spearman")
cor.test(df_sel$U,df_sel$U_nbin4, method="spearman")
# ok, both have significant correlation: for L 0.9, p-value <2.2e-16
# ok, both have significant correlation: for U 0.9, p-value <2.2e-16

#=================================================================================
#check consistency in the model what if you consider only LT or UT instead of A?
#=================================================================================
#-----Now, make table for Summary estimates from 100 runs, LT and UT only, separate -----------
rm(list=ls())
library(tidyverse)
library(tidybayes)

source("./call_toymodel_fixed_realm_LT_and_UT_sep.R")

source("./test_multires_multigroup_bayes.R")

source("./test_multires_multigroup_bayes_w_newcormetric.R")










