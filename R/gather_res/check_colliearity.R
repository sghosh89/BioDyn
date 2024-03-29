rm(list=ls())
library(car)
library(lmerTest)
library(tidyverse)
#With whole data: 2668 communities
df1<-read.csv("../../Results/gather_res/data_summary.csv") # whole data summary
df1<-df1%>%dplyr::select(REALM,TAXA,newsite,
                         stability=iCV,STUDY_ID,source,
                         R=nsp,VR_LdM=phi_LdM,L,U)
                         
df1$A<-df1$L+abs(df1$U)
df1$UID<-paste(df1$source,df1$STUDY_ID,sep=",")

# scale data
df1_scaled<-df1
df1_scaled$stability<-scale(df1_scaled$stability)
df1_scaled$R<-scale(df1_scaled$R)
df1_scaled$VR_LdM<- scale(df1_scaled$VR_LdM) 
df1_scaled$A<- scale(df1_scaled$A)

mod1<-lmer(stability ~ R+ A + VR_LdM + REALM + (1|TAXA/UID), 
                              data=df1_scaled)
car::vif(mod1) # VIF was always <2
mod1wi<-lmer(stability ~ (R+ A + VR_LdM)* REALM + (1|TAXA/UID), 
           data=df1_scaled)
car::vif(mod1wi) # here with interaction term, of course VIF shows high values
# But you can ignore those high VIFs as you include same variables twice, and when the interaction terms significant
# https://stats.stackexchange.com/questions/52856/vif-values-and-interactions-in-multiple-regression
# see this post: https://statisticalhorizons.com/multicollinearity/

#library("olsrr")
#ols_eigen_cindex(mod1) # condition matrix was also ok for lm model

#With subsetted data: 1768 communities
df2<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")
df2<-df2%>%dplyr::select(REALM,TAXA,newsite,
                         stability=iCV,STUDY_ID,source,
                         R=nsp,VR_LdM=phi_LdM,L,U)

df2$A<-df2$L+abs(df2$U)
df2$UID<-paste(df2$source,df2$STUDY_ID,sep=",")

# scale data
df2_scaled<-df2
df2_scaled$stability<-scale(df2_scaled$stability)
df2_scaled$R<-scale(df2_scaled$R)
df2_scaled$VR_LdM<- scale(df2_scaled$VR_LdM) 
df2_scaled$A<- scale(df2_scaled$A)

mod2<-lmer(stability ~ R+ A + VR_LdM + REALM + (1|TAXA/UID), 
           data=df2_scaled)
car::vif(mod2) # VIF was always <2

#With further subsetted data: 105 communities in each replicates
#vf_all_max<-c()
vif_df<-data.frame(R=NA*numeric(100),A=NA*numeric(100),VR_LdM=NA*numeric(100),REALM=NA*numeric(100))
for(i in 1:100){
  mydat_scaled<-readRDS(paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  mydat_scaled$stability<-scale(mydat_scaled$iCV)
  mydat_scaled$R<-scale(mydat_scaled$R)
  mydat_scaled$VR_LdM<- scale(mydat_scaled$VR_LdM) 
  mydat_scaled$A<- scale(mydat_scaled$A)
  
  mod1<-lmer(stability ~ R+ A + VR_LdM + REALM + (1|TAXA/UID), 
             data=mydat_scaled)
  vf<-car::vif(mod1)
  #print(max(vf))
  #vf_all_max<-c(vf_all_max,max(vf))
  vif_df[i,]<-vf
}

# for all 100 replicates, always vIF<5

#======== plot ==============
pdf("../../Results/plot_vif.pdf",height=7,width=8)
op<-par(mfrow=c(2,2), mar=c(3,3,2,2),mgp=c(2,1,0))

#create vector of VIF values for whole data
vif_values <- vif(mod1)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF with whole data: n=2668", horiz = TRUE, 
        col = "steelblue", xlim=c(0,5), cex.axis=1.5)
#add vertical line at 5

#create vector of VIF values for subsetted data
vif_values2 <- vif(mod2)
#create horizontal bar chart to display each VIF value
barplot(vif_values2, main = "VIF with subsetted data: n=1768", 
        horiz = TRUE, col = "deepskyblue", xlim=c(0,5), cex.axis=1.5)
#add vertical line at 5

vif_df_l<-gather(vif_df, key="key", value="value")
vif_df_l$key<-as.factor(vif_df_l$key)
vif_df_l$key<-factor(vif_df_l$key,     # Reorder factor levels
                     c("R", "A", "VR_LdM", "REALM"))
boxplot(value~key, data=vif_df_l,horizontal = T, ylim=c(0,5),#col="lightskyblue", 
        cex.axis=1.2, main="VIF for 100 replicates",col="white", xlab="", ylab="")
stripchart(value ~ key,
           data = vif_df_l,
           method = "overplot",
           pch = 19,col=rgb(0,0,1,0.3),
           add = TRUE)

par(op)
dev.off()

#vif_df_l<-gather(vif_df, key="key", value="value")
#ggplot(data=vif_df_l, aes(y=value, x=key)) +
#  geom_boxplot()

