rm(list=ls())
library(tidyverse)
df<-read.csv("../../Results/gather_res/data_summary.csv")
df$A<-df$L+abs(df$U) # total asymmetry

resloc<-"../../Results/gather_res/"
sink(paste(resloc,"/datasummary_descriptive_stats.txt",sep=""),
     append=TRUE, split=TRUE)
# stability 
ct<-df%>%select(stability=iCV,REALM)
ct1<-ct%>%group_by(REALM)%>%summarise(n=n(),# number of data points
                                      median=median(stability), #median
                                      mean=mean(stability), # mean
                                      # standard deviation and interquartile range
                                      sd=sd(stability), 
                                      IQR=IQR(stability))%>%ungroup()

cat("------- stability summary in the raw data -------------- \n ")
ct1

# richness (dominant species)
ct<-df%>%select(richness=nsp,REALM)
ct1<-ct%>%group_by(REALM)%>%summarise(n=n(),# number of data points
                                      median=median(richness), #median
                                      mean=mean(richness), # mean
                                      # standard deviation and interquartile range
                                      sd=sd(richness), 
                                      IQR=IQR(richness))%>%ungroup()

cat("------- richness (dominant sp.) summary in the raw data -------------- \n ")
ct1

# variance ratio, LdM 
ct<-df%>%select(VR=phi_LdM,REALM)
ct1<-ct%>%group_by(REALM)%>%summarise(n=n(),# number of data points
                                      median=median(VR), #median
                                      mean=mean(VR), # mean
                                      # standard deviation and interquartile range
                                      sd=sd(VR), 
                                      IQR=IQR(VR))%>%ungroup()

cat("------- variance ratio (LdM) summary in the raw data -------------- \n ")
ct1

# totatl tail asymmetry
ct<-df%>%select(A,REALM)
ct1<-ct%>%group_by(REALM)%>%summarise(n=n(),# number of data points
                                      median=median(A), #median
                                      mean=mean(A), # mean
                                      # standard deviation and interquartile range
                                      sd=sd(A), 
                                      IQR=IQR(A))%>%ungroup()

cat("------- total tail asymmetry summary in the raw data -------------- \n ")
ct1
sink()





