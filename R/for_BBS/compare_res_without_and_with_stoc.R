#==========================================================
# first visually compare the stability and its drivers
#==========================================================
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(dplyr)

if(!dir.exists("../../Results/for_BBS/bayesian_model/")){
  dir.create("../../Results/for_BBS/bayesian_model/")
}

if(!dir.exists("../../Results/for_BBS/bayesian_model/without_stoc/")){
  dir.create("../../Results/for_BBS/bayesian_model/without_stoc/")
}

if(!dir.exists("../../Results/for_BBS/bayesian_model/with_stoc/")){
  dir.create("../../Results/for_BBS/bayesian_model/with_stoc/")
}

r_BBS_stoc<-readRDS("../../Results/for_BBS/res_with_stoc/stability_metric.RDS")
r_BBS_stoc<-r_BBS_stoc%>%dplyr::select(siteid,R=nsp,VR_LdM=phi_LdM,
                                       L,U,stability=iCV,strata=Stratum_name)
r_BBS_stoc$A<-r_BBS_stoc$L+abs(r_BBS_stoc$U)
saveRDS(r_BBS_stoc,"../../Results/for_BBS/bayesian_model/with_stoc/input_model_data.RDS")

r_BBS<-readRDS("../../Results/for_BBS/stability_metric.RDS")
r_BBS<-r_BBS%>%dplyr::select(siteid,R=nsp,VR_LdM=phi_LdM,
                                       L,U,stability=iCV,strata=Stratum_name)
r_BBS$A<-r_BBS$L+abs(r_BBS$U)
saveRDS(r_BBS,"../../Results/for_BBS/bayesian_model/without_stoc/input_model_data.RDS")

#============================
# Now make the plot
tempo<-data.frame(stability=r_BBS$stability, label="observed")
tempos<-data.frame(stability=r_BBS_stoc$stability, label="observed + stochasticity")
df<-rbind(tempo,tempos)

g1<-df %>%
  ggplot( aes(x=stability, color=label, fill=label)) +
  geom_density(alpha=0.4) +
  theme_bw()+theme(legend.position = c(0.6,0.8),
                   legend.title = element_blank(),
                   text = element_text(size=18))+
  ylab("") +
  xlab("Stability")
g1

tempo<-data.frame(R=r_BBS$R, label="observed")
tempos<-data.frame(R=r_BBS_stoc$R, label="observed + stochasticity")
df<-rbind(tempo,tempos)

gR<-df %>%
  ggplot( aes(x=R, color=label, fill=label)) +
  geom_density(alpha=0.4) +
  theme_bw()+theme(legend.position = "none",
                   legend.title = element_blank(),
                   text = element_text(size=18))+
  ylab("") +
  xlab("Richness")
gR

tempo<-data.frame(VR=r_BBS$VR_LdM, label="observed")
tempos<-data.frame(VR=r_BBS_stoc$VR_LdM, label="observed + stochasticity")
df<-rbind(tempo,tempos)

gVR<-df %>%
  ggplot( aes(x=VR, color=label, fill=label)) +
  geom_density(alpha=0.4) +
  theme_bw()+theme(legend.position = "none",
                   legend.title = element_blank(),
                   text = element_text(size=18))+
  ylab("") +
  xlab("Overall synchrony")
gVR

tempo<-data.frame(A=r_BBS$A, label="observed")
tempos<-data.frame(A=r_BBS_stoc$A, label="observed + stochasticity")
df<-rbind(tempo,tempos)

gA<-df %>%
  ggplot( aes(x=A, color=label, fill=label)) +
  geom_density(alpha=0.4) +
  theme_bw()+theme(legend.position = "none",
                   legend.title = element_blank(),
                   text = element_text(size=18))+
  ylab("") +
  xlab("Tail-dep. synchrony")
gA


pdf("../../Results/for_BBS/bayesian_model/compare_density.pdf",height=8,width=10)
gridExtra::grid.arrange(g1,gR,gVR,gA,nrow=2)
dev.off()

source("./call_bayesian_model_for_BBS.R")







