# summary plot for posterior parameter distribution
library(brms)
library(tidybayes)
library(tidyverse)
library(gridExtra)

full_model<-readRDS("../../Results/gather_res/test/fullmodel.RDS")
post<-posterior_samples(full_model)

#--------------------------- Intercept ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_Intercept","b_REALMTerrestrial")) %>% 
  rename(b_Intercept_Fresh=b_Intercept,
    `b_Intercept(Terres-Fresh)`=`b_REALMTerrestrial`)%>%
  mutate("b_Intercept_Terres" = 
           b_Intercept_Fresh + `b_Intercept(Terres-Fresh)`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_Intercept_Fresh" , 
                                      "b_Intercept_Terres",
                                      "b_Intercept(Terres-Fresh)")))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-3
gathered_post$color[which(gathered_post$key==keys[2])]<-2
gathered_post$color[which(gathered_post$key==keys[3])]<-1

g_I<-gathered_post %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(color))) +
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2, alpha=0.6)+guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_wrap(~key, scales = "free")

#--------------------------- Richness ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_R","b_R:REALMTerrestrial")) %>% 
  rename(b_R_Fresh=b_R,
         `b_R(Terres-Fresh)`=`b_R:REALMTerrestrial`)%>%
  mutate("b_R_Terres" = 
           b_R_Fresh + `b_R(Terres-Fresh)`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_R_Fresh" , 
                                      "b_R_Terres",
                                      "b_R(Terres-Fresh)")))

  keys<-levels(gathered_post$key)
  gathered_post$color<-NA
  gathered_post$color[which(gathered_post$key==keys[1])]<-3
  gathered_post$color[which(gathered_post$key==keys[2])]<-2
  gathered_post$color[which(gathered_post$key==keys[3])]<-1

g_R<-gathered_post %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(color))) +
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2, alpha=0.6)+guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                       interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_wrap(~key, scales = "free")

#gathered_post %>% 
#  group_by(key) %>% 
#  mean_hdi(.width = c(.95, 0.75,.50))

#--------------------------- Variance ratio ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_VR","b_VR:REALMTerrestrial")) %>% 
  rename(b_VR_Fresh=b_VR,
         `b_VR(Terres-Fresh)`=`b_VR:REALMTerrestrial`)%>%
  mutate("b_VR_Terres" = 
           b_VR_Fresh + `b_VR(Terres-Fresh)`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_VR_Fresh" , 
                                      "b_VR_Terres",
                                      "b_VR(Terres-Fresh)")))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-3
gathered_post$color[which(gathered_post$key==keys[2])]<-2
gathered_post$color[which(gathered_post$key==keys[3])]<-1

g_VR<-gathered_post %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(color))) +
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2, alpha=0.6)+guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_wrap(~key, scales = "free")

#--------------------------- Total asymmetry A ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_A","b_A:REALMTerrestrial")) %>% 
  rename(b_A_Fresh=b_A,
         `b_A(Terres-Fresh)`=`b_A:REALMTerrestrial`)%>%
  mutate("b_A_Terres" = 
           b_A_Fresh + `b_A(Terres-Fresh)`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_A_Fresh" , 
                                      "b_A_Terres",
                                      "b_A(Terres-Fresh)")))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-3
gathered_post$color[which(gathered_post$key==keys[2])]<-2
gathered_post$color[which(gathered_post$key==keys[3])]<-1

g_A<-gathered_post %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(color))) +
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2, alpha=0.6)+guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_wrap(~key, scales = "free")

#--------------------------- Net asymmetry: uniA ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_uniA","b_uniA:REALMTerrestrial")) %>% 
  rename(b_uniA_Fresh=b_uniA,
         `b_uniA(Terres-Fresh)`=`b_uniA:REALMTerrestrial`)%>%
  mutate("b_uniA_Terres" = 
           b_uniA_Fresh + `b_uniA(Terres-Fresh)`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_uniA_Fresh" , 
                                      "b_uniA_Terres",
                                      "b_uniA(Terres-Fresh)")))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-3
gathered_post$color[which(gathered_post$key==keys[2])]<-2
gathered_post$color[which(gathered_post$key==keys[3])]<-1

g_uniA<-gathered_post %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(color))) +
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2, alpha=0.6)+guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  #scale_y_continuous(NULL, breaks = NULL) +
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  #facet_wrap(~key, scales = "free_x")
  facet_wrap(~key, scales = "free")

#--------------------------- Skewness ratio ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_SR","b_SR:REALMTerrestrial")) %>% 
  rename(b_SR_Fresh=b_SR,
         `b_SR(Terres-Fresh)`=`b_SR:REALMTerrestrial`)%>%
  mutate("b_SR_Terres" = 
           b_SR_Fresh + `b_SR(Terres-Fresh)`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_SR_Fresh" , 
                                      "b_SR_Terres",
                                      "b_SR(Terres-Fresh)")))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-3
gathered_post$color[which(gathered_post$key==keys[2])]<-2
gathered_post$color[which(gathered_post$key==keys[3])]<-1

g_SR<-gathered_post %>% 
  ggplot(aes(x = value, group = key, fill=as.factor(color))) +
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2, alpha=0.6)+guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(aes(y = 0), 
                     point_interval = median_qi, .width = c(.95, 0.75), 
                     interval_size_range = c(0.5, 2.5))+
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())+
  facet_wrap(~key, scales = "free")

pdf("../../Results/gather_res/test/plot_posterior.pdf",height=24,width=15)
op<-par(mar=c(2,2,2,2))
gridExtra::grid.arrange(g_I,g_R,g_VR,g_A,g_uniA,g_SR,nrow=6)
par(op)
dev.off()
