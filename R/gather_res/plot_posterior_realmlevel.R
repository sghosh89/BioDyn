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

gathered_postI<-gathered_post
g_I<-gathered_postI %>% 
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

gathered_postR<-gathered_post
g_R<-gathered_postR %>% 
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

gathered_postVR<-gathered_post
g_VR<-gathered_postVR %>% 
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

gathered_postA<-gathered_post
g_A<-gathered_postA %>% 
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

gathered_postuniA<-gathered_post
g_uniA<-gathered_postuniA %>% 
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

gathered_postSR<-gathered_post
g_SR<-gathered_postSR %>% 
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

#=================================================================================
#  point estimate plot
#=================================================================================
g_I<-gathered_postI%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-1, y=3.4, label= "Intercept", size=5)+
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())
  
g_R<-gathered_postR%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=0.4, y=3.4, label= "slope_R", size=5)+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

g_VR<-gathered_postVR%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-0.2, y=3.4, label= "slope_VR", size=5)+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

g_A<-gathered_postA%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-0.1, y=3.4, label= "slope_A", size=5)+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

g_uniA<-gathered_postuniA%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=0.12, y=3.4, label= "slope_uniA", size=5)+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())


g_SR<-gathered_postSR%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=0.5, y=3.4, label= "slope_SR", size=5)+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())


pdf("../../Results/gather_res/test/plot_posterior_pointinterval.pdf",height=6,width=12)
op<-par(mar=c(2,2,2,2))
gridExtra::grid.arrange(g_I,g_R,g_VR,g_A,g_uniA,g_SR,nrow=2)
par(op)
dev.off()

#=================================================================================
# Effect size of taxa plot
#=================================================================================

objc<-coef(full_model)
objc$TAXA[,,"Intercept"]

tx<-post%>%select(b_Intercept,
                  `r_TAXA[fish,Intercept]`,
                  `r_TAXA[freshwater.invertebrates,Intercept]`,
                  `r_TAXA[freshwater.plants,Intercept]`,
                  `r_TAXA[birds,Intercept]`,
                  `r_TAXA[terrestrial.invertebrates,Intercept]`,
                  `r_TAXA[terrestrial.plants,Intercept]`,
                  `r_TAXA[mammals,Intercept]`
                  )

# following columns are the original source for objc$TAXA[,,"Intercept"]
tx$fish<-tx$b_Intercept+tx$`r_TAXA[fish,Intercept]`
tx$freshwater.inv<-tx$b_Intercept+tx$`r_TAXA[freshwater.invertebrates,Intercept]`
tx$freshwater.plants<-tx$b_Intercept+tx$`r_TAXA[freshwater.plants,Intercept]`
tx$birds<-tx$b_Intercept+tx$`r_TAXA[birds,Intercept]`
tx$terrestrial.inv<-tx$b_Intercept+tx$`r_TAXA[terrestrial.invertebrates,Intercept]`
tx$terrestrial.plants<-tx$b_Intercept+tx$`r_TAXA[terrestrial.plants,Intercept]`
tx$mammals<-tx$b_Intercept+tx$`r_TAXA[mammals,Intercept]`

tx_gather <-
  tx %>% 
  select(fish,freshwater.inv,freshwater.plants,
         birds,terrestrial.inv,terrestrial.plants,mammals)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "freshwater.inv",
                                      "freshwater.plants",
                                      "birds",
                                      "terrestrial.inv",
                                      "terrestrial.plants",
                                      "mammals")))

g_tx<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-1.6, y=1, label= "Effect of taxa on Intercept", size=4.5,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

pdf("../../Results/gather_res/test/plot_taxaeffect.pdf",height=5,width=8)
op<-par(mar=c(2,2,2,2))
g_tx
par(op)
dev.off()

#=================================================================================
# conditional effect plot
#=================================================================================
x<-conditional_effects(full_model)

#plot conditional effects
pdf("../../Results/gather_res/test/conditional_effects.pdf",height=12,width=16)
op<-par(mfrow=c(3,2),mar=c(3,3,3,3))

p1<-plot(x,points=F,plot=F)[[1]]+theme_classic()

p2<-plot(x,points=F,plot=F)[[2]]+theme_classic()

p3<-plot(x,points=F,plot=F)[[3]]+theme_classic()

p4<-plot(x,points=F,plot=F)[[4]]+theme_classic()

p5<-plot(x,points=F,plot=F)[[5]]+theme_classic()

p6<-plot(x,points=F,plot=F)[[6]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))

p7<-plot(x,points=F,plot=F)[[7]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p8<-plot(x,points=F,plot=F)[[8]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p9<-plot(x,points=F,plot=F)[[9]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p10<-plot(x,points=F,plot=F)[[10]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))+
  theme(legend.position = "none")

p11<-plot(x,points=F,plot=F)[[11]]+theme_classic()+
  scale_color_manual(values=c("skyblue","green"))+
  scale_fill_manual(values=c("skyblue","green"))

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, nrow =3, ncol=4)
par(op)
dev.off()












