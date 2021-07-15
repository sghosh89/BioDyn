# summary plot for posterior parameter distribution
rm(list=ls())
library(brms)
library(tidybayes)
library(tidyverse)
library(gridExtra)

full_model<-readRDS("../../Results/gather_res/taxa_fixedeffect/fullmodel.RDS")
post<-posterior_samples(full_model)


#------------------- Intercept --------------------------
tx<-post%>%select(fish=b_TAXAfish,
                  fresh.inv=b_TAXAfreshwaterinvertebrates,
                  fresh.plants=b_TAXAfreshwaterplants,
                  birds=b_Intercept,
                  terres.inv=b_TAXAterrestrialinvertebrates,
                  terres.plants=b_TAXAterrestrialplants,
                  mammals=b_TAXAmammals)

tx$fish<-tx$fish+tx$birds
tx$fresh.inv<-tx$fresh.inv+tx$birds
tx$fresh.plants<-tx$fresh.plants+tx$birds
tx$terres.inv<-tx$terres.inv+tx$birds
tx$terres.plants<-tx$terres.plants+tx$birds
tx$mammals<-tx$mammals+tx$birds

tx_gather <-
  tx %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "fresh.inv",
                                      "fresh.plants",
                                      "birds",
                                      "terres.inv",
                                      "terres.plants",
                                      "mammals")))

g_I<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=2, y=1, label= "Intercept", size=6,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())


#------------------- Richness --------------------------
tx<-post%>%select(fish=`b_R:TAXAfish`,
                  fresh.inv=`b_R:TAXAfreshwaterinvertebrates`,
                  fresh.plants=`b_R:TAXAfreshwaterplants`,
                  birds=b_R,
                  terres.inv=`b_R:TAXAterrestrialinvertebrates`,
                  terres.plants=`b_R:TAXAterrestrialplants`,
                  mammals=`b_R:TAXAmammals`)

tx$fish<-tx$fish+tx$birds
tx$fresh.inv<-tx$fresh.inv+tx$birds
tx$fresh.plants<-tx$fresh.plants+tx$birds
tx$terres.inv<-tx$terres.inv+tx$birds
tx$terres.plants<-tx$terres.plants+tx$birds
tx$mammals<-tx$mammals+tx$birds

tx_gather <-
  tx %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "fresh.inv",
                                      "fresh.plants",
                                      "birds",
                                      "terres.inv",
                                      "terres.plants",
                                      "mammals")))

g_R<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=2, y=1, label= "R", size=6,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

#------------------- VR --------------------------
tx<-post%>%select(fish=`b_VR:TAXAfish`,
                  fresh.inv=`b_VR:TAXAfreshwaterinvertebrates`,
                  fresh.plants=`b_VR:TAXAfreshwaterplants`,
                  birds=b_VR,
                  terres.inv=`b_VR:TAXAterrestrialinvertebrates`,
                  terres.plants=`b_VR:TAXAterrestrialplants`,
                  mammals=`b_VR:TAXAmammals`)

tx$fish<-tx$fish+tx$birds
tx$fresh.inv<-tx$fresh.inv+tx$birds
tx$fresh.plants<-tx$fresh.plants+tx$birds
tx$terres.inv<-tx$terres.inv+tx$birds
tx$terres.plants<-tx$terres.plants+tx$birds
tx$mammals<-tx$mammals+tx$birds

tx_gather <-
  tx %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "fresh.inv",
                                      "fresh.plants",
                                      "birds",
                                      "terres.inv",
                                      "terres.plants",
                                      "mammals")))

g_VR<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=2, y=1, label= "VR", size=6,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

#------------------- A --------------------------
tx<-post%>%select(fish=`b_A:TAXAfish`,
                  fresh.inv=`b_A:TAXAfreshwaterinvertebrates`,
                  fresh.plants=`b_A:TAXAfreshwaterplants`,
                  birds=b_A,
                  terres.inv=`b_A:TAXAterrestrialinvertebrates`,
                  terres.plants=`b_A:TAXAterrestrialplants`,
                  mammals=`b_A:TAXAmammals`)

tx$fish<-tx$fish+tx$birds
tx$fresh.inv<-tx$fresh.inv+tx$birds
tx$fresh.plants<-tx$fresh.plants+tx$birds
tx$terres.inv<-tx$terres.inv+tx$birds
tx$terres.plants<-tx$terres.plants+tx$birds
tx$mammals<-tx$mammals+tx$birds

tx_gather <-
  tx %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "fresh.inv",
                                      "fresh.plants",
                                      "birds",
                                      "terres.inv",
                                      "terres.plants",
                                      "mammals")))

g_A<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-2, y=1, label= "A", size=6,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

#------------------- uniA --------------------------
tx<-post%>%select(fish=`b_uniA:TAXAfish`,
                  fresh.inv=`b_uniA:TAXAfreshwaterinvertebrates`,
                  fresh.plants=`b_uniA:TAXAfreshwaterplants`,
                  birds=b_uniA,
                  terres.inv=`b_uniA:TAXAterrestrialinvertebrates`,
                  terres.plants=`b_uniA:TAXAterrestrialplants`,
                  mammals=`b_uniA:TAXAmammals`)

tx$fish<-tx$fish+tx$birds
tx$fresh.inv<-tx$fresh.inv+tx$birds
tx$fresh.plants<-tx$fresh.plants+tx$birds
tx$terres.inv<-tx$terres.inv+tx$birds
tx$terres.plants<-tx$terres.plants+tx$birds
tx$mammals<-tx$mammals+tx$birds

tx_gather <-
  tx %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "fresh.inv",
                                      "fresh.plants",
                                      "birds",
                                      "terres.inv",
                                      "terres.plants",
                                      "mammals")))

g_uniA<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-5, y=1, label= "uniA", size=6,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

#------------------- SR --------------------------
tx<-post%>%select(fish=`b_SR:TAXAfish`,
                  fresh.inv=`b_SR:TAXAfreshwaterinvertebrates`,
                  fresh.plants=`b_SR:TAXAfreshwaterplants`,
                  birds=b_SR,
                  terres.inv=`b_SR:TAXAterrestrialinvertebrates`,
                  terres.plants=`b_SR:TAXAterrestrialplants`,
                  mammals=`b_SR:TAXAmammals`)

tx$fish<-tx$fish+tx$birds
tx$fresh.inv<-tx$fresh.inv+tx$birds
tx$fresh.plants<-tx$fresh.plants+tx$birds
tx$terres.inv<-tx$terres.inv+tx$birds
tx$terres.plants<-tx$terres.plants+tx$birds
tx$mammals<-tx$mammals+tx$birds

tx_gather <-
  tx %>% 
  gather() %>% 
  mutate(key = factor(key, levels = c("fish", 
                                      "fresh.inv",
                                      "fresh.plants",
                                      "birds",
                                      "terres.inv",
                                      "terres.plants",
                                      "mammals")))

g_SR<-tx_gather%>%ggplot(aes(y=key,x=value))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=-10, y=1, label= "SR", size=6,col="blue")+  
  theme_bw(base_size = 24)+theme(axis.title=element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 #axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank())

pdf("../../Results/gather_res/taxa_fixedeffect/plot_posterior_pointinterval.pdf",height=6,width=20)
op<-par(mar=c(2,2,2,2))
gridExtra::grid.arrange(g_I,g_R,g_VR,g_A,g_uniA,g_SR,nrow=2)
par(op)
dev.off()
