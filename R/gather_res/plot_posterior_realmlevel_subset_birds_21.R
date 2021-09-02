# summary plot for posterior parameter distribution
rm(list=ls())
library(brms)
library(tidybayes)
library(tidyverse)
library(gridExtra)

full_model<-readRDS("../../Results/gather_res/subset_birds_21/fullmodel.RDS")
post<-posterior_samples(full_model)

#--------------------------- Intercept ----------------------------------
gathered_post <-
  post %>% 
  select(c("b_Intercept","b_REALMTerrestrial")) %>% 
  rename(b_Intercept_Fresh=b_Intercept,
         `b_Intercept_diff_T-F`=`b_REALMTerrestrial`)%>%
  mutate("b_Intercept_Terres" = 
           b_Intercept_Fresh + `b_Intercept_diff_T-F`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_Intercept_diff_T-F",
                                      "b_Intercept_Fresh" , 
                                      "b_Intercept_Terres"
  )))
keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-3
gathered_post$color[which(gathered_post$key==keys[3])]<-2

myColors <- c("blueviolet", "dodgerblue", "green3")

gathered_postI<-gathered_post
g_I<-gathered_postI %>% 
  ggplot(aes(x = value, group = key,fill=as.factor(key))) +
  scale_fill_manual(values=alpha(myColors, 0.6))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  geom_density(size = .2)+guides(fill=FALSE, color=FALSE)+
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
         `b_R_diff_T-F`=`b_R:REALMTerrestrial`)%>%
  mutate("b_R_Terres" = 
           b_R_Fresh + `b_R_diff_T-F`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_R_diff_T-F",
                                      "b_R_Fresh" , 
                                      "b_R_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-3
gathered_post$color[which(gathered_post$key==keys[3])]<-2

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
         `b_VR_diff_T-F`=`b_VR:REALMTerrestrial`)%>%
  mutate("b_VR_Terres" = 
           b_VR_Fresh + `b_VR_diff_T-F`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_VR_diff_T-F",
                                      "b_VR_Fresh" , 
                                      "b_VR_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-3
gathered_post$color[which(gathered_post$key==keys[3])]<-2

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
         `b_A_diff_T-F`=`b_A:REALMTerrestrial`)%>%
  mutate("b_A_Terres" = 
           b_A_Fresh + `b_A_diff_T-F`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_A_diff_T-F",
                                      "b_A_Fresh" , 
                                      "b_A_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-3
gathered_post$color[which(gathered_post$key==keys[3])]<-2

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
         `b_uniA_diff_T-F`=`b_uniA:REALMTerrestrial`)%>%
  mutate("b_uniA_Terres" = 
           b_uniA_Fresh + `b_uniA_diff_T-F`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_uniA_diff_T-F",
                                      "b_uniA_Fresh" , 
                                      "b_uniA_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-3
gathered_post$color[which(gathered_post$key==keys[3])]<-2

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
         `b_SR_diff_T-F`=`b_SR:REALMTerrestrial`)%>%
  mutate("b_SR_Terres" = 
           b_SR_Fresh + `b_SR_diff_T-F`)%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_SR_diff_T-F",
                                      "b_SR_Fresh" , 
                                      "b_SR_Terres"
  )))

keys<-levels(gathered_post$key)
gathered_post$color<-NA
gathered_post$color[which(gathered_post$key==keys[1])]<-1
gathered_post$color[which(gathered_post$key==keys[2])]<-3
gathered_post$color[which(gathered_post$key==keys[3])]<-2

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

pdf("../../Results/gather_res/subset_birds_21/plot_posterior.pdf",height=24,width=15)
op<-par(mar=c(2,2,2,2))
gridExtra::grid.arrange(g_I,g_R,g_VR,g_A,g_uniA,g_SR,nrow=6)
par(op)
dev.off()

#=================================================================================
#  point estimate plot
#=================================================================================

#gathered_postI%>%ggplot(aes(fill = key, color = key, x = value)) +
#  scale_fill_manual(values=alpha(myColors, 0.6))+
#  stat_slab(alpha = .3) +
#  stat_pointinterval(position = position_dodge(width = .4, preserve = "single"))+

g_I<-gathered_postI%>%ggplot(aes(y=key,x=value,col=as.factor(color)))+
  geom_vline(xintercept = 0, linetype="dashed", color="dimgray")+
  guides(fill=FALSE, color=FALSE)+
  stat_pointinterval(.width=c(0.75,0.95))+
  annotate("text", x=1, y=3.4, label= "Intercept", size=5)+
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


pdf("../../Results/gather_res/subset_birds_21/plot_posterior_pointinterval.pdf",height=6,width=12)
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

#tx$fish<-tx$`r_TAXA[fish,Intercept]`
#tx$freshwater.inv<-tx$`r_TAXA[freshwater.invertebrates,Intercept]`
#tx$freshwater.plants<-tx$`r_TAXA[freshwater.plants,Intercept]`
#tx$birds<-tx$`r_TAXA[birds,Intercept]`
#tx$terrestrial.inv<-tx$`r_TAXA[terrestrial.invertebrates,Intercept]`
#tx$terrestrial.plants<-tx$`r_TAXA[terrestrial.plants,Intercept]`
#tx$mammals<-tx$`r_TAXA[mammals,Intercept]`

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

pdf("../../Results/gather_res/subset_birds_21/plot_taxaeffect.pdf",height=5,width=8)
op<-par(mar=c(2,2,2,2))
g_tx
par(op)
dev.off()

##################################################################
# model summary output for subset_birds_21

rm(list=ls())
library(brms)
library(tidyverse)
library(performance)
library(tidybayes)

###########################################################################
sink("../../Results/gather_res/console_model_summary_table_subset_birds_21.txt", append=TRUE, split=TRUE)

# see summary from basic model without realm
bm<-readRDS("../../Results/gather_res/subset_birds_21/basic_model.RDS")
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

nm<-readRDS("../../Results/gather_res/subset_birds_21/basic_model_w_REALM.RDS")
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

fm<-readRDS("../../Results/gather_res/subset_birds_21/fullmodel.RDS")
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











