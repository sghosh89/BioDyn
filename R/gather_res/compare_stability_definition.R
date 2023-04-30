rm(list=ls())
library(tidyverse)
library(gridExtra)
library(sn)

# plot with real data
df<-read.csv("../../Results/gather_res/data_summary.csv")
dfn1<-as.data.frame(df$iCV)
dfn2<-as.data.frame(df$iCValt)
dfn1$stability<-"iCV"
dfn2$stability<-"iCValt"
colnames(dfn1)[1]<-colnames(dfn2)[1]<-"value"
dfn<-rbind(dfn1,dfn2)
dfn$stability<-as.factor(dfn$stability)
g0<-ggplot(dfn, aes(x = value, fill = stability)) +                       
  geom_histogram(position = "identity", alpha = 0.3, bins = 50)+
  scale_fill_brewer(labels=c("mean/sd","median/IQR"), palette = "Dark2")+
  theme_classic()+theme(legend.position=c(0.5,0.5))

g1s<-ggplot(df, aes(skw_real)) +
  geom_histogram(fill="gray90", col="black")+xlab("Skewness")+
  theme_classic()

#-----------------------------------------
set.seed(seed=123)
yg<-rsn(n=10000, xi = 50, omega = 4, alpha = 0) # nearly normal
hist(yg,100)
range(yg)

set.seed(seed=123)
yp<-rsn(n=10000, xi = 37, omega = 7.5, alpha = 28) # pos skw
hist(yp,100)
range(yp)

set.seed(seed=123)
yn<-rsn(n=10000, xi = 65, omega = 7.5, alpha = -28) # neg skw
hist(yn,100)
range(yn)

#-----------------------------
yg<-as.data.frame(yg)
yg$type<-"Normal"
colnames(yg)[1]<-"Biomass"

yp<-as.data.frame(yp)
yp$type<-"Pos.skew"
colnames(yp)[1]<-"Biomass"

yn<-as.data.frame(yn)
yn$type<-"Neg.skew"
colnames(yn)[1]<-"Biomass"

df<-rbind(yp, yg, yn)
df$type<-as.factor(df$type)

g2<-ggplot(df, aes(type, Biomass)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "red") +
  stat_summary(
    fun.min = function(x) mean(x) - 0.5*sd(x), 
    fun.max = function(x) mean(x) + 0.5*sd(x), 
    geom = "errorbar",
    color = rgb(1,0,0,0.3),
    width = .3
  )+theme_classic()

g3<-ggplot(df, aes(Biomass)) +
  geom_histogram(fill="gray90", col="black")+facet_grid(~type) +theme_classic()
  
df2<-df%>%group_by(type)%>%summarise(iCV=mean(Biomass)/sd(Biomass), 
                                iCValt=median(Biomass)/IQR(Biomass,type=7))%>%ungroup()

g4<-ggplot(df2, aes(y=iCV, x=iCValt)) + 
  ylab("Stability = mean(M) / sd(M)")+ 
  xlab("Stability = median(M) / IQR(M)")+
  geom_point(alpha=0.2) + xlim(6,13)+ylim(6,13)+
  geom_abline(intercept = 0, col="black")+
  coord_fixed()+theme_bw()


pdf("../../Results/stability_defn.pdf", height=6, width=8)
grid.arrange(g3,g2,g0,g1s,nrow=2)
dev.off()


#g4
