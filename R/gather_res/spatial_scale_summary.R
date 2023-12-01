rm(list=ls())
library(tidyverse)
library(here)
m<-read.csv(here("Results/metadata_area_edited.csv"))
m<-m%>%dplyr::select(STUDY_ID, newsite, area=estimated_area_in_sqKm, REALM)

m<-m%>%na.omit()
range(m$area,na.rm=T)
range(log10(m$area))

class(m$REALM)
m$REALM<-as.factor(m$REALM)
m$arealog<-log10(m$area)

m_mn<-m%>%group_by(REALM)%>%summarise(mn=mean(arealog))%>%ungroup()
m_md<-m%>%group_by(REALM)%>%summarise(md=median(arealog))%>%ungroup()
m_sd<-m%>%group_by(REALM)%>%summarise(msd=sd(arealog))%>%ungroup()


gp<-ggplot(m, aes(x=arealog, fill=REALM))+geom_density(alpha=0.4)+
  geom_vline(data=m_mn, aes(xintercept=mn, color=REALM),
             linetype="dashed")+
  geom_vline(data=m_md, aes(xintercept=md, color=REALM),
             linetype="solid")+
  theme_bw()+
  xlab("log10(area in Sq. Km.)")+
  scale_color_manual(values=c("dodgerblue", "green3"))+
  scale_fill_manual(values=c("dodgerblue", "green3"))+
  theme(legend.position = c(0.8,0.9),text = element_text(size=19),
        axis.text=element_text(size=22))
gp

mT<-m%>%filter(REALM=="Terrestrial")
summary(mT$arealog)
quantile(mT$arealog, probs=c(0.025,0.975))

mF<-m%>%filter(REALM=="Freshwater")
summary(mF$arealog)
quantile(mF$arealog, probs=c(0.025,0.975))

pdf("../../Results/spatial_scale_summary.pdf",height=6,width=7)
gp
dev.off()

####################
# Now get the distribution for 100 replicates across taxa

#rm(list=ls())
library(tidyverse)
library(here)
m<-read.csv(here("Results/metadata_area_edited.csv"))
m<-m%>%dplyr::select(source,STUDY_ID, newsite, area=estimated_area_in_sqKm, REALM)
m$UID<-paste(m$source,m$STUDY_ID,sep=",")
  
sm_all<-readRDS(here("Results/gather_res/stability_metric_all.RDS"))
sm_all$UID<-paste(sm_all$source,sm_all$STUDY_ID,sep=",")
sm_all<-sm_all%>%dplyr::select(UID,iCV,newsite)

df_all<-c()
for(i in 1:100){
  d<-readRDS(here(paste("Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep="")))
  d<-d%>%dplyr::select(UID,iCV)
  d1<-inner_join(d,sm_all,c("UID"="UID","iCV"="iCV"))
  d1<-d1%>%dplyr::select(-iCV)
  df<-inner_join(d1,m,c("UID"="UID","newsite"="newsite"))
  
  df$replicate<-i
  df_all<-rbind(df_all,df)
}

df_all$REALM<-as.factor(df_all$REALM)
df_all$arealog<-log10(df_all$area)
df_all<-na.omit(df_all)
df_all$replicate<-as.factor(df_all$replicate)

dfr<-df_all%>%filter(replicate==50)
gp2_rep50<-ggplot(dfr, aes(x=arealog, fill=REALM))+geom_density(alpha=0.4)+
    theme_bw()+
  xlab("log10(area in Sq. Km.)")+
  scale_color_manual(values=c("dodgerblue", "green3"))+
  scale_fill_manual(values=c("dodgerblue", "green3"))+
  theme(legend.position = c(0.8,0.9),text = element_text(size=19),
        axis.text=element_text(size=22))

library(ggridges)
gp2_all<-ggplot(df_all, aes(x = arealog, y = replicate, fill = REALM)) +
  geom_density_ridges(alpha=0.4) +
  scale_fill_manual(values=c("dodgerblue", "green3"))+
  theme_classic() + 
  xlab("log10(area in Sq. Km.)")+ylab("Replicates")+
  theme(legend.position = "none", text = element_text(size=30),
        axis.text=element_text(size=22))

m_mn100<-df_all%>%group_by(REALM)%>%summarise(mn=mean(arealog))%>%ungroup()
m_md100<-df_all%>%group_by(REALM)%>%summarise(md=median(arealog))%>%ungroup()

pdf("../../Results/spatial_scale_summary_all_replicates.pdf",height=26,width=7)
gp2_all
dev.off()

pdf("../../Results/spatial_scale_summary_rep50th.pdf",height=6,width=7)
gp2_rep50
dev.off()

m_mn$summary<-"mean, all data"
m_md$summary<-"median, all data"
m_mn100$summary<-"mean, across replicates"
m_md100$summary<-"median, across replicates"

colnames(m_mn)<-colnames(m_md)<-colnames(m_mn100)<-colnames(m_md100)<-c("REALM","Area","Statistics")

mn<-rbind(m_mn,m_mn100, m_md, m_md100)

ggs<-ggplot(data=mn, aes(y=Statistics, x=Area, fill=REALM)) +
  geom_bar(stat="identity", position=position_dodge(), width=.3, alpha=0.4)+
  scale_fill_manual(values=c("dodgerblue", "green3"))+xlim(-4,2)+
  xlab("log10(area in Sq. Km.)")+
  theme_bw()+theme(legend.position = "none",
                   text = element_text(size=24),
                   axis.text=element_text(size=15),
                        axis.text.y = element_text(angle = 90, vjust = 0.1, hjust=0.5))

pdf("../../Results/spatial_scale_summary_mean_and_median.pdf",height=10,width=7)
ggs
dev.off()


