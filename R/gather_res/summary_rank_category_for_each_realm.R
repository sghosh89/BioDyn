rm(list=ls())
library(tidyverse)
library(gridExtra)
#=====================================================================
# gather categorized interaction summary for all data you have analyzed

# for BioTIME
#----------------------------------
#ci_BioTIME_f<-readRDS("../../Results/for_BioTIME/Freshwater/interaction_freq_for_freshwater.RDS")
#ci_BioTIME_f$REALM<-"Freshwater"
#ci_BioTIME_t<-readRDS("../../Results/for_BioTIME/Terrestrial/interaction_freq_for_terrestrial.RDS")
#ci_BioTIME_t$REALM<-"Terrestrial"
#ci_BioTIME_m<-readRDS("../../Results/for_BioTIME/Marine/interaction_freq_for_marine.RDS")
#ci_BioTIME_m$REALM<-"Marine"
#ci_BioTIME<-rbind(ci_BioTIME_f,ci_BioTIME_m,ci_BioTIME_t)
#ci_BioTIME$source<-"BioTIME"
#----------------------------------- for plotlevel analysis ----------------------------
ci_BioTIME_f<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/interaction_freq_for_freshwater.RDS")
ci_BioTIME_f$REALM<-"Freshwater"
ci_BioTIME_t<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/interaction_freq_for_terrestrial.RDS")
ci_BioTIME_t$REALM<-"Terrestrial"
ci_BioTIME<-rbind(ci_BioTIME_f,ci_BioTIME_t)
ci_BioTIME$source<-"BioTIME"

# for BioTIMEx
ci_BioTIMEx<-readRDS("../../Results/for_BioTIMEx/interaction_freq_for_BioTIMEx.RDS")
ci_BioTIMEx$source<-"BioTIMEx"

# for BBS
ci_BBS<-readRDS("../../Results/for_BBS/interaction_freq_for_BBS.RDS")
ci_BBS$source<-"BBS"
ci_BBS$REALM<-"Terrestrial"

# for RivFishTIME
ci_RF<-readRDS("../../Results/for_RivFishTIME/interaction_freq_for_RivFishTime.RDS")
ci_RF$source<-"RivFishTIME"
ci_RF$REALM<-"Freshwater"

# for swisslake
ci_SL_P<-readRDS("../../Results/for_swisslake/interaction_freq_for_phytoplankton.RDS")
ci_SL_P$source<-"SwissLakePhyto"
ci_SL_P$REALM<-"Freshwater"
ci_SL_Z<-readRDS("../../Results/for_swisslake/interaction_freq_for_zooplankton.RDS")
ci_SL_Z$source<-"SwissLakeZoo"
ci_SL_Z$REALM<-"Freshwater"

# for zoop2014
ci_z2014<-readRDS("../../Results/for_zoop_2014/interaction_freq_for_zoop2014.RDS")
ci_z2014$source<-"Zooplankton2014"
ci_z2014$REALM<-"Freshwater"

ci_all<-rbind(ci_BioTIME,ci_BioTIMEx,ci_BBS,ci_RF,ci_SL_P,ci_SL_Z,ci_z2014)

#################################################################
#         plot the interaction freq plot for each realm
#######################################################################


res_freshw<-ci_all%>%filter(REALM=="Freshwater")%>%select(Interaction,freq_type,Frequency)
res_terres<-ci_all%>%filter(REALM=="Terrestrial")%>%select(Interaction,freq_type,Frequency)


pdf("../../Results/gather_res/interaction_freq_for_eachrealm.pdf",height=4,width=16)

avg_freshw_intfreq<-res_freshw%>%group_by(Interaction,freq_type)%>%
  summarize(Frequency=mean(Frequency,na.rm=T))%>%ungroup()
avg_freshw_intfreq_list<-split(avg_freshw_intfreq,f=avg_freshw_intfreq$freq_type)
freshw<-ggplot(data=res_freshw,aes(x=Interaction,y=Frequency,col=freq_type))+geom_point(pch=19)+
  geom_point(data=avg_freshw_intfreq,size=6,alpha=0.3)+
  geom_line(data=avg_freshw_intfreq_list$freq_comp,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_freshw_intfreq_list$freq_indep,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_freshw_intfreq_list$freq_syn,aes(x=1:10,y=Frequency),lwd=1)+
  scale_color_manual(values=c("green", "gold1", "orchid"),guide=F)+
  theme_bw()


avg_terres_intfreq<-res_terres%>%group_by(Interaction,freq_type)%>%
  summarize(Frequency=mean(Frequency,na.rm=T))%>%ungroup()
avg_terres_intfreq_list<-split(avg_terres_intfreq,f=avg_terres_intfreq$freq_type)
terres<-ggplot(data=res_terres,aes(x=Interaction,y=Frequency,col=freq_type))+geom_point(pch=19)+
  geom_point(data=avg_terres_intfreq,size=6,alpha=0.3)+
  geom_line(data=avg_terres_intfreq_list$freq_comp,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_terres_intfreq_list$freq_indep,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_terres_intfreq_list$freq_syn,aes(x=1:10,y=Frequency),lwd=1)+
  scale_color_manual(values=c("green", "gold1", "orchid"),guide=F)+
  theme_bw()

grid.arrange(freshw,terres, ncol=2, nrow=1)

dev.off()

avg_freshw_intfreq_syn<-avg_freshw_intfreq%>%filter(freq_type=="freq_syn")
avg_freshw_intfreq_comp<-avg_freshw_intfreq%>%filter(freq_type=="freq_comp")
avg_terres_intfreq_syn<-avg_terres_intfreq%>%filter(freq_type=="freq_syn")
avg_terres_intfreq_comp<-avg_terres_intfreq%>%filter(freq_type=="freq_comp")

syn_comp_ratio_freshw<-avg_freshw_intfreq_syn$Frequency/avg_freshw_intfreq_comp$Frequency
syn_comp_ratio_terres<-avg_terres_intfreq_syn$Frequency/avg_terres_intfreq_comp$Frequency


dd_f<-data.frame(di=avg_freshw_intfreq_syn$Interaction,ratio=syn_comp_ratio_freshw,REALM="Freshwater")
dd_t<-data.frame(di=avg_terres_intfreq_syn$Interaction,ratio=syn_comp_ratio_terres,REALM="Terrestrial")
dd=rbind(dd_f,dd_t)
gp<-ggplot(data=dd,aes(x=di,y=ratio,col=REALM))+
  geom_point(pch=19)+
  geom_line(data=dd_f,aes(x=1:10,y=ratio),lwd=0.8)+
  geom_line(data=dd_t,aes(x=1:10,y=ratio),lwd=0.8)+
  scale_color_manual(values=c("blue","green"))+
  xlab("Dominant species pairwise interaction")+ylab("Syn/Asyn ratio")+
  theme_bw()

pdf("../../Results/gather_res/ratio_for_eachrealm.pdf",height=5,width=8)
gp
dev.off()
