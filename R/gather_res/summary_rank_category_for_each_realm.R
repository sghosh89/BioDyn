rm(list=ls())
library(tidyverse)
library(gridExtra)
#=====================================================================
# gather categorized interaction summary for all data you have analyzed

# for BioTIME
ci_BioTIME_f<-readRDS("../../Results/for_BioTIME/Freshwater/interaction_freq_for_freshwater.RDS")
ci_BioTIME_f$REALM<-"Freshwater"
ci_BioTIME_t<-readRDS("../../Results/for_BioTIME/Terrestrial/interaction_freq_for_terrestrial.RDS")
ci_BioTIME_t$REALM<-"Terrestrial"
ci_BioTIME_m<-readRDS("../../Results/for_BioTIME/Marine/interaction_freq_for_marine.RDS")
ci_BioTIME_m$REALM<-"Marine"
ci_BioTIME<-rbind(ci_BioTIME_f,ci_BioTIME_m,ci_BioTIME_t)
ci_BioTIME$source<-"BioTIME"

# for BioTIMEx
#x<-readRDS("../../Results/for_BioTIMEx/carpenter_2016/categorized_interaction.RDS")


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

ci_all<-rbind(ci_BioTIME,ci_BBS,ci_RF,ci_SL_P,ci_SL_Z,ci_z2014)

#################################################################
#         plot the interaction freq plot for each realm
#######################################################################


res_freshw<-ci_all%>%filter(REALM=="Freshwater")%>%select(Interaction,freq_type,Frequency)
res_marine<-ci_all%>%filter(REALM=="Marine")%>%select(Interaction,freq_type,Frequency)
res_terres<-ci_all%>%filter(REALM=="Terrestrial")%>%select(Interaction,freq_type,Frequency)


pdf("../../Results/gather_res/interaction_freq_for_eachrealm.pdf",height=8,width=16)

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


avg_marine_intfreq<-res_marine%>%group_by(Interaction,freq_type)%>%
  summarize(Frequency=mean(Frequency,na.rm=T))%>%ungroup()
avg_marine_intfreq_list<-split(avg_marine_intfreq,f=avg_marine_intfreq$freq_type)
marine<-ggplot(data=res_marine,aes(x=Interaction,y=Frequency,col=freq_type))+geom_point(pch=19)+
  geom_point(data=avg_marine_intfreq,size=6,alpha=0.3)+
  geom_line(data=avg_marine_intfreq_list$freq_comp,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_marine_intfreq_list$freq_indep,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_marine_intfreq_list$freq_syn,aes(x=1:10,y=Frequency),lwd=1)+
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
  scale_color_manual(values=c("green", "gold1", "orchid"))+
  theme_bw()

grid.arrange(freshw,terres,marine, ncol=2, nrow=2)

dev.off()




