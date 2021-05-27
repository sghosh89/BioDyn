library(BiodiversityR)
library(tidyverse)
library(gridExtra)
#--------------------------------
# The following function will take the input matrix for tail analysis and tag the species
# into top 4 category based on their rank abundance

get_rankabun<-function(summary_table,realm){
  
  for(i in 1:nrow(summary_table)){
    STUDY_ID<-summary_table$STUDY_ID[i]
    newsite<-summary_table$newsite[i]
    
    if(STUDY_ID==newsite){
      myresloc<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"/",STUDY_ID,"/",sep="")
    }else{
      myresloc<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"/",STUDY_ID,"/",newsite,"/",sep="")
    }
    
    i_mat<-readRDS(paste(myresloc,"input_tailanal.RDS",sep=""))
    if(tail(colnames(i_mat),1)=="raresp"){
      i_mat<-i_mat[,-ncol(i_mat)]
    }
    
    rankabun <- rankabundance(i_mat)
    rankabun<-as.data.frame(rankabun)
    qs<-quantile(rankabun$abundance) # 0,0.25,0.5,0.75,1
    # category: top four: 1: within 75-100%, 2: within 50-75%, 3: within 25-50%, 4: within 0-25%
    rankabun$category<-NA
    qs<-unname(qs)
    for(j in 1:nrow(rankabun)){
      r<-rankabun$abundance[j]
      if(r<qs[2]){
        rankabun$category[j]<-4
      }else if(r>=qs[2] & r<qs[3]){
        rankabun$category[j]<-3
      }else if(r>=qs[3] & r<qs[4]){
        rankabun$category[j]<-2
      }else{
        rankabun$category[j]<-1
      }
    }
    
    if(STUDY_ID==newsite){
      outresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",sep="")
    }else{
      outresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",newsite,"/",sep="")
    }
    saveRDS(rankabun,paste(outresloc,"rankabun.RDS",sep=""))
    
  }
}

#########################################
# now call the function for freshwater
r_frs<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")
realm<-"Freshwater_plotlevel"
get_rankabun(summary_table = r_frs,realm = realm)

# now call the function for terrestrial
r_ter<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")
realm<-"Terrestrial_plotlevel"
get_rankabun(summary_table = r_ter,realm = realm)

#=========================================================
source("./get_categorized_interaction.R")

# for freshwater
summary_table<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")
realm<-"Freshwater_plotlevel"
for(i in 1:nrow(summary_table)){
  STUDY_ID<-summary_table$STUDY_ID[i]
  newsite<-summary_table$newsite[i]
  if(STUDY_ID==newsite){
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",sep="")
  }else{
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",newsite,"/",sep="")
  }
  x<-readRDS(paste(myresloc,"NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(myresloc,"rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(myresloc,"categorized_interaction.RDS",sep=""))
}

# for terrestrial
summary_table<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")
realm<-"Terrestrial_plotlevel"
for(i in 1:nrow(summary_table)){
  STUDY_ID<-summary_table$STUDY_ID[i]
  newsite<-summary_table$newsite[i]
  if(STUDY_ID==newsite){
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",sep="")
  }else{
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",newsite,"/",sep="")
  }
  x<-readRDS(paste(myresloc,"NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(myresloc,"rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(myresloc,"categorized_interaction.RDS",sep=""))
}

#==========================================================================
# plot the results 

# for freshwater
summary_table<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")
realm<-"Freshwater_plotlevel"
res<-c()
for(i in 1:nrow(summary_table)){
  STUDY_ID<-summary_table$STUDY_ID[i]
  newsite<-summary_table$newsite[i]
  if(STUDY_ID==newsite){
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",sep="")
  }else{
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",newsite,"/",sep="")
  }
  x<-readRDS(paste(myresloc,"categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_freshw<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# for terrestrial
summary_table<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")
realm<-"Terrestrial_plotlevel"
res<-c()
for(i in 1:nrow(summary_table)){
  STUDY_ID<-summary_table$STUDY_ID[i]
  newsite<-summary_table$newsite[i]
  if(STUDY_ID==newsite){
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",sep="")
  }else{
    myresloc<-paste("../../Results/for_BioTIME/",realm,"/",STUDY_ID,"/",newsite,"/",sep="")
  }
  x<-readRDS(paste(myresloc,"categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_terres<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# save the result
saveRDS(res_freshw,"../../Results/for_BioTIME/Freshwater_plotlevel/interaction_freq_for_freshwater.RDS")
saveRDS(res_terres,"../../Results/for_BioTIME/Terrestrial_plotlevel/interaction_freq_for_terrestrial.RDS")

#################################################################
#         plot the interaction freq plot for each realm
#######################################################################
pdf("../../Results/for_BioTIME/interaction_freq_for_eachrealm_plotlevel.pdf",height=8,width=16)
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
  scale_color_manual(values=c("green", "gold1", "orchid"))+
  theme_bw()
grid.arrange(freshw,terres, ncol=2, nrow=1)
dev.off()












