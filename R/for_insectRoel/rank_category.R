library(BiodiversityR)
library(tidyverse)
library(gridExtra)
#--------------------------------
# The following function will take the input matrix for tail analysis and tag the species
# into top 4 category based on their rank abundance
get_rankabun<-function(summary_table){
  
  for(i in 1:nrow(summary_table)){
   
    myresloc<-paste("../../DATA/for_insectRoel/wrangled_data/",
                    summary_table$STUDY_ID[i],"/",summary_table$newsite[i],"/",sep="")
    i_mat<-readRDS(paste(myresloc,"inputmat_for_tailanal.RDS",sep=""))
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
    
    
    outresloc<-paste("../../Results/for_insectRoel/",summary_table$STUDY_ID[i],"/",
                       summary_table$newsite[i],"/",sep="")
    
    saveRDS(rankabun,paste(outresloc,"rankabun.RDS",sep=""))
    
  }
}

#########################################
# now call the function for freshwater
r_insect<-readRDS("../../Results/for_insectRoel/stability_metric.RDS")
#r_insect<-r_insect%>%filter(f_nind!=1)# remove the community which has only independent interactions
get_rankabun(summary_table = r_insect)

#==================================================================================
source("./get_categorized_interaction.R")

# for insects
mypath<-"../../Results/for_insectRoel/"
for(i in 1:nrow(r_insect)){
  x<-readRDS(paste(mypath,r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(mypath,r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(mypath,r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/categorized_interaction.RDS",sep=""))
}
#########################################################################################
# save the results for interaction freq plot

# for insects
mypath<-"../../Results/for_insectRoel/"
res<-c()
for(i in 1:nrow(r_insect)){
  x<-readRDS(paste(mypath,r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_insect<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# save the result
saveRDS(res_insect,"../../Results/for_insectRoel/interaction_freq_for_insects.RDS")


#################################################################
#         plot the interaction freq plot 
#######################################################################
#res_insect<-na.omit(res_insect)
avg_insect_intfreq<-res_insect%>%group_by(Interaction,freq_type)%>%
  summarize(Frequency=mean(Frequency,na.rm=T))%>%ungroup()
avg_insect_intfreq_list<-split(avg_insect_intfreq,f=avg_insect_intfreq$freq_type)
insect<-ggplot(data=res_insect,aes(x=Interaction,y=Frequency,col=freq_type))+
  geom_point(pch=19)+
  geom_point(data=avg_insect_intfreq,size=6,alpha=0.3)+
  geom_line(data=avg_insect_intfreq_list$freq_comp,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_insect_intfreq_list$freq_indep,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_insect_intfreq_list$freq_syn,aes(x=1:10,y=Frequency),lwd=1)+
  scale_color_manual(values=c("green", "gold1", "orchid"))+
  theme_bw()
insect


































