library(BiodiversityR)
library(tidyverse)
library(gridExtra)
#--------------------------------
# The following function will take the input matrix for tail analysis and tag the species
# into top 4 category based on their rank abundance

get_rankabun<-function(siteid){
  for(i in 1:length(siteid)){
    site<-siteid[i]
    i_mat<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",site,"/commonspecies_timeseries.RDS",sep=""))
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
    
    saveRDS(rankabun,paste("../../Results/for_RivFishTIME/",site,"/rankabun.RDS",sep=""))
    
  }
}

#########################################
# now call the function 
r_freshw<-readRDS("../../Results/for_RivFishTIME/summary_table.RDS")
siteid<-r_freshw$siteid
get_rankabun(siteid = siteid)

#==================================================================================
source("./get_categorized_interaction.R")

# for freshwater River fish
mypath<-"../../Results/for_RivFishTIME/"
siteid<-r_freshw$siteid
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(mypath,"/",siteid[i],"/rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
}
#########################################################################################
# save the results for interaction freq plot

# for freshwater
mypath<-"../../Results/for_RivFishTIME/"
siteid<-r_freshw$siteid
res<-c()
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_freshw<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# save the result
saveRDS(res_freshw,"../../Results/for_RivFishTIME/interaction_freq_for_RivFishTime.RDS")


#################################################################
#         plot the interaction freq plot 
#######################################################################

avg_freshw_intfreq<-res_freshw%>%group_by(Interaction,freq_type)%>%
  summarize(Frequency=mean(Frequency,na.rm=T))%>%ungroup()
avg_freshw_intfreq_list<-split(avg_freshw_intfreq,f=avg_freshw_intfreq$freq_type)
freshw<-ggplot(data=res_freshw,aes(x=Interaction,y=Frequency,col=freq_type))+geom_point(pch=19)+
  geom_point(data=avg_freshw_intfreq,size=6,alpha=0.3)+
  geom_line(data=avg_freshw_intfreq_list$freq_comp,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_freshw_intfreq_list$freq_indep,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_freshw_intfreq_list$freq_syn,aes(x=1:10,y=Frequency),lwd=1)+
  scale_color_manual(values=c("green", "gold1", "orchid"))+
  theme_bw()
#freshw


