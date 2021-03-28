library(BiodiversityR)
library(tidyverse)
library(gridExtra)
#--------------------------------
# The following function will take the input matrix for tail analysis and tag the species
# into top 4 category based on their rank abundance

get_rankabun<-function(siteid,realm){
  for(i in 1:length(siteid)){
    site<-siteid[i]
    i_mat<-readRDS(paste("../../Results/for_BioTIME/",realm,"/",site,"/input_tailanal.RDS",sep=""))
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
    
    saveRDS(rankabun,paste("../../Results/for_BioTIME/",realm,"/",site,"/rankabun.RDS",sep=""))
    
  }
}

#########################################
# now call the function for freshwater
r_frs<-readRDS("../../Results/for_BioTIME/Freshwater/summary_table.RDS")
siteid<-r_frs$siteid
realm<-"Freshwater"
get_rankabun(siteid = siteid,realm = realm)

# now call the function for marine
r_mar<-readRDS("../../Results/for_BioTIME/Marine/summary_table.RDS")
siteid<-r_mar$siteid
realm<-"Marine"
get_rankabun(siteid = siteid,realm = realm)

# now call the function for terrestrial
r_ter<-readRDS("../../Results/for_BioTIME/Terrestrial/summary_table.RDS")
siteid<-r_ter$siteid
realm<-"Terrestrial"
get_rankabun(siteid = siteid,realm = realm)

#==================================================================================
source("./get_categorized_interaction.R")

# for freshwater
mypath<-"../../Results/for_BioTIME/Freshwater/"
siteid<-r_frs$siteid
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(mypath,"/",siteid[i],"/rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
}

# for marine
mypath<-"../../Results/for_BioTIME/Marine/"
siteid<-r_mar$siteid
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(mypath,"/",siteid[i],"/rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
}

# for terrestrial
mypath<-"../../Results/for_BioTIME/Terrestrial/"
siteid<-r_ter$siteid
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(mypath,"/",siteid[i],"/rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
}
#########################################################################################
# plot the results 

# for freshwater
mypath<-"../../Results/for_BioTIME/Freshwater/"
siteid<-r_frs$siteid
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

# for marine
mypath<-"../../Results/for_BioTIME/Marine/"
siteid<-r_mar$siteid
res<-c()
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_marine<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# for terrestrial
mypath<-"../../Results/for_BioTIME/Terrestrial/"
siteid<-r_ter$siteid
res<-c()
for(i in 1:length(siteid)){
  x<-readRDS(paste(mypath,"/",siteid[i],"/categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_terres<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# save the result
saveRDS(res_freshw,"../../Results/for_BioTIME/Freshwater/interaction_freq_for_freshwater.RDS")
saveRDS(res_marine,"../../Results/for_BioTIME/Marine/interaction_freq_for_marine.RDS")
saveRDS(res_terres,"../../Results/for_BioTIME/Terrestrial/interaction_freq_for_terrestrial.RDS")


