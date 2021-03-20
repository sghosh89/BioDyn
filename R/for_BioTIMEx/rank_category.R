library(BiodiversityR)
library(tidyverse)
library(gridExtra)
#--------------------------------
# The following function will take the input matrix for tail analysis and tag the species
# into top 4 category based on their rank abundance

# inputmatfile_list: the file you want to read with full path
# pathlist: path of the folder where you want to save the output

get_rankabun<-function(inputmatfile_list,pathlist){
  for(i in 1:length(pathlist)){
    i_mat<-readRDS(inputmatfile_list[i])
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
    
    saveRDS(rankabun,paste(pathlist[i],"rankabun.RDS",sep=""))
    
  }
}

#########################################
# now call the function
pathlist<-c("../../Results/for_BioTIMEx/carpenter_2016/",
            "../../Results/for_BioTIMEx/gross_2016/",
            "../../Results/for_BioTIMEx/landis_2018/poplarT5/",
            "../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E/",
            "../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_L/",
            "../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_E/",
            "../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_L/",
            "../../Results/for_BioTIMEx/oneida_phytopl_1975/"
            )

inputmatfile_list<-c(
  "../../DATA/for_BioTIMEx/wrangled_data/carpenter_2016/carpenter_2016_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/gross_2016/gross_2016_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/landis_2018/landis_2018_inputmatrix_tailanal_poplarT5.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_L_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_E_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_L_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/oneida_phytopl_1975/oneida_phytopl_1975_inputmatrix_tailanal.RDS"
)

get_rankabun(inputmatfile_list = inputmatfile_list,pathlist = pathlist)

#===============================================================================
# get summary table for BioTIMEx data
# first we need to decide the REALM, organism for each dataset
summary_table<-c()
for(i in 1:length(pathlist)){
  tempo<-readRDS(paste(pathlist[i],"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,tempo)
}
summary_table$siteid<-c("carpenter_2016","gross_2016","landis_2018",
                  "lightfoot_2015_BOER_E","lightfoot_2015_BOER_L","lightfoot_2015_LATR_E","lightfoot_2015_LATR_L",
                  "oneida_phytopl_1975")
summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)
# save the summary table
saveRDS(summary_table,"../../Results/for_BioTIMEx/summary_table.RDS")

#==================================================================================
source("./get_categorized_interaction.R")

for(i in 1:length(pathlist)){
  x<-readRDS(paste(pathlist[i],"/NonParamStat.RDS",sep=""))
  xcat<-readRDS(paste(pathlist[i],"/rankabun.RDS",sep=""))
  res<-get_categorized_interaction(x=x,xcat=xcat)
  cat("i = ",i,"\n")
  saveRDS(res,paste(pathlist[i],"categorized_interaction.RDS",sep=""))
}
#########################################################################################
# save the results for interaction freq plot

# for 
res<-c()
for(i in 1:length(pathlist)){
  x<-readRDS(paste(pathlist[i],"categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  res<-rbind(res,d)
}
rownames(res)<-NULL
res_combo<-gather(res,key="freq_type",value = "Frequency",freq_indep,freq_syn,freq_comp)

# save the result
saveRDS(res_combo,"../../Results/for_BioTIMEx/interaction_freq_for_BioTIMEx.RDS")


#################################################################
#         plot the interaction freq plot 
#######################################################################

avg_combo_intfreq<-res_combo%>%group_by(Interaction,freq_type)%>%
  summarize(Frequency=mean(Frequency,na.rm=T))%>%ungroup()
avg_combo_intfreq_list<-split(avg_combo_intfreq,f=avg_combo_intfreq$freq_type)
combo<-ggplot(data=res_combo,aes(x=Interaction,y=Frequency,col=freq_type))+geom_point(pch=19)+
  geom_point(data=avg_combo_intfreq,size=6,alpha=0.3)+
  geom_line(data=avg_combo_intfreq_list$freq_comp,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_combo_intfreq_list$freq_indep,aes(x=1:10,y=Frequency),lwd=1)+
  geom_line(data=avg_combo_intfreq_list$freq_syn,aes(x=1:10,y=Frequency),lwd=1)+
  scale_color_manual(values=c("green", "gold1", "orchid"))+
  theme_bw()
#combo


