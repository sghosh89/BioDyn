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
    print(i)
    
  }
}

#########################################
# now call the function
get_inputloc_table<-function(dataset_idset){
  STUDY_ID<-c()
  newsite<-c()
  inputloc<-c()
  resloc<-c()
  for(i in 1:length(dataset_idset)){
    dataset_id<-dataset_idset[i]
    inputmatpath<-paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",sep="")
    if(!file.exists(paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/sitelist.RDS",sep=""))){
      readF<- list.files(inputmatpath, pattern = "_inputmatrix_tailanal.RDS", full.names = TRUE)
      outpath<-paste("../../Results/for_BioTIMEx/",dataset_id,"/",sep="")
      STUDY_ID<-c(STUDY_ID,dataset_id)
      newsite<-c(newsite,dataset_id)
      inputloc<-c(inputloc,readF)
      resloc<-c(resloc,outpath)
    }else{
      sitelist<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/sitelist.RDS",sep=""))
      for(k in 1:length(sitelist)){
        readF<- list.files(inputmatpath, pattern = paste(sitelist[k],"_inputmatrix_tailanal.RDS",sep=""), full.names = TRUE)
        outpath<-paste("../../Results/for_BioTIMEx/",dataset_id,"/",sitelist[k],"/",sep="")
        STUDY_ID<-c(STUDY_ID,dataset_id)
        newsite<-c(newsite,sitelist[k])
        inputloc<-c(inputloc,readF)
        resloc<-c(resloc,outpath)
      }
    }
    print(i)
  }
  
  inputloc_table<-as.data.frame(cbind(STUDY_ID,newsite,inputloc,resloc))
  
  return(inputloc_table)
}

dataset_idset<-c("baikal_phyto","carpenter_2016","cumbrian_phyto","cumbrian_zoo",
                 "gross_2016","landis_2018","lightfoot_2015",
                 "oneida_fish_gillnets","oneida_fish_trawl","oneida_phytopl_1975")
inputloc_table<-get_inputloc_table(dataset_idset=dataset_idset)
get_rankabun(inputmatfile_list = inputloc_table$inputloc, pathlist = inputloc_table$resloc)
#=============================================================================================

saveRDS(inputloc_table,"../../Results/for_BioTIMEx/inputloc_table.RDS")

#=============================================================================================
# get summary table for BioTIMEx data
# first we need to decide the REALM, organism for each dataset
summary_table<-c()
pathlist <- inputloc_table$resloc
for(i in 1:length(pathlist)){
  tempo<-readRDS(paste(pathlist[i],"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,tempo)
}
summary_table<-cbind(STUDY_ID=inputloc_table$STUDY_ID,newsite=inputloc_table$newsite,summary_table)

summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

#==================================================================================
summary_table$REALM<-NA
summary_table$TAXA<-NA
summary_table$ORGANISMS<-NA
summary_table$METHOD<-NA

# ---------------- following done manually, help file saved in wrangled data ----------
id<-which(summary_table$STUDY_ID=="baikal_phyto")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Freshwater plants"
summary_table$ORGANISMS[id]<-"Phytoplankton"
summary_table$METHOD[id]<-"Net"

id<-which(summary_table$STUDY_ID=="carpenter_2016")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Freshwater invertebrates"
summary_table$ORGANISMS[id]<-"Zooplankton"
summary_table$METHOD[id]<-"Net" # https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.355.4

id<-which(summary_table$STUDY_ID=="cumbrian_phyto")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Freshwater plants"
summary_table$ORGANISMS[id]<-"Phytoplankton"
summary_table$METHOD[id]<-"Water column sample"

id<-which(summary_table$STUDY_ID=="cumbrian_zoo")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Freshwater invertebrates"
summary_table$ORGANISMS[id]<-"Zooplankton"
summary_table$METHOD[id]<-"Net"

id<-which(summary_table$STUDY_ID=="gross_2016")
summary_table$REALM[id]<-"Terrestrial"
summary_table$TAXA[id]<-"Terrestrial plants"
summary_table$ORGANISMS[id]<-"Plant"
summary_table$METHOD[id]<-"Control"

id<-which(summary_table$STUDY_ID=="landis_2018")
summary_table$REALM[id]<-"Terrestrial"
summary_table$TAXA[id]<-"Terrestrial invertebrates"
summary_table$ORGANISMS[id]<-"Insect"
summary_table$METHOD[id]<-"Sticky trap"

id<-which(summary_table$STUDY_ID=="lightfoot_2015")
summary_table$REALM[id]<-"Terrestrial"
summary_table$TAXA[id]<-"Terrestrial invertebrates"
summary_table$ORGANISMS[id]<-"Grasshopper"
summary_table$METHOD[id]<-"Web trap"

id<-which(summary_table$STUDY_ID=="oneida_fish_gillnets")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Fish"
summary_table$ORGANISMS[id]<-"Fish"
summary_table$METHOD[id]<-"Gillnets"

id<-which(summary_table$STUDY_ID=="oneida_fish_trawl")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Fish"
summary_table$ORGANISMS[id]<-"Fish"
summary_table$METHOD[id]<-"Trawl"

id<-which(summary_table$STUDY_ID=="oneida_phytopl_1975")
summary_table$REALM[id]<-"Freshwater"
summary_table$TAXA[id]<-"Freshwater plants"
summary_table$ORGANISMS[id]<-"Phytoplankton"
summary_table$METHOD[id]<-"Net"

summary_table<-summary_table%>%filter(f_nind!=1)
# save the summary table
saveRDS(summary_table,"../../Results/for_BioTIMEx/summary_table.RDS")
#====================================================================================
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

# now we need only community with f_nind!=1
mytable<-left_join(summary_table,inputloc_table,by=c("STUDY_ID","newsite"))
pathlist<-mytable$resloc

# for 
res<-c()
for(i in 1:length(pathlist)){
  x<-readRDS(paste(pathlist[i],"categorized_interaction.RDS",sep=""))
  d<-x$table_interaction
  d<-d[,5:7]
  d$Interaction<-rownames(d)
  d$REALM<-mytable$REALM[i]
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
combo


