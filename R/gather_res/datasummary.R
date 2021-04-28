# This file will serve as a template for metadata production, results summary+ combined with
# data I have used for this study.
#=======================================
rm(list=ls())
library(tidyverse)
#=====================
sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")

#---------------------- for BioTIME -----------------------------------------------------
meta_BioTIME<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
meta_BioTIME<-meta_BioTIME%>%select(STUDY_ID,TAXA,ORGANISMS,CENT_LAT,CENT_LONG)
meta_BioTIME$STUDY_ID<-as.character(meta_BioTIME$STUDY_ID)

data_BioTIME<-sm_all%>%filter(source=="BioTIME")
data_BioTIME<-inner_join(x=data_BioTIME,y=meta_BioTIME, by=c("siteid" = "STUDY_ID"))
colnames(data_BioTIME)[28:29]<-c("Latitude","Longitude")

data_BioTIME$nyr<-NA
for(i in 1:nrow(data_BioTIME)){
  mypath<-paste("../../Results/for_BioTIME/",data_BioTIME$REALM[i],"/",data_BioTIME$siteid[i],"/",sep="")
  xx<-readRDS(paste(mypath,"input_tailanal.RDS",sep=""))
  data_BioTIME$nyr[i]<-nrow(xx)
}

#---------------------- for BioTIMEx -----------------------------------------------------
data_BioTIMEx<-sm_all%>%filter(source=="BioTIMEx")
data_BioTIMEx$TAXA<-NA
data_BioTIMEx$ORGANISMS<-NA
data_BioTIMEx$Latitude<-NA
data_BioTIMEx$Longitude<-NA

#meta_BioTIMEx<-readxl::read_excel("../../DATA/for_BioTIMEx/list of data sets.xlsx")

#carpenter_2016
data_BioTIMEx$TAXA[1]<-"Freshwater invertebrates"
data_BioTIMEx$ORGANISMS[1]<-"Zooplankton"
data_BioTIMEx$Latitude[1]<-46.5964545 # wikipeadia
data_BioTIMEx$Longitude[1]<- -95.6993638

# gross_2016
data_BioTIMEx$TAXA[2]<-"Terrestrial plants"
data_BioTIMEx$ORGANISMS[2]<-"Trees"
data_BioTIMEx$Latitude[2]<-42.405642
data_BioTIMEx$Longitude[2]<- -85.385778

#landis_2018
data_BioTIMEx$TAXA[3]<-"Terrestrial invertebrates"
data_BioTIMEx$ORGANISMS[3]<-"insects"
data_BioTIMEx$Latitude[3]<-42.405642
data_BioTIMEx$Longitude[3]<- -85.385778

#lightfoot_2015
data_BioTIMEx$TAXA[4:7]<-"Terrestrial invertebrates"
data_BioTIMEx$ORGANISMS[4:7]<-"Grasshoppers"
data_BioTIMEx$Latitude[4:7]<-34.3417
data_BioTIMEx$Longitude[4:7]<- -106.9733

#oneida_phytopl_1975
data_BioTIMEx$TAXA[8]<-"Freshwater plants"
data_BioTIMEx$ORGANISMS[8]<-"Phytoplankton"
data_BioTIMEx$Latitude[8]<-43.2046
data_BioTIMEx$Longitude[8]<- 75.9231

data_BioTIMEx$nyr<-NA
for(i in 1:nrow(data_BioTIMEx)){
  siteid<-data_BioTIMEx$siteid[i]
  if(siteid%in%c("carpenter_2016","gross_2016","oneida_phytopl_1975")){
    xx<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/",siteid,"/",siteid,"_inputmatrix_tailanal.RDS",sep=""))
  }else if(siteid=="landis_2018"){
    xx<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/landis_2018/landis_2018_inputmatrix_tailanal_poplarT5.RDS")
  }else if(siteid=="lightfoot_2015_BOER_E"){
    xx<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS")
  }else if(siteid=="lightfoot_2015_BOER_L"){
    xx<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS")
  }else if(siteid=="lightfoot_2015_LATR_E"){
    xx<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS")
  }else if(siteid=="lightfoot_2015_LATR_L"){
    xx<-readRDS("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS")
  }else{
    cat("---------- Error: missmatched name for BioTIMEx -------------- \n")
  }
  data_BioTIMEx$nyr[i]<-nrow(xx)
}

#------------ for BBS -------------------------------------------------------------------
meta_BBS<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all_metadata.RDS")
meta_BBS<-meta_BBS%>%select(Country_State_Route,Latitude,Longitude)

data_BBS<-sm_all%>%filter(source=="BBS")
data_BBS<-inner_join(x=data_BBS,y=meta_BBS, by=c("siteid" = "Country_State_Route"))
data_BBS$TAXA <-"Birds"
data_BBS$ORGANISMS <-"Birds"

data_BBS$nyr<-NA
for(i in 1:nrow(data_BBS)){
  xx<-readRDS(paste("../../DATA/for_BBS/wrangled_data/",data_BBS$siteid[i],"/input_mat_for_tailanal.RDS",sep=""))
  data_BBS$nyr[i]<-nrow(xx)
}

#------------ for RivFishTIME -------------------------------------------------------------------
meta_RF<-read.csv("../../DATA/for_RivFishTIME/wrangled_data/metadata_for_goodtimeseries.csv")
meta_RF<-meta_RF%>%select(TimeSeriesID,Latitude,Longitude)

data_RF<-sm_all%>%filter(source=="RivFishTIME")
data_RF<-inner_join(x=data_RF,y=meta_RF, by=c("siteid" = "TimeSeriesID"))
data_RF$TAXA <-"Fish"
data_RF$ORGANISMS<-"Fish"

data_RF$nyr<-NA
for(i in 1:nrow(data_RF)){
  xx<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",data_RF$siteid[i],"/commonspecies_timeseries.RDS",sep=""))
  data_RF$nyr[i]<-nrow(xx)
}
#------------ for SwissLakePhyto -------------------------------------------------------------------
data_Phyto<-sm_all%>%filter(source=="SwissLakePhyto")
data_Phyto$TAXA<-"Freshwater plants" # phytoplanktons are tagged as invertebrates in BioTIME?
data_Phyto$ORGANISMS<-"Phytoplankton"
data_Phyto$Latitude<-NA
data_Phyto$Longitude<-NA

data_Phyto$siteid

# form wikipedia
# lake Walensee
data_Phyto$Latitude[1]<-47.1233
data_Phyto$Longitude[1]<-9.2022

# lake zurich
data_Phyto$Latitude[c(2,4)]<-47.2225
data_Phyto$Longitude[c(2,4)]<-8.7527

# lake luzern
data_Phyto$Latitude[3]<-47.0136
data_Phyto$Longitude[3]<-8.4372

# lake sempach
data_Phyto$Latitude[5]<-47.1441
data_Phyto$Longitude[5]<-8.1549

# lake hallwil
data_Phyto$Latitude[6]<-47.2772
data_Phyto$Longitude[6]<-8.2173

# lake baldegg
data_Phyto$Latitude[7]<-47.2003
data_Phyto$Longitude[7]<-8.2600

#lake greifensee
data_Phyto$Latitude[8]<-47.3666
data_Phyto$Longitude[8]<-8.6795

data_Phyto$nyr<-NA
for(i in 1:nrow(data_Phyto)){
  xx<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",data_Phyto$siteid[i],".RDS",sep=""))
  data_Phyto$nyr[i]<-nrow(xx)
}

#------------ for SwissLakezoo -------------------------------------------------------------------
data_zoo<-sm_all%>%filter(source=="SwissLakeZoo")
data_zoo$TAXA<-"Freshwater invertebrates" # zooplankton
data_zoo$ORGANISMS<-"Zooplankton"
data_zoo$Latitude<-NA
data_zoo$Longitude<-NA

data_zoo$siteid

# lake zurich
data_zoo$Latitude[1]<-47.2225
data_zoo$Longitude[1]<-8.7527

# lake luzern
data_zoo$Latitude[2]<-47.0136
data_zoo$Longitude[2]<-8.4372

# lake sempach
data_zoo$Latitude[3]<-47.1441
data_zoo$Longitude[3]<-8.1549

# lake hallwil
data_zoo$Latitude[4]<-47.2772
data_zoo$Longitude[4]<-8.2173

#lake greifensee
data_zoo$Latitude[5]<-47.3666
data_zoo$Longitude[5]<-8.6795

# lake baldegg
data_zoo$Latitude[6]<-47.2003
data_zoo$Longitude[6]<-8.2600

data_zoo$nyr<-NA
for(i in 1:nrow(data_zoo)){
  if(data_zoo$siteid[i]=="LU"){
    xx<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_LU_site3A01.RDS")
  }else{
    xx<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_",data_zoo$siteid[i],".RDS",sep="")) 
  }
  data_zoo$nyr[i]<-nrow(xx)
}

#------------ for zoop2014 -------------------------------------------------------------------
data_zoo2014<-sm_all%>%filter(source=="Zooplankton2014")
data_zoo2014$TAXA<-"Freshwater invertebrates" # zooplankton
data_zoo2014$ORGANISMS<-"Zooplankton"

meta_zoo2014<-readxl::read_excel("../../DATA/for_zoop_2014/LakeNameMasterand coords.xlsx")
colnames(meta_zoo2014)[5:6]<-c("Latitude","Longitude")
meta_zoo2014<-meta_zoo2014%>%select(c(1,2,5,6))
meta_zoo2014$`LakeID to Use`[49]<-"RC"
meta_zoo2014<-meta_zoo2014%>%select(c(1,3,4))
data_zoo2014<-inner_join(x=data_zoo2014,y=meta_zoo2014, by=c("siteid"="LakeID to Use"))

data_zoo2014$nyr<-NA
for(i in 1:nrow(data_zoo2014)){
  xx<-readRDS(paste("../../DATA/for_zoop_2014/wrangled_data/",data_zoo2014$siteid[i],"/inputmat_for_tailanal.RDS",sep="")) 
  data_zoo2014$nyr[i]<-nrow(xx)
}
#========================================================================================
# Now, combine all data
df<-full_join(data_BioTIME,data_BioTIMEx)
df<-full_join(df,data_BBS)
df<-full_join(df,data_RF)
df<-full_join(df,data_Phyto)
df<-full_join(df,data_zoo)
df<-full_join(df,data_zoo2014)

df$ORGANISMS<-tolower(df$ORGANISMS)
unique(df$ORGANISMS)
df$ORGANISMS[df$ORGANISMS=="zooplanton"]<-"zooplankton"
df$ORGANISMS[df$ORGANISMS%in%c("trees ","plants","tree")]<-"trees"


write.csv(df,"../../Results/gather_res/data_summary.csv",row.names = F)

# Pie Chart with Percentages for realm
df_realm<-as.data.frame(table(df$REALM))
slices <- df_realm$Freq
lbls <- df_realm$Var1
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, border = NA, col=c("skyblue","blue","green"),main="Data for different realms")

# Pie Chart with Percentages for taxa
df_tf<-df%>%filter(REALM%in%c("Freshwater","Terrestrial"))
df_tf<-na.omit(df_tf)
df_taxa<-as.data.frame(table(df_tf$TAXA))
slices <- df_taxa$Freq
lbls <- df_taxa$Var1
pct <- round(slices/sum(slices)*100,1)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = NA, 
    col=c("red","green","skyblue","blue","darkturquoise","yellow3","purple","green4"),
    border = NA,
    main="Data for different taxa")
legend(.9, 0.9, lbls, cex = 0.7, 
       fill = c("red","green","skyblue","blue","darkturquoise","yellow3","purple","green4"),
       bty="n")

# Pie Chart with Percentages for coverage
library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=df_tf,aes(y=Latitude,x=Longitude,shape=factor(REALM),col=factor(TAXA)),alpha=0.4,size=2)+
  theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("Data: min 20 years",sep=""))
g1


df_tf$REALM<-as.character(df_tf$REALM)
boxplot(iCValt ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Stability", main = "",col=c("skyblue","green"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(phi ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Synchrony: VR", main = "",col=c("skyblue","green"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(phi_LdM ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Synchrony: VR_LdM", main = "",col=c("skyblue","green"))
