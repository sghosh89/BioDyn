# This file will serve as a template for metadata production, results summary+ combined with
# lat-lon data I have used for this study.
#=======================================
rm(list=ls())
library(tidyverse)
#=====================
sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")

#---------------------- for BioTIME -----------------------------------------------------
meta_BioTIME<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
meta_BioTIME<-meta_BioTIME%>%select(STUDY_ID,CENT_LAT,CENT_LONG)
meta_BioTIME$STUDY_ID<-as.character(meta_BioTIME$STUDY_ID)

data_BioTIME<-sm_all%>%filter(source=="BioTIME")
data_BioTIME<-inner_join(x=data_BioTIME,y=meta_BioTIME, by=c("STUDY_ID"))
#colnames(data_BioTIME)[28:29]<-c("Latitude","Longitude")

data_BioTIME$nyr<-NA
for(i in 1:nrow(data_BioTIME)){
  if(data_BioTIME$STUDY_ID[i]==data_BioTIME$newsite[i]){
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",data_BioTIME$REALM[i],"_plotlevel/",data_BioTIME$STUDY_ID[i],"/",sep="")
  }else{
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",data_BioTIME$REALM[i],"_plotlevel/",data_BioTIME$STUDY_ID[i],"/",data_BioTIME$newsite[i],"/",sep="")
  }
  xx<-readRDS(paste(mypath,"input_tailanal.RDS",sep=""))
  data_BioTIME$nyr[i]<-nrow(xx)
}

#---------------------- for BioTIMEx -----------------------------------------------------
data_BioTIMEx<-sm_all%>%filter(source=="BioTIMEx")
data_BioTIMEx$CENT_LAT<-NA
data_BioTIMEx$CENT_LONG<-NA

#meta_BioTIMEx<-readxl::read_excel("../../DATA/for_BioTIMEx/list of data sets.xlsx")
unique(data_BioTIMEx$STUDY_ID)

#baikal_phyto
id<-which(data_BioTIMEx$STUDY_ID=="baikal_phyto")
data_BioTIMEx$CENT_LAT[id]<-51.9033 # from alban metadata
data_BioTIMEx$CENT_LONG[id]<- 105.0706

#carpenter_2016
id<-which(data_BioTIMEx$STUDY_ID=="carpenter_2016")
data_BioTIMEx$CENT_LAT[id]<-46.5964545 # wikipeadia
data_BioTIMEx$CENT_LONG[id]<- -95.6993638

#cumbrian_phyto
id<-which(data_BioTIMEx$STUDY_ID=="cumbrian_phyto")
data_BioTIMEx$CENT_LAT[id]<-54.36119026  # from alban metadata
data_BioTIMEx$CENT_LONG[id]<- -2.964047316

#cumbrian_zoo
id<-which(data_BioTIMEx$STUDY_ID=="cumbrian_zoo")
data_BioTIMEx$CENT_LAT[id]<-54.37396341  # from alban metadata
data_BioTIMEx$CENT_LONG[id]<- -2.953536754

# gross_2016
id<-which(data_BioTIMEx$STUDY_ID=="gross_2016")
data_BioTIMEx$CENT_LAT[id]<-42.405642
data_BioTIMEx$CENT_LONG[id]<- -85.385778

#landis_2018
id<-which(data_BioTIMEx$STUDY_ID=="landis_2018")
data_BioTIMEx$CENT_LAT[id]<-42.405642
data_BioTIMEx$CENT_LONG[id]<- -85.385778

#lightfoot_2015
id<-which(data_BioTIMEx$STUDY_ID=="lightfoot_2015")
data_BioTIMEx$CENT_LAT[id]<-34.3417
data_BioTIMEx$CENT_LONG[id]<- -106.9733

#oneida_fish_gillnets
id<-which(data_BioTIMEx$STUDY_ID=="oneida_fish_gillnets")
data_BioTIMEx$CENT_LAT[id]<-43.20204266
data_BioTIMEx$CENT_LONG[id]<- -75.92089251

#oneida_fish_trawl
id<-which(data_BioTIMEx$STUDY_ID=="oneida_fish_trawl")
data_BioTIMEx$CENT_LAT[id]<-43.2046
data_BioTIMEx$CENT_LONG[id]<- -75.9231

#oneida_phytopl_1975
id<-which(data_BioTIMEx$STUDY_ID=="oneida_phytopl_1975")
data_BioTIMEx$CENT_LAT[id]<-43.2046
data_BioTIMEx$CENT_LONG[id]<- -75.9231

data_BioTIMEx$nyr<-NA
zz<-readRDS("../../Results/for_BioTIMEx/inputloc_table.RDS")
zz<-inner_join(data_BioTIMEx,zz,by=c("STUDY_ID","newsite"))

for(i in 1:nrow(zz)){
  xx<-readRDS(zz$inputloc[i])
  data_BioTIMEx$nyr[i]<-nrow(xx)
}

#------------ for BBS -------------------------------------------------------------------
meta_BBS<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all_metadata.RDS")
meta_BBS<-meta_BBS%>%select(Country_State_Route,CENT_LAT=Latitude,CENT_LONG=Longitude)

data_BBS<-sm_all%>%filter(source=="BBS")
data_BBS<-inner_join(x=data_BBS,y=meta_BBS, by=c("newsite" = "Country_State_Route"))

data_BBS$nyr<-NA
for(i in 1:nrow(data_BBS)){
  mypath<-paste("../../DATA/for_BBS/wrangled_data/",data_BBS$newsite[i],"/",sep="")
  xx<-readRDS(paste(mypath,"input_mat_for_tailanal.RDS",sep=""))
  data_BBS$nyr[i]<-nrow(xx)
}

#------------ for RivFishTIME -------------------------------------------------------------------
meta_RF<-read.csv("../../DATA/for_RivFishTIME/wrangled_data/metadata_for_goodtimeseries.csv")
meta_RF<-meta_RF%>%select(TimeSeriesID,CENT_LAT=Latitude,CENT_LONG=Longitude)

data_RF<-sm_all%>%filter(source=="RivFishTIME")
data_RF<-inner_join(x=data_RF,y=meta_RF, by=c("newsite" = "TimeSeriesID"))
data_RF$nyr<-NA
for(i in 1:nrow(data_RF)){
  xx<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",data_RF$newsite[i],"/commonspecies_timeseries.RDS",sep=""))
  data_RF$nyr[i]<-nrow(xx)
}
#------------ for SwissLakePhyto -------------------------------------------------------------------
data_Phyto<-sm_all%>%filter(source=="SwissLakePhyto")
data_Phyto$CENT_LAT<-NA
data_Phyto$CENT_LONG<-NA

data_Phyto$newsite

# form wikipedia
# lake walensee
data_Phyto$CENT_LAT[1]<-47.1233
data_Phyto$CENT_LONG[1]<-9.2022

# lake zurich
data_Phyto$CENT_LAT[c(2,4)]<-47.2225
data_Phyto$CENT_LONG[c(2,4)]<-8.7527

# lake luzern
data_Phyto$CENT_LAT[3]<-47.0136
data_Phyto$CENT_LONG[3]<-8.4372

# lake sempach
data_Phyto$CENT_LAT[5]<-47.1441
data_Phyto$CENT_LONG[5]<-8.1549

# lake hallwil
data_Phyto$CENT_LAT[6]<-47.2772
data_Phyto$CENT_LONG[6]<-8.2173

# lake baldegg
data_Phyto$CENT_LAT[7]<-47.2003
data_Phyto$CENT_LONG[7]<-8.2600

#lake greifensee
data_Phyto$CENT_LAT[8]<-47.3666
data_Phyto$CENT_LONG[8]<-8.6795

data_Phyto$nyr<-NA
for(i in 1:nrow(data_Phyto)){
  xx<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",data_Phyto$newsite[i],".RDS",sep=""))
  data_Phyto$nyr[i]<-nrow(xx)
}

#------------ for SwissLakezoo -------------------------------------------------------------------
data_zoo<-sm_all%>%filter(source=="SwissLakeZoo")
data_zoo$TAXA<-"Freshwater invertebrates" # zooplankton
data_zoo$ORGANISMS<-"Zooplankton"
data_zoo$CENT_LAT<-NA
data_zoo$CENT_LONG<-NA

data_zoo$newsite

# lake zurich
data_zoo$CENT_LAT[1]<-47.2225
data_zoo$CENT_LONG[1]<-8.7527

# lake luzern
data_zoo$CENT_LAT[2]<-47.0136
data_zoo$CENT_LONG[2]<-8.4372

# lake sempach
data_zoo$CENT_LAT[3]<-47.1441
data_zoo$CENT_LONG[3]<-8.1549

# lake hallwil
data_zoo$CENT_LAT[4]<-47.2772
data_zoo$CENT_LONG[4]<-8.2173

#lake greifensee
data_zoo$CENT_LAT[5]<-47.3666
data_zoo$CENT_LONG[5]<-8.6795

# lake baldegg
data_zoo$CENT_LAT[6]<-47.2003
data_zoo$CENT_LONG[6]<-8.2600

data_zoo$nyr<-NA
for(i in 1:nrow(data_zoo)){
  if(data_zoo$newsite[i]=="LU"){
    xx<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_LU_site3A01.RDS")
  }else{
    xx<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_",data_zoo$newsite[i],".RDS",sep="")) 
  }
  data_zoo$nyr[i]<-nrow(xx)
}

#------------ for zoop2014 -------------------------------------------------------------------
data_zoo2014<-sm_all%>%filter(source=="Zooplankton2014")

meta_zoo2014<-readxl::read_excel("../../DATA/for_zoop_2014/LakeNameMasterand coords.xlsx")
colnames(meta_zoo2014)[5:6]<-c("CENT_LAT","CENT_LONG")
meta_zoo2014<-meta_zoo2014%>%select(c(1,2,5,6))
meta_zoo2014$`LakeID to Use`[49]<-"RC"
meta_zoo2014<-meta_zoo2014%>%select(c(1,3,4))
data_zoo2014<-inner_join(x=data_zoo2014,y=meta_zoo2014, by=c("newsite"="LakeID to Use"))

data_zoo2014$nyr<-NA
for(i in 1:nrow(data_zoo2014)){
  xx<-readRDS(paste("../../DATA/for_zoop_2014/wrangled_data/",data_zoo2014$newsite[i],"/inputmat_for_tailanal.RDS",sep="")) 
  data_zoo2014$nyr[i]<-nrow(xx)
}

#------------ for insectRoel -------------------------------------------------------------------
data_insect<-sm_all%>%filter(source=="InsectRoel")
data_insect$nyr<-NA
for(i in 1:nrow(data_insect)){
  xx<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",data_insect$STUDY_ID[i],
                    "/",data_insect$newsite[i],"/inputmat_for_tailanal.RDS",sep="")) 
  data_insect$nyr[i]<-nrow(xx)
}

meta_insect<-read.csv("../../DATA/for_insectRoel/20yrFreshwater_Metadata.csv")
meta_insect<-meta_insect%>%select(Plot_ID,CENT_LAT=Latitude,CENT_LONG=Longitude)
meta_insect$Plot_ID<-as.character(meta_insect$Plot_ID)
data_insect<-inner_join(data_insect,meta_insect,by=c("newsite"="Plot_ID"))
#========================================================================================

# Now, combine all data
df<-full_join(data_BioTIME,data_BioTIMEx)
df<-full_join(df,data_BBS)
df<-full_join(df,data_RF)
df<-full_join(df,data_Phyto)
df<-full_join(df,data_zoo)
df<-full_join(df,data_zoo2014)
df<-full_join(df,data_insect)

unique(df$TAXA)
df$TAXA<-tolower(df$TAXA)
#df$ORGANISMS[df$ORGANISMS=="zooplanton"]<-"zooplankton"
#df$ORGANISMS[df$ORGANISMS%in%c("trees","plants","plant")]<-"plants"

write.csv(df,"../../Results/gather_res/data_summary.csv",row.names = F)

# Pie Chart with Percentages for realm
df<-read.csv("../../Results/gather_res/data_summary.csv")

pdf("../../Results/gather_res/data_summary_map.pdf",height=6,width=12)
op<-par(mar=c(4,5,2,2),mgp=c(3,1,0),cex.lab=1.5,cex.axis=1.5)

df_realm<-as.data.frame(table(df$REALM))
slices <- df_realm$Freq
lbls <- df_realm$Var1
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, border = NA, col=c("dodgerblue","green3"),main="Data for different realms")

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
    col=c("green3","royalblue","skyblue","blue","seagreen1","olivedrab2","yellowgreen"),
    border = NA,
    main="Data for different taxa")
legend("bottomleft", lbls, cex = 0.7, 
       fill = c("green3","royalblue","skyblue","blue","seagreen1","olivedrab2","yellowgreen"),
       bty="n")

# Pie Chart with Percentages for coverage
myColors<- c("green3","royalblue","skyblue","blue","seagreen1","olivedrab2","yellowgreen")
library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=df_tf,aes(y=CENT_LAT,x=CENT_LONG,shape=factor(REALM),col=factor(TAXA)),size=2)+
  scale_color_manual(values=alpha(myColors, 1))+
  theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("Data: min 20 years",sep=""))
g1

par(op)
dev.off()


pdf("../../Results/gather_res/data_summary.pdf",height=6,width=12)
op<-par(mar=c(4,5,2,2),mgp=c(3,1,0),mfrow=c(2,3),cex.lab=1.5,cex.axis=1.5)

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(iCValt ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Stability", main = "",col=c("dodgerblue","green3"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(nsp ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Species richness", main = "",col=c("dodgerblue","green3"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(phi ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Variance ratio", main = "",col=c("dodgerblue","green3"))

#df_tf$REALM<-as.character(df_tf$REALM)
#boxplot(phi_LdM ~ REALM, data = df_tf, xlab = "Realms",
#        ylab = "Synchrony: VR_LdM", main = "",col=c("dodgerblue","green3"))

df_tf$A<-df_tf$f_nL+df_tf$f_nU # total asymmetry
boxplot(A ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Total asymmetry", main = "",col=c("dodgerblue","green3"))

df_tf$uniA<-df_tf$f_nL-df_tf$f_nU # net asymmetry
boxplot(uniA ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Net asymmetry", main = "",col=c("dodgerblue","green3"))

boxplot(phi_skw ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Skewness Ratio", main = "",col=c("dodgerblue","green3"))

par(op)
dev.off()


#=====================================================================================
# doing a metadata summary table for John

df<-read.csv("../../Results/gather_res/data_summary.csv")
df_md<-df%>%select(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,CENT_LAT,CENT_LONG,nsp,nyr)
write.csv(df_md,"../../Results/gather_res/metadata_summary.csv",row.names = F)








