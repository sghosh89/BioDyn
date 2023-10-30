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
data_BioTIME$startyr<-NA
data_BioTIME$endyr<-NA
for(i in 1:nrow(data_BioTIME)){
  if(data_BioTIME$STUDY_ID[i]==data_BioTIME$newsite[i]){
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",data_BioTIME$REALM[i],"_plotlevel/",data_BioTIME$STUDY_ID[i],"/",sep="")
  }else{
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",data_BioTIME$REALM[i],"_plotlevel/",data_BioTIME$STUDY_ID[i],"/",data_BioTIME$newsite[i],"/",sep="")
  }
  xx<-readRDS(paste(mypath,"input_tailanal.RDS",sep=""))
  data_BioTIME$nyr[i]<-nrow(xx)
  data_BioTIME$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_BioTIME$endyr[i]<-as.integer(tail(rownames(xx),1)) 
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
data_BioTIMEx$startyr<-NA
data_BioTIMEx$endyr<-NA
zz<-readRDS("../../Results/for_BioTIMEx/inputloc_table.RDS")
zz<-inner_join(data_BioTIMEx,zz,by=c("STUDY_ID","newsite"))

for(i in 1:nrow(zz)){
  xx<-readRDS(zz$inputloc[i])
  data_BioTIMEx$nyr[i]<-nrow(xx)
  data_BioTIMEx$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_BioTIMEx$endyr[i]<-as.integer(tail(rownames(xx),1)) 
}

#------------ for BBS -------------------------------------------------------------------
meta_BBS<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all_metadata.RDS")
meta_BBS<-meta_BBS%>%select(Country_State_Route,CENT_LAT=Latitude,CENT_LONG=Longitude)

data_BBS<-sm_all%>%filter(source=="BBS")
data_BBS<-inner_join(x=data_BBS,y=meta_BBS, by=c("newsite" = "Country_State_Route"))

data_BBS$nyr<-NA
data_BBS$startyr<-NA
data_BBS$endyr<-NA
for(i in 1:nrow(data_BBS)){
  mypath<-paste("../../DATA/for_BBS/wrangled_data/",data_BBS$newsite[i],"/",sep="")
  xx<-readRDS(paste(mypath,"input_mat_for_tailanal.RDS",sep=""))
  data_BBS$nyr[i]<-nrow(xx)
  data_BBS$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_BBS$endyr[i]<-as.integer(tail(rownames(xx),1)) 
}

#------------ for RivFishTIME -------------------------------------------------------------------
meta_RF<-read.csv("../../DATA/for_RivFishTIME/wrangled_data/metadata_for_goodtimeseries.csv")
meta_RF<-meta_RF%>%select(TimeSeriesID,CENT_LAT=Latitude,CENT_LONG=Longitude)

data_RF<-sm_all%>%filter(source=="RivFishTIME")
data_RF<-inner_join(x=data_RF,y=meta_RF, by=c("newsite" = "TimeSeriesID"))
data_RF$nyr<-NA
data_RF$startyr<-NA
data_RF$endyr<-NA
for(i in 1:nrow(data_RF)){
  xx<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",data_RF$newsite[i],"/commonspecies_timeseries.RDS",sep=""))
  data_RF$nyr[i]<-nrow(xx)
  data_RF$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_RF$endyr[i]<-as.integer(tail(rownames(xx),1)) 
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
data_Phyto$startyr<-NA
data_Phyto$endyr<-NA
for(i in 1:nrow(data_Phyto)){
  xx<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",data_Phyto$newsite[i],".RDS",sep=""))
  data_Phyto$nyr[i]<-nrow(xx)
  data_Phyto$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_Phyto$endyr[i]<-as.integer(tail(rownames(xx),1))
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
data_zoo$startyr<-NA
data_zoo$endyr<-NA
for(i in 1:nrow(data_zoo)){
  if(data_zoo$newsite[i]=="LU"){
    xx<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_LU_site3A01.RDS")
  }else{
    xx<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_",data_zoo$newsite[i],".RDS",sep="")) 
  }
  data_zoo$nyr[i]<-nrow(xx)
  data_zoo$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_zoo$endyr[i]<-as.integer(tail(rownames(xx),1))
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
data_zoo2014$startyr<-NA
data_zoo2014$endyr<-NA
for(i in 1:nrow(data_zoo2014)){
  xx<-readRDS(paste("../../DATA/for_zoop_2014/wrangled_data/",data_zoo2014$newsite[i],"/inputmat_for_tailanal.RDS",sep="")) 
  data_zoo2014$nyr[i]<-nrow(xx)
  data_zoo2014$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_zoo2014$endyr[i]<-as.integer(tail(rownames(xx),1))
}

#------------ for insectRoel -------------------------------------------------------------------
data_insect<-sm_all%>%filter(source=="InsectRoel")
data_insect$nyr<-NA
data_insect$startyr<-NA
data_insect$endyr<-NA
for(i in 1:nrow(data_insect)){
  xx<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",data_insect$STUDY_ID[i],
                    "/",data_insect$newsite[i],"/inputmat_for_tailanal.RDS",sep="")) 
  data_insect$nyr[i]<-nrow(xx)
  data_insect$startyr[i]<-as.integer(head(rownames(xx),1)) 
  data_insect$endyr[i]<-as.integer(tail(rownames(xx),1))
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

#---------------------------------------------------------------------------------------------
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
op<-par(mar=c(4,5,2,2),mgp=c(3,1,0),mfrow=c(2,2),cex.lab=1.5,cex.axis=1.5)

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(iCValt ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Stability", main = "",col=c("dodgerblue","green3"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(nsp ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Species richness, R", main = "",col=c("dodgerblue","green3"))

#df_tf$REALM<-as.character(df_tf$REALM)
#boxplot(phi ~ REALM, data = df_tf, xlab = "Realms",
#        ylab = "Variance ratio, VR", main = "",col=c("dodgerblue","green3"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(phi_LdM ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Synchrony: VR_LdM", main = "",col=c("dodgerblue","green3"))

#df_tf$A<-df_tf$f_nL+df_tf$f_nU # total asymmetry
df_tf$A<-df_tf$L+abs(df_tf$U)
boxplot(A ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Total asymmetry, A", main = "",col=c("dodgerblue","green3"))

#df_tf$uniA<-df_tf$f_nL-df_tf$f_nU # net asymmetry
#boxplot(uniA ~ REALM, data = df_tf, xlab = "Realms",
#        ylab = "Net asymmetry, uniA", main = "",col=c("dodgerblue","green3"))

#boxplot(phi_skw ~ REALM, data = df_tf, xlab = "Realms",
#        ylab = "Skewness Ratio, SR", main = "",col=c("dodgerblue","green3"))

par(op)
dev.off()


#=====================================================================================
# doing a metadata summary table for John

df<-read.csv("../../Results/gather_res/data_summary.csv")
df_md<-df%>%select(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,CENT_LAT,CENT_LONG,initR,nsp,nyr,startyr,endyr)
write.csv(df_md,"../../Results/gather_res/metadata_summary.csv",row.names = F)

#=====================================================================================================
#rm(list=ls())
#library(tidyverse)
#df_md<-read.csv("../../Results/gather_res/metadata_summary.csv")
#dfc<-read.csv("../../DATA/metadata_summary_with_citation_old.csv")
#dfc<-dfc%>%dplyr::select(source,STUDY_ID,newsite,
#                         citation_1,citation_2,
#                         citation_3,citation_4,
#                         citation_5,citation_6,
#                         citation_7,citation_8)
#dfnew<-left_join(df_md,dfc,by=c("source"="source",
#                                "STUDY_ID"="STUDY_ID",
#                                "newsite"="newsite"))
#write.csv(dfnew,"../../DATA/metadata_summary_with_citation.csv",row.names = F)

# now get how many STUDY_ID
tb<-df_md%>%group_by(source)%>%summarize(n=n_distinct(STUDY_ID))%>%ungroup()
sum(tb$n)# 242 unique STUDY_ID
tb<-df_md%>%group_by(source)%>%summarize(n=unique(STUDY_ID))%>%ungroup()

#################################################################
rm(list=ls())
library(tidyverse)
library(PupillometryR)
#---------------------------
df<-read.csv("../../Results/gather_res/data_summary.csv") # whole data summary

# stability
gs<-ggplot(data = df, aes(y = iCV, x = REALM, fill = REALM)) +
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = iCV, color = REALM), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Stability")+xlab("Realms")+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=20))+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1))+ 
  annotate("text",  x=0.85, y = 20, label = "(a)", vjust=1.5, hjust=1.5, size=10)
#gs
# richness
g1<-ggplot(data = df, aes(y = nsp, x = REALM, fill = REALM)) +
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = nsp, color = REALM), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+
  ylab("Richness")+xlab("Realms")+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=20))+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1))+ 
  annotate("text",  x=0.85, y = 90, label = "(b)", vjust=1.5, hjust=1.5, size=10)
#g1
# variance ratio
g2<-ggplot(data = df, aes(y = phi_LdM, x = REALM, fill = REALM)) +
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #geom_hline(yintercept = 1,linetype=2)+ 
  #coord_flip()+
  geom_point(aes(y = phi_LdM, color = REALM), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Overall synchrony \n (LM synchrony metric)")+xlab("Realms")+ 
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=20))+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1))+ 
  annotate("text",  x=0.85, y = 1, label = "(c)", vjust=1.5, hjust=1.5, size=10)
#g2

# total asymmetry
df$A<-df$L+abs(df$U) 
g3<-ggplot(data = df, aes(y = A, x = REALM, fill = REALM)) +
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1))+
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = A, color = REALM), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Tail-dependent synchrony \n (Total tail asymmetry)")+xlab("Realms")+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=20))+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1))+ 
  annotate("text",  x=0.85, y = 140, label = "(d)", vjust=1.5, hjust=1.5, size=10)
#g3
pdf("../../Results/gather_res/rawdata2668_rainclouds.pdf",height=8,width=10)
gridExtra::grid.arrange(gs,g1,g2,g3,nrow=2)
dev.off()
