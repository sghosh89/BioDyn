# This script is for subsetted data (reducing birds), and making all plots for subsetted data
#=======================================
rm(list=ls())
library(tidyverse)
#=====================
df<-read.csv("../../Results/gather_res/data_summary.csv") # whole data summary

# read subsetted data
sm_subset<-readRDS("../../Results/gather_res/stability_metric_all_subset_birds_21.RDS") 
sm_subset<-sm_subset[,1:2]

# now join to get subsetted data summary
df_subset<-right_join(df,sm_subset,by=c("STUDY_ID"="STUDY_ID","newsite"="newsite"))

if(!dir.exists("../../Results/gather_res/datasummary_subset_birds_res")){
  dir.create("../../Results/gather_res/datasummary_subset_birds_res")
}     
write.csv(df_subset,"../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv",row.names = F)

#=====================================================================================
# doing a metadata summary table for John

df_md<-df_subset%>%select(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,CENT_LAT,CENT_LONG,nsp,nyr)
write.csv(df_md,"../../Results/gather_res/datasummary_subset_birds_res/metadata_summary_subset_birds.csv",row.names = F)

#=====================================================================================================

#========================== now plotting ============================================

#---------- first plot the geographic locations on world map -------------

# Pie Chart with Percentages for realm
df<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")

pdf("../../Results/gather_res/datasummary_subset_birds_res/data_summary_map.pdf",height=6,width=12)
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

#------------------------------------------------------
p1 <- df %>%
  ggplot( aes(x=nyr, fill=REALM)) +
  geom_histogram(alpha=0.6, position = 'dodge', binwidth = 0.8) +
  scale_fill_manual(values=c("dodgerblue","green3"))+ theme_classic()+
  theme(text=element_text(size=20)) +
  labs(fill="")
p1

p2 <- df %>%
  ggplot( aes(x=(nsp/initR)*100, fill=REALM)) +
  geom_histogram( alpha=0.6, position = 'dodge', binwidth = 2) +
  scale_fill_manual(values=c("dodgerblue","green3")) + theme_classic()+
  theme(text=element_text(size=20)) +
  labs(fill="")
p2

pdf("../../Results/gather_res/datasummary_subset_birds_res/year_percsp_hist.pdf",height=5,width=10)
gridExtra::grid.arrange(p1,p2,nrow=2)
dev.off()

#-----------------------------------------------------------------------
pdf("../../Results/gather_res/datasummary_subset_birds_res/data_summary.pdf",height=6,width=8)
op<-par(mar=c(4,5,2,2),mgp=c(3,1,0),mfrow=c(2,2),cex.lab=1.5,cex.axis=1.5)

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(iCValt ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Stability", main = "",col=c("dodgerblue","green3"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(nsp ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Species richness, R", main = "",col=c("dodgerblue","green3"))

df_tf$REALM<-as.character(df_tf$REALM)
boxplot(phi ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Variance ratio, VR", main = "",col=c("dodgerblue","green3"))


df_tf$A<-df_tf$L+abs(df_tf$U) # total asymmetry
boxplot(A ~ REALM, data = df_tf, xlab = "Realms",
        ylab = "Total asymmetry, A", main = "",col=c("dodgerblue","green3"))

par(op)
dev.off()


#=====================================================================================

#------------------ plot Synchrony vs asynchrony for both realms ------------------------------
df<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")

tempo<-df%>%mutate(f_syn=f_nL+f_nU,f_asyn=f_nneg)%>%dplyr::select(f_syn,f_asyn,REALM)
tempo2<-tempo%>%dplyr::select(f=f_syn,REALM)%>%mutate(Type="f_syn")
tempo3<-tempo%>%dplyr::select(f=f_asyn,REALM)%>%mutate(Type="f_asyn")

tempo4<-rbind(tempo2,tempo3)
gp<-ggplot(tempo4, aes(x=REALM, y=f, fill=Type)) +
  geom_boxplot()+
  scale_fill_manual(values=alpha(c("lawngreen","plum2"),0.7))+theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  ylab("Synchrony and Asynchrony")
gp

pdf("../../Results/gather_res/datasummary_subset_birds_res/syn_and_asyn_ratio_for_eachrealm.pdf",height=5,width=8)
gp
dev.off()

#------------------ plot f_nL vs f_nU for both realms ------------------------------

sm_all_2<-df%>%dplyr::select(f=f_nL,REALM)%>%mutate(Type="LT")
sm_all_3<-df%>%dplyr::select(f=f_nU,REALM)%>%mutate(Type="UT")
sm_all_4<-rbind(sm_all_2,sm_all_3)
gp<-ggplot(sm_all_4, aes(x=REALM, y=f, fill=Type)) +
  geom_boxplot()+
  scale_fill_manual(values=alpha(c("red","blue"),0.7))+theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  ylab("Fraction in synchrony")
gp

pdf("../../Results/gather_res/datasummary_subset_birds_res/f_nL_f_nU_ratio_for_eachrealm.pdf",height=5,width=8)
gp
dev.off()

sm_all_2<-df%>%dplyr::select(f=L,REALM)%>%mutate(Type="LT")
sm_all_3<-df%>%dplyr::select(f=U,REALM)%>%mutate(Type="UT")
sm_all_4<-rbind(sm_all_2,sm_all_3)
gp<-ggplot(sm_all_4, aes(x=REALM, y=f, fill=Type)) +
  geom_boxplot()+
  scale_fill_manual(values=alpha(c("red","blue"),0.7))+theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  ylab("Fraction in synchrony")
gp

pdf("../../Results/gather_res/datasummary_subset_birds_res/L_U_values_for_eachrealm.pdf",height=5,width=8)
gp
dev.off()


#------------ plot richness, variance ratio, total tail asymmetry on map ----------------

# first richness
df<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")
df$nsp<-as.numeric(df$nsp)
library(maps)
wd<-map_data("world")
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())

g1_richness<-g1+geom_point(aes(x = CENT_LONG, y = CENT_LAT, color=nsp),
                              data = df, alpha=0.3,size=0.2) + 
  scale_color_gradient(low="blue",high="red")+
  facet_wrap(~ REALM)

pdf("../../Results/gather_res/datasummary_subset_birds_res/richness_map.pdf",height=2,width=7)
g1_richness
dev.off()

# variance ratio
g1_vr<-g1+geom_point(aes(x = CENT_LONG, y = CENT_LAT, color=phi/10),
                           data = df, alpha=0.3,size=0.2) + 
  scale_color_gradient(low="blue",high="red")+
  facet_wrap(~ REALM)

pdf("../../Results/gather_res/datasummary_subset_birds_res/vr_map.pdf",height=2,width=7)
g1_vr
dev.off()

df$A<-df$L+abs(df$U)
# total tail asymmetry
g1_A<-g1+geom_point(aes(x = CENT_LONG, y = CENT_LAT, color=A),
                     data = df, alpha=0.3, size=0.2) + 
  scale_color_gradient(low="blue",high="red")+
  facet_wrap(~ REALM)

pdf("../../Results/gather_res/datasummary_subset_birds_res/tail_asymmetry_map.pdf",height=2,width=7)
g1_A
dev.off()

# stability
g1_stab<-g1+geom_point(aes(x = CENT_LONG, y = CENT_LAT, color=iCValt),
                    data = df, alpha=0.3, size=0.2) + 
  scale_color_gradient(low="blue",high="red")+
  facet_wrap(~ REALM)

pdf("../../Results/gather_res/datasummary_subset_birds_res/stability_map.pdf",height=2,width=7)
g1_stab
dev.off()

