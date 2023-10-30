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

df_md<-df_subset%>%select(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,CENT_LAT,CENT_LONG,initR,nsp,nyr,startyr,endyr)
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

############################### rainclouds plot with raw data, VR_LdM ####################################################
rm(list=ls())
library(tidyverse)
library(PupillometryR)
#---------------------------
df<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")

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
  annotate("text",  x=0.85, y = 15, label = "(a)", vjust=1.5, hjust=1.5, size=10)
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
  annotate("text",  x=0.85, y = 75, label = "(d)", vjust=1.5, hjust=1.5, size=10)
#g3
pdf("../../Results/gather_res/datasummary_subset_birds_res/rawdata_rainclouds.pdf",height=8,width=10)
gridExtra::grid.arrange(gs,g1,g2,g3,nrow=2)
dev.off()

w<-wilcox.test(phi_LdM~REALM,data=df, 
               alternative = "greater")
w
# VR_LdM shows freshwater has higher mean than terrestrial, more syn in freshwater then.

#------- similar conclusion from pairwise interaction: synchrony higher for freshwater ---------
df<-read.csv("../../Results/gather_res/datasummary_subset_birds_res/data_summary_subset_birds.csv")

tempo<-df%>%mutate(f_syn=f_nL+f_nU,f_asyn=f_nneg)%>%dplyr::select(f_syn,f_asyn,REALM)
tempo$fsyn_minus_fasyn<-tempo$f_syn - tempo$f_asyn

tempo$REALM<-as.factor(tempo$REALM)

gsma<-ggplot(data = tempo, aes(y = fsyn_minus_fasyn, x = REALM, fill = REALM)) +
  scale_fill_manual(values=alpha(c("dodgerblue","green3"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  geom_hline(yintercept = 0,linetype=2)+ 
  #coord_flip()+
  geom_point(aes(y = fsyn_minus_fasyn, color = REALM), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Synchrony-Asynchrony")+xlab("Realms")+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=20))+
  scale_color_manual(values=alpha(c("dodgerblue","green3"), 1))
tempo2<-tempo%>%select(REALM,fsyn_minus_fasyn)
# unpaired two sample wilcox test (non-parametric good for non-normal distribution)
w<-wilcox.test(fsyn_minus_fasyn~REALM,data=tempo2, 
            alternative = "greater")
w

pdf("../../Results/gather_res/datasummary_subset_birds_res/syn_minus_asyn_rainclouds.pdf",height=4,width=5)
gsma
dev.off()














