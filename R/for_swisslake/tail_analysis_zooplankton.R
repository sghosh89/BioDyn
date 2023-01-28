rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_swisslake/zooplankton/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------

# ============= for lake Zurich =======================
resloc1<-"../../Results/for_swisslake/zooplankton/zoo_ZH/"
if(!dir.exists(resloc1)){
  dir.create(resloc1)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_ZH.RDS")
tail_analysis(mat=mat, resloc=resloc1, nbin=2)

# ============= lake vierwaldst?ttersee/ lake lucerne =======================
resloc2<-"../../Results/for_swisslake/zooplankton/zoo_LU/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_LU_site3A01.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# ============= for lake sempachersee =======================
resloc3<-"../../Results/for_swisslake/zooplankton/zoo_SEM/"
if(!dir.exists(resloc3)){
  dir.create(resloc3)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_SEM.RDS")
tail_analysis(mat=mat, resloc=resloc3, nbin=2)

# ============= for lake  hallwilersee =======================
resloc4<-"../../Results/for_swisslake/zooplankton/zoo_HAL/"
if(!dir.exists(resloc4)){
  dir.create(resloc4)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_HAL.RDS")
tail_analysis(mat=mat, resloc=resloc4, nbin=2)

# ============= for lake  greifensee =======================
resloc5<-"../../Results/for_swisslake/zooplankton/zoo_GRE/"
if(!dir.exists(resloc5)){
  dir.create(resloc5)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_GRE.RDS")
tail_analysis(mat=mat, resloc=resloc5, nbin=2)

# ============= for lake  baldeggersee =======================
resloc6<-"../../Results/for_swisslake/zooplankton/zoo_BAL/"
if(!dir.exists(resloc6)){
  dir.create(resloc6)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_BAL.RDS")
tail_analysis(mat=mat, resloc=resloc6, nbin=2)

#####################################################################################
# Now, do the summary results

resloc_list<-c(resloc1,resloc2,resloc3,resloc4,resloc5,resloc6)
summary_df<-c()
for(i in 1:6){
  resl<-resloc_list[i]
  df<-readRDS(paste(resl,"summary_df.RDS",sep=""))
  summary_df<-rbind(summary_df,df)
}
summary_df<-summary_df%>%mutate(f_nind=nind/nint,
                                f_npos=npos/nint,
                                f_nL=nL/nint,
                                f_nU=nU/nint,
                                f_nneg=nneg/nint)
summary_df$siteid<-c("ZH","LU","SEM","HAL","GRE","BAL")
summary_df$initR<-NA
for(i in 1:nrow(summary_df)){
  bigM<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/allspmat_zoo_",summary_df$siteid[i],".RDS",sep=""))
  summary_df$initR[i]<-ncol(bigM)
}

# to get pairwise Spearman correlation
summary_df$tot_spear_sig<-NA # sum of all significant positive and negative correlation

for(i in 1:nrow(summary_df)){
  nsp<-summary_df$nsp[i]
  resloc<-resloc_list[i]
  resloc_input<-paste(resloc,"/",sep="")
  x<-readRDS(paste(resloc_input,"NonParamStat.RDS",sep=""))
  spx<-x$spear
  
  posnn<-x$posn_notneeded
  #posN_ind<-which(x$posnN==1, arr.ind = T)
  posI_ind<-which(x$posnI==1, arr.ind = T)
  
  spx[posI_ind]<-NA # only exclude indep. interaction
  spx[posnn]<-NA
  
  spx<-spx[1:nsp,1:nsp]
  
  summary_df$tot_spear_sig[i]<-sum(spx, na.rm=T) # you have to normalize it by dividing with nsp*(nsp-1)/2
}
saveRDS(summary_df,"../../Results/for_swisslake/summary_table_zooplankton.RDS")

df<-summary_df%>%select(nsp,f_nind,f_nL,f_nU,f_nneg)
dat<-t(df)
colnames(dat)<-summary_df$siteid
nsp<-dat[1,]
dat<-dat[-1,]

pdf(paste("../../Results/for_swisslake/zooplankton/summary_plot_zooplankton.pdf",sep=""),width=10,height=5)
op<-par(mar=c(3,5,5,1))
x<-barplot(dat,width=4,border = "black", col=c("yellow", "red", "skyblue","green"),cex.axis = 1.5, ylim=c(0,1.4), 
           ylab="Frequency", cex.lab=2)
legend(x=0,y=1.3,horiz=T,bty="n",cex=0.8,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory"),
       fill = c("yellow","red","skyblue","green"))
text(x = x, y = 1, label = nsp, pos = 3, cex = 1, col = "purple")
par(op)
dev.off()

#################################################################################################

my_summary_boxplot<-function(summary_table,nametag,myresloc){
  
  # for how many sites +ve corr were dominant?
  nP<-sum((summary_table$f_nL+summary_table$f_nU)>summary_table$f_nneg)
  shorttab<-summary_table[which(summary_table$f_nL+summary_table$f_nU>summary_table$f_nneg),]
  
  # out of those sites: for how many sites LT asymmetry were dominant?
  nLT<-sum(shorttab$f_nL>shorttab$f_nU)
  
  # for how many sites UT asymmetry were dominant?
  nUT<-sum(shorttab$f_nL<shorttab$f_nU)
  
  # for how many sites no asymmetry were dominant?
  nSym<-sum(shorttab$f_nL==shorttab$f_nU)
  
  # for how many sites -ve corr were dominant?
  nC<-sum((summary_table$f_nL+summary_table$f_nU)<summary_table$f_nneg)
  
  # for how many sites syn==comp?
  nEqSynComp<-sum((summary_table$f_nL+summary_table$f_nU)==summary_table$f_nneg)
  
  z<-summary_table%>%select(f_nind,f_nL,f_nU,f_nneg)
  colnames(z)<-c("Independent","Synchrony(rare)","Synchrony(abundant)","Compensatory")
  y <- gather(z, Pairwise.Interaction, Frequency) 
  boxplot(Frequency~Pairwise.Interaction,y,ylim=c(0,1),
          col=c("green","yellow","skyblue","red"),
          main=paste(nametag,", #sites: ",nrow(z),", #sites(more syn.): ",nP,", #sites(more comp.): ",nC))
  
  dtable<-data.frame(nSyn=nP,nLT=nLT,nUT=nUT,nSym=nSym,nComp=nC,nEqSynComp=nEqSynComp)
  rownames(dtable)<-nametag
  print(dtable)
  saveRDS(dtable,paste(myresloc,"summary_dtable_from_boxplot_",str_replace(nametag,"/","_"),".RDS",sep=""))
}
###################################################################################################

pdf("../../Results/for_swisslake/zooplankton/summary_boxplot_zooplankton.pdf",width=14,height=6)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
my_summary_boxplot(summary_table = summary_df,nametag = "Swisslakes' zooplankton",myresloc = "../../Results/for_swisslake/zooplankton/")
par(op)
dev.off()











