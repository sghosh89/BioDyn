rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_swisslake/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------

# ============= for lake 1 =======================
resloc1<-"../../Results/for_swisslake/L1WA/"
if(!dir.exists(resloc1)){
  dir.create(resloc1)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L1WA.RDS")
tail_analysis(mat=mat, resloc=resloc1, nbin=2)

# =========== for lake 2 ===========================
resloc2<-"../../Results/for_swisslake/L2UZ/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L2UZ.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 3 ===========================
resloc3<-"../../Results/for_swisslake/L3LU/"
if(!dir.exists(resloc3)){
  dir.create(resloc3)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L3LU.RDS")
tail_analysis(mat=mat, resloc=resloc3, nbin=2)

# =========== for lake 4 ===========================
resloc4<-"../../Results/for_swisslake/L4LZ/"
if(!dir.exists(resloc4)){
  dir.create(resloc4)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L4LZ.RDS")
tail_analysis(mat=mat, resloc=resloc4, nbin=2)

# =========== for lake 5 ===========================
resloc5<-"../../Results/for_swisslake/L5SE/"
if(!dir.exists(resloc5)){
  dir.create(resloc5)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L5SE.RDS")
tail_analysis(mat=mat, resloc=resloc5, nbin=2)

# =========== for lake 6 ===========================
resloc6<-"../../Results/for_swisslake/L6HA/"
if(!dir.exists(resloc6)){
  dir.create(resloc6)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L6HA.RDS")
tail_analysis(mat=mat, resloc=resloc6, nbin=2)

# =========== for lake 7 ===========================
resloc7<-"../../Results/for_swisslake/L7BA/"
if(!dir.exists(resloc7)){
  dir.create(resloc7)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L7BA.RDS")
tail_analysis(mat=mat, resloc=resloc7, nbin=2)

# =========== for lake 8 ===========================
resloc8<-"../../Results/for_swisslake/L8GR/"
if(!dir.exists(resloc8)){
  dir.create(resloc8)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L8GR.RDS")
tail_analysis(mat=mat, resloc=resloc8, nbin=2)

#####################################################################################
# Now, do the summary results

resloc_list<-c(resloc1,resloc2,resloc3,resloc4,resloc5,resloc6,resloc7,resloc8)
summary_df<-c()
for(i in 1:8){
  resl<-resloc_list[i]
  df<-readRDS(paste(resl,"summary_df.RDS",sep=""))
  summary_df<-rbind(summary_df,df)
}

summary_df<-summary_df%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

df<-summary_df%>%select(nsp,f_nind,f_nL,f_nU,f_nneg)
dat<-t(df)
colnames(dat)<-c("WA","UZ","LU","LZ","SE","HA","BA","GR")
nsp<-dat[1,]
dat<-dat[-1,]

pdf(paste("../../Results/for_swisslake/summary_plot_phytoplankton.pdf",sep=""),width=10,height=5)
op<-par(mar=c(3,5,5,1))
x<-barplot(dat,width=4,border = "black", col=c("yellow", "red", "blue","green"),cex.axis = 1.5, ylim=c(0,1.4), 
        ylab="Frequency", cex.lab=2)
legend(x=0,y=1.3,horiz=T,bty="n",cex=0.8,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory"),
       fill = c("yellow","red","blue","green"))
text(x = x, y = 1, label = nsp, pos = 3, cex = 1, col = "purple")
par(op)
dev.off()


