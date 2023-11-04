rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_BBS/res_with_stoc/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

uroutes<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all_with_stoc.RDS")
uroutes<-data.frame(Country_State_Route=uroutes)
x_meta<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/routes.csv")
x_meta<-x_meta%>%unite("Country_State_Route",CountryNum,StateNum,Route,sep="_")
metadata<-inner_join(uroutes,x_meta,by="Country_State_Route")%>%
  rename(Stratum_code=Stratum)

bbs_strata1<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/BBS_physiographic_strataname.csv")
bbs_strata1<-bbs_strata1%>%dplyr::select(Stratum_code=Stratum,Stratum_name=Name,Stratum_area=Area.Km2)
#bbs_strata2<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/BBS_physiographic_strataname_statemap.csv")
metadata<-inner_join(metadata,bbs_strata1,by="Stratum_code")
#saveRDS(metadata,"../../DATA/for_BBS/wrangled_data/unique_routes_all_metadata.RDS")

#=================== create results folder for each study sites/routes ==================

for(i in 1:nrow(uroutes)){
  k<-paste(resloc,uroutes$Country_State_Route[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}
#------------ Now compute and plot the tail stats ---------------------

for(i in 1:nrow(uroutes)){
  siteid<-uroutes$Country_State_Route[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../../DATA/for_BBS/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"input_mat_for_tailanal_with_stoc.RDS",sep="")) # dataframe with species timeseries along column
  
  #----------- analysis with covary sp ----------------
  res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 2)
  cat("---------- i= ",i," routeid=",siteid," ----------\n")
}

#--------------- Do a summary stats for all routes ------------------
summary_table<-c()
for(i in 1:nrow(uroutes)){
  siteid<-uroutes$Country_State_Route[i]
  resloc_input<-paste(resloc,siteid,"/",sep="")
  x<-readRDS(paste(resloc_input,"summary_df.RDS",sep=""))
  summary_table<-rbind(summary_table,x)
}
summary_table<-cbind(siteid=uroutes$Country_State_Route,summary_table)

summary_table<-inner_join(summary_table,metadata,by=c("siteid"="Country_State_Route"))
saveRDS(summary_table,"../../Results/for_BBS/res_with_stoc/summary_table_detail_version.RDS")

##########################################
rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_BBS<-readRDS("../../Results/for_BBS/res_with_stoc/summary_table_detail_version.RDS")
r_BBS$ens<-NA # effective number of species
r_BBS$cvsq_real<-NA # square of CV for the real community data
r_BBS$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_BBS$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_BBS$phi_LdM<-NA # Loreau's variance ratio
r_BBS$skw_real <-NA # skewness for the real community data
r_BBS$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_BBS$phi_skw<-NA # skewness ratio
r_BBS$iCV<-NA # inverse of CV: stability metric
r_BBS$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

for(i in 1:nrow(r_BBS)){
  mypath<-paste("../../DATA/for_BBS/wrangled_data/",r_BBS$siteid[i],"/",sep="")
  m<-readRDS(paste(mypath,"input_mat_for_tailanal_with_stoc.RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_BBS$ens[i]<-df$ens
  r_BBS$cvsq_real[i]<-df$cvsq_real
  r_BBS$cvsq_indep[i]<-df$cvsq_indep
  r_BBS$phi[i]<-df$phi
  r_BBS$phi_LdM[i]<-df$phi_LdM
  r_BBS$skw_real[i]<-df$skw_real
  r_BBS$skw_indep[i]<-df$skw_indep
  r_BBS$phi_skw[i]<-df$phi_skw
  r_BBS$iCV[i]<-df$iCV
  r_BBS$iCValt[i]<-df$iCValt
}
r_BBS$REALM<-as.factor("Terrestrial")
saveRDS(r_BBS,"../../Results/for_BBS/res_with_stoc/stability_metric.RDS")




