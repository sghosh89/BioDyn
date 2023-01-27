# This is the tail analysis with 75% threshold 
# so we chose only for communities which are longer than 45 years

yr_threshold<-41 # mean of nyr across all communities used for BioDyn study
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_RivFishTIME/"
good_TimeSeriesID_q3q4<-readRDS("../../DATA/for_RivFishTIME/wrangled_data/good_TimeSeriesID_q3q4.RDS")

#------------ Now compute and plot the tail stats ---------------------
siteid_nbin3<-c()
for(i in 1:length(good_TimeSeriesID_q3q4)){
  siteid<-good_TimeSeriesID_q3q4[i]
  resloc_input<-paste("../../DATA/for_RivFishTIME/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"commonspecies_timeseries.RDS",sep="")) # dataframe with species timeseries along column
  nyrused<-nrow(df)
  
  if(nyrused<yr_threshold){
    cat("communities with less than ", yr_threshold, " years of data, i = ", i," \n")
  }else{
    siteid_nbin3<-c(siteid_nbin3,siteid)
    resloc_output<-paste(resloc,siteid,"/","nbin3/",sep="")
    if(!dir.exists(resloc_output)){
      dir.create(resloc_output)
    }
    #----------- analysis with covary sp ----------------
    res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 3)
    cat("---------- i= ",i," routeid=",siteid," ----------\n")
  }
}

saveRDS(siteid_nbin3,"../../Results/for_RivFishTIME/siteid_nbin3.RDS")


