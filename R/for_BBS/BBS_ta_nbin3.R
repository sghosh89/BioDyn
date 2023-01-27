# This is the tail analysis with 75% threshold 
# so we chose only for communities which are longer than 45 years

yr_threshold<-41 # mean of nyr across all communities used for BioDyn study
resloc<-"../../Results/for_BBS/"
uroutes<-readRDS("../../DATA/for_BBS/wrangled_data/unique_routes_all.RDS")
uroutes<-data.frame(Country_State_Route=uroutes)

#------------ Now compute and plot the tail stats ---------------------
siteid_nbin3<-c()
for(i in 1:nrow(uroutes)){
  siteid<-uroutes$Country_State_Route[i]
  resloc_input<-paste("../../DATA/for_BBS/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"input_mat_for_tailanal.RDS",sep="")) # dataframe with species timeseries along column
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






