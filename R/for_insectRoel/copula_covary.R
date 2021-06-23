
# Input: 
# df = a dataframe with each target (common) species time series along column, 
#             last column having the aggregated rare species
# nbin = 2 (default) number of bins used for tail dep. analysis 

# Output:
# A matrix where each row having info from copula analysis 
# between a target sp and the rest of all aggregated sp.

source("vivj_matrix.R")
source("NonParamStat.R")
set.seed(seed=101)
copula_covary<-function(df,nbin=2){
  
  id_rare<-which(colnames(df)=="raresp")#ncol(df)
  if(length(id_rare)!=0){df<-df[,-id_rare]}
  
  dc<-df  # This matrix needs to be overwritten by the total abundance - target sp
  nsp<-ncol(dc)
  colnames(dc)<-paste("cov_",colnames(df),sep="")
  allsp<-apply(X=df,MARGIN=1,FUN=sum)
  for (i in 1:nsp){
    dc[,i]<-allsp-df[,i]
  }
  
  d_all<-cbind(dc,df) # note: the order here: first dc, then df so that 
  #                             always for -ve correlation 
  #                         target sp gets inverted later (as j index)
  year<-as.integer(rownames(dc))
  d_allsp<-vector("list",length=ncol(d_all))
  for(i in 1:length(d_allsp)){
    tempo<-data.frame(Year=year,d_all[,i])
    d_allsp[[i]]<-tempo
  }
  names(d_allsp)<-colnames(d_all)
  
  # initialize to store value
  spear<-matrix(NA,1,nsp)
  kend<-Corl<-Coru<-posnI<-posnN<-pval_BiCopIndep<-spear
  corval<-spear
  
 
  for(k in 1:nsp){
    i<-k
    j<-k+nsp
    
    ms<-vivj_matrix(d_allsp=d_allsp,i=i,j=j,level=0.05,ploton=F)
    m<-ms$mat
    
    corval[1,k]<-ms$corval
    pval_BiCopIndep[1,k]<-ms$IndepTestRes
    
    thisres<-copsync(m=m,nbin=nbin)
    
    spear[1,k]<-thisres$spear
    kend[1,k]<-thisres$kend
    
    Corl[1,k]<-thisres$Corl
    Coru[1,k]<-thisres$Coru
    
    level<-0.05
    posnIind<-which(pval_BiCopIndep>=level, arr.ind = T) #indices of indep. pair
    posnNind<-which(pval_BiCopIndep<level & corval <0, arr.ind = T) #indices of significantly neg. correlated pair
  
    posnI[posnIind]<-1
    posnN[posnNind]<-1
    
  }
  
  res<-rbind(spear,kend,
             Corl,Coru,
             posnI,
             posnN,
             corval)
  rownames(res)<-c("spear","kend","Corl","Coru","posnI","posnN","corval")
  
  return(res)
}



#df<-d$m_df # dataframe with species timeseries along column



