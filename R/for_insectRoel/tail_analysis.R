# This script will do the copula analysis, will generate necessary plots, will show how a species 
# tail dep. fluctuates with the rest of species in the phyto-plankton community

source("NonParamStat.R")
source("copula_covary.R")
source("NonParamStat_matrixplot.R")

# Input:
# mat: a matrix or dataframe where each target species time series along each column, 
#         last column for all rest of the aggregated species, rows have name for sampling years
#resloc: path to save results
#nbin: 2 (default) to measure tail-dep.

# Output:
# a list and several plots to be saved in resloc path

tail_analysis<-function(mat, resloc, nbin=2){
  
  splist<-vector(mode="list",length=ncol(mat))
  names(splist)<-colnames(mat)
  for(i in 1:length(splist)){
    splist[[i]]<-data.frame(Year=rownames(mat),Dat=mat[,i])
  }
  
  z<-multcall(d_allsp = splist, resloc = resloc, nbin=nbin)
  zcov<-copula_covary(df=mat,nbin=2)
  
  if(tail(colnames(mat),1)=="raresp"){
    tot_target_sp<-ncol(mat)-1 # last column as aggregated rest of the species
  }else{
    tot_target_sp<-ncol(mat)
  }
  
  #----- now combine the results ------------
  
  # for spearman
  zs<-z$spear
  
  tempo<-zs
  indI<-z$posnI
  indI<-which(indI==1,arr.ind = T)
  tempo[indI]<-NA
  diag(tempo)<-NA
  
  if(all(is.na(tempo))==T){
  # all sp are indep to each other
    nsp<-tot_target_sp
    nint<-nsp*(nsp-1)/2
    summary_df<-data.frame(nsp=tot_target_sp,nint=nint,nind=nint,npos=0,nL=0,nU=0,nneg=0,L=0,U=0)
    saveRDS(summary_df,paste(resloc,"summary_df.RDS",sep=""))
    return()
  }
  
  tsp<-ncol(zs)-ncol(zcov)
  tempo<-matrix(0.999,nrow=nrow(zcov),ncol=tsp) # 0.999 value will be filled in with black color in the plot
  zcov<-cbind(zcov,tempo)
  zs<-rbind(zs,zcov[1,])
  zs<-cbind(zs,matrix(0.999,nrow=nrow(zs),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zs)[nrow(zs)]<-"covsp"
  
  # for kend
  zk<-z$kend
  zk<-rbind(zk,zcov[2,])
  zk<-cbind(zk,matrix(0.999,nrow=nrow(zk),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zk)[nrow(zk)]<-"covsp"
  
  # for Corl
  zcl<-z$Corl
  zcl<-rbind(zcl,zcov[3,])
  zcl<-cbind(zcl,matrix(0.999,nrow=nrow(zcl),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zcl)[nrow(zcl)]<-"covsp"
  
  # for Coru
  zcu<-z$Coru
  zcu<-rbind(zcu,zcov[4,])
  zcu<-cbind(zcu,matrix(0.999,nrow=nrow(zcu),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zcu)[nrow(zcu)]<-"covsp"
  
  # for posnI
  zpI<-z$posnI
  zpI<-rbind(zpI,zcov[5,])
  zpI<-cbind(zpI,matrix(0.999,nrow=nrow(zpI),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zpI)[nrow(zpI)]<-"covsp"
  
  # for posnN
  zpN<-z$posnN
  zpN<-rbind(zpN,zcov[6,])
  zpN<-cbind(zpN,matrix(0.999,nrow=nrow(zpN),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zpN)[nrow(zpN)]<-"covsp"
  
  # for corval
  zcval<-z$corval
  zcval<-rbind(zcval,zcov[7,])
  zcval<-cbind(zcval,matrix(0.999,nrow=nrow(zcval),ncol=tsp)) # 0.999 value will be filled in with black color in the plot
  rownames(zcval)[nrow(zcval)]<-"covsp"
  
  # for cells to not show in the plot
  posn_notneeded<-which(zs==0.999,arr.ind=T)
  
  zres<-list(spear=zs,
             kend=zk,
             Corl=zcl,
             Coru=zcu,
             posnI=zpI,
             posnN=zpN,
             corval=zcval,
             posn_notneeded=posn_notneeded)
  saveRDS(zres,paste(resloc,"NonParamStat.RDS",sep=""))
  
  NonParamStat_matrixplot(data=zres,
                          resloc=resloc,
                          posn_notneeded=posn_notneeded,
                          tot_target_sp=tot_target_sp,
                          tl.cex=1.2,cl.cex=2,line=1)
}