# This function is written to generate matrix plot for non-parametric stat results 
# Input : 
#   data 
#   resloc : location to save the results
#   wd,ht : width and height of fenarated plot
#     tl.cex,cl.cex,line : numeric values for size of text in the labels and colorbar, line of mtext
#---------------------------
source("mycorrplot.R")
#---------------------------

NonParamStat_matrixplot<-function(data,resloc,tl.cex,cl.cex,line){
 
  
  #--------------------------Spearman plot---------------------------
  
  tempo<-data$spear
  indI<-data$posnI
  tempo[indI]<-NA
  diag(tempo)<-NA
  
  minval<-min(tempo,na.rm=T)
  maxval<-max(tempo,na.rm=T)
  
  cr<-max(abs(minval),abs(maxval))
  
  nsp<-nrow(tempo)# number of species considered  
  wd<-nsp
  ht<-nsp
  
  
  pdf(paste(resloc,file="Spearman.pdf",sep=''),width=wd, height=ht)
  
  mycorrplot(z=tempo,
               posnI_ind=data$posnI,
               posnN_ind=data$posnN,
               colrange=c(0,cr),tl.cex=tl.cex,cl.cex=cl.cex)
  dev.off()
 
  
  #--------------------------Kendall plot---------------------------
  
  tempo<-data$kend
  indI<-data$posnI
  tempo[indI]<-NA
  diag(tempo)<-NA
  
  minval<-min(tempo,na.rm=T)
  maxval<-max(tempo,na.rm=T)
  
  cr<-max(abs(minval),abs(maxval))
  
  
  pdf(paste(resloc,file="Kendall.pdf",sep=''),width=wd, height=ht)
  
  mycorrplot(z=tempo,
             posnI_ind=data$posnI,
             posnN_ind=data$posnN,
             colrange=c(0,cr),tl.cex=tl.cex,cl.cex=cl.cex)
  dev.off()
 

  #========================================= For cor npa stats ===============================================
  
  #--------------------------Corl plot---------------------------
  
  tempo<-data$Corl
  indI<-data$posnI
  tempo[indI]<-NA
  diag(tempo)<-NA
  
  
  minval<-min(tempo,na.rm=T)
  maxval<-max(tempo,na.rm=T)
  
  cr<-max(abs(minval),abs(maxval))
  
  pdf(paste(resloc,file="Corl.pdf",sep=''),width=wd, height=ht)
  
  mycorrplot(z=tempo,
             posnI_ind=data$posnI,
             posnN_ind=data$posnN,
             colrange=c(-cr,cr),tl.cex=tl.cex,cl.cex=cl.cex)
  dev.off()
  
  
  #--------------------------Coru plot---------------------------
  
  tempo<-data$Coru
  indI<-data$posnI
  tempo[indI]<-NA
  diag(tempo)<-NA
  
  
  minval<-min(tempo,na.rm=T)
  maxval<-max(tempo,na.rm=T)
  
  cr<-max(abs(minval),abs(maxval))
  
  pdf(paste(resloc,file="Coru.pdf",sep=''),width=wd, height=ht)
  
  mycorrplot(z=tempo,
             posnI_ind=data$posnI,
             posnN_ind=data$posnN,
             colrange=c(-cr,cr),tl.cex=tl.cex,cl.cex=cl.cex)
  dev.off()
 
  #--------------------------Corl-Coru plot---------------------------
  
  tempo<-data$Corl - data$Coru
  indI<-data$posnI
  tempo[indI]<-NA
  diag(tempo)<-NA
  
  minval<-min(tempo,na.rm=T)
  maxval<-max(tempo,na.rm=T)
  
  cr<-max(abs(minval),abs(maxval))
  
  #pdf(paste(resloc,file="Corl-Coru.pdf",sep=''),width=wd, height=ht)
  png(paste(resloc,file="Corl-Coru.png",sep=''), width = 5000,height = 5000,  res = 300)
  z<-tempo
  mycorrplot(z=z,
             posnI_ind=data$posnI,
             posnN_ind=data$posnN,
             colrange=c(-cr,cr),tl.cex=tl.cex,cl.cex=cl.cex)
  z1<-z
  z1[data$posnN]<-NA # this line was added to exclude -vely correlated species pair from nL,nU,L,U 
    # calculation, but it does not matter as for -vely correlated cells [sp_i,sp_j] and 
    # [sp_j,sp_i] nL,nU both will increase by same number, whereas L+U remains same and
    # both L, U will change by same +, - factor
    
    
    
  z1[upper.tri(z1)]<-NA # symmetric matrix, so consider only lower triangular part
    
  nL<-sum(z1>0,na.rm = T)
  nU<-sum(z1<0,na.rm = T)
  L<-sum(z1[which(z1>0,arr.ind=T)])
  U<-sum(z1[which(z1<0,arr.ind=T)])
  
  
  npos<-nL+nU # number of positive correlation
  nint<-nrow(z)*(nrow(z)-1)/2 # number of all pairwise interaction
  zind<-z[lower.tri(z)]
  nind<-sum(is.na(zind)==T) # number of indep. interaction
    
  nneg<-nrow(data$posnN)/2 # negative correlaion in lower.tri of a symmetric matrix
    
    #mtext(paste0("nL =",nL,", nU =",nU),cex=5,side=1,adj=0.7)
  mtext((as.expression(bquote('N'['+']*' = '*.(npos)))),cex=tl.cex,line=line,side=1,adj=0.2,col="gray")# number of cells with pos. correlation
  mtext((as.expression(bquote(', N'['L']*' = '*.(nL)))),cex=tl.cex,line=line,side=1,adj=0.4,col="red")# number of cells with Lower or Left tail dep.
  mtext((as.expression(bquote(', N'['R']*' = '*.(nU)))),cex=tl.cex,line=line,side=1,adj=0.6,col="blue")# number of cells with Upper or Right tail dep.
  mtext((as.expression(bquote(', N'['-']*' = '*.(nneg)))),cex=tl.cex,line=line,side=1,adj=0.8,col="green")# number of cells with neg. correlation
   
  dev.off()
  
  summary_df<-data.frame(nsp=nsp,nint=nint,nind=nind,npos=npos,nL=nL,nU=nU,nneg=nneg)
  
  res<-list(CorlmCoru=z,
            summary_df=summary_df)
  
  saveRDS(summary_df,paste(resloc,"summary_df.RDS",sep=""))
  
  #return(res)
}