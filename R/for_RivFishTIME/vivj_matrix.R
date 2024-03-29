# Select any two species pair [i,j] for a copula 
# This function gives you a matrix with vi and vj as two columns

# Input :
# d_allsp : dataset in data[[sp]] format
# sp : species index
# i,j : sp-pair indices
# level : significance level for BiCopIndepTest p-value
# ploton : (optional) logical, if T gives copula plot without transforming j-th 
# variable to it's -ve value

# Output :
# A list of 4 elements:
#                      mat : a matrix : copula of (vi,vj) with transforming j-th variable to it's -ve value for -ve corr.
#                      corval : Spearman's correlation
#                      pval   : pvalue of Kendall's cor.test
#                      IndepTestRes : BiCopIndepTest p-value

# and an optional plot of the copula

library(VineCopula)
vivj_matrix<-function(d_allsp,i,j,level=0.05,ploton){
  
  ds1<-d_allsp[[i]]
  ds2<-d_allsp[[j]]
  #----------------------------
  colnames(ds1)<-c("Year","Dat")  # ensuring column names
  colnames(ds2)<-c("Year","Dat")
  
  a1<-ds1$Year[1]
  a2<-ds2$Year[1]
  a3<-ds1$Year[dim(ds1)[1]]
  a4<-ds2$Year[dim(ds2)[1]]
  year_s<-max(a1,a2)
  year_e<-min(a3,a4)
  ind_s1<-which(ds1$Year==year_s)
  ind_s2<-which(ds2$Year==year_s)
  ind_e1<-which(ds1$Year==year_e)
  ind_e2<-which(ds2$Year==year_e)
  ds1<-ds1[ind_s1:ind_e1,]
  ds2<-ds2[ind_s2:ind_e2,]
  # Omitting the years and data containing NA in either d1 or d2 
  #from both d1 and d2
  if(anyNA(ds1$Dat)==T | anyNA(ds2$Dat)==T){
    ind_na1<-which(is.na(ds1$Dat))
    ind_na2<-c(ind_na1,which(is.na(ds2$Dat)))
    ind_na<-unique(ind_na2)
    
    d1Dat<-ds1$Dat[-ind_na]
    d2Dat<-ds2$Dat[-ind_na]
    Years<-ds1$Year[-ind_na]
    d1<-data.frame(Year=Years,Dat=d1Dat)
    d2<-data.frame(Year=Years,Dat=d2Dat)
  }else{
    d1<-ds1
    d2<-ds2
  }
  
  colnames(d1)[2]<-"Dat"  # ensuring column names
  colnames(d2)[2]<-"Dat"
  
  #get ranks modified now
  vi<-VineCopula::pobs(d1$Dat)
  vj<-VineCopula::pobs(d2$Dat)
  
  IndepTestRes<-VineCopula::BiCopIndTest(vi,vj)$p.value
  ct<-cor.test(vi,vj,alternative = "two.sided",method="spearman",exact=F)
  corval<-unname(ct$estimate)
  pval<-ct$p.value
  
  
  if(IndepTestRes<level && corval>0){ # for significant positive correlation
    if(ploton==T){
      plot(vi,vj,type='p',col=rgb(0,0,0,0.3),pch=19,xlim=c(0,1),ylim=c(0,1),
           xlab=names(d_allsp)[i],ylab=names(d_allsp)[j],cex.lab=1.5)
      mtext(paste0("(sp_x, sp_y) = (",i," , ",j,")"),
            side = 3, line=0.15, adj=0.5, col="black")
    }
    
  }else if(IndepTestRes<level && corval<0){ # for significant negative correlation
    if(ploton==T){
      plot(vi,vj,type='p',col=rgb(0,1,0,0.3),pch=19,xlim=c(0,1),ylim=c(0,1),
           xlab=names(d_allsp)[i],ylab=names(d_allsp)[j],cex.lab=1.5)
      mtext(paste0("(sp_x, sp_y) = (",i," , ",j,")"),
            side = 3, line=0.15, adj=0.5, col="black")
    }
    vj<-VineCopula::pobs(-(d2$Dat)) # reverse the variable
    
  }else{ # independent case
    if(ploton==T){
      plot(-1,0,xlim=c(0,1),ylim=c(0,1),xlab=names(d_allsp)[i],ylab=names(d_allsp)[j],cex.lab=1.5)
      text(0.5,0.5,"Indep.",adj=c(0.5,.5),cex=2)
      mtext(paste0("(sp_x, sp_y) = (",i," , ",j,")"),
            side = 3, line=0.15, adj=0.5, col="black")
    }
  }
  
  
  Years<-d1$Year
  #-------------------------
  #n_datapt<-length(vi)
  #--------------------
  #plot(vi,vj,type="p")
  #-------------------------
  mat<-as.matrix(cbind(vi,vj))
  return(list(mat=mat,   # return reversed mat so that if you plot this mat you get +ve correlation 
              corval=corval,  # but return the actual -ve corr. value  
              pval=pval,
              IndepTestRes=IndepTestRes))  
}
