rm(list=ls())
library(tidyverse)
`%notin%` <- Negate(`%in%`)
set.seed(seed=123)
resloc<-"../../DATA/for_BBS/wrangled_data/"
fshort_list<-readRDS(paste(resloc,"sourcefile_list.RDS",sep=""))
uroutes<-readRDS(paste(resloc,"unique_routes_all.RDS",sep=""))

#table of detection probabilities
detect_prob_tab<-expand.grid(Country_State_Route=uroutes, Year=1997:2019)
detect_prob_tab$detection_prob<-runif(nrow(detect_prob_tab),0.5,1)


badroutes<-c()
for (i in 1:length(fshort_list)){
  x<-fshort_list[[i]]
  resloc2<-paste(resloc,names(fshort_list)[i],"/",sep="")
  #saveRDS(x,paste(resloc2,"sourcefile.RDS",sep=""))
  
  #######generate stochastic abundances
  x=merge(x,detect_prob_tab,by=c("Country_State_Route","Year"),all.x=T,all.y=F)
  func=function(y,z){rnbinom(n=1,size=y,prob=z)}
  x$n_bird_missed_stoc<-mapply(func,y=x$Stop1to50,z=x$detection_prob)
  x$n_bird_missed_avg<-x$Stop1to50*(1-x$detection_prob)/x$detection_prob
  x$true_abund<-round(x$n_bird_missed_avg+x$Stop1to50)
  ######## end
  
  y<-x%>%select(Year,AOU,true_abund)
  y<-y%>%spread(AOU,true_abund,fill=0)%>%as.data.frame()
  rownames(y)<-y$Year # and colnames are species code
  y<-y[,-1]
  count_non0<-apply(y,MARGIN=2,FUN=function(x){sum(x!=0)})
  commonspmat<-y[,which(count_non0>=0.7*nrow(y))] # common sp present atleast 70% of sampling years
  
  if(ncol(commonspmat)>=2){
    rarespmat<-y[,which(count_non0<0.7*nrow(y))]
    if(ncol(rarespmat)>0){
      rarespmat<-as.matrix(rarespmat)
      commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
    }else{
      cat("-------- No rare sp in ",names(fshort_list)[i]," ----------\n")
    }
    saveRDS(commonspmat,paste(resloc2,"input_mat_for_tailanal_with_stoc.RDS",sep=""))
  }else{
    badroutes<-c(badroutes,names(fshort_list)[i])
    cat("-------- commonsp is less than 2 in ",names(fshort_list)[i]," ----------\n")
  }
}
uroutes<-setdiff(uroutes,badroutes)
saveRDS(uroutes,paste(resloc,"unique_routes_all_with_stoc.RDS",sep=""))