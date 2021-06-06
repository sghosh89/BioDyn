# function to get interaction type (indep, synchrony, compensatory) between two species 
# categorized into top 4 category based on rank abundance
# Input:
# x = output from NonParamStat function
# xcat = output from get_rankabun function: a dataframe having target species name and category type: 1,2,3,4

library(tidyverse)
get_categorized_interaction<-function(x,xcat){
  xcat$targetsp<-rownames(xcat)
  xcat<-xcat[,c("targetsp","category")]
  CorlmCoru<-x$Corl - x$Coru
  #colnames(CorlmCoru)<-rownames(CorlmCoru) # when no raresp still covsp could be present then nrow>ncol
  
  numtargetsp<-rownames(xcat)
  numtargetsp<-setdiff(numtargetsp,c("raresp","covsp"))
  numtargetsp<-length(numtargetsp)
  
  # for indep. interaction pair
  indepmat<-which(x$posnI==1,arr.ind = T)
  if(nrow(indepmat)>0){
    indepmat<-as.data.frame(indepmat)
    indepmat$rowsp<-rownames(CorlmCoru)[indepmat$row]
    indepmat$colsp<-rownames(CorlmCoru)[indepmat$col] # rownames and colnames are same for individual sp
    badid<-which(indepmat$rowsp%in%c("raresp","covsp"))
    if(length(badid)>0){indepmat<-indepmat[-badid,]}
    badid<-which(indepmat$colsp%in%c("ra"))
    if(length(badid)>0){indepmat<-indepmat[-badid,]}
    indepmat<-left_join(indepmat,xcat,by=c("rowsp"="targetsp"))
    indepmat<-left_join(indepmat,xcat,by=c("colsp"="targetsp"))
    colnames(indepmat)[5:6]<-c("rowsp_category","colsp_category")
    indepmat$interaction_category<-paste(indepmat$rowsp_category,indepmat$colsp_category,sep="_")
    indepmat$interaction_category[which(indepmat$interaction_category=="2_1")]<-"1_2"
    indepmat$interaction_category[which(indepmat$interaction_category=="3_1")]<-"1_3"
    indepmat$interaction_category[which(indepmat$interaction_category=="4_1")]<-"1_4"
    indepmat$interaction_category[which(indepmat$interaction_category=="3_2")]<-"2_3"
    indepmat$interaction_category[which(indepmat$interaction_category=="4_2")]<-"2_4"
    indepmat$interaction_category[which(indepmat$interaction_category=="4_3")]<-"3_4"
  }else{
    indepmat<-NA
  }
  #------------- save indepmat for indep. interaction category info -----------------
  
  # for synchronous interaction
  posnI<-which(x$posnI==1,arr.ind = T)
  posnN<-which(x$posnN==1,arr.ind = T)
  z<-CorlmCoru
  z[posnI]<-NA
  if(nrow(posnN)>0){z[posnN]<-NA}
  synmat<-which(z!=0,arr.ind = T)
  if(nrow(synmat)>0){ # if there is any synchronous dynamics
    synmat<-as.data.frame(synmat)
    synmat$rowsp<-rownames(CorlmCoru)[synmat$row]
    synmat$colsp<-rownames(CorlmCoru)[synmat$col]
    badid<-which(synmat$rowsp%in%c("raresp","covsp"))
    if(length(badid)>0){synmat<-synmat[-badid,]}
    badid<-which(synmat$colsp%in%c("raresp","covsp"))
    if(length(badid)>0){synmat<-synmat[-badid,]}
    synmat<-left_join(synmat,xcat,by=c("rowsp"="targetsp"))
    synmat<-left_join(synmat,xcat,by=c("colsp"="targetsp"))
    colnames(synmat)[5:6]<-c("rowsp_category","colsp_category")
    synmat$LTmUT<-z[as.matrix(synmat[,1:2])]
    synmat$type<-ifelse(synmat$LTmUT>0,"LT","UT")
    synmat$interaction_category<-paste(synmat$rowsp_category,synmat$colsp_category,sep="_")
    synmat$interaction_category[which(synmat$interaction_category=="2_1")]<-"1_2"
    synmat$interaction_category[which(synmat$interaction_category=="3_1")]<-"1_3"
    synmat$interaction_category[which(synmat$interaction_category=="4_1")]<-"1_4"
    synmat$interaction_category[which(synmat$interaction_category=="3_2")]<-"2_3"
    synmat$interaction_category[which(synmat$interaction_category=="4_2")]<-"2_4"
    synmat$interaction_category[which(synmat$interaction_category=="4_3")]<-"3_4"
    #barplot((table(synmat$interaction_category)/nrow(synmat)),ylim=c(0,1))
    #synmat_LT<-synmat%>%filter(type=="LT")
    #barplot((table(synmat_LT$interaction_category)/nrow(synmat)),col="red")
    #synmat_UT<-synmat%>%filter(type=="UT")
    #barplot((table(synmat_UT$interaction_category)/nrow(synmat)),col="blue")
  }else{
    synmat<-NA
  }
  #------------- save synmat for synchronous interaction category info -----------------
  
  # for compensatory interaction
  compmat<-which(x$posnN==1,arr.ind = T)
  if(nrow(compmat)>0){ # if there is any compensatory dynamics
    compmat<-as.data.frame(compmat)
    compmat$rowsp<-rownames(CorlmCoru)[compmat$row]
    compmat$colsp<-rownames(CorlmCoru)[compmat$col]
    badid<-which(compmat$rowsp%in%c("raresp","covsp"))
    if(length(badid)>0){compmat<-compmat[-badid,]}
    badid<-which(compmat$colsp%in%c("raresp","covsp"))
    if(length(badid)>0){compmat<-compmat[-badid,]}
    compmat<-left_join(compmat,xcat,by=c("rowsp"="targetsp"))
    compmat<-left_join(compmat,xcat,by=c("colsp"="targetsp"))
    colnames(compmat)[5:6]<-c("rowsp_category","colsp_category")
    compmat$LTmUT<-CorlmCoru[as.matrix(compmat[,1:2])]
    compmat$strongcomp_sp<-ifelse(compmat$LTmUT>0,compmat$colsp,compmat$rowsp)
    compmat$strongcomp_category<-ifelse(compmat$LTmUT>0,compmat$colsp_category,compmat$rowsp_category)
    compmat$interaction_category<-paste(compmat$rowsp_category,compmat$colsp_category,sep="_")
    compmat$interaction_category[which(compmat$interaction_category=="2_1")]<-"1_2"
    compmat$interaction_category[which(compmat$interaction_category=="3_1")]<-"1_3"
    compmat$interaction_category[which(compmat$interaction_category=="4_1")]<-"1_4"
    compmat$interaction_category[which(compmat$interaction_category=="3_2")]<-"2_3"
    compmat$interaction_category[which(compmat$interaction_category=="4_2")]<-"2_4"
    compmat$interaction_category[which(compmat$interaction_category=="4_3")]<-"3_4"
  }else{
    compmat<-NA
  }
  
  # barplot(table(compmat$interaction_category)/nrow(compmat))
  #------------- save compmat for compensatory interaction category info -----------------
  
  interaction_type<-c("1_1","1_2","1_3","1_4","2_2","2_3","2_4","3_3","3_4","4_4")
  table_indep<-as.data.frame(matrix(NA,nrow=1,ncol=10))
  colnames(table_indep)<-interaction_type
  table_syn<-table_indep
  table_comp<-table_indep
  if(is.data.frame(indepmat)){
    table_indep$`1_1`<-sum(indepmat$interaction_category=="1_1")
    table_indep$`1_2`<-sum(indepmat$interaction_category=="1_2")
    table_indep$`1_3`<-sum( indepmat$interaction_category=="1_3")
    table_indep$`1_4`<-sum( indepmat$interaction_category=="1_4")
    table_indep$`2_2`<-sum( indepmat$interaction_category=="2_2")
    table_indep$`2_3`<-sum( indepmat$interaction_category=="2_3")
    table_indep$`2_4`<-sum( indepmat$interaction_category=="2_4")
    table_indep$`3_3`<-sum( indepmat$interaction_category=="3_3")
    table_indep$`3_4`<-sum( indepmat$interaction_category=="3_4")
    table_indep$`4_4`<-sum( indepmat$interaction_category=="4_4")
  }
  
  if(is.data.frame(synmat)){
    table_syn$`1_1`<-sum( synmat$interaction_category=="1_1")
    table_syn$`1_2`<-sum( synmat$interaction_category=="1_2")
    table_syn$`1_3`<-sum( synmat$interaction_category=="1_3")
    table_syn$`1_4`<-sum( synmat$interaction_category=="1_4")
    table_syn$`2_2`<-sum( synmat$interaction_category=="2_2")
    table_syn$`2_3`<-sum( synmat$interaction_category=="2_3")
    table_syn$`2_4`<-sum( synmat$interaction_category=="2_4")
    table_syn$`3_3`<-sum( synmat$interaction_category=="3_3")
    table_syn$`3_4`<-sum( synmat$interaction_category=="3_4")
    table_syn$`4_4`<-sum( synmat$interaction_category=="4_4")
  }
  
  if(is.data.frame(compmat)){
    table_comp$`1_1`<-sum( compmat$interaction_category=="1_1")
    table_comp$`1_2`<-sum( compmat$interaction_category=="1_2")
    table_comp$`1_3`<-sum( compmat$interaction_category=="1_3")
    table_comp$`1_4`<-sum( compmat$interaction_category=="1_4")
    table_comp$`2_2`<-sum( compmat$interaction_category=="2_2")
    table_comp$`2_3`<-sum( compmat$interaction_category=="2_3")
    table_comp$`2_4`<-sum( compmat$interaction_category=="2_4")
    table_comp$`3_3`<-sum( compmat$interaction_category=="3_3")
    table_comp$`3_4`<-sum( compmat$interaction_category=="3_4")
    table_comp$`4_4`<-sum( compmat$interaction_category=="4_4")
  }
  
  table_indep<-t(table_indep)
  table_syn<-t(table_syn)
  table_comp<-t(table_comp)
  
  table_interaction<-cbind(table_indep,table_syn,table_comp)
  colnames(table_interaction)<-colnames(table_interaction)<-c("indep","syn","comp")
  table_interaction<-as.data.frame(table_interaction)
  
  table_interaction$poss_interaction<-NA
  tt<-as.data.frame(table(xcat$category))
  
  tt_dummy<-data.frame(Var1=as.character(1:4))
  tt<-left_join(tt_dummy,tt,"Var1")
  tt$Freq[is.na(tt$Freq)]<-0
  
  table_interaction$poss_interaction[1]<-tt$Freq[1]*(tt$Freq[1]-1)/2 
  table_interaction$poss_interaction[2]<-(2*tt$Freq[1]*tt$Freq[2])/2 # as 1-2 and 2-1 are the same: multiply by 2, 
                                                                      # but only compare the lower triangular
                                                                      #   matrix: so divide by 2
  table_interaction$poss_interaction[3]<-(2*tt$Freq[1]*tt$Freq[3])/2
  table_interaction$poss_interaction[4]<-(2*tt$Freq[1]*tt$Freq[4])/2
  table_interaction$poss_interaction[5]<-tt$Freq[2]*(tt$Freq[2]-1)/2 
  table_interaction$poss_interaction[6]<-(2*tt$Freq[2]*tt$Freq[3])/2
  table_interaction$poss_interaction[7]<-(2*tt$Freq[2]*tt$Freq[4])/2
  table_interaction$poss_interaction[8]<-tt$Freq[3]*(tt$Freq[3]-1)/2
  table_interaction$poss_interaction[9]<-(2*tt$Freq[3]*tt$Freq[4])/2
  table_interaction$poss_interaction[10]<-tt$Freq[4]*(tt$Freq[4]-1)/2
  table_interaction$freq_indep<-table_interaction$indep/table_interaction$poss_interaction
  table_interaction$freq_syn<-table_interaction$syn/table_interaction$poss_interaction
  table_interaction$freq_comp<-table_interaction$comp/table_interaction$poss_interaction

  res<-list(numtargetsp=numtargetsp,
            indepmat=indepmat,
            synmat=synmat,
            compmat=compmat,
            table_interaction=table_interaction)
  
  return(res)
}


