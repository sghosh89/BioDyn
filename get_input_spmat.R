# This function takes arguments:
# x: a matrix sampled from the whole database (same format as of the whole csv file downloaded from BIOTIME database)
# xmeta: metadata file corresponding to x

# output: a list of two: a sp mat and its list format

get_input_spmat<-function(x,xmeta){
  
  ab_na<-is.na(xmeta$ABUNDANCE_TYPE)
  
  # we give priority to abundance type category first, 
  # and then take average over all samples from a given year for a given species.
  if(ab_na==F){ 
    x<-x%>%select(YEAR,sum.allrawdata.ABUNDANCE,GENUS_SPECIES)%>%
      group_by(YEAR,GENUS_SPECIES)%>%summarize(mean_estimate=mean(sum.allrawdata.ABUNDANCE))%>%ungroup()
  }else{
    x<-x%>%select(YEAR,sum.allrawdata.BIOMASS,GENUS_SPECIES)%>%
      group_by(YEAR,GENUS_SPECIES)%>%summarize(mean_estimate=mean(sum.allrawdata.BIOMASS))%>%ungroup()
  }
  
  x<-x%>%mutate(yr_sp=paste(YEAR,GENUS_SPECIES,sep="_"))
  xall<-x%>%expand(YEAR,GENUS_SPECIES)%>%mutate(yr_sp=paste(YEAR,GENUS_SPECIES,sep="_"))
  xfill<-left_join(x=xall,y=x,by=c("yr_sp"="yr_sp"))%>%
    select(YEAR=YEAR.x,GENUS_SPECIES=GENUS_SPECIES.x,mean_estimate)
  xfill$mean_estimate[is.na(xfill$mean_estimate)]<-0 # absent species are zero
  
  z<-split(xfill,f=xfill$GENUS_SPECIES)
  z<-purrr::map(z,~ (.x %>% select(YEAR,mean_estimate)))
  
  xyr<-unique(x$YEAR)
  xsp<-sort(unique(x$GENUS_SPECIES))
  
  spmat<-matrix(NA,nrow=length(xyr),ncol=length(xsp))
  rownames(spmat)<-xyr
  colnames(spmat)<-xsp
  for(i in 1:ncol(spmat)){
    spname<-xsp[i]
    y<-xfill%>%filter(GENUS_SPECIES==spname)
    spmat[,i]<-y$mean_estimate
    #spmat[,i]<-z[[i]]$mean_estimate
  }
  
  return(list(spmat=spmat,
              splist=z))
  
}
