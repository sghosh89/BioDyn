rm(list=ls())
library(tidyverse)
#----------------------------------------
# read the data
x<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_SurveyTable.csv") # a dataframe
str(x)
x_meta<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_TimeseriesTable.csv")
#x_readme<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_Readme.csv")

# summary table: 11386 unique timeseries 
c<-x%>%group_by(TimeSeriesID)%>%summarise(nyr=n_distinct(Year),
                                          nsp=n_distinct(Species),
                                          nq=n_distinct(Quarter),
                                          uq=unique(Quarter))%>%ungroup()
# but we will consider only those having >=20 years data
c20<-c%>%filter(nyr>=20) # there are 967 timeseries like that

table(c20$uq) # this table shows quarter 3,4 are the most frequent sampling time

# so to make consistent sampling effort, 
# we will consider sites which were sampled for both quarter 3, 4 (July-Dec)
c20_q3q4<-c20%>%filter(uq%in%c("3","4"))
table(c20_q3q4$uq) 
c20_q3q4<-c20_q3q4%>%group_by(TimeSeriesID)%>%
                    summarise(q3q4=n_distinct(uq))%>%ungroup()%>%
                    filter(q3q4==2) 

c20_q3q4<-inner_join(c20_q3q4,x_meta,by="TimeSeriesID")%>%select(-c(q3q4,X))

# so, there are 829 sites which were sampled in both quarters: q3, q4 (July-Dec)
write.csv(c20_q3q4,"../../DATA/for_RivFishTIME/wrangled_data/TimeSeriesID_sampled_bothq3q4.csv",row.names = F)

################################################################################################################
# Now filtering main data for abundance (unique(x$UnitAbundance)="Ind.100m2", "CPUE", "Count" )

xq3q4<-x%>%filter(TimeSeriesID%in%c20_q3q4$TimeSeriesID & Quarter%in%c("3","4"))%>%
        group_by(TimeSeriesID,Year,Species)%>%
        summarise(Abundance=sum(Abundance))%>%ungroup()
        
all(unique(xq3q4$TimeSeriesID)==unique(c20_q3q4$TimeSeriesID))==T

ts_ids<-unique(xq3q4$TimeSeriesID)

bad_TimeSeriesID_w_singlesp<-c()
for(i in 1:length(ts_ids)){
  
  ts_id<-ts_ids[i]
  tsid_resloc<-paste("../../DATA/for_RivFishTIME/wrangled_data/",ts_id,"/",sep="")
  if(!dir.exists(tsid_resloc)){
    dir.create(tsid_resloc)
  }
  x_tsid<-xq3q4%>%filter(TimeSeriesID%in%ts_id)%>%
                  select(-TimeSeriesID)%>%
                  spread(Species,Abundance,fill=0)
  x_tsid<-as.data.frame(x_tsid)
  rownames(x_tsid)<-x_tsid$Year
  x_tsid<-x_tsid[,-1]
  
  saveRDS(x_tsid,paste(tsid_resloc,"allspecies_timeseries.RDS",sep=""))
  write.csv(x_tsid,paste(tsid_resloc,"allspecies_timeseries.csv",sep=""))
  
  x_tsid<-as.data.frame(x_tsid) # this is needed when there is only 1 sp
  presentyr<-apply(x_tsid,MARGIN=2,FUN=function(x){sum(x!=0)})
  commonsp<-which(presentyr>=0.7*nrow(x_tsid))
  commonsp_mat<-x_tsid[,commonsp]
  commonsp_mat<-as.data.frame(commonsp_mat) # to avoid: coercing warning coming from this line ref: :)))))
  raresp<-which(presentyr<0.7*nrow(x_tsid))
  minsp<-ncol(commonsp_mat)
  
  if(length(raresp)!=0){
      raresp<-x_tsid[,raresp]
      raresp<-as.data.frame(raresp) # this is needed when there is only 1 raresp
      raresp<-apply(raresp,MARGIN=1,FUN=sum)   # ref: :)))))
      commonsp_mat$raresp<-raresp
    }else{
      cat("no raresp found at this site: ",ts_id,"\n")
    }
  
  if(minsp>=2){ # atleast 2 species should be present at a site to consider
    saveRDS(commonsp_mat,paste(tsid_resloc,"commonspecies_timeseries.RDS",sep=""))
    metafile<-c20_q3q4%>%filter(TimeSeriesID==ts_id)
    saveRDS(metafile,paste(tsid_resloc,"metadata.RDS",sep=""))
  }else{
    bad_TimeSeriesID_w_singlesp<-c(bad_TimeSeriesID_w_singlesp,ts_id)
  }
  cat("--- i = ",i," ---- done \n")
}
good_TimeSeriesID_q3q4<-setdiff(ts_ids,bad_TimeSeriesID_w_singlesp)
saveRDS(good_TimeSeriesID_q3q4,"../../DATA/for_RivFishTIME/wrangled_data/good_TimeSeriesID_q3q4.RDS")
saveRDS(bad_TimeSeriesID_w_singlesp,"../../DATA/for_RivFishTIME/wrangled_data/bad_TimeSeriesID_w_singlesp.RDS")







