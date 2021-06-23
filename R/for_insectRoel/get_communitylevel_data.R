library(tidyverse)
`%notin%` <- Negate(`%in%`)

# cc: community 
get_communitylevel_data<-function(cc,resloc){
  
  cc<-apply(X=cc,MARGIN=2,FUN=tolower)
  cc<-apply(X=cc,MARGIN=2,FUN=trimws)
  cc<-as.data.frame(cc)
  cc$Phylum[cc$Phylum==""]<-"NA.phylum"
  cc$Class[cc$Class==""]<-"NA.class"
  cc$Subclass[cc$Subclass==""]<-"NA.subclass"
  cc$Order[cc$Order==""]<-"NA.order"
  cc$Suborder[cc$Suborder==""]<-"NA.suborder"
  cc$Family[cc$Family==""]<-"NA.family"
  cc$Subfamily[cc$Subfamily==""]<-"NA.subfamily"
  cc$Genus[cc$Genus%in%c("","1")]<-"NA.genus"
  cc$Species[cc$Species%in%c("",".")]<-"NA.species"
  
  write.csv(cc,paste(resloc,"rawdata_community.csv",sep=""),row.names = F)
  
  # set unique identifier
  cc$uid0<-paste(cc$Phylum,cc$Class,cc$Subclass,cc$Order,cc$Suborder,cc$Family,sep="_") # uid upto family level
  cc$uid1<-paste(cc$uid0,cc$Subfamily,sep="_") # sequential addition of subfamily
  cc$uid2<-paste(cc$uid1,cc$Genus,sep="_") # then add genus
  cc$uid3<-paste(cc$uid2,cc$Species,sep="_") # then add species
  
  nyr<-length(sort(unique(cc$Year)))
  nyr_thrs<-nyr*0.7 # 70% of year of study period should be present 
  
  tt<-cc%>%group_by(uid3)%>%summarize(nyr=n_distinct(Year),
                                      nuid3=n(),  
                                      nuid0=n_distinct(uid0), # this must be 1 as it is higher taxonomic level than species
                                      nuid1=n_distinct(uid1), # this must be 1
                                      nuid2=n_distinct(uid2))%>% # this must be 1
    ungroup()%>%arrange(desc(nuid3))
  all(tt$nyr==tt$nuid3)==T
  
  # get the species level resolution 
  tt_splevel<-tt%>%filter(nyr>=nyr_thrs)
  cc_splevel<-cc%>%filter(uid3%in%tt_splevel$uid3)
  
  cc_splevel<-arrange(cc_splevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
  cc_splevel$selection<-"splevel"
  
  if(nrow(cc_splevel)==nrow(cc)){ # all data are identified at species level
    
    write.csv(cc_splevel,paste(resloc,"wrangleddata_community.csv",sep=""),row.names = F)
    cat("------------- all species level wrangled data -----------------\n")
    cc_all<-cc_splevel
    
  }else{ # but what if some are not?
    
    # update cc, tt
    cc<-cc%>%filter(uid3%notin%tt_splevel$uid3)
    
    tt<-cc%>%group_by(uid2)%>%summarize(nyr=n_distinct(Year),
                                        nuid2=n(),  
                                        nuid0=n_distinct(uid0), # this must be 1 as it is higher taxonomic level than species
                                        nuid1=n_distinct(uid1))%>% # this must be 1
      ungroup()%>%arrange(desc(nuid2))
    #all(tt$nyr==tt$nuid2)==T # this is false 
    # because sp1, sp2 both have same genus (uid2) so 1990, 1992, 1994 repeat
    cc<-arrange(cc,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
    
    # get the genus level resolution 
    tt_genuslevel<-tt%>%filter(nyr>=nyr_thrs)
    if(nrow(tt_genuslevel>0)){
      cc_genuslevel<-cc%>%filter(uid2%in%tt_genuslevel$uid2)
      cc_genuslevel_extra<-cc_genuslevel%>%filter(Genus=="NA.genus")
      cc_genuslevel<-cc_genuslevel%>%filter(Genus!="NA.genus")
      cc_genuslevel<-arrange(cc_genuslevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
      if(nrow(cc_genuslevel)>0){cc_genuslevel$selection<-"genuslevel"}
    }else{
      cc_genuslevel<-c()
      cc_genuslevel_extra<-c()
    }
    
    # update cc, tt
    cc<-cc%>%filter(uid2%notin%tt_genuslevel$uid2)
    cc<-rbind(cc,cc_genuslevel_extra)
    
    tt<-cc%>%group_by(uid1)%>%summarize(nyr=n_distinct(Year),
                                        nuid1=n(),  
                                        nuid0=n_distinct(uid0))%>% # this must be 1 as it is higher taxonomic level than species
      ungroup()%>%arrange(desc(nuid1))
    #all(tt$nyr==tt$nuid1)==T # this is false 
    cc<-arrange(cc,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
    
    # get the subfamily level resolution 
    tt_sfamlevel<-tt%>%filter(nyr>=nyr_thrs)
    if(nrow(tt_sfamlevel)>0){
      cc_sfamlevel<-cc%>%filter(uid1%in%tt_sfamlevel$uid1)
      cc_sfamlevel<-cc_sfamlevel%>%filter(Subfamily!="NA.subfamily")
      cc_sfamlevel_extra<-cc_sfamlevel%>%filter(Subfamily=="NA.subfamily")
      cc_sfamlevel<-arrange(cc_sfamlevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
      cc_sfamlevel$selection<-"sfamlevel"
    }else{
      cc_sfamlevel<-c()
      cc_sfamlevel_extra<-c()
    }
    
    # the rest extra left upto family level
    cc_extra<-cc%>%filter(uid1%notin%tt_sfamlevel$uid1)
    cc_extra<-rbind(cc_extra,cc_sfamlevel_extra)
    tt_extra<-cc_extra%>%group_by(uid0)%>%summarize(nyr=n_distinct(Year),
                                                    nuid0=n(),
                                                    nuid1=n_distinct(uid1))%>%ungroup()%>%
      arrange(desc(nuid0))
    #all(tt$nyr==tt$nuid0)==T # this is false 
    cc_extra<-arrange(cc_extra,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
    
    # update
    tt<-tt_extra%>%filter(nyr>=nyr_thrs)
    cc_famlevel<-cc_extra%>%filter(uid0%in%tt$uid0)
    cc_famlevel<-arrange(cc_famlevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
    
    if(nrow(cc_famlevel)>0){
      cc_famlevel$selection<-"famlevel"
      # but now we need to further sort down based on family level - 
      #     we can do subfamily or genus level aggregation
      t1<-cc_famlevel%>%group_by(Family)%>%count(Subfamily)%>%ungroup()
      
      t11<-t1%>%group_by(Family)%>%
        summarize(usf=n_distinct(Subfamily),Subfamily=unique(Subfamily))%>%
        ungroup()
      t11<-inner_join(t11,t1, by=c("Family"="Family","Subfamily"="Subfamily"))
      
      t11k<-t11%>%filter(n>=nyr_thrs) # keep this 
      t11nk<-t11%>%filter(n<nyr_thrs)
      
      # per Family aggregate only 2 unique Subfamily-levels if one of them is NA.Subfamily
      t1<-t11nk%>%filter(usf==2)
      t1n<-t1%>%filter(Subfamily=="NA.subfamily")
      # this t1n table indicates the Family name which has only two Subfamily levels 
      # ( a NA.subfamily and a known subfamily) - so we can aggregate the sfam for this family
      cc_famlevel_agg2sfam<-cc_famlevel%>%filter(Family%in%t1n$Family)
      
      if(nrow(cc_famlevel_agg2sfam)>0){
       cc_famlevel_agg2sfam$selection<-"famlevel_agg2sfam"
       #----------------- but keep if per family more than >= nyr_thrs obs was there -----------------------
       tempo<-cc_famlevel_agg2sfam%>%group_by(Family)%>%summarize(nyr=n_distinct(Year))%>%ungroup()
       tempo<-tempo%>%filter(nyr>=nyr_thrs)
       cc_famlevel_agg2sfam<-cc_famlevel_agg2sfam%>%filter(Family%in%tempo$Family)
       #----------------------------------------------------------------------------------------------------
       t11k_p2<-t11k%>%filter(usf>2)
       
       c2<-cc_famlevel%>%filter(Family%in%t11k_p2$Family & Subfamily%in%t11k_p2$Subfamily)
       t2<-c2%>%group_by(Family,Subfamily)%>%count(Genus)%>%ungroup()
       t2<-t2%>%filter(t2$n>=nyr_thrs)
       c2<-c2%>%filter(Family%in%t2$Family & Subfamily%in%t2$Subfamily & Genus%in%t2$Genus)
       
       if(nrow(c2)>0){
         c2$selection<-"famlevel_agg2genus"
         #----------------- but keep if per family more than >= nyr_thrs obs was there -----------------------
         tempo<-c2%>%group_by(Family)%>%summarize(nyr=n_distinct(Year))%>%ungroup()
         tempo<-tempo%>%filter(nyr>=nyr_thrs)
         cc_famlevel_agg2sfam<-cc_famlevel_agg2sfam%>%filter(Family%in%tempo$Family)
         if(nrow(cc_famlevel_agg2sfam)>0){cc_famlevel_agg2sfam$selection<-"famlevel_agg2sfam"}
       }
      }
    }
    
    # okay, now combine all data
    cc_all<-cc_splevel
    if("Selection"%in%colnames(cc_genuslevel)){
      cc_all<-rbind(cc_all,cc_genuslevel)
    }
    if("Selection"%in%colnames(cc_sfamlevel)){
      cc_all<-rbind(cc_all,cc_sfamlevel)
    }
    
    if(exists("cc_famlevel_agg2sfam")){
      if("Selection"%in%colnames(cc_famlevel_agg2sfam)){
        cc_all<-rbind(cc_all,cc_famlevel_agg2sfam)
      }
    }
    
    if(exists("c2")){
      if("Selection"%in%colnames(c2)){
        cc_all<-rbind(cc_all,c2)
      }
    }
    
    cc_all<-arrange(cc_all,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
    cc_all<-cc_all%>%select(-c(uid0,uid1,uid2,uid3))
    
    write.csv(cc_all,paste(resloc,"wrangleddata_community.csv",sep=""),row.names = F)
  }
  
  #------------------------- now get inputmat_tailanalysis -------------------------------
  
  mat<-cc_all%>%select(Year,Phylum, Class,Family,Subfamily,Genus,Species,Number,selection)
  mat$sp<-NA
  id<-which(mat$selection=="splevel" & mat$Genus=="NA.genus" & mat$Species=="NA.species")
  mat$sp[id]<-paste(mat$Phylum[id],mat$Class[id],mat$Family[id],sep=" ")
  id<-which(mat$selection=="splevel" & mat$Genus!="NA.genus" & mat$Species!="NA.species")
  mat$sp[id]<-paste(mat$Genus[id],mat$Species[id],sep=" ")
  id<-which(mat$selection=="splevel" & mat$Genus!="NA.genus" & mat$Species=="NA.species")
  mat$sp[id]<-paste(mat$Family[id],mat$Genus[id],sep=" ")
  id<-which(mat$selection=="splevel" & mat$Genus=="NA.genus" & mat$Species!="NA.species")
  mat$sp[id]<-paste(mat$Family[id],mat$Species[id],sep=" ")
  id<-which(mat$selection=="genuslevel")
  mat$sp[id]<-mat$Genus[id]
  id<-which(mat$selection=="sfamlevel")
  mat$sp[id]<-mat$Subfamily[id]
  id<-which(mat$selection=="famlevel")
  mat$sp[id]<-mat$Family[id]
  id<-which(mat$selection=="famlevel_agg2sfam")
  mat$sp[id]<-paste(mat$Family[id],mat$Subfamily[id],sep=" ")
  id<-which(mat$selection=="famlevel_agg2genus")
  mat$sp[id]<-paste(mat$Family[id],mat$Genus[id],sep=" ")
  
  # ok, now check if each sp >= nyr_thrs or not?
  checktable<-as.data.frame(table(mat$sp))
  allT<-checktable$Freq>=nyr_thrs
  if(any(allT)==F){
    cat("------------- You got a problem: check aggregation of sp name --------------")
  }
  
  write.csv(mat,paste(resloc,"wrangleddata_community_with_aggsp.csv",sep=""),row.names = F)
  
  # =========== now split it by sp column so that each sp along the column ==========
  
  mat<-mat%>%select(Year,sp,Number)
  yrs<-sort(unique(mat$Year))
  sp<-sort(unique(mat$sp))
  mat$Number<-as.numeric(mat$Number) # just to ensure
  mat<-mat%>%group_by(sp,Year)%>%summarize(Number=mean(Number))%>%ungroup()
  
  tempo<-mat%>%complete(Year, nesting(sp),fill=list(Number=0))
    
  tempo2<-split(tempo,f=tempo$sp)
  spmat<-matrix(NA,nrow=length(yrs),ncol=length(tempo2))
  rownames(spmat)<-yrs
  colnames(spmat)<-names(tempo2)
  for(n in 1:length(tempo2)){
    spmat[,n]<-tempo2[[n]]$Number # note: this are all common sp, 
                                  # all rare sp are excluded during data wrangling
  }
  
  saveRDS(spmat,paste(resloc,"inputmat_for_tailanal.RDS",sep=""))
  
}