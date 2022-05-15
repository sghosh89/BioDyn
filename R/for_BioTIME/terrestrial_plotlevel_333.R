rm(list=ls())
source("tail_analysis.R")
source("monthly_rarefy.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)
xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
grid_terres<-readRDS("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/bt_terres_min20yr_rawdata.RDS")
df<-readRDS("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map.RDS")
df<-df%>%filter(site==333)

# single lat-lon is reported, sampled for different monthly freq in year

#----------- create result folder for wrangle data -------------------------
resloc<-"../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/333/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------------------------------------------

site<-df$site
x<-grid_terres%>%filter(STUDY_ID==site)
newsite<-site
unique(x$MONTH)

# Now, create folder for all these newsite
if(length(newsite)>1){
  for(k in 1:length(newsite)){
    resloc2<-paste("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/333/",newsite[k],"/",sep="")
    if(!dir.exists(resloc2)){
      dir.create(resloc2)
    }
  }
}

#-------------------------------------------------------------------------------------------------------------
# sometimes months have different multiple sampling dates within a year
# so, take the average
x$Abundance<-as.numeric(x$Abundance)
x$Biomass<-as.numeric(x$Biomass)
x<-x%>%group_by(YEAR,MONTH,Species)%>%
  summarise(Abundance=mean(Abundance,na.rm=T),
            Biomass=mean(Biomass,na.rm=T),
            ABUNDANCE_TYPE=unique(ABUNDANCE_TYPE))%>%ungroup()
# NOTE: ABUNDANCE TYPE should be kept as it is - if NA then keep NA
#------------------------------------------------------------------------------------------------------------

newsite_bad<-c()


if(length(newsite)>1){
  x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_PLOT_",PLOT,sep=""))
  newsite<-sort(unique(x$newsite))
  
  # check if each newsite visited for >20 years?
  tt<-x%>%group_by(newsite)%>%summarise(n=n_distinct(YEAR))%>%ungroup()
  
  # include sites which are sampled > 20 years
  tt<-tt%>%filter(n>=20)
  x_allsite<- x %>% filter(newsite %in% tt$newsite)
  newsite<-tt$newsite
}else{
  x<-grid_terres%>%filter(STUDY_ID==site)
  newsite<-site
  x_allsite<-x
}


for(k in 1:length(newsite)){
  
  x<-x_allsite%>%filter(newsite==newsite[k])
  
  # do not consider these unknown sp into analysis
  x<-x%>%filter(Species%notin%c("Unknown","Unknown "))
  
  t0<-x%>%group_by(YEAR)%>%summarise(nm=n_distinct(MONTH))%>%ungroup()
  #t1<-x%>%group_by(YEAR,MONTH)%>%summarise(nd=n_distinct(DAY))%>%ungroup()
  
  #---------- ok, after seeing t0, we need to rarefy --------------
  min_samp<-min(t0$nm) # min months sampled each year
  cat("---------- min_samp = ",min_samp," , newsite = ",newsite[k]," ------------------- \n")
  need_rarefy<-length(unique(t0$nm))>1
  
  AB<-is.na(x$ABUNDANCE_TYPE)[1]
  if(AB==F){
    field<-"Abundance"
  }else{
    field<-"Biomass"
  }
  
  id<-which(colnames(x)==field)
  
  if(need_rarefy==T){
    study<-x%>%dplyr::select(MONTH,YEAR,Species,Value=id)
    x_c<-monthly_rarefy(study = study,resamples = 100,field = field)
  }else{
    x<-x%>%dplyr::select(YEAR,Species,Value=id)
    x<-x%>%group_by(Species,YEAR)%>%
      dplyr::summarise(Value=mean(Value))%>%ungroup()
    c1<-x%>%group_by(Species)%>%summarise(n_distinct(YEAR))%>%ungroup() 
    # As all species are not found each year, we need to fill in the missing values with 0.
    x_c<-x %>% 
      complete(Species, 
               nesting(YEAR), 
               fill = list(Value = 0))
  }
  
  #-----------------------------------------------------------------
  xmat<-x_c%>%spread(Species, Value)
  year<-xmat$YEAR
  xmat<-as.matrix(xmat[,-1])
  rownames(xmat)<-year
  
  xmeta<-xxm%>%filter(STUDY_ID==site)
  
  input_sp<-list(spmat=xmat,meta=xmeta)
  
  if(length(newsite)>1){
    resloc<-paste("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/333/",newsite[k],"/",sep="")
  }else{
    resloc<-"../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/333/"
  }
  
  saveRDS(input_sp,paste(resloc,"spmat.RDS",sep=""))
  
  #----------- saving input spmat for tailanal ---------------------
  m<-readRDS(paste(resloc,"spmat.RDS",sep=""))
  # first we aggregated the rare sp (present even less than 30% of sampled years) into a pseudo sp 
  presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
  presentyr<-unname(presentyr)
  rareid<-which(presentyr<=0.3*nrow(m$spmat)) # rare sp = present less than 30% of sampled year
  
  allraresp<-ncol(m$spmat)==length(rareid) # that means not all sp are rare
  
  if(allraresp==T){
    
    newsite_bad<-c(newsite_bad,newsite[k])
    
  }else{
    if(length(rareid)!=0){
      raresp<-m$spmat[,rareid]
      raresp<-as.matrix(raresp) # this line is for when you have only one rare sp
      raresp<-apply(X=raresp,MARGIN=1,FUN=sum)
      m1<-m$spmat[,-rareid]
      m1<-cbind(m1,raresp=raresp)
      m1<-as.data.frame(m1)
      input_tailanal<-m1
    }else{
      m1<-m$spmat
      input_tailanal<-m1
    }
    saveRDS(input_tailanal,paste(resloc,"input_tailanal.RDS",sep=""))
    
    #----------------- now do tail analysis ----------------------
    resloc2<-paste("../../Results/for_BioTIME/Terrestrial_plotlevel/",site,"/",sep="")
    if(!dir.exists(resloc2)){
      dir.create(resloc2)
    }
    
    #----------- analysis with covary sp ----------------
    if(length(newsite)>1){
      resloc<-paste(resloc2,newsite[k],"/",sep="")
    }else{
      resloc<-resloc2
    }
    
    if(!dir.exists(resloc)){
      dir.create(resloc)
    }
    res<-tail_analysis(mat = input_tailanal, resloc = resloc, nbin = 2)
  }
  cat("---------- k = ",k,"-----------\n")
}


#--------------------------------------------------------
newsite<-setdiff(newsite,newsite_bad)
saveRDS(newsite,"../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/333/newsite.RDS")
