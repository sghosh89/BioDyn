library(tidyverse)
`%notin%` <- Negate(`%in%`)
#-----------------------------------------------------------------------------
resloc_z<-"../../DATA/for_swisslake/wrangled_data/zooplankton/"
if(!dir.exists(resloc_z)){
  dir.create(resloc_z)
}
#----------------------------------------------------------------------------
# read zooplankton data

path<-"../../DATA/for_swisslake/raw_data/SwissLakes_data_FP11dec2020/raw_data/zooplankton/"
xm<-read.csv(paste(path,"zooplankton_MD.csv",sep=""),sep=";")
xm<-xm%>%select(id_CH,empire,kingdom,phylum,
                class,order,family,genus,species,stage,
                affiliation,synonyms,comments,size_category)
#---------------------------------------------------------------------
# lake  baldeggersee
l7z_bal<-read.csv(paste(path,"zoo_BAL.csv",sep=""),sep=";")

#===================================================================================================
#------------------ lake bal: 34 years of data (1976-2014) ------------------
#===================================================================================================

unique(l7z_bal$site) # 1 site
unique(l7z_bal$upper_depth) # in cm
unique(l7z_bal$lower_depth)

l7z_bal<-l7z_bal%>%separate(date, c("year","month","day"), "-")
l7z_bal<-l7z_bal%>%filter(month%in%c("06","07","08","09"))

unique(l7z_bal$unit)


# convert from 1/m3 to 1/l
l7z_bal_m3<-l7z_bal%>%filter(unit=="1/m3")
l7z_bal_m3$value<-l7z_bal_m3$value /1000
l7z_bal_m3$unit<-"1/l"

# convert from 1/l to 1/m2
l7z_bal_l<-l7z_bal%>%filter(unit=="1/l")


l7z_bal_mod<-rbind(l7z_bal_m3,l7z_bal_l)
l7z_bal_mod<-inner_join(l7z_bal_mod,xm,by=c("taxon"="id_CH"))
l7z_bal_mod$genus_sp<-paste(l7z_bal_mod$genus,l7z_bal_mod$species)

# is every year uniformly sampled? no
c<-l7z_bal_mod%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()

# for continuous and consistent sampling
l7z_bal_mod<-l7z_bal_mod%>%filter(year>=1977)

# is every month uniformly sampled? no (1-5 times)
c1<-l7z_bal_mod%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

syr<-length(unique(l7z_bal_mod$year))
blake_z_bal<-l7z_bal_mod%>%group_by(genus_sp)%>%summarise(present_yr=n_distinct(year),
                                                          sampled_yr=syr)%>%ungroup()
blake_z_bal<-blake_z_bal[-1,]
write.csv(blake_z_bal,paste(resloc_z,"splist_z_l7z_bal.csv",sep=""),row.names = F)

#--------------------------------------------------------------------------------------------
splist_l7bal<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l7z_bal_BM_SG.csv")
splist_l7bal<-splist_l7bal%>%filter(include==1)
splist_l7bal$aggregate[c(3,6:8,27,35:36,47:48)]<-splist_l7bal$genus_sp[c(3,6:8,27,35:36,47:48)]

l7z_bal_mod<-l7z_bal_mod%>%filter(genus_sp%in%splist_l7bal$genus_sp)
l7z_bal_mod<-inner_join(l7z_bal_mod,splist_l7bal,by="genus_sp")%>%
  select(year,month,day,value,sp=aggregate)

# is every year uniformly sampled? no
c<-l7z_bal_mod%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()

badyr<-c$year[which(c$nm<4)] # for consistent sampling effort

# is every month uniformly sampled? no (1-4 times)
c1<-l7z_bal_mod%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# for those bad years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort

l7z_bal_mod<-l7z_bal_mod%>%filter(year%notin%badyr)

spmat<-l7z_bal_mod%>%group_by(year,month,sp)%>%summarise(val=mean(value))%>%ungroup()

spmat<-spmat%>%group_by(year,sp)%>%summarise(mean_val=mean(val))%>%ungroup()%>%
  spread(sp,mean_val,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$year
spmat<-spmat[,-1]

saveRDS(spmat,
        paste(resloc_z,"allspmat_zoo_BAL.RDS",sep=""))

presentyr<-apply(spmat,MARGIN = 2,FUN = function(x){sum(x!=0)})
commonsp<-which(presentyr>=0.7*nrow(spmat)) # commonsp present 70% of sampling year 
raresp<-which(presentyr<0.7*nrow(spmat))

commonspmat<-spmat[,commonsp]

if(length(raresp!=0)){
  rarespmat<-spmat[,raresp]
  commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
}

saveRDS(commonspmat,
        paste(resloc_z,"input_mat_for_tail_analysis_zoo_BAL.RDS",sep=""))




























