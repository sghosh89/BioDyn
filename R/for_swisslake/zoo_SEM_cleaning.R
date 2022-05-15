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
# lake sempachersee
l4z_sem<-read.csv(paste(path,"zoo_SEM.csv",sep=""),sep=";")

#===================================================================================================
#------------------ lake sem: 36 years of data (1972-2014) ------------------
#===================================================================================================

unique(l4z_sem$site) # 2 sites

# max depth of lake sem is 87 m @wiki

unique(l4z_sem$upper_depth) # then the depth must be in cm
unique(l4z_sem$lower_depth)

unique(l4z_sem$unit)

# Francesko suggests to keep site 2C01 
# (most probably this site is consistent with his chemistry
#   and phytoplannkton data and also it is most frequently sampled site)

l4z_sem_2C01<-l4z_sem%>%filter(site=="'2C01'")
l4z_sem_2C01<-l4z_sem_2C01%>%separate(date, c("year","month","day"), "-")
l4z_sem_2C01<-l4z_sem_2C01%>%filter(month%in%c("06","07","08","09"))

l4z_sem_2C01_m2<-l4z_sem_2C01%>%filter(unit=="1/m2") # but all taxon 9999 are useless - unidentified
l4z_sem_2C01_m3<-l4z_sem_2C01%>%filter(unit=="1/m3") # convert into l
l4z_sem_2C01_l<-l4z_sem_2C01%>%filter(unit=="1/l") # don't do anything

# Francesko commented 1984 was weird
# overwrite the value and unit
l4z_sem_2C01_m3$value<-l4z_sem_2C01_m3$value / 1000
l4z_sem_2C01_m3$unit<-"1/l"

l4z_sem_2C01_modified<-rbind(l4z_sem_2C01_m3,l4z_sem_2C01_l)

# is every year uniformly sampled? yes 
c<-l4z_sem_2C01_modified%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()

# is every month uniformly sampled? no (1-5 times)
c1<-l4z_sem_2C01_modified%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# for continuous year we consider from 1985
l4z_sem_2C01_modified<-l4z_sem_2C01_modified%>%filter(year>=1985)
l4z_sem_2C01_modified<-inner_join(l4z_sem_2C01_modified,xm,by=c("taxon"="id_CH"))
l4z_sem_2C01_modified$genus_sp<-paste(l4z_sem_2C01_modified$genus,l4z_sem_2C01_modified$species)

# for site 2C01
syr<-length(unique(l4z_sem_2C01_modified$year))
blake_z_sem_2C01<-l4z_sem_2C01_modified%>%group_by(genus_sp)%>%summarise(present_yr=n_distinct(year),
                                                                         sampled_yr=syr)%>%ungroup()
blake_z_sem_2C01<-blake_z_sem_2C01[-1,]
write.csv(blake_z_sem_2C01,paste(resloc_z,"splist_z_l4zsem_site2C01.csv",sep=""),row.names = F)

splist_l4sem<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l4zsem_site2C01_BM_SG.csv")
splist_l4sem<-splist_l4sem%>%filter(include==1)
splist_l4sem$aggregate[c(3:6,24,29:31,36,38:39)]<-splist_l4sem$genus_sp[c(3:6,24,29:31,36,38:39)]

l4z_sem_2C01_modified<-l4z_sem_2C01_modified%>%filter(genus_sp%in%splist_l4sem$genus_sp)
l4z_sem_2C01_modified<-inner_join(l4z_sem_2C01_modified,splist_l4sem,by="genus_sp")%>%
  select(year,month,day,value,sp=aggregate)

# is every month uniformly sampled? yes (1-5 days in month)
c1<-l4z_sem_2C01_modified%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# is every year uniformly sampled? yes (2-4 months in year)
c1<-l4z_sem_2C01_modified%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()
unique(c1$nm)

badyr<-c1$year[which(c1$nm<4)] # for these years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort

l4z_sem_2C01_modified<-l4z_sem_2C01_modified%>%filter(year%notin%badyr)

spmat<-l4z_sem_2C01_modified%>%group_by(year,month,sp)%>%summarise(val=mean(value))%>%ungroup()

spmat<-spmat%>%group_by(year,sp)%>%summarise(mean_val=mean(val))%>%ungroup()%>%
  spread(sp,mean_val,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$year
spmat<-spmat[,-1]

saveRDS(spmat,
        paste(resloc_z,"allspmat_zoo_SEM.RDS",sep=""))

presentyr<-apply(spmat,MARGIN = 2,FUN = function(x){sum(x!=0)})
commonsp<-which(presentyr>=0.7*nrow(spmat)) # commonsp present 70% of sampling year 
raresp<-which(presentyr<0.7*nrow(spmat))

commonspmat<-spmat[,commonsp]

if(length(raresp!=0)){
  rarespmat<-spmat[,raresp]
  commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
}

saveRDS(commonspmat,
        paste(resloc_z,"input_mat_for_tail_analysis_zoo_SEM.RDS",sep=""))








