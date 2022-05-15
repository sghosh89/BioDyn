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
# lake vierwaldstättersee/ lake lucerne
l3z_lu<-read.csv(paste(path,"zoo_vws.csv",sep=""),sep=";")

#===================================================================================================
#------------------ lake lucerne: 55 years of data (1960-2014) ------------------
#===================================================================================================

unique(l3z_lu$site) #5 sites
l3z_lu$date<-as.Date(l3z_lu$date,format = "%d.%m.%Y")
l3z_lu<-l3z_lu%>%separate(date, c("year","month","day"), "-")

# lake lucerne max depth 214m @wiki
unique(l3z_lu$lower_depth) # should I consider this info?
unique(l3z_lu$upper_depth)

l3z_lu<-l3z_lu%>%filter(month%in%c("06","07","08","09"))
#l3z_lu<-na.omit(l3z_lu) # omitting rows with no info about lower depth

# there are 5 sites but Francesko chose 3A01 (date > 1967-12-14) and site 3A04 (date > 1974-07-16)
table(l3z_lu$site) # ithink because those two sites are sampled more than the others
l3z_lu<-l3z_lu%>%filter(site%in%c("'3A01'","'3A04'"))

unique(l3z_lu$unit) #3 different units: per L, per m^3, per m^2
# we want to convert them into per l
id<-which(l3z_lu$unit=="1/m2") 
l3z_lu_m2<-l3z_lu%>%filter(unit=="1/m2")
l3z_lu_m2$diff_depth<-(l3z_lu_m2$lower_depth - l3z_lu_m2$upper_depth)/100 # cm to m
l3z_lu_m2$value<-(l3z_lu_m2$value / l3z_lu_m2$diff_depth )/1000
l3z_lu_m2$unit<-"1/l" # now it is in unit: /l


id<-which(l3z_lu$unit=="1/m3") # need to convert them into 1/l unit
l3z_lu_m3<-l3z_lu%>%filter(unit=="1/m3")
l3z_lu_m3$value<-l3z_lu_m3$value / 1000
l3z_lu_m3$unit<-"1/l" # now it is in unit: /l


l3z_lu_l<-l3z_lu%>%filter(unit=="1/l") 

l3z_lu_l<-rbind(l3z_lu_l,l3z_lu_m2[,1:10],l3z_lu_m3) # all in same unit
l3z_lu_l<-l3z_lu_l[order(l3z_lu_l$year,l3z_lu_l$month,l3z_lu_l$day,l3z_lu_l$time),]
l3z_lu_l<-inner_join(l3z_lu_l,xm,by=c("taxon"="id_CH"))
l3z_lu_l$genus_sp<-paste(l3z_lu_l$genus,l3z_lu_l$species)

l3z_lu_l_3A01<-l3z_lu_l%>%filter(site=="'3A01'")
l3z_lu_l_3A04<-l3z_lu_l%>%filter(site=="'3A04'")

# for site 3A01
syr<-length(unique(l3z_lu_l_3A01$year))
blake_z_lu_3A01<-l3z_lu_l_3A01%>%group_by(genus_sp)%>%summarise(present_yr=n_distinct(year),
                                                                sampled_yr=syr)%>%ungroup()
blake_z_lu_3A01<-blake_z_lu_3A01[-1,]
write.csv(blake_z_lu_3A01,paste(resloc_z,"splist_z_l3zlu_site3A01.csv",sep=""),row.names = F)


# for site 3A04
syr<-length(unique(l3z_lu_l_3A04$year))
blake_z_lu_3A04<-l3z_lu_l_3A04%>%group_by(genus_sp)%>%summarise(present_yr=n_distinct(year),
                                                                sampled_yr=syr)%>%ungroup()
blake_z_lu_3A04<-blake_z_lu_3A04[-1,]
write.csv(blake_z_lu_3A04,paste(resloc_z,"splist_z_l3zlu_site3A04.csv",sep=""),row.names = F)

###############################################################################################
#                              site 3A01
###############################################################################################
splist_l3lu_A301_BM<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l3zlu_site3A01_BM_SG.csv")
splist_l3lu_A301_BM<-splist_l3lu_A301_BM%>%filter(include==1)
splist_l3lu_A301_BM$aggregation[c(5,19,22:23,30:31)]<-splist_l3lu_A301_BM$genus_sp[c(5,19,22:23,30:31)]

l3z_lu_l_3A01<-l3z_lu_l_3A01%>%filter(genus_sp%in%splist_l3lu_A301_BM$genus_sp)
l3z_lu_l_3A01<-inner_join(l3z_lu_l_3A01,splist_l3lu_A301_BM,by=c("genus_sp"))%>%
  select(year,month,day,value,sp=aggregation)

# is every month uniformly sampled? (1-5 days in month)
c1<-l3z_lu_l_3A01%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# is every year uniformly sampled? yes (3-4 months in year)
c1<-l3z_lu_l_3A01%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()
unique(c1$nm)

badyr<-c1$year[which(c1$nm<4)] # for consistent sampling effort exclude the year which has not all 4 months sampled

spmat<-l3z_lu_l_3A01%>%filter(year%notin%badyr)%>%
          group_by(year,month,sp)%>%summarise(val=mean(value))%>%ungroup()

spmat<-spmat%>%group_by(year,sp)%>%summarise(mean_val=mean(val))%>%ungroup()%>%
  spread(sp,mean_val,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$year
spmat<-spmat[,-1]

saveRDS(spmat,
        paste(resloc_z,"allspmat_zoo_LU.RDS",sep=""))

presentyr<-apply(spmat,MARGIN = 2,FUN = function(x){sum(x!=0)})
commonsp<-which(presentyr>=0.7*nrow(spmat)) # commonsp present 70% of sampling year 
raresp<-which(presentyr<0.7*nrow(spmat))

commonspmat<-spmat[,commonsp]

if(length(raresp!=0)){
  rarespmat<-spmat[,raresp]
  commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
}

saveRDS(commonspmat,
        paste(resloc_z,"input_mat_for_tail_analysis_zoo_LU_site3A01.RDS",sep=""))

###############################################################################################
#                              site 3A04
###############################################################################################
splist_l3lu_A304_BM<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l3zlu_site3A04_BM.csv")
splist_l3lu_A304_BM<-splist_l3lu_A304_BM%>%filter(include==1)
splist_l3lu_A304_BM$aggregation[c(2:6,10:18,21:22)]<-splist_l3lu_A304_BM$genus_sp[c(2:6,10:18,21:22)]

l3z_lu_l_3A04<-l3z_lu_l_3A04%>%filter(genus_sp%in%splist_l3lu_A304_BM$genus_sp)
l3z_lu_l_3A04<-inner_join(l3z_lu_l_3A04,splist_l3lu_A304_BM,by=c("genus_sp"))%>%
  select(year,month,day,value,sp=aggregation)

# is every month uniformly sampled? (1-4 days in month)
c1<-l3z_lu_l_3A04%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# is every year uniformly sampled? yes (2-4 months in year)
c1<-l3z_lu_l_3A04%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()
unique(c1$nm)

# don't consider site 3A04 as it has only 1998-2014 consistent years and they are only 17 years

