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
# lake  hallwilersee
l5z_hal<-read.csv(paste(path,"zoo_HAL.csv",sep=""),sep=";")

#===================================================================================================
#------------------ lake hal: 32 years of data (1982-2014) ------------------
#===================================================================================================

unique(l5z_hal$site) # 1 sites

unique(l5z_hal$upper_depth) # depths are in cm
unique(l5z_hal$lower_depth)

l5z_hal<-l5z_hal%>%separate(date, c("year","month","day"), "-")
l5z_hal<-l5z_hal%>%filter(month%in%c("06","07","08","09"))

l5z_hal_m2<-l5z_hal%>%filter(unit%in%"1/m2")
l5z_hal_m3<-l5z_hal%>%filter(unit%in%"1/m3")
l5z_hal_l<-l5z_hal%>%filter(unit%in%"1/l") # do nothing

# convert into unit/l
l5z_hal_m3$value<-l5z_hal_m3$value/1000
l5z_hal_m3$unit<-"1/l"

# convert into unit/l
l5z_hal_m2$diffdepth<-(l5z_hal_m2$lower_depth - l5z_hal_m2$upper_depth)/100 # cm to m
l5z_hal_m2$value<-(l5z_hal_m2$value / l5z_hal_m2$diffdepth)/1000
l5z_hal_m2$unit<-"1/l"

l5z_hal_mod<-rbind(l5z_hal_m2[,1:10],l5z_hal_m3,l5z_hal_l)

# is every year uniformly sampled? yes 
c<-l5z_hal_mod%>%group_by(year)%>%summarize(nm=n_distinct(month))%>%ungroup()
unique(c$nm)

# is every month uniformly sampled? no (1-5 times)
c1<-l5z_hal_mod%>%group_by(year,month)%>%summarize(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

l5z_hal_mod<-inner_join(l5z_hal_mod,xm,by=c("taxon"="id_CH"))
l5z_hal_mod$genus_sp<-paste(l5z_hal_mod$genus,l5z_hal_mod$species)

# note year = 1982, 1985 to 2014, so you may exclude the 1982 because of continuous data
syr<-length(unique(l5z_hal_mod$year))
blake_z_hal<-l5z_hal_mod%>%group_by(genus_sp)%>%summarize(present_yr=n_distinct(year),
                                                          sampled_yr=syr)%>%ungroup()
blake_z_hal<-blake_z_hal[-1,]
write.csv(blake_z_hal,paste(resloc_z,"splist_z_l5z_hal.csv",sep=""),row.names = F)

splist_l5hal<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l5z_hal_BM_SG.csv")
splist_l5hal<-splist_l5hal%>%filter(include==1)
splist_l5hal$aggregate[c(3,8:9,25,31:34,37,40:41)]<-splist_l5hal$genus_sp[c(3,8:9,25,31:34,37,40:41)]

l5z_hal_mod<-l5z_hal_mod%>%filter(genus_sp%in%splist_l5hal$genus_sp)
l5z_hal_mod<-inner_join(l5z_hal_mod,splist_l5hal,by="genus_sp")%>%
  select(year,month,day,value,sp=aggregate)

# is every year uniformly sampled? no
c<-l5z_hal_mod%>%group_by(year)%>%summarize(nm=n_distinct(month))%>%ungroup()

badyr<-c$year[which(c$nm<4)] # for consistent sampling effort

# is every month uniformly sampled? no (1-4 times)
c1<-l5z_hal_mod%>%group_by(year,month)%>%summarize(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# for those bad years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort

l5z_hal_mod<-l5z_hal_mod%>%filter(year%notin%badyr)

spmat<-l5z_hal_mod%>%group_by(year,month,sp)%>%summarize(val=mean(value))%>%ungroup()
spmat<-spmat%>%group_by(year,sp)%>%summarize(mean_val=mean(val))%>%ungroup()%>%
  spread(sp,mean_val,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$year
spmat<-spmat[,-1]

presentyr<-apply(spmat,MARGIN = 2,FUN = function(x){sum(x!=0)})
commonsp<-which(presentyr>=0.7*nrow(spmat)) # commonsp present 70% of sampling year 
raresp<-which(presentyr<0.7*nrow(spmat))

commonspmat<-spmat[,commonsp]

if(length(raresp!=0)){
  rarespmat<-spmat[,raresp]
  commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
}

saveRDS(commonspmat,
        paste(resloc_z,"input_mat_for_tail_analysis_zoo_HAL.RDS",sep=""))













