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
# lake Zurich
l2z_zh<-read.csv(paste(path,"zoo_ZH.csv",sep=""),sep=";")

#===================================================================================================
#------------------ lake zurich: 32 years of data (1977-2008) ------------------
#===================================================================================================

l2z_zh<-l2z_zh%>%separate(date, c("year","month","day"), "-")
unique(l2z_zh$year)

unique(l2z_zh$depth)# should I consider this info?

# is every year uniformly sampled? yes (except 1981 and 2006)
c<-l2z_zh%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()
l2z_zh<-l2z_zh%>%filter(year%notin%c(1981,2006))

# all in same unit: number/m^2
unique(l2z_zh$unit)

# consider the months: June to Sept
l2z_zh<-l2z_zh%>%filter(month%in%c("06","07","08","09"))

# is every month uniformly sampled? yes (1-2 times)
c1<-l2z_zh%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

#============================== unit conversion ===========================================
# max depth for zurich lake is 136m @wiki
# in the data the depth column is reported in m

# we want to convert the unit number/m2 to number/l

# depth in m
l2z_zh<-l2z_zh%>%separate(depth,c("upper_depth","lower_depth"),"_")
l2z_zh$lower_depth<-str_sub(l2z_zh$lower_depth[1:3], end=-2)
l2z_zh$upper_depth<-as.integer(l2z_zh$upper_depth)
l2z_zh$lower_depth<-as.integer(l2z_zh$lower_depth)

# a=number/m2 => b = a/(lower_depth - upper_depth) => b/1000 l
l2z_zh$value<-(l2z_zh$value/(l2z_zh$lower_depth - l2z_zh$upper_depth))/1000
l2z_zh$unit<-"number/l"

#=============================================================================================

l2z_zh<-l2z_zh%>%mutate("genus_sp"=paste(genus,species))
sp_m<-data.frame(sort(unique(l2z_zh$genus_sp)))
sp_m<-sp_m[-1,]
sp_m<-as.data.frame(sp_m)
sp_m$sampled_yr<-length(unique(l2z_zh$year)) # 31 year sampled: 1977-2005,2007,2008

blake_z_zh<-l2z_zh%>%group_by(genus_sp)%>%summarise(present_yr=n_distinct(year))%>%ungroup()
sp_m<-inner_join(sp_m,blake_z_zh,by=c("sp_m"="genus_sp"))
write.csv(sp_m,paste(resloc_z,"splist_z_l2zh.csv",sep=""),row.names = F)

splist_l2zh<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l2zh_BM_SG.csv")
splist_l2zh<-splist_l2zh%>%filter(include==1)
splist_l2zh$aggregation[4]<-splist_l2zh$sp_m[4]
splist_l2zh$aggregation[19]<-splist_l2zh$sp_m[19]
splist_l2zh$aggregation[22]<-splist_l2zh$sp_m[22]

l2z_zh<-l2z_zh%>%filter(genus_sp%in%splist_l2zh$sp_m)
l2z_zh<-inner_join(l2z_zh,splist_l2zh,by=c("genus_sp"="sp_m"))%>%
  select(year,month,day,value,sp=aggregation)

# is every month uniformly sampled? yes (1-2 days in month)
c1<-l2z_zh%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# is every year uniformly sampled? yes (3-4 months in year)
c1<-l2z_zh%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()
unique(c1$nm)

badyr<-c1$year[which(c1$nm<4)] # for consistent sampling effort: all 4 months should be sampled for each year

spmat<-l2z_zh%>%filter(year%notin%badyr)%>%
                group_by(year,month,sp)%>%summarise(val=mean(value))%>%ungroup()

spmat<-spmat%>%group_by(year,sp)%>%summarise(mean_val=mean(val))%>%ungroup()%>%
                spread(sp,mean_val,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$year
spmat<-spmat[,-1]

saveRDS(spmat,
        paste(resloc_z,"allspmat_zoo_ZH.RDS",sep=""))


presentyr<-apply(spmat,MARGIN = 2,FUN = function(x){sum(x!=0)})
commonsp<-which(presentyr>=0.7*nrow(spmat)) # commonsp present 70% of sampling year 
raresp<-which(presentyr<0.7*nrow(spmat))

commonspmat<-spmat[,commonsp]

if(length(raresp!=0)){
  rarespmat<-spmat[,raresp]
  commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
}

saveRDS(commonspmat,
        paste(resloc_z,"input_mat_for_tail_analysis_zoo_ZH.RDS",sep=""))




