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
# lake  greifensee
l6z_gre<-read.csv(paste(path,"zoo_GRE.csv",sep=""),sep=";")

#===================================================================================================
#------------------ lake greifensee: 48 years of data (1960-2015) ------------------
#===================================================================================================

unique(l6z_gre$site) # 1 site
unique(l6z_gre$upper_depth) # in cm
unique(l6z_gre$lower_depth)
unique(l6z_gre$unit)

l6z_gre$date<-as.Date(l6z_gre$date,format = "%d.%m.%Y")
l6z_gre<-l6z_gre%>%separate(date, c("year","month","day"), "-")

l6z_gre<-l6z_gre%>%filter(month%in%c("06","07","08","09"))

unique(l6z_gre$unit)

# convert from 1/m2 to 1/l
l6z_gre_m2<-l6z_gre%>%filter(unit=="1/m2")
l6z_gre_m2$diffdepth<-(l6z_gre_m2$lower_depth - l6z_gre_m2$upper_depth)/100 # cm to m
l6z_gre_m2$value<-(l6z_gre_m2$value / l6z_gre_m2$diffdepth) / 1000
l6z_gre_m2$unit<-"1/l"

# convert from 1/m3 to 1/l
l6z_gre_m3<-l6z_gre%>%filter(unit=="1/m3")
l6z_gre_m3$value<-l6z_gre_m3$value /1000
l6z_gre_m3$unit<-"1/l"

# convert from 1/l to 1/m2
l6z_gre_l<-l6z_gre%>%filter(unit=="1/l")

l6z_gre_mod<-rbind(l6z_gre_m2[,1:10],l6z_gre_m3,l6z_gre_l)
l6z_gre_mod<-inner_join(l6z_gre_mod,xm,by=c("taxon"="id_CH"))
l6z_gre_mod$genus_sp<-paste(l6z_gre_mod$genus,l6z_gre_mod$species)

# is every year uniformly sampled? no
c<-l6z_gre_mod%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()

# for continuous and consistent sampling
#l6z_gre_mod<-l6z_gre_mod%>%filter(year>=1973)

# is every month uniformly sampled? no (1-4 times)
c1<-l6z_gre%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

syr<-length(unique(l6z_gre_mod$year))
blake_z_gre<-l6z_gre_mod%>%group_by(genus_sp)%>%summarise(present_yr=n_distinct(year),
                                                          sampled_yr=syr)%>%ungroup()
blake_z_gre<-blake_z_gre[-1,]
write.csv(blake_z_gre,paste(resloc_z,"splist_z_l6z_gre.csv",sep=""),row.names = F)

#--------------------------
splist_l6gre<-read.csv("../../DATA/for_swisslake/wrangled_data/zooplankton/splist_z_l6z_gre_BM_SG.csv")
splist_l6gre<-splist_l6gre%>%filter(include==1)
splist_l6gre$aggregate[c(3,6:8,11:12,22,27,33,36:37,42,47:49)]<-splist_l6gre$genus_sp[c(3,6:8,11:12,22,27,33,36:37,42,47:49)]

l6z_gre_mod<-l6z_gre_mod%>%filter(genus_sp%in%splist_l6gre$genus_sp)
l6z_gre_mod<-inner_join(l6z_gre_mod,splist_l6gre,by="genus_sp")%>%
  select(year,month,day,value,sp=aggregate)

# is every year uniformly sampled? no
c<-l6z_gre_mod%>%group_by(year)%>%summarise(nm=n_distinct(month))%>%ungroup()

badyr<-c$year[which(c$nm<4)] # for consistent sampling effort

# is every month uniformly sampled? no (1-4 times)
c1<-l6z_gre_mod%>%group_by(year,month)%>%summarise(nd=n_distinct(day))%>%ungroup()
unique(c1$nd)

# for those bad years, all 4 months are not sampled, 
# so we need to exclude these years for consistent sampling effort

l6z_gre_mod<-l6z_gre_mod%>%filter(year%notin%badyr)

spmat<-l6z_gre_mod%>%group_by(year,month,sp)%>%summarise(val=mean(value))%>%ungroup()

spmat<-spmat%>%group_by(year,sp)%>%summarise(mean_val=mean(val))%>%ungroup()%>%
  spread(sp,mean_val,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$year
spmat<-spmat[,-1]

saveRDS(spmat,
        paste(resloc_z,"allspmat_zoo_GRE.RDS",sep=""))

presentyr<-apply(spmat,MARGIN = 2,FUN = function(x){sum(x!=0)})
commonsp<-which(presentyr>=0.7*nrow(spmat)) # commonsp present 70% of sampling year 
raresp<-which(presentyr<0.7*nrow(spmat))

commonspmat<-spmat[,commonsp]

if(length(raresp!=0)){
  rarespmat<-spmat[,raresp]
  commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
}

saveRDS(commonspmat,
        paste(resloc_z,"input_mat_for_tail_analysis_zoo_GRE.RDS",sep=""))










