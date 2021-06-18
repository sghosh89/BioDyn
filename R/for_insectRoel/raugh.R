rm(list=ls())
library(tidyverse)
`%notin%` <- Negate(`%in%`)
#------------------------------------------
xm<-read.csv("../../DATA/for_insectRoel/20yrFreshwater_Metadata.csv")
x<-readRDS("../../DATA/for_insectRoel/20yrFreshwaterData.rds")

#x$Genus_Species<-paste(x$Genus," ",x$Species,sep="")

# total 8 unique Datasource_ID
unique(xm$Datasource_ID)
# 6 datasources: 63(BioTIME)  478 1347 1388 1488 1520, two are not included in x : 1551, 1553
#------------------------------------------------------------------------------------------------
#x1<-x%>%filter(Datasource_ID==63)
#plot(x1$Year,x1$Rank,col=as.factor(x1$Genus_Species))
ranklevels<-x%>%distinct(Rank,.keep_all = T)%>%select(Level,Rank)%>%arrange(Rank)

#tt<-x%>%group_by(Datasource_ID)%>%summarize(n=n_distinct(Plot_ID))

#cc<-x%>%filter(Datasource_ID==1388, Plot_ID==1049)
#plot(cc$Rank~jitter(cc$Year,0.1),col=rgb(0,0,0,0.5))
#tt<-cc%>%group_by(Genus_Species,Year)%>%summarize(n=unique(Rank))%>%ungroup()
#================================================================================================
# we need to standardize the data so that the Genus_Species which 
# have consistent rank throughout the study periods 
# will be considered
# let's start with a toy example

sp<-as.data.frame(sort(unique(x$Species)))

cc<-x%>%filter(Datasource_ID==1388, Plot_ID==1049)
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

# okay, so which decision should we make?
cc$Species[cc$Species%in%
             c("",".")]<-"NA.species"

#cc$Species[cc$Species%in%
#             c("",".","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","sp1","sp2")]<-"NA.species"

unique(cc$Class)

write.csv(cc,"./Datasource_ID_1388_Plot_ID_1049_rawdata.csv",row.names = F)

# set unique identifier
cc$uid0<-paste(cc$Phylum,cc$Class,cc$Subclass,cc$Order,cc$Suborder,cc$Family,sep="_") # uid upto family level
cc$uid1<-paste(cc$uid0,cc$Subfamily,sep="_") # sequential addition of subfamily
cc$uid2<-paste(cc$uid1,cc$Genus,sep="_") # then add genus
cc$uid3<-paste(cc$uid2,cc$Species,sep="_") # then add species

nyr<-length(sort(unique(cc$Year)))
nyr_thrs<-nyr*0.7

# This table will summarize the taxonomic info
#tt<-cc%>%group_by(uid0)%>%summarize(nuid0=n(), # count of uid0 in 'cc' 
#                                    nuid1=n_distinct(uid1), # count of uid1 in 'cc' nested under uid0
#                                    nuid2=n_distinct(uid2), # count of uid2 in 'cc' nested under uid0
#                                    nuid3=n_distinct(uid3))%>% # count of uid3 in 'cc' nested under uid0
#                        ungroup()%>%arrange(desc(nuid0))


tt<-cc%>%group_by(uid3)%>%summarize(nyr=n_distinct(Year),
                                    nuid3=n(),  
                                    nuid0=n_distinct(uid0), # this must be 1 as it is higher taxonomic level than species
                                    nuid1=n_distinct(uid1), # this must be 1
                                    nuid2=n_distinct(uid2))%>% # this must be 1
  ungroup()%>%arrange(desc(nuid3))
all(tt$nyr==tt$nuid3)==T

# get the species level resolution 
tt_splevel<-tt%>%filter(nuid3>=nyr_thrs)
cc_splevel<-cc%>%filter(uid3%in%tt_splevel$uid3)

cc_splevel<-arrange(cc_splevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
cc_splevel$selection<-"splevel"


# update cc, tt
cc<-cc%>%filter(uid3%notin%tt_splevel$uid3)
tt<-cc%>%group_by(uid2)%>%summarize(nyr=n_distinct(Year),
                                    nuid2=n(),  
                                    nuid0=n_distinct(uid0), # this must be 1 as it is higher taxonomic level than species
                                    nuid1=n_distinct(uid1))%>% # this must be 1
  ungroup()%>%arrange(desc(nuid2))
all(tt$nyr==tt$nuid2)==T # this is false 
# because sp1, sp2 both have same genus (uid2) so 1990, 1992, 1994 repeat
cc<-arrange(cc,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)

# get the genus level resolution 
tt_genuslevel<-tt%>%filter(nyr>=nyr_thrs)
cc_genuslevel<-cc%>%filter(uid2%in%tt_genuslevel$uid2)
cc_genuslevel<-cc_genuslevel%>%filter(Genus!="NA.genus")
cc_genuslevel_extra<-cc_genuslevel%>%filter(Genus=="NA.genus")
cc_genuslevel<-arrange(cc_genuslevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
cc_genuslevel$selection<-"genuslevel"


# update cc, tt
cc<-cc%>%filter(uid2%notin%tt_genuslevel$uid2)
cc<-rbind(cc,cc_genuslevel_extra)
tt<-cc%>%group_by(uid1)%>%summarize(nyr=n_distinct(Year),
                                    nuid1=n(),  
                                    nuid0=n_distinct(uid0))%>% # this must be 1 as it is higher taxonomic level than species
  ungroup()%>%arrange(desc(nuid1))
all(tt$nyr==tt$nuid1)==T # this is false 
cc<-arrange(cc,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)

# get the subfamily level resolution 
tt_sfamlevel<-tt%>%filter(nyr>=nyr_thrs)
cc_sfamlevel<-cc%>%filter(uid1%in%tt_sfamlevel$uid1)
cc_sfamlevel<-cc_sfamlevel%>%filter(Subfamily!="NA.subfamily")
cc_sfamlevel_extra<-cc_sfamlevel%>%filter(Subfamily=="NA.subfamily")
cc_sfamlevel<-arrange(cc_sfamlevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
cc_sfamlevel$selection<-"sfamlevel"

# the rest extra left upto family level
cc_extra<-cc%>%filter(uid1%notin%tt_sfamlevel$uid1)
cc_extra<-rbind(cc_extra,cc_sfamlevel_extra)
tt_extra<-cc_extra%>%group_by(uid0)%>%summarize(nyr=n_distinct(Year),
                                                nuid0=n(),
                                                nuid1=n_distinct(uid1))%>%ungroup()%>%
  arrange(desc(nuid0))
all(tt$nyr==tt$nuid0)==T # this is false 
cc_extra<-arrange(cc_extra,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)

# update
tt<-tt_extra%>%filter(nyr>=nyr_thrs)
cc_famlevel<-cc_extra%>%filter(uid0%in%tt$uid0)
cc_famlevel<-arrange(cc_famlevel,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
cc_famlevel$selection<-"famlevel"

# the rest is left for aggregation if you want
#cc_extra<-cc_extra%>%filter(uid0%notin%tt$uid0)
#cc_extra<-arrange(cc_extra,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)

# but now we need to further sort down based on family level - we can do genus level aggregation
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
c2$selection<-"famlevel_agg2genus"
#----------------- but keep if per family more than >= nyr_thrs obs was there -----------------------
#tempo<-c2%>%group_by(Family)%>%summarize(nyr=n_distinct(Year))%>%ungroup()
#tempo<-tempo%>%filter(nyr>=nyr_thrs)
#cc_famlevel_agg2sfam<-cc_famlevel_agg2sfam%>%filter(Family%in%tempo$Family)
#---------------------------------------------------------------------------------------

# okay, now combine all data
cc_all<-rbind(cc_splevel,cc_genuslevel,cc_sfamlevel,cc_famlevel_agg2sfam, c2)
cc_all<-arrange(cc_all,Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus,Species,Year)
nrow(cc_all)

class(cc_all)

#cc_all<-cc_all%>%select(-c(uid0,uid1,uid2,uid3))
#cc_extra<-cc_extra%>%select(-c(uid0,uid1,uid2,uid3))

write.csv(cc_all,"./Datasource_ID_1388_Plot_ID_1049_screeneddata_v2.csv",row.names = F)
write.csv(cc_extra,"./Datasource_ID_1388_Plot_ID_1049_leftoverdata_v2.csv",row.names = F)

#######################

#zz<-cc_extra%>%group_by(uid0)%>%summarize(nyr=n_distinct(Year),
#                                             nuid0=n(),  
#                                             nuid1=n_distinct(uid1), # this must be 1 as it is higher taxonomic level than species
#                                             nuid2=n_distinct(uid2))%>%ungroup()%>%
#  arrange(desc(nuid0))

