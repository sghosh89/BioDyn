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

tt<-cc%>%group_by(uid3)%>%summarize(nuid3=n(),  
                                    nuid0=n_distinct(uid0), # this must be 1 as it is higher taxonomic level than species
                                    nuid1=n_distinct(uid1), # this must be 1
                                    nuid2=n_distinct(uid2))%>% # this must be 1
  ungroup()%>%arrange(desc(nuid3))

# get the species level resolution 
tt_splevel<-tt%>%filter(nuid3>=nyr_thrs)
cc_splevel<-cc%>%filter(uid3%in%tt_splevel$uid3)

# update cc, tt
cc<-cc%>%filter(uid3%notin%tt_splevel$uid3)
tt<-cc%>%group_by(uid2)%>%summarize(nuid2=n(),  
                                    nuid0=n_distinct(uid0), # this must be 1 as it is higher taxonomic level than species
                                    nuid1=n_distinct(uid1))%>% # this must be 1
  ungroup()%>%arrange(desc(nuid2))

# get the genus level resolution 
tt_genuslevel<-tt%>%filter(nuid2>=nyr_thrs)
cc_genuslevel<-cc%>%filter(uid2%in%tt_genuslevel$uid2)

# update cc, tt
cc<-cc%>%filter(uid2%notin%tt_genuslevel$uid2)
tt<-cc%>%group_by(uid1)%>%summarize(nuid1=n(),  
                                    nuid0=n_distinct(uid0))%>% # this must be 1 as it is higher taxonomic level than species
                          ungroup()%>%arrange(desc(nuid1))

# get the subfamily level resolution 
tt_sfamlevel<-tt%>%filter(nuid1>=nyr_thrs)
cc_sfamlevel<-cc%>%filter(uid1%in%tt_sfamlevel$uid1)

# the rest extra left upto family level
cc_extra<-cc%>%filter(uid1%notin%tt_sfamlevel$uid1)
tt_extra<-cc_extra%>%group_by(uid0)%>%summarize(nuid0=n(),
                                                nuid1=n_distinct(uid1))%>%ungroup()%>%
                                  arrange(desc(nuid0))
# update
tt<-tt_extra%>%filter(nuid0>=nyr_thrs)
cc_famlevel<-cc_extra%>%filter(uid0%in%tt$uid0)

# the rest is left for aggregation if you want
cc_extra<-cc_extra%>%filter(uid0%notin%tt$uid0)


# okay, now combine all data
cc_all<-rbind(cc_famlevel,cc_sfamlevel,cc_genuslevel,cc_splevel)
class(cc_all)

cc_all<-cc_all%>%select(-c(uid0,uid1,uid2,uid3))
cc_extra<-cc_extra%>%select(-c(uid0,uid1,uid2,uid3))

write.csv(cc_all,"./Datasource_ID_1388_Plot_ID_1049_screeneddata.csv",row.names = F)
write.csv(cc_extra,"./Datasource_ID_1388_Plot_ID_1049_leftoverdata.csv",row.names = F)











