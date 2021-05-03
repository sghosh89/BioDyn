# This file will search for bodymass and/ or bodysize for the target sp. used in this study
rm(list=ls())
library(tidyverse)
library(readxl)
#library(rfishbase)
#data(fishbase)
#===================
tgsp<-read.csv("../../Results/gather_res/targetspecies_alldata.csv")# length(unique(tgsp$species)) = 3543sp.
tgsp$species<-tolower(tgsp$species)
tgsp_uq<-tgsp%>%distinct(species,.keep_all = T)
#------------
# downloading fish trait data
#f<-rfishbase::length_weight("Oreochromis niloticus")
#f<-rfishbase::morphology("Oreochromis niloticus")
#-------------
# read traits data from the files Imran sent
t_BT<-read_excel("../../DATA/ImranSent/Biotime_body_size.xlsx") # unit in m
t_BT<-apply(t_BT,MARGIN=2,FUN=tolower)
t_BT<-t_BT%>%as.data.frame()%>%mutate(lengthunit="m")

t_Birds<-read.csv("../../DATA/ImranSent/Birds.csv")# adult_svl_cm
t_Birds<-apply(t_Birds,MARGIN=2,FUN=tolower)
t_Birds<-t_Birds%>%as.data.frame()%>%mutate(lengthunit="cm")

t_frsh_inv<-read_excel("../../DATA/ImranSent/Freshwater_invertebrates_body_size.xlsx")#size various unit
t_frsh_inv<-apply(t_frsh_inv,MARGIN=2,FUN=tolower)
t_frsh_inv<-t_frsh_inv%>%as.data.frame()%>%mutate(lengthunit=unit)

t_frsh_pl<-read_excel("../../DATA/ImranSent/freshwater_plants_biotime_body_size.xlsx")# size various unit
t_frsh_pl<-apply(t_frsh_pl,MARGIN=2,FUN=tolower)
t_frsh_pl<-t_frsh_pl%>%as.data.frame()%>%mutate(lengthunit=unit)

t_fish<-read_excel("../../DATA/ImranSent/morphology_fishbase_data.xlsx") # Length-Weight column in cm-gm 
t_fish$Genus_sp<-paste(t_fish$Genus,t_fish$Species)
t_fish<-apply(t_fish,MARGIN=2,FUN=tolower)
t_fish<-t_fish%>%as.data.frame()%>%mutate(lengthunit="cm")

#############################
nrow(tgsp_uq)
# Now join data
tgsp_uq1<-left_join(x=tgsp_uq,y=t_BT,by="species")
#nrow(tgsp_uq1)
tgsp_uq1<-rename(tgsp_uq1,bodysize=Body)
#sum(is.na(tgsp_uq1$bodysize))

# after you use left_join, sometimes if there are multiple matches between x and y, 
# all combinations of the matches are returned. and we should look for the multiple 
# matches manually.
x<-tgsp_uq1%>%group_by(species)%>%filter(n()>1) # so, these are the repeats

# I'm going to do a mean estimate of the bodysize for these repeats at the last stage
class(tgsp_uq1$bodysize)
tgsp_uq1$bodysize<-as.numeric(tgsp_uq1$bodysize)
xm<-tgsp_uq1%>%group_by(species)%>%summarize(meanbodysize=mean(bodysize,na.rm=T))%>%ungroup()
tgsp_uq1<-left_join(x=tgsp_uq,y=xm,by="species")

#nrow(tgsp_uq1) # 3543 unique species
#sum(is.na(tgsp_uq1$meanbodysize)) # unit in m
tgsp_uq1<-tgsp_uq1%>%mutate(lengthunit=ifelse(is.finite(meanbodysize),"m",NA))

# this is the table with unique sp. after matching from BioTIME data
tgsp_uq12<-tgsp_uq1%>%filter(is.na(meanbodysize))

# now match with birds
tgsp_uq12<-left_join(x=tgsp_uq12,y=t_Birds,by=c("species"="Best_guess_binomial"))
sum(!is.na(tgsp_uq12$Body_mass_g)) # 122 match found based on birds bodymass
sum(!is.na(tgsp_uq12$Adult_svl_cm)) # 28 match found based on birds bodysize

# ok, now merge meanbodysize and Adult_svl_cm into a single column
class(tgsp_uq12$Adult_svl_cm)
tgsp_uq12$Adult_svl_cm<-as.numeric(tgsp_uq12$Adult_svl_cm)
tgsp_uq12$meanbodysize<-coalesce(tgsp_uq12$meanbodysize,
                                   tgsp_uq12$Adult_svl_cm)
tgsp_uq12$lengthunit.x<-ifelse(is.finite(tgsp_uq12$Adult_svl_cm),
                               tgsp_uq12$lengthunit.y,
                               tgsp_uq12$lengthunit.x)
tgsp_uq12<-rename(tgsp_uq12,lengthunit=lengthunit.x)
tgsp_uq12<-tgsp_uq12%>%select(species,Body_mass_g,meanbodysize,lengthunit)

tgsp_uq1<-left_join(tgsp_uq1,tgsp_uq12,by="species")
tgsp_uq1$meanbodysize<-coalesce(tgsp_uq1$meanbodysize.x,
                                tgsp_uq1$meanbodysize.y)
tgsp_uq1$lengthunit<-coalesce(tgsp_uq1$lengthunit.x,
                                tgsp_uq1$lengthunit.y)
tgsp_uq1<-tgsp_uq1%>%select(-meanbodysize.x,-meanbodysize.y,-lengthunit.x,-lengthunit.y)
# ok, so till now, tgsp_uq1 is a table with all unique sp. and traits matched for bioTIME and Birds
# but there are still some gaps

# now match with freshwater invertebrates
t_frsh_inv<-t_frsh_inv%>%select(species,size,lengthunit)
tgsp_uq123<-left_join(tgsp_uq1,t_frsh_inv,by="species")
tgsp_uq123$size<-as.numeric(tgsp_uq123$size)
#sum(!is.na(tgsp_uq123$size))
tgsp_uq123_known<-tgsp_uq123%>%filter(is.finite(tgsp_uq123$size)) # see these are the same sp.
# whose bodysize you already extracted from BioTIME in meter
# so, skipping that: now going to match with freshwater plants

# update with freshwater plants
tgsp_uq123<-left_join(tgsp_uq1,t_frsh_pl,by="species")
tgsp_uq123_known<-tgsp_uq123[which(!is.na(tgsp_uq123$size)),] # this matches are also pre-known
# so, skipping that: now going to match with fishdatabase

#join with fishbase
t_fish<-t_fish%>%select(Genus_sp,Length,lengthunit)
tgsp_uq123<-left_join(tgsp_uq1,t_fish,by=c("species"="Genus_sp"))
tgsp_uq123_known<-tgsp_uq123[which(!is.na(tgsp_uq123$Length)),]
tgsp_uq123_known$Length<-as.numeric(tgsp_uq123_known$Length)
#class(tgsp_uq123_known$Length)
id<-which(is.na(tgsp_uq123_known$meanbodysize)&is.finite(tgsp_uq123_known$Length))
x<-tgsp_uq123_known[id,] # okay, so 88 extra match found with fishbase
# so, we will update with only this extra matches
tgsp_uq123$Length<-as.numeric(tgsp_uq123$Length)
tgsp_uq123$meanbodysize<-coalesce(tgsp_uq123$meanbodysize,tgsp_uq123$Length)
tgsp_uq123$lengthunit.x<-coalesce(tgsp_uq123$lengthunit.x,tgsp_uq123$lengthunit.y)
tgsp_uq123<-rename(tgsp_uq123,lengthunit=lengthunit.x)
tgsp_uq123<-tgsp_uq123%>%select(-Length,-lengthunit.y)

# tgsp_uq123 is the table containing all clean info
# Now, check how many are still unknown-size?
sum(is.na(tgsp_uq123$meanbodysize))/nrow(tgsp_uq123) # ~55% still unknown

tgsp_uq123_unk<-tgsp_uq123[which(is.na(tgsp_uq123$meanbodysize)),]
table(tgsp_uq123_unk$REALM) # most unknown are for Marine
tgsp_uq123_unk%>%group_by(REALM)%>%count(TAXA)

write.csv(tgsp_uq123_unk,"../../Results/gather_res/unknown_size_afterImranData.csv",row.names = F)

x<-tgsp_uq123_unk%>%group_by(REALM)%>%count(TAXA,siteid)%>%ungroup()
x<-x%>%filter(REALM%in%c("Freshwater","Terrestrial"))
range(x$n)

#==================== ok, now for freshwater fish unknown ============================
unk_frs_fish<-tgsp_uq123_unk%>%filter(REALM=="Freshwater" & TAXA=="Fish")
unk_frs_fish$comments<-NA
unk_frs_fish$webhelp<-"FishTraits Database (http://www.fishtraits.info/)"
unk_frs_fish$lengthunit<-"cm"
write.csv(unk_frs_fish,"../../DATA/ImranSent/freshwater_fish_manualmatch_maxlength.csv",row.names=F)

# the above csv file later edited manually by Shyamolina and saved as 
# "../../DATA/ImranSent/freshwater_fish_manualmatch_maxlength_edited.csv"

#=============================== Freshwater Fish done =========================================
unk_frs_fish<-read.csv("../../DATA/ImranSent/freshwater_fish_manualmatch_maxlength_edited.csv")
unk_frs_fish<-unk_frs_fish%>%select(species,meanbodysize,lengthunit)

# match with tgsp_uq123 for freshwater fishes
tgsp_uq123<-left_join(tgsp_uq123,unk_frs_fish,by="species")
tgsp_uq123$meanbodysize.x<-coalesce(tgsp_uq123$meanbodysize.x,tgsp_uq123$meanbodysize.y)
tgsp_uq123$lengthunit.x<-coalesce(tgsp_uq123$lengthunit.x,tgsp_uq123$lengthunit.y)
tgsp_uq123<-tgsp_uq123%>%select(-meanbodysize.y,-lengthunit.y)%>%
                        rename(meanbodysize=meanbodysize.x,lengthunit=lengthunit.x)
# ok, all freshwater fish done!

# now, get the unknowns?
tgsp_uq123_unk<-tgsp_uq123[which(is.na(tgsp_uq123$meanbodysize)),]
tgsp_uq123_unk%>%group_by(REALM)%>%count(TAXA)%>%ungroup()

write.csv(tgsp_uq123_unk,"../../DATA/ImranSent/stillunknown.csv",row.names=F)

x<-tgsp_uq123_unk%>%filter(TAXA=="Freshwater invertebrates")
x%>%count(ORGANISMS)







