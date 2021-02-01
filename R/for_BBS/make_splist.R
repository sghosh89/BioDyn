# read species list files
x<-read.fwf(file = "../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/SpeciesList_edited.txt",
            widths = c(6, 
                       7, 
                       50, 51, 50, 
                       19,51,57,65), 
            strip.white = T)

colnames(x)<-c("Seq",
               "AOU",
               "English_Common_Name","Freanch_Common_Name","Spanish_Common_Name",
               "ORDER","Family","Genus","Species")
x$ScientificName<-paste(x$Genus,x$Species,sep=" ")

write.csv(x,"../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/SpeciesList_edited.csv",row.names = F)                                       

# https://www.mbr-pwrc.usgs.gov/bbs/genintro.html (why do we need guild?)

##################################################################################
library(tidyverse)
y<-read.delim("../../DATA/for_BBS/raw_data/EltonTraits/BirdFuncDat.txt")

getcat<-function(x){
  res<-which(x>50)
  if(length(res)==0){
    resy<-"mixed"
  }else{
    resy<-names(x)[res]
    resy<-str_split(resy,pattern="\\.",n=2)[[1]][2]
  }
  return(resy)
}
z1<-y%>%select(ForStrat.watbelowsurf,
               ForStrat.wataroundsurf,
               ForStrat.ground,ForStrat.understory,
               ForStrat.midhigh,ForStrat.canopy,
               ForStrat.aerial)
z11<-apply(z1,MARGIN=1,FUN=sum)
table(z11)# not all 100 as sometimes rounded number would give 99 

s<-apply(z1,MARGIN = 1, FUN=getcat)
y$Strat.7Cat<-s
y<-y%>%select(SpecID,PassNonPass,IOCOrder,BLFamilyLatin,BLFamilyEnglish,BLFamSequID,Taxo,
              Scientific,English,
              Diet.5Cat,Diet.Certainty,
              Strat.7Cat,ForStrat.SpecLevel,
              Nocturnal,PelagicSpecialist)

# matching by scientific name: 534 out of 756 matched
z<-inner_join(x,y, by=c("ScientificName"="Scientific"))
table(z$Diet.5Cat) # 5 diet categories
# PlantSeed: Plant and Seeds, 
# FruiNect: Fruits and Nectar, 
# Invertebrate: Invertebrates, 
# VertFishScav: Vertebrates and Fish and Carrion, 
# Omnivore: score of <= 50 in all four categories
table(z$Strat.7Cat)
# 7 categories based on prevalence of Foraging
# ForStrat-watbelowsurf: Prevalence of Foraging below the water surfaces
# ForStrat-wataroundsurf: Prevalence of Foraging on or just (<5 inches) below water surface
# ForStrat-ground: Prevalence of Foraging on ground
# ForStrat-understory: Prevalence of Foraging below 2m in understory in forest, forest edges, bushes or shrubs
# ForStrat-midhigh: Prevalence of Foraging in mid to high levels in trees or high bushes (2m upward), but below canopy
# ForStrat-canopy: Prevalence of Foraging in or just above (from) tree canopy
# ForStrat-aerial: Prevalence of Foraging well above vegetation or any structures

z0<-anti_join(x,y, by=c("ScientificName"="Scientific"))# 222 out of 756 not matched

# 71% matched after scientific name matching 

##########################################################
# Now, we will check if some unmatched (no scientific name matching) entries could be matched based on english name?
ynoScientificmatch<-anti_join(y,z, by=c("Scientific"="ScientificName"))
zEngName<-inner_join(z0,ynoScientificmatch, by=c("English_Common_Name"="English")) # 117 matched
table(zEngName$Diet.5Cat) 
table(zEngName$Strat.7Cat)

z0EngName<-anti_join(z0,ynoScientificmatch, by=c("English_Common_Name"="English"))# 105 not matched

# 86% matched after scientific and english name matching (534+117) out of 756

###########################################################################################
ynoScientificmatch_noEngmatch<-anti_join(ynoScientificmatch,zEngName, by=c("English"="English_Common_Name"))
#z0EngName
# we have to find out link (if any) between z0EngName sp. to sp. in ynoScientificmatch_noEngmatch table

#----------------------------------------------------------------------------------------------------
# trying taxadb
#----------------------------------------------------------------------------------------------------
#library(taxadb)
#library(tidyverse)
#tryit<-z0EngName[1:10,]
#birds <- tryit %>% 
#  select(ScientificName) %>% 
#  mutate(id = get_ids(ScientificName, "col"))

#zaccepted<-filter_name(z0EngName$ScientificName[13:14])  %>%
#  mutate(acceptedNameUsage = get_names(acceptedNameUsageID)) %>% 
#  select(scientificName, taxonomicStatus, acceptedNameUsage, acceptedNameUsageID)

#filter_name(z0EngName$ScientificName[13:14])  %>%
#  mutate(acceptedNameUsage = get_names(acceptedNameUsageID)) %>% 
# select(scientificName, taxonomicStatus, acceptedNameUsage, acceptedNameUsageID)
#-----------------------------------------
library(rangeBuilder)
getSynonymsFromAccepted(sp="Motacilla tschutschensis", db = 'birds')
getAcceptedFromSynonym(sp="Motacilla tschutschensis", db = 'birds')

birds_accepted<-getAcceptedNames(db="birds")
z0EngName$synScientific_rangeBuilder<-NA
for(i in 1:nrow(z0EngName)){
  findsp<-z0EngName$ScientificName[i]
  findsp<-str_replace(findsp," ","_")
  if((findsp%in%birds_accepted)==T){
    ans<-getSynonymsFromAccepted(sp=findsp, db = 'birds')
    if(length(ans)!=0){
      z0EngName$synScientific_rangeBuilder[i]<-ans
    }
  }
  cat("i = ",i," is done \n")
}


#-------------------------------
library(taxize)
#zz<-gnr_resolve(sci = c("Caprimulgus vociferus"))
#zz<-synonyms(z0EngName$ScientificName,db="itis")
#zz

#sapply(zz, "[", i = 4)
z0EngName$synScientific_taxize<-NA
for(i in 1:nrow(z0EngName)){
  findsp<-z0EngName$ScientificName[i]
  zz<-synonyms(findsp,db="itis")
  if(length(zz[[1]])>1){
    z0EngName$synScientific_taxize[i]<-list(zz[[1]]$syn_name)
  }else{
    z0EngName$synScientific_taxize[i]<-NA
  }
  cat("i = ",i," is done \n")
}
# the above code stopped at i=77
# so we again start from 78
for(i in 78:nrow(z0EngName)){
  findsp<-z0EngName$ScientificName[i]
  zz<-synonyms(findsp,db="itis")
  if(length(zz[[1]])>1){
    z0EngName$synScientific_taxize[i]<-list(zz[[1]]$syn_name)
  }else{
    z0EngName$synScientific_taxize[i]<-NA
  }
  cat("i = ",i," is done \n")
}

# synonyms("Oceanodroma leucorhoa",db="itis") # this one is the special case
z_itis<-subset(z0EngName,!is.na(z0EngName$synScientific_taxize) | !is.na(z0EngName$synScientific_rangeBuilder))
getrowids<-c()# this is the vector to store the matching rowids in EltonTraits table
z_itis$synScientific_ultimate<-NA
for(i in 1:nrow(z_itis)){
  temp<-unlist(z_itis$synScientific_taxize[i])
  id<-match(temp,y$Scientific)
  id<-na.omit(id)
  if(length(id)==1){
    z_itis$synScientific_ultimate[i]<-y$Scientific[id]
    getrowids<-c(getrowids,id)
  }
}
z_itis$synScientific_ultimate[which(z_itis$ScientificName=="Hydrobates leucorhous")]<-"Oceanodroma leucorhoa"
z_itis<-subset(z_itis,!is.na(z_itis$synScientific_ultimate))
z_itis<-z_itis[,c(1:10,13)]
zsmall<-inner_join(z_itis,y,by=c("synScientific_ultimate"="Scientific"))%>%as.data.frame()
zsmall<-zsmall%>%dplyr::select(Seq,AOU,English_Common_Name,ORDER,
                        Family,Genus,Species,ScientificName,Diet.5Cat)
######################################################
# z, zEngName: combine these
zall<-rbind(z[,c(1:17,19:49)],zEngName[,c(1:17,19:49)])
zall<-zall%>%dplyr::select(Seq,AOU,English_Common_Name,ORDER,
                               Family,Genus,Species,ScientificName,Diet.5Cat)
zall<-rbind(zall,zsmall)

z0all<-anti_join(x,zall,by="AOU")
z0all$Diet.5Cat<-NA
z0all<-z0all%>%dplyr::select(Seq,AOU,English_Common_Name,ORDER,
                             Family,Genus,Species,ScientificName,Diet.5Cat)

colnames(zall)==colnames(z0all)
zall<-rbind(zall,z0all)


write.csv(zall,"../../DATA/for_BBS/wrangled_data/species_with_taxainfo_diet.csv",row.names = F)                                       












