rm(list=ls())
graphics.off()

# read data
L1WA<-read.csv("../../DATA/for_swisslake/raw_data/1.WA_phytoplankton_data.csv")
L2UZ<-read.csv("../../DATA/for_swisslake/raw_data/2.UZ_phytoplankton_data.csv")
L3LU<-read.csv("../../DATA/for_swisslake/raw_data/3.LU_phytoplankton_data.csv")
L4LZ<-read.csv("../../DATA/for_swisslake/raw_data/4.LZ_phytoplankton_data.csv")
L5SE<-read.csv("../../DATA/for_swisslake/raw_data/5.SE_phytoplankton_data.csv")
L6HA<-read.csv("../../DATA/for_swisslake/raw_data/6.HA_phytoplankton_data.csv")
L7BA<-read.csv("../../DATA/for_swisslake/raw_data/7.BA_phytoplankton_data.csv")
L8GR<-read.csv("../../DATA/for_swisslake/raw_data/8.GR_phytoplankton_data.csv")

meta<-read.csv2("../../DATA/for_swisslake/raw_data/0.phytoplankton_meta_database.csv") 
tables1<- read.csv("../../DATA/for_swisslake/raw_data/swisslake_suppmat/Table_S1_phytoplankton_DB.csv") # from suppmat


library(tidyverse)
`%notin%` <- Negate(`%in%`)
# ------------------- Cleaning duplicates ----------------------------------
# NOTE: duplicated entries of same species but different codes!!! 
taxa_info<-select(tables1,c("id_Eawag","affiliation","synonyms"))
taxa_dup<-taxa_info[!(taxa_info$affiliation==""),] # trimming out unassigned affiliation 
                                                    # for a given id_Eawag
taxa_dup<-taxa_dup[duplicated(taxa_dup$affiliation)|
                     duplicated(taxa_dup$affiliation,fromLast=TRUE),]
taxa_dup<-taxa_dup[order(taxa_dup$affiliation),]
nrow(taxa_dup)
taxa_duplist<-split(taxa_dup,f=taxa_dup$affiliation)
dupsp<-unique(taxa_dup$affiliation)
dup_id_affiliation<-taxa_dup[duplicated(taxa_dup$affiliation),] # these are repeats, 
                                                                   # so to be excluded

# --------------- I need a common data format for all swiss lake data ---------------------

# ------------- for Lake1 --------------------
L1WA$taxon <- gsub('sp.', 'sp', L1WA$taxon) # needed for left join two data frames later 
taxa_info$synonyms <- gsub('sp.', 'sp', taxa_info$synonyms) # needed for left join two data frames later 
L1WA_c<-left_join(x=L1WA,y=taxa_info,by=c("taxon"="affiliation"))
#L1WA_c<-left_join(x=L1WA,y=meta,by=c("taxon"="affiliation")) # this will give same 
                                                            # info as of previous line.

L1WA_c<-left_join(x=L1WA_c,y=taxa_info,by=c("taxon"="synonyms")) # also matched by synonyms
L1WA_c<-L1WA_c %>%
  mutate(id_Eawag = coalesce(id_Eawag.x,id_Eawag.y))%>% # merging two columns
  select(-id_Eawag.x,-id_Eawag.y,-affiliation) # exclude unnecessary columns

id_na<-L1WA_c%>%filter(is.na(id_Eawag))
id_na<-unique(id_na$taxon)

# NOTE: I checked the id_na vectors which store the species name appeared in L1WA_c 
# without any id_Eawag. But some of them have those id_Eawag in taxa_info table
# though there are some mispelling and that's why overlooked by the pattern matching 
# synonyms thing before. So, we have to do manually.

L1WA_c$taxon[L1WA_c$taxon=="Nitzschia sygmoidea"]<-"Nitzschia sigmoidea"
L1WA_c$taxon[L1WA_c$taxon=="Planktospaeria gelatinosa"]<-"Planktosphaeria gelatinosa"
L1WA_c$taxon[L1WA_c$taxon=="Chroococcus disprsus"]<-"Chroococcus dispersus"
L1WA_c$taxon[L1WA_c$taxon=="Diatoma hiemale"]<-"Diatoma hyemalis"
L1WA_c$taxon[L1WA_c$taxon=="Pseudospaerocystis lacustris"]<-"Pseudosphaerocystis lacustris"
L1WA_c$taxon[L1WA_c$taxon=="Monorhaphidium sp"]<-"Monoraphidium sp"
L1WA_c$taxon[L1WA_c$taxon=="Coccomyxa sp"]<-"Coccomyxa/Choricystis sp"
L1WA_c$taxon[L1WA_c$taxon=="Diatoma vulgare"]<-"Diatoma vulgaris"

# now do the join
L1WA_c<-left_join(x=L1WA_c,y=taxa_info,by=c("taxon"="affiliation"))%>%
      mutate(id_Eawag = coalesce(id_Eawag.x,id_Eawag.y))%>% # merging two columns
      select(-id_Eawag.x,-id_Eawag.y,-synonyms.x,-synonyms.y) # exclude unnecessary columns


id_na<-L1WA_c%>%filter(is.na(id_Eawag))
id_na2<-as.data.frame(table(id_na$taxon)) # still there are some species without 
                                          # any specific identifier from id_Eawag


L1WA_c<-L1WA_c%>%filter(id_Eawag%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
                select(c("date","id_Eawag","taxon","biov","abundance"))%>% # rearrange
                rename(c(species=taxon)) # rename

L1WA_c$id_Eawag[is.na(L1WA_c$id_Eawag)]<-"000" # unidentified id recoded


# ------------- for Lake2 --------------------
L2UZ$taxon <- gsub('sp.', 'sp', L2UZ$taxon) # needed for left join two data frames later 
L2UZ_c<-left_join(x=L2UZ,y=taxa_info,by=c("taxon"="affiliation"))
L2UZ_c<-L2UZ_c%>%filter(id_Eawag%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","id_Eawag","taxon","biov","abundance"))%>% # rearrange
  rename(c(species=taxon)) # rename

id_na<-L2UZ_c%>%filter(is.na(id_Eawag))
id_na<-unique(id_na$species)

L2UZ_c<-left_join(x=L2UZ_c,y=taxa_info,by=c("species"="synonyms")) # also matched by synonyms
L2UZ_c<-L2UZ_c %>%
  mutate(id_Eawag = coalesce(id_Eawag.x,id_Eawag.y))%>% # merging two columns
  select(-id_Eawag.x,-id_Eawag.y,-affiliation) # exclude unnecessary columns

id_na<-L2UZ_c%>%filter(is.na(id_Eawag))
id_na<-unique(id_na$species)

L2UZ_c$species[L2UZ_c$species=="Diatoma vulgare"]<-"Diatoma vulgaris"
L2UZ_c$species[L2UZ_c$species=="Tetraëdron minimum-apertum"]<-"Tetraedron minimum-apertum"
L2UZ_c$species[L2UZ_c$species=="Nitzschia sygmoidea"]<-"Nitzschia sigmoidea"
L2UZ_c$species[L2UZ_c$species=="Monorhaphidium sp"]<-"Monoraphidium sp"
L2UZ_c$species[L2UZ_c$species=="Tetraëdron caudatum"]<-"Tetraedron caudatum"
L2UZ_c$species[L2UZ_c$species=="Diatoma hiemale"]<-"Diatoma hyemalis"
L2UZ_c$species[L2UZ_c$species=="Gomphospaeria sp"]<-"Gomphosphaeria sp"
L2UZ_c$species[L2UZ_c$species=="Dictyospaerium sp"]<-"Dictyosphaerium sp"
L2UZ_c$species[L2UZ_c$species=="Coccomyxa sp"]<-"Coccomyxa/Choricystis sp"
L2UZ_c$species[L2UZ_c$species=="Planktospaeria gelatinosa"]<-"Planktosphaeria gelatinosa"
L2UZ_c$species[L2UZ_c$species=="Chroococcus disprsus"]<-"Chroococcus dispersus"
L2UZ_c$species[L2UZ_c$species=="Rhoicospenia abbreviata"]<-"Rhoicosphenia abbreviata"
L2UZ_c$species[L2UZ_c$species=="Pseudospaerocystis lacustris"]<-"Pseudosphaerocystis lacustris"


# now do the join
L2UZ_c<-left_join(x=L2UZ_c,y=taxa_info,by=c("species"="affiliation"))%>%
  mutate(id_Eawag = coalesce(id_Eawag.x,id_Eawag.y))%>% # merging two columns
  select(-id_Eawag.x,-id_Eawag.y,-synonyms) # exclude unnecessary columns

id_na<-L2UZ_c%>%filter(is.na(id_Eawag))
id_na2<-as.data.frame(table(id_na$species)) # still there are some species without 
# any specific identifier from id_Eawag

L2UZ_c<-L2UZ_c%>%filter(id_Eawag%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","id_Eawag","species","biov","abundance"))

L2UZ_c$id_Eawag[is.na(L2UZ_c$id_Eawag)]<-"000" # unidentified id recoded

# ------------- for Lake3 --------------------
L3LU_c<-left_join(x=L3LU,y=taxa_info,by=c("taxon"="id_Eawag"))
L3LU_c<-L3LU_c%>%
          filter(taxon%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
          select(c("date","taxon","affiliation","biov","value"))%>% # rearrange
          rename(c(id_Eawag=taxon,species=affiliation,abundance=value)) # rename

# ------------- for Lake4 --------------------
L4LZ_c<-left_join(x=L4LZ,y=taxa_info,by=c("taxon"="id_Eawag"))
L4LZ_c<-L4LZ_c%>%
  filter(taxon%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","taxon","affiliation","biov","abundance"))%>% # rearrange
  rename(c(id_Eawag=taxon,species=affiliation)) # rename

# ------------- for Lake5 --------------------
L5SE_c<-left_join(x=L5SE,y=taxa_info,by=c("taxon"="id_Eawag"))
L5SE_c<-L5SE_c%>%
  filter(taxon%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","taxon","affiliation","biov","value"))%>% # rearrange
  rename(c(id_Eawag=taxon,species=affiliation,abundance=value)) # rename

# ------------- for Lake6 --------------------
L6HA_c<-left_join(x=L6HA,y=taxa_info,by=c("taxon"="id_Eawag"))
L6HA_c<-L6HA_c%>%
  filter(taxon%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","taxon","affiliation","biov","value"))%>% # rearrange
  rename(c(id_Eawag=taxon,species=affiliation,abundance=value)) # rename

# ------------- for Lake7 --------------------
L7BA_c<-left_join(x=L7BA,y=taxa_info,by=c("taxon"="id_Eawag"))
L7BA_c<-L7BA_c%>%
  filter(taxon%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","taxon","affiliation","biov","value"))%>% # rearrange
  rename(c(id_Eawag=taxon,species=affiliation,abundance=value)) # rename

# ------------- for Lake8 --------------------
L8GR_c<-left_join(x=L8GR,y=taxa_info,by=c("taxon"="id_Eawag"))
L8GR_c<-L8GR_c%>%
  filter(taxon%notin%dup_id_affiliation$id_Eawag)%>% # exclude repeats
  select(c("date","taxon","affiliation","biov","value"))%>% # rearrange
  rename(c(id_Eawag=taxon,species=affiliation,abundance=value)) # rename


#-------------------------------------------------------------------------------
# ALSO NOTE: there are multiple samples on same date taken at same sites
# For example: the following two entries I got on Lake3

# Date      id_Eawag  species              biov    abundance
#1968-01-08 1733    Ceratium hirundinella 45000     320
#1968-01-08 1733    Ceratium hirundinella 45000     320

# and also multiple sampling days within a month of a given year for the same species
# Date         id_Eawag  species     biov    abundance
# 1992-08-03    1740      " "     10000     1500
# 1992-08-17    1740      " "     10000     2900
# 1992-08-31    1740      " "     10000     2200

# The following function will give you mean biov and mean abundance for each 
# phytoplankton species multiple sampling taken for each month-year combo
get_meanstat_multisamp<-function(dat){
  dat$date<-as.Date(dat$date,format="%Y-%m-%d")
  dat$daynum<-format(dat$date, "%j")
  dat<-dat%>%separate(date, c("year","month","day"), "-")
  dat$species[dat$species==""]<-"Unknown" # unknown species name for given id_Eawag identifier
  dat<-dat%>%filter(species!="Unknown")%>% # remove unknown species
              filter(id_Eawag!="000")%>% # remove unknown species identifier
              filter(month %in% c("06","07","08","09"))
  #=======================================================================================
  # to check even sampling effort per year? I am going to consider lake data for 06-09 months
  c<-dat%>%group_by(year)%>%summarise(ns=n_distinct(month))%>%ungroup()
  id_badyr<-which(c$ns!=4)
  if(length(id_badyr)!=0){
    c<-c[-id_badyr,] # exclude those years which were not sampled all 4 months
  }
  dat<-dat%>%filter(year%in%c$year)
  c1<-dat%>%group_by(year,month)%>%summarise(ns=n_distinct(day))%>%ungroup()
  range(c1$ns)
  # Lake----------total sampled years-----------years excluded as all 4 months not sampled/ comments-------
  #  1 -----------------29------------None: perfect sampling effort: all months in all year sampled once-------
  #  2 -----------------29------------2 (1973, 1982) : other included years have 1-2 sampling date per month------
  #  3 ----------------47-------------7 (1974,1985,1991,2011-2014): other included years have 1-5 sampling date per month-----------------------------
  #  4 -----------------35------------None: perfect sampling effort: all months in all year sampled once-------
  #  5 -------------------31----------8 (1995,1997,2000,2010-2014): other included years have 1-3 sampling date per month-----------------------------
  #  6 -------------------33----------None: perfect sampling effort per year: included years have 1-3 sampling date per month-------
  #  7 -------------------31----------6 (2003,2010-2014): other included years have 1-3 sampling date per month-----------------------------
  #  8 --------------------32---------2 (2015-2016): other included years have 1-4 sampling date per month-----------------------------
  #==========================================================================================
  
  # first summarise for each month in a year for multiple sampling days
  x<-dat%>%group_by(year,month,id_Eawag,species)%>%
    summarise(biov=mean(biov),abundance=mean(abundance))%>%ungroup()
  
  # then summarise for each year for multiple sampling months
  x<-x%>%group_by(year,id_Eawag,species) %>%   # group by combinations
    summarise(biov = mean(biov),                 # get summary stats
              abun_mean = mean(abundance)) %>%ungroup()
  return(x)
}

L1WA_c<-get_meanstat_multisamp(dat=L1WA_c)
L2UZ_c<-get_meanstat_multisamp(dat=L2UZ_c)
L3LU_c<-get_meanstat_multisamp(dat=L3LU_c)
L4LZ_c<-get_meanstat_multisamp(dat=L4LZ_c)
L5SE_c<-get_meanstat_multisamp(dat=L5SE_c)
L6HA_c<-get_meanstat_multisamp(dat=L6HA_c)
L7BA_c<-get_meanstat_multisamp(dat=L7BA_c)
L8GR_c<-get_meanstat_multisamp(dat=L8GR_c)

L1to8_c<-list(L1WA_c=L1WA_c,
              L2UZ_c=L2UZ_c,
              L3LU_c=L3LU_c,
              L4LZ_c=L4LZ_c,
              L5SE_c=L5SE_c,
              L6HA_c=L6HA_c,
              L7BA_c=L7BA_c,
              L8GR_c=L8GR_c)

saveRDS(L1to8_c,"../../DATA/for_swisslake/wrangled_data/alllake_cleanedlist.RDS")
#-------------------------------------------------

