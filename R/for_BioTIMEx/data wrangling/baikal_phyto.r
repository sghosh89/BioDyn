# Phytoplankton data in baikal lake given by Alban (J. Chase group)
library(tidyverse)

`%notin%` <- Negate(`%in%`)

dataset_id <- 'baikal_phyto'

# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

#--------------------------------------------------
# read data
allrawdata<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/allrawdata.csv")
# abundance type = mean count
abun<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/abundance.csv")
# data source
contact<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/contacts.csv")
# curation = summary
curation<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/curation.csv")
# metadatasets
datasets<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/datasets.csv")
# methods summary: one sample site, 1000m from shore, net sampling from 50 meter deep to surface
methods<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/methods.csv")
# sample 
samp<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/sample.csv")
# sites 
sites<-read.csv("../../DATA/for_BioTIMEx/wrangled_data/baikal_phyto/sites.csv")

allrawdata<-allrawdata%>%filter(MONTH%in%c(5:8)) # May to August
tt<-allrawdata%>%group_by(YEAR)%>%summarise(nm=n_distinct(MONTH),nsp=n_distinct(ID_SPECIES))%>%ungroup()
# consistently, 4 months sampled for each of 23 years for all 6 groups of sp.
# single site (lat-lon) sampled

spmat<-allrawdata%>%group_by(ID_SPECIES,YEAR)%>%summarise(ABUNDANCE=mean(ABUNDANCE))%>%ungroup()
tt<-spmat%>%group_by(ID_SPECIES)%>%summarise(ny=n_distinct(YEAR))%>%ungroup()

spmat<-spmat%>%spread(ID_SPECIES,ABUNDANCE,fill=0)%>%as.data.frame()
rownames(spmat)<-spmat$YEAR
spmat<-spmat[,-1]

# all 6 groups considered: no rare sp.
saveRDS(spmat,paste("../../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",dataset_id,"_inputmatrix_tailanal.RDS",sep=""))






