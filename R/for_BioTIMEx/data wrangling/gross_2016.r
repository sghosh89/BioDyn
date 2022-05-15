## gross_2016: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-kbs&identifier=55&revision=16
# using this dataset in a publication requires permission!!!!

library(tidyverse)

dataset_id <- 'gross_2016'
# create directory
resloc<-paste('../../DATA/for_BioTIMEx/wrangled_data/',dataset_id,'/',sep='')
dir.create(resloc, showWarnings = FALSE)

# ------------ for 1975-95 ---------------
x<-read.delim(file='../../DATA/for_BioTIMEx/raw_data/gross_2016/Early Successional Microplot Biomass - by Species_extracted.txt',sep=",")
x<-x[-1,]
x$biomass_g<-as.numeric(x$biomass_g)
x<-x%>%filter(disturbed=="undisturbed" & fertilized=="unfertilized") # control sites only
x<-x%>%mutate(year=str_sub(sample_date,1,4),
              month=str_sub(sample_date,6,7))
c1<-x%>%group_by(year)%>%summarise(nsd=n_distinct(month))%>%ungroup()

#for consistent sampling effort in July,August
x<-x%>%filter(month%in%c("07","08"))
c1<-x%>%group_by(year)%>%summarise(nsd=n_distinct(month))%>%ungroup()

badsp<-c("Surface Litter","unknown Asteraceae","unknown Brassicaceae","Unknown dicot (*)",
         "Unknown grass","Unknown monocot (*)","Unknown Solanaceae","UnSorted","Woody")
x<-x%>%filter(!is.na(biomass_g)&biomass_g>0)%>%
       select(year,species,biomass_g)%>%filter(!species%in%badsp)

x<-x%>%group_by(year,species)%>%
                    summarise(mean_biomass_gm=mean(biomass_g))%>%ungroup()%>%
             spread(species,mean_biomass_gm,fill=0)%>%as.data.frame()

rownames(x)<-x$year
x<-x[,-1]
countnon_0<-apply(x,MARGIN=2,FUN=function(y){sum(y>0)})
# common sp present atleast 70% of sampling years, rest of the species are considered rare and aggregated into single one
presentyr<-0.7*nrow(x) 
commonsp<-which(countnon_0>=presentyr)
rare_sp<-which(countnon_0<presentyr)
raresp<-x[,rare_sp]
raresp<-apply(raresp,MARGIN=1,FUN=sum)
commonsp<-x[,commonsp]
commonsp$raresp<-raresp

write.csv2(commonsp,paste(resloc,dataset_id,"_inputmatrix_tailanal.csv",sep=""),row.names = T)
saveRDS(commonsp,paste(resloc,dataset_id,"_inputmatrix_tailanal.RDS",sep=""))



