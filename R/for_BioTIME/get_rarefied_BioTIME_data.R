
source("rarefy_ts.R")

#============ now call the function ==================
`%notin%` <- Negate(`%in%`)
data_pt_thrs<-20

# read the meta data
xxm<-read.csv("../../DATA/for_BioTIME/raw_data/accessed18Nov2020/BioTIMEMetadata_02_04_2018.csv") # a dataframe
# read the meta data from private version
xxm_private<-read.csv("../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/bioTIMEmetadataScienceStudies.csv") # a dataframe
xxm_extra<-anti_join(x=xxm_private,y=xxm,by="STUDY_ID") # 26 extra data
xxm_extra<-xxm_extra%>%filter(DATA_POINTS>=data_pt_thrs) # 10 extra data with >= 20 years
# but we will exclude relative biomass type: park grass exp. as it is not frequently sampled
xxm_extra<-xxm_extra%>%filter(BIOMASS_TYPE%notin%"Relative biomass") # 9 extra data
saveRDS(xxm_extra,"../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019_extra_metadata.RDS")

# read data first
xx<-read.csv("../../DATA/for_BioTIME/raw_data/accessed18Nov2020/BioTIMEQuery02_04_2018.csv") # a dataframe
saveRDS(xx,"../../DATA/for_BioTIME/raw_data/accessed18Nov2020/BioTIMEQuery02_04_2018.RDS")
# read the data from private version
xx_private<-read.csv("../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019.csv")
xx_extra<-xx_private%>%filter(STUDY_ID%in%xxm_extra$STUDY_ID)
saveRDS(xx_extra,"../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019_extra.RDS")

# now select some columns and combine the metadata and data
xxm<-xxm%>%select(STUDY_ID,REALM,ORGANISMS,CLIMATE, HABITAT,TAXA,BIOME_MAP,
                  AB_BIO,ABUNDANCE_TYPE, BIOMASS_TYPE,
                  TITLE, AREA_SQ_KM, GRAIN_SQ_KM, PROTECTED_AREA, 
                  DATA_POINTS,START_YEAR,END_YEAR,CENT_LAT,CENT_LONG,NUMBER_LAT_LONG,       
                  NUMBER_OF_SPECIES,NUMBER_OF_SAMPLES,SAMPLE_DESC_NAME, DATE_STUDY_ADDED,
                  # below info available only in public version 
                  METHODS,SUMMARY_METHODS, COMMENTS,WEB_LINK,LICENSE,DATA_SOURCE)

xxm_extra<-xxm_extra%>%select(STUDY_ID,REALM,ORGANISMS,CLIMATE, HABITAT,TAXA,BIOME_MAP,
                              AB_BIO,ABUNDANCE_TYPE, BIOMASS_TYPE,
                              TITLE, AREA_SQ_KM, GRAIN_SQ_KM, PROTECTED_AREA, 
                              DATA_POINTS,START_YEAR,END_YEAR,CENT_LAT,CENT_LONG,NUMBER_LAT_LONG,       
                              NUMBER_OF_SPECIES,NUMBER_OF_SAMPLES,SAMPLE_DESC_NAME, DATE_STUDY_ADDED)%>%
                      mutate( # below info available only in public version 
                              METHODS=NA,SUMMARY_METHODS=NA, COMMENTS=NA,WEB_LINK=NA,LICENSE=NA,DATA_SOURCE=NA)

xxm_all<-rbind(xxm,xxm_extra)
saveRDS(xxm_all,"../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")

all(colnames(xx)==colnames(xx_extra))==T
xx_all<-rbind(xx,xx_extra)
saveRDS(xx_all,"../../DATA/for_BioTIME/BioTIME_public_private_query_data.RDS")

#=========================================================================================

bt<-dplyr::inner_join(xxm_all, xx_all, by='STUDY_ID')

##	reduce to data required for rarefying, and rename a couple of columns
##	NB: we will rarefy to the smallest number of ObsEventID's within studies
grid <- bt %>% 
  dplyr::select(CLIMATE, REALM, TAXA, ABUNDANCE_TYPE, BIOMASS_TYPE, STUDY_ID, YEAR, PLOT,
                GENUS_SPECIES, sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS)
colnames(grid)[9:11] <- c('Species', 'Abundance', 'Biomass')

## Get rarefied time series for each type of measurement (all data)

#------------------------------- for abundance --------------------------------------------
bt_grid_abund<-grid%>%filter(ABUNDANCE_TYPE %in% c("Count", "Density", "MeanCount"))

rarefy_abund <- rarefy_ts(grid=bt_grid_abund, 
                          type="count", 
                          resamples=100, 
                          trimsamples = F, 
                          data_pt_thrs = data_pt_thrs)
# saving results
saveRDS(rarefy_abund,paste("../../DATA/for_BioTIME/wrangled_data/rarefy_abund_data_pt_thrs_",data_pt_thrs,".RDS",sep=""))


#------------------------------- for biomass --------------------------------------------
bt_grid_bmass <- grid %>%
  filter(is.na(ABUNDANCE_TYPE)) #only want to calculate on biomass data 
# when abundance data is also not available

rarefy_bmass <- rarefy_ts(grid=bt_grid_bmass, 
                          type="biomass", 
                          resamples=100, 
                          trimsamples = F, 
                          data_pt_thrs = data_pt_thrs)

# saving results
saveRDS(rarefy_bmass,paste("../../DATA/for_BioTIME/wrangled_data/rarefy_bmass_data_pt_thrs_",data_pt_thrs,".RDS",sep=""))

#---------------------- now clean a bit and merge both Abund/biomass data ----------------------
z_abund<-rarefy_abund%>%group_by(STUDY_ID,Species,YEAR)%>%
  dplyr::summarise(Value=median(Value))%>%ungroup()

z_bmass<-rarefy_bmass%>%group_by(STUDY_ID,Species,YEAR)%>%
  dplyr::summarise(Value=median(Value))%>%ungroup()

bt_rarefied<-rbind(z_abund,z_bmass)

# saving results
saveRDS(bt_rarefied,paste("../../DATA/for_BioTIME/wrangled_data/bt_rarefied_data_pt_thrs_",data_pt_thrs,".RDS",sep=""))



