`%notin%` <- Negate(`%in%`)
library(dplyr)
data_pt_thrs<-20

# read the meta data
xxm<-read.csv("../../DATA/for_BioTIME/raw_data/accessed31Aug2021/BioTIMEMetadata_24_06_2021.csv") # a dataframe
# read the meta data from private version
xxm_private<-read.csv("../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/bioTIMEmetadataScienceStudies.csv") # a dataframe
xxm_extra<-anti_join(x=xxm_private,y=xxm,by="STUDY_ID") # extra data
xxm_extra<-xxm_extra%>%filter(DATA_POINTS>=data_pt_thrs) # extra data with >= 20 years
# but we will exclude relative biomass type: park grass exp. as it is not frequently sampled
xxm_extra<-xxm_extra%>%filter(BIOMASS_TYPE%notin%"Relative biomass") # 9 extra data
saveRDS(xxm_extra,"../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019_extra_metadata.RDS")

# read data first
xx<-read.csv("../../DATA/for_BioTIME/raw_data/accessed31Aug2021/BioTIMEQuery_24_06_2021.csv") # a dataframe
saveRDS(xx,"../../DATA/for_BioTIME/raw_data/accessed31Aug2021/BioTIMEQuery24_06_2021.RDS")
# read the data from private version
xx_private<-read.csv("../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019.csv")
xx_extra<-xx_private%>%filter(STUDY_ID%in%xxm_extra$STUDY_ID)
saveRDS(xx_extra,"../../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019_extra.RDS")

# now select some columns and combine the metadata and data
xxm<-xxm%>%dplyr::select(STUDY_ID,REALM,ORGANISMS,CLIMATE, HABITAT,TAXA,BIOME_MAP,
                  AB_BIO,ABUNDANCE_TYPE, BIOMASS_TYPE,
                  TITLE, AREA_SQ_KM, GRAIN_SQ_KM, PROTECTED_AREA, 
                  DATA_POINTS,START_YEAR,END_YEAR,CENT_LAT,CENT_LONG,NUMBER_LAT_LONG,       
                  NUMBER_OF_SPECIES,NUMBER_OF_SAMPLES,SAMPLE_DESC_NAME, DATE_STUDY_ADDED,
                  # below info available only in public version 
                  METHODS,SUMMARY_METHODS, COMMENTS,WEB_LINK,LICENSE,DATA_SOURCE)

xxm_extra<-xxm_extra%>%dplyr::select(STUDY_ID,REALM,ORGANISMS,CLIMATE, HABITAT,TAXA,BIOME_MAP,
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

bt<-dplyr::inner_join(xxm_all, xx_all, by='STUDY_ID')
saveRDS(bt,"../../DATA/for_BioTIME/BioTIME_public_private_query_data_metadata.RDS")







