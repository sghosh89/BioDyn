
source("./rarefy_ts.R")

#============ now call the function ==================
# read data first
xx<-read.csv("./Data/accessed18Nov2020/BioTIMEQuery02_04_2018.csv") # a dataframe
saveRDS(xx,"./Data/accessed18Nov2020/BioTIMEQuery02_04_2018.RDS")
# read the meta data
xxm<-read.csv("./Data/accessed18Nov2020/BioTIMEMetadata_02_04_2018.csv") # a dataframe

bt<-dplyr::inner_join(xxm, xx, by='STUDY_ID')

##	reduce to data required for rarefying, and rename a couple of columns
##	NB: we will rarefy to the smallest number of ObsEventID's within studies
grid <- bt %>% 
  dplyr::select(CLIMATE, REALM, TAXA, ABUNDANCE_TYPE, BIOMASS_TYPE, STUDY_ID, YEAR, PLOT,
                GENUS_SPECIES, sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS)
colnames(grid)[9:11] <- c('Species', 'Abundance', 'Biomass')

data_pt_thrs<-20

## Get rarefied time series for each type of measurement (all data)

#------------------------------- for abundance --------------------------------------------
bt_grid_abund<-grid%>%filter(ABUNDANCE_TYPE %in% c("Count", "Density", "MeanCount"))

rarefy_abund <- rarefy_ts(grid=bt_grid_abund, 
                          type="count", 
                          resamples=100, 
                          trimsamples = F, 
                          data_pt_thrs = data_pt_thrs)
# saving results
saveRDS(rarefy_abund,paste("./Results/rarefy_abund_data_pt_thrs_",data_pt_thrs,".RDS",sep=""))


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
saveRDS(rarefy_bmass,paste("./Results/rarefy_bmass_data_pt_thrs_",data_pt_thrs,".RDS",sep=""))

#---------------------- now clean a bit and merge both Abund/biomass data ----------------------
z_abund<-rarefy_abund%>%group_by(STUDY_ID,Species,YEAR)%>%
  dplyr::summarise(Value=median(Value))%>%ungroup()

z_bmass<-rarefy_bmass%>%group_by(STUDY_ID,Species,YEAR)%>%
  dplyr::summarise(Value=median(Value))%>%ungroup()

bt_rarefied<-rbind(z_abund,z_bmass)

# saving results
saveRDS(bt_rarefied,paste("./Results/bt_rarefied_data_pt_thrs_",data_pt_thrs,".RDS",sep=""))



