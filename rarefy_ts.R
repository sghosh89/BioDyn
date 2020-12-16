##======================================================================

rm(list=ls())

##	load packages
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(lazyeval)


set.seed(seed=101)

#=================================FUNCTION TO RAREFY DATA============================

# Arguments:
# grid: input dataset (could be spatially gridded or raw BioTIME data)
# type: count, or biomass (we don't need presence/absence data)
# resamples: the number of bootstrap resampling events desired (default is 100)
# trimsamples: TRUE means that years with < 1/2 the average number of samples 
              #should be removed to avoid excessive information loss. Default is FALSE.
              #calculate the number of sampling events per year, and find the minimum
              #resample the data to rarefy the diversity metrics
# data_pt_thrs: 20 (default) years for a given STUDY_ID

# output: a new dataframe with rarefied time series

rarefy_ts <- function(grid, type=c("count", "biomass"), resamples=100, trimsamples=FALSE, data_pt_thrs=20){
  
  #	CALCULATE RAREFIED METRICS for each study for all years 
  #	restrict calculations to where there is abundance>0 AND
  #	following removal of NAs and 0's there are still more than 2 years
  
  # Check if the data is count abundance: if yes, calculate all rarefied metrics
  # Check if the data is presence or biomass: if yes, calculate only S and Jaccards
  # If is.na(ABUNDANCE_TYPE), then should calculate on the Biomass column
  
  
  if(type == "count"){ 
    field <- "Abundance" 
  }else{ 
    field <- "Biomass"
  }
  
  # Get the sample-size to rarefy to. How many sampling events per STUDY_ID per year?
  # This is handled differently depending on the data type
  
  # define a filter for 'field' to be >0 and not NA 
  zero_NA_filter <- interp(~y > x & !is.na(y), 
                           .values = list(y = as.name(field), x = 0))
  
  #	define a function to calculate the sum(field) for use on the rarefied sample
  sum_field <- interp(~sum(as.numeric(var), na.rm=T),
                      var= as.name(field))
  
  grid <- grid %>% unite(col=ObsEventID, STUDY_ID, PLOT, YEAR, sep="_", remove=FALSE)
  nsamples <- ungroup(grid) %>%
    group_by(STUDY_ID, YEAR) %>%
    # remove 0's or NA's (this is to catch any places where 
    # abundance wasn't actually recorded 
    # or the species is indicated as absent)
    filter_(.dots=zero_NA_filter) %>%
    # calculate how many observations per year per study: i.e. total number of plots per STUDY_ID+YEAR combo
    dplyr::summarise(nsamples = n_distinct(ObsEventID)) 
  
  # Check if you wanted to remove years with especially low samples (< 1/2 the average number of samples)
  if(trimsamples){
    # Calculate the mean number of samples per STUDY_ID
    mean_samp <- ungroup(nsamples) %>% group_by(STUDY_ID) %>%
      mutate(mean_samp = mean(nsamples), 
             lower_bound = mean(nsamples)/2) %>%
      filter(nsamples >= lower_bound)
    
    min_samp <- ungroup(mean_samp) %>% group_by(STUDY_ID) %>%
      mutate(min_samp = min(nsamples)) %>%
      # retain only the rows with the minimum sample size for a given cell
      filter(nsamples==min_samp) %>%
      distinct(STUDY_ID, min_samp, .keep_all=TRUE)
    
    # join the data and filter out years with  nsamples less than 1/2 the mean number of samples
    grid <- inner_join(grid, dplyr::select(min_samp, - YEAR, -nsamples)) %>%
      filter(n_samps >= lower_bound)
    rm(mean_samp,min_samp)
    print("trimsamples==TRUE: Removing years with < 1/2 the average number of samples for a given ID")
    
  }else{
    # Calculate the minimum number of samples per STUDY_ID
    min_samp <- ungroup(nsamples) %>% group_by(STUDY_ID) %>%
      mutate(min_samp = min(nsamples)) %>%
      # retain only the rows with the minimum sample size for a given cell
      filter(nsamples==min_samp) %>%
      distinct(STUDY_ID, min_samp, .keep_all=TRUE)
    
    #	Add the min_samp to the data and tidy a little
    grid <- inner_join(grid, dplyr::select(min_samp, - YEAR, -nsamples))
    rm(min_samp)
  }
  
  # Re-calculate metadata
  new_meta <- ungroup(grid) %>%
    # remove 0's or NA's (this is to catch any places where 
    # abundance wasn't actually recorded 
    # or the species is indicated as absent)
    filter_(.dots=zero_NA_filter) %>%
    group_by(STUDY_ID) %>%
    summarise(
      # total number of species in STUDY_ID time-series 
      SamplePool = n_distinct(Species),
      # total number of individuals
      SampleN = ifelse(type=='count', sum(as.numeric(Abundance)), NA),
      # number of years sampled
      num_years = n_distinct(YEAR),
      # duration of time series, start and end points
      duration = max(YEAR) - min(YEAR) + 1,
      startYear = min(YEAR),
      endYear = max(YEAR)) 
  
  #	Create dataframe where unique observations (i.e., the data of an
  #	ObsEventID's [individual species abundances])
  #	are nested within cells within years within studies 
  bt_grid_nest <- ungroup(grid) %>%
    group_by(STUDY_ID, ObsEventID, YEAR, min_samp) %>%
    # remove 0's or NA's (to catch any places where abundance wasn't actually recorded or the species is indicated as absent)
    filter_(.dots=zero_NA_filter) %>%
    # depending on type: nest(Species, Abundance) OR nest(Species, Biomass)
    nest(nest_cols=c("Species", field))%>%
    #nest_(key_col="data", nest_cols=c("Species", field)) %>%
    # reduce to studies that have more than two time points for a given cell
    group_by(STUDY_ID) %>%
    #keeps all study_cells with 'data_pt_thrs' more years of data
    filter(n_distinct(YEAR)>=data_pt_thrs) %>%  # 
    ungroup()
  
  
  ##	initialise df to store all biochange metrics 
  rarefied_metrics <- data.frame()
  
  ##	rarefy rarefy_resamps times
  for(i in 1:resamples){
    
    ## loop to do rarefaction for each study
    for(j in 1:length(unique(bt_grid_nest$STUDY_ID))){
      #for(j in 1:2){
      print(paste('rarefaction', i, 'out of', resamples, 'for study_cell', j, '(', unique(bt_grid_nest$STUDY_ID)[j], ')',  'in', length(unique(bt_grid_nest$STUDY_ID))))
      
      ##	get the jth study_cell
      study <- bt_grid_nest %>%
        filter(STUDY_ID==unique(bt_grid_nest$STUDY_ID)[j]) 
      
      # get minimum sample size for rarefaction	
      min_samp <- study %>% distinct(min_samp) %>% .$min_samp
      
      # ------------------------------- some checks for gridding -------------------------------------
      # check that there is only one cell represented (This shouldn't be a problem)
      #if(length(unique(study$STUDY_ID))>1) { 
      #  stop(paste0("ERROR: ", unique(study$STUDY_ID), " contains more than one grid cell")) }
      
      # check there there is more than one year in the cell
      #if(length(unique(study$YEAR))<2) {
      #  print(paste0("ERROR: ", unique(study$STUDY_ID), " does not have more than one year"))
      #  next }
      # ------------------------------------------------------------------------------------------------
      
      rare_samp <- study %>%
        # rarefy to min_samp 
        group_by(STUDY_ID, YEAR) %>%
        sample_n(size=min_samp) %>% # here is the stochasticity to choose a set of species at every resampling 
        # unpack and collate taxa from rarefied sample
        unnest(nest_cols)%>%
        # add unique counter for a resampling event
        #mutate(rarefy_resamp = uniq_id) %>% 
        # collate species within years
        group_by(STUDY_ID, YEAR, Species) %>%
        dplyr::summarise_(
          Value=sum_field) %>% # this could be Abundance/ Biomass
        ungroup()	
      
      # complete the data
      rare_samp_c<-rare_samp %>% 
                   complete(Species, 
                            nesting(STUDY_ID, YEAR), 
                            fill = list(Value = 0)) #%>%spread(Species, Value) 
      rare_samp_c<- rare_samp_c%>% mutate(i_numresamp=i, j_num_STUDY_ID=j)
      #(c1<-colnames(rare_samp_c))
      
      #---------- cross check with Sarah's code --------------
      # create community matrix of rarefied sample
      # rare_comm  <- ungroup(rare_samp) %>%
      #  spread(Species, Value, fill=0) 
    
      #dim(rare_comm)
      #(c2<-colnames(rare_comm))
     
      #c1==c2
     #------------------------------------------------------------
      
      rarefied_metrics<-rbind(rarefied_metrics, rare_samp_c)
      
    }	# STUDY_ID loop 
  }	# rarefaction loop
  
  rarefied_metrics <- as_tibble(rarefied_metrics) 
  
  # combine with the new metadata
  rarefied_metrics <- inner_join(new_meta, rarefied_metrics, by='STUDY_ID') 
  
  return(rarefied_metrics)
} 


