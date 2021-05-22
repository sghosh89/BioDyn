set.seed(seed=101)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(lazyeval)
# 
#study<-x 
#study<-study%>%select(DAY,MONTH,YEAR,Species,Abundance)

# Arg:
# study = a data frame with DAY, MONTH, YEAR, Species, Value (Abundance or Biomass) from raw data
# field = "Abundance" or "Biomass"
monthly_rarefy<-function(study,resamples=100,field){
   
  # how many months sampled each year?
  nsample<-study%>%group_by(YEAR)%>%
    summarize(nm=n_distinct(MONTH))%>%ungroup()
  
  #resamples<-100
  # min months sampled per year throughout the study period
  min_samp<-min(nsample$nm)
  
  #	define a function to calculate the sum(field) for use on the rarefied sample
  sum_field <- interp(~sum(as.numeric(var), na.rm=T),
                      var= as.name("Value"))
  
  zero_NA_filter <- interp(~y > x & !is.na(y), 
                           .values = list(y = as.name("Value"), x = 0))
  
  study <- study %>% unite(col=ObsEventID, YEAR, MONTH, sep="_", remove=FALSE)
  
  ##	initialise 
  rarefied_metrics <- data.frame()
  
  #not needed
  #nsamples <- ungroup(study) %>%
  #  group_by(YEAR) %>%
    # remove 0's or NA's (this is to catch any places where 
    # abundance wasn't actually recorded 
    # or the species is indicated as absent)
  #  filter_(.dots=zero_NA_filter) %>%
    # calculate how many months sampled per year 
  #  dplyr::summarise(nsamples = n_distinct(ObsEventID)) 
  
  study_nest<-study%>%group_by(ObsEventID,YEAR,MONTH)%>%
                      filter_(.dots=zero_NA_filter) %>%
                      nest(nest_cols=c("Species", "Value"))%>% 
                  ungroup()
    
  
  for(i in 1:resamples){
    
    rare_samp <- study_nest %>%
      # rarefy to min_samp 
      group_by(YEAR) %>%
      sample_n(size=min_samp) %>% # here is the stochasticity to choose a set of species at every resampling 
      # unpack and collate taxa from rarefied sample
      unnest(nest_cols)%>%
      group_by(YEAR, Species) %>%
      dplyr::summarise_(
        Value=sum_field) %>% # this could be Abundance/ Biomass
      ungroup()	
    
    # complete the data
    rare_samp_c<-rare_samp %>% 
      complete(Species, 
               nesting(YEAR), 
               fill = list(Value = 0))
    rare_samp_c<- rare_samp_c%>% mutate(i_numresamp=i)
    
    rarefied_metrics<-rbind(rarefied_metrics, rare_samp_c)
  }
  
  rarefied_metrics <- as_tibble(rarefied_metrics) 
  rarefied_metrics<-rarefied_metrics%>%group_by(Species,YEAR)%>%
    dplyr::summarise(Value=mean(Value))%>%ungroup()
}


