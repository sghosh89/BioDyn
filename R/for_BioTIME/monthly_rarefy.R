# This function tries to make consistent (number of sampling months) sampling effort
# with maximum possible overlap of sampling months from each year

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
  
  # this is a list of months sampled for each year, some months in a year maybe sampled for multiple days
  monthlist<-study%>%group_by(YEAR)%>%
    summarize(um = unique(MONTH),nm= n_distinct(MONTH), min_m=min(MONTH),max_m=max(MONTH))%>%ungroup()
  
  # now we want min_samp of months (a common set as far as possible) from each year 
   mydf<-data.frame(YEAR=nsample$YEAR,MONTH=NA)
   months_peryr <- vector(mode = "list", length = nrow(mydf))
   for (i in 1:nrow(mydf)){
     yr<-mydf$YEAR[i]
     mydf$YEAR[i]<-yr
     tempo<-monthlist%>%filter(YEAR==yr)
     mydf$MONTH[i]<-list(sort(tempo$um))
     months_peryr[[i]]<-sort(tempo$um)
   }
  
   # get the common months sampled throughout the study years
   common_months<-Reduce(intersect, months_peryr)
   
   mydfc<-vector(mode = "list", length = nrow(mydf)) 
   names(mydfc)<-mydf$YEAR
   
   # first check, are there any common months throughout years? 
   # at least one common month make this non-zero
   if(length(common_months)==0){ # no common months
     cat("========== Not a single common month found throughout the years =========")
     # then do usual rarefaction
     
     # need to fill in as it is
     mydfc<-months_peryr
     
   }
   
   if(length(common_months)!=0){ # with common months
     cat("--------- months common from each year =",paste(common_months, collapse = ", ")," -------- \n")
     
     for(i in 1:length(months_peryr)){
       
       mym<-months_peryr[[i]]
       
       if(length(common_months)==min_samp){ # common months are min sampling months for a year
         selected_cms<-common_months
       }else if (length(common_months)>1 & length(common_months)<min_samp){ # logically, lengh(common_months)<min_samp, min_samp>1
         fm<-head(common_months,1) # first month
         em<-tail(common_months,1) # end month
         cmr<-c(fm:em) # common months basic range
         needm<- min_samp - length(cmr) # this number of months needed
         
         # our goal is to choose possible nearest 'needm' months of fm and em
         nearfm<-sort(abs(mym-fm), index.return=T)$ix[-1] # indicies arranged near to far, fm itself excluded 
         nearem<-sort(abs(mym-em), index.return=T)$ix[-1] # indicies arranged near to far, em itself excluded 
         if(needm%%2==0){# needm is even number
           needm<-needm/2
         }else{
           needm<-(needm+1)/2
         }
         
         # now choose equal number of nearest neighbors from fm and em
         # but it should be a range expansion: so leftside limit should be < fm
         # right side limit should be > em
         
         fm_extra<-mym[nearfm]
         fm_extra<-fm_extra[fm_extra<fm]
         fm_extra<-fm_extra[1:needm]
         
         em_extra<-mym[nearem]
         em_extra<-em_extra[em_extra>em]
         em_extra<-em_extra[1:needm]
         
         selected_cms<-sort(c(cmr,fm_extra,em_extra))
       }
       
       mydfc[[i]]<-selected_cms
       
     }
     
   }
  
  #	define a function to calculate the sum(field) for use on the rarefied sample
  sum_field <- interp(~sum(as.numeric(var), na.rm=T),
                      var= as.name("Value"))
  
  zero_NA_filter <- interp(~y > x & !is.na(y), 
                           .values = list(y = as.name("Value"), x = 0))
  
  # Now, we need to select only those months for each year according to 'mydfc' list
  study_upd<-c()
  for(i in 1:length(mydfc)){
    tempo<-study%>% filter(YEAR==names(mydfc)[i] & MONTH%in%mydfc[[i]])
    study_upd<-rbind(study_upd,tempo)
  }
  
  study_upd <- study_upd %>% unite(col=ObsEventID, YEAR, MONTH, sep="_", remove=FALSE)
  
  ##	initialise 
  rarefied_metrics <- data.frame()
  
  #not needed
  #nsamples <- ungroup(study_upd) %>%
  #  group_by(YEAR) %>%
    # remove 0's or NA's (this is to catch any places where 
    # abundance wasn't actually recorded 
    # or the species is indicated as absent)
  #  filter_(.dots=zero_NA_filter) %>%
    # calculate how many months sampled per year 
  #  dplyr::summarise(nsamples = n_distinct(ObsEventID)) 
  
  study_nest<-study_upd%>%group_by(ObsEventID,YEAR,MONTH)%>%
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





