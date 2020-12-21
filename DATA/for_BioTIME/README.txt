for_BioTIME: 
raw_data: accessed on 18 Nov 2020 from http://biotime.st-andrews.ac.uk/downloadFull.php
wrangled_data: rarefied version based on Sarah's code from
		 https://github.com/sChange-workshop/BioGeo-BioDiv-Change/blob/master/R/02_rarefy_griddedData_clusterVersion.R


Method:
- I have downloaded the data from BioTIME database **(is there any possibility to get data - a bit extra- from Sarah? 
  Sarah told me she will contact M. Dornales for data usage permission)**
- I **did not use spatial gridding - is it okay?** Too big multilocation have an obvious higher richness in species than single sites. 
  Is it a problem in my case? 
- Then with the raw data (no Presence/ Absence data is used and always abundance data has preference over biomass data), I have rarefied -
     1) Compute the number of unique plots per STUDY_ID+YEAR combo and 
           took the minimum number of samplings for a given STUDY_ID from the entire sampling period.
     2) We have the info about the species list and abundance/ biomass data for each STUDY_ID, PLOT, YEAR combination. 
        But only choose those STUDY_ID which had >= 20 YEAR data points. (and there were 62 such STUDY_IDs)
     3) Now for every resampling (total 100 times) we randomly sampled (without replacement) the minimum number 
        of sampling (i.e. the number of plots) recorded for a given STUDY_ID for a given YEAR - kept the species list 
        and abundance from those plots and repeated 100 times - fill the absent species for a given STUDY_ID+YEAR with 
        0 - finally took the median values of abundance/ biomass for each species for each STUDy_ID for each 
        YEAR. - This is the rarefied version of BioTIME.
- For each REALM (= Freshwater, Marine, Terrestrial) we did the following procedure to get tail-dependence results.
    1) We did a pairwise tail-dependence estimate for common target species for each STUDY_ID. So the species which were 
       present <= 30% of sampling years were considered not so common (rare) and aggregated as rare species. In this 
       process, some sites again got discarded as sometimes they had no species left. **(there 
       are some heterogeneity between super species-rich sites and normal sites - this could be due to not using spatial grinding as 
       some sites are MultiLocations and some SingleLocation)**.
    2) For each good sites (with some common species) we got a summary table: the number of total 
       target common species in the sites (n), number of pairwise interaction possible (nC2), 
       number of independent interactions (I), number of synchronous (when rare) interaction (L), 
       number of synchronous (when abundant) interaction (U), number of compensatory interaction (C). We made 
       a histogram for each site with (I, L, U, C)/nC2.
