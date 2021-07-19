for_BioTIME: 
raw_data: accessed on 18 Nov 2020 from http://biotime.st-andrews.ac.uk/downloadFull.php, also few private data from Sarah

wrangled_data: rarefied version 
- considered Freshwater and Terrestrial realms, Marine were very few.
- selected STUDY_ID those had >= 20 YEAR data points.
- for each STUDY_ID, we considered nested plots within as a separate community.
- made consistent (number of sampling months) sampling effort with maximum possible overlap of sampling months from each year (R/for_BioTIME/monthly_rarefy.R)
- >= 2 species those present atleast 70% of study period were considered as common species for the community.

