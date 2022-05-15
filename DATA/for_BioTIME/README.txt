for_BioTIME: 
raw_data: accessed on 31 Aug 2021 from BioTIME database
https://biotime.st-andrews.ac.uk/download.php 
also few private data from Sarah

To know more about the database, see https://doi.org/10.1111/geb.12729

wrangled_data: rarefied version 
- considered Freshwater and Terrestrial realms, Marine were very few.
- selected STUDY_ID those had >= 20 YEAR data points.
- for each STUDY_ID, we considered nested plots within as a separate community.
- made consistent (number of sampling months) sampling effort with maximum possible overlap of sampling months from each year (R/for_BioTIME/monthly_rarefy.R)
- >= 2 species those present atleast 70% of study period were considered as common species for the community.

