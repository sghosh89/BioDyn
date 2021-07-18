for_BBS: data folder

# How did we wrangle BBS data to be used for our analysis? (guide: R/for_BBS/data_wrangling.R)

Routes those maintained BBS protocol and surveyed for atleast 20 years of total study period (1997-2019) 
were considered. We took total abundance from 50 stops (months: 4 to 7) for each such route.
We made unique ID for combination of CountryNum, StateNum, Route and for each such uniqueID we considered 
community of >= 2 bird species which were commonly found for 70% of study period.
