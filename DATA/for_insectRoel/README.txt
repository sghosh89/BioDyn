data folder contains:
20yrFreshwater_Metadata.csv - metadata
20yrFreshwaterData 202106.rds - raw data in rds format
20yrFreshwaterData 202106.csv - raw data in csv format

# How did we clean the data?

- atleast 20 year long time series, >=2 species those present at least 70% of 
  total study period were considered for each community. Plots were nested within each STUDY_ID.
- mostly specification level is 'species' but when it is not then aggregated to 
  most commonly specified level: genus, family, subfamily.