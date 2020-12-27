# Wrangle raw data

if(!dir.exists('../../DATA/for_BioTIMEx/wrangled_data/'))   dir.create('../../DATA/for_BioTIMEx/wrangled_data/')
listF <- list.files('data wrangling', pattern = ".R|.r", full.names = TRUE)
lapply(listF, function(fullPath) source(fullPath, encoding = 'UTF-8', echo = FALSE, local = TRUE))
