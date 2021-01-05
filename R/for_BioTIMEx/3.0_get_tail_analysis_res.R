listF <- list.files('./', pattern = "_ta.R|_ta.r", full.names = TRUE)
lapply(listF, function(fullPath) source(fullPath, encoding = 'UTF-8', echo = FALSE, local = TRUE))
