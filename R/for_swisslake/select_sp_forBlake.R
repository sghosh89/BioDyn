rm(list=ls())
graphics.off()

# read clean phytoplankton data
L1to8_clean<-readRDS("../../DATA/for_swisslake/wrangled_data/alllake_cleanedlist.RDS")

for(i in 1:8){
  cat("i=",i,"\n")
  L_c<-L1to8_clean[[i]]
  L1<-L_c%>%filter(abun_mean!=0)
  L1tb<-as.data.frame(table(L1$species))
  L1s<-L1%>%group_by(species)%>%summarise(total_yr=length(unique(L1$year)),
                                          present_yr=n_distinct(year),
                                          abundance=sum(abun_mean))%>%ungroup()
 
  L1s$include<-0 # Blake needs to replace the 0 in this column with 1 if he would like to keep that species
  
  write.csv(L1s,
            paste("../../DATA/for_swisslake/wrangled_data/species_list_",names(L1to8_clean)[i],".csv",sep=""),
            row.names = F)
}





