library(tidyverse)
`%notin%` <- Negate(`%in%`)

# function to get input spmat for tail analysis of swiss phytoplankton
# Input:
# Ldata: cleaned data for a lake
# badsp: a character string, if you want to exclude some species
# nametag: a character for saving results for a lake

get_input_spmat_phytoplankton<-function(Ldata,badsp,nametag){
  
  # exclude badsp
  Ldata<-Ldata%>%filter(species%notin%badsp)
  
  # genus level aggregation
  Ldata<-Ldata%>%separate(species,c("genus","sp")," ")
  Ldata$sp<-"sp"
  Ldata<-Ldata%>%unite("species",genus:sp,sep=" ")
  
  Ldata<-Ldata%>%group_by(year,species)%>%
    summarise(abundance=sum(abun_mean))%>%ungroup()
  mat<-Ldata%>%spread(species,abundance,fill=0)%>%as.data.frame()
  rownames(mat)<-mat$year
  mat<-mat[,-1]
  presentyr<-apply(mat,MARGIN = 2,FUN = function(x){sum(x!=0)})
  commonsp<-which(presentyr>=0.7*nrow(mat)) # commonsp present 70% of sampling year 
  raresp<-which(presentyr<0.7*nrow(mat))
  
  commonspmat<-mat[,commonsp]
  
  if(length(raresp!=0)){
    rarespmat<-mat[,raresp]
    commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
  }
  
  saveRDS(commonspmat,paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",nametag,".RDS",sep=""))
  
}

##########################################################
# now call the function

L1to8_c<-readRDS("../../DATA/for_swisslake/wrangled_data/alllake_cleanedlist.RDS")

# for lake 1
Ldata<-L1to8_c$L1WA_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L1WA_c_blake_sorted.csv")
badsp<-spfile$species[which(spfile$include==0)]
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L1WA")


# for lake 2
Ldata<-L1to8_c$L2UZ_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L2UZ_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L2UZ")


# for lake 3
Ldata<-L1to8_c$L3LU_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L3LU_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L3LU")


# for lake 4
Ldata<-L1to8_c$L4LZ_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L4LZ_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L4LZ")

# for lake 5
Ldata<-L1to8_c$L5SE_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L5SE_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L5SE")

# for lake 6
Ldata<-L1to8_c$L6HA_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L6HA_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L6HA")

# for lake 7
Ldata<-L1to8_c$L7BA_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L7BA_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L7BA")

# for lake 8
Ldata<-L1to8_c$L8GR_c
spfile<-read.csv("../../DATA/for_swisslake/wrangled_data/species_list_L8GR_c_blake_sorted.csv")
(badsp<-spfile$species[which(spfile$include==0)])
get_input_spmat_phytoplankton(Ldata = Ldata, badsp = badsp, nametag = "L8GR")


