rm(list=ls())
library(tidyverse)
`%notin%` <- Negate(`%in%`)
#--------------------------------
# read the data
xroutes<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/routes.csv") # route meta data
xweather<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/weather.csv") # weather data

# it's a problem to read this text file: not in good format
xsplist<-read.delim2("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/SpeciesList_edited.txt",
                     header=F,sep=" ")

# no individual species level - only a summery info in States folder: though from 1966-
# so, I am not going to use them

#-----------  I am choosing the species level info for each route (1997-2019) from 50-StopData folder --------------
# for StateNum = 02,Alabama; 03,Alaska; 04,Alberta; 06,Arizona; 07,Arkansas
f1<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty1.csv")
# for StateNum = 11,BritishColumbia; 14,California; 17,Colorado; 18,Connecticut; 21,Delaware
f2<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty2.csv")
# for StateNum = 25,Florida;27,Georgia; 33,Idaho; 34,Illinois; 35,Indiana; 36,Iowa
f3<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty3.csv")
# for StateNum = 38,Kansas; 39,Kentucky;42,Louisiana; 43,Northwest Territories; 44,Maine; 45,Manitoba; 46,Maryland;47,Massachusetts;
f4<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty4.csv")
# for StateNum = 49,Michigan; 50,Minnesota; 51,Mississippi; 52,Missouri;53,Montana; 54,Nebraska; 55,Nevada; 56,New Brunswick; 57,Newfoundlandand Labrador;
f5<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty5.csv")
# for StateNum = 58,New Hampshire; 59,New Jersey; 60,New Mexico; 61,NewYork; 63,North Carolina;
f6<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty6.csv")
# for StateNum = 62,Nunavut; 64,North Dakota; 65,Nova Scotia;66,Ohio; 67,Oklahoma; 68,Ontario;
f7<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty7.csv")
# for StateNum = 69,Oregon; 72,Pennsylvania; 75,PrinceEdward Island; 76,Quebec; 77,Rhode Island; 79,Saskatchewan; 80,SouthCarolina;
f8<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty8.csv")
# for StateNum = 81,South Dakota; 82,Tennessee; 83,Texas; 85,Utah; 87,Vermont
f9<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty9.csv")
# for StateNum = 88,Virginia; 89,Washington; 90,West Virginia; 91,Wisconsin; 92,Wyoming;93,Yukon
f10<-read.csv("../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty10.csv")

ffull<-rbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10)
saveRDS(ffull,"../../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/50-StopData/1997ToPresent_SurveyWide/fifty1to10.RDS")

#------------------------ now data cleaning --------------------------------
fshort<-ffull%>%filter(Year%in%c(1997:2019)) # filter for 1997-2019
colnames(fshort)
sum1to50<-apply(fshort[,8:57],MARGIN=1,FUN=sum)
fshort<-fshort%>%
            mutate(Stop1to50=sum1to50)%>% # total count for all 50 stop on each route
            select(RouteDataID,CountryNum,StateNum,Route,RPID,Year,AOU,Stop1to50) # selecting some columns

# now, consider the RouteDataID which were consistently maintained BBS protocol
xweather_good<-xweather%>%filter(Year%in%c(1997:2019))%>%
                          filter(RunType==1)
fshort<-fshort%>%filter(RouteDataID%in%xweather_good$RouteDataID)%>%select(-RPID)

# check some summary 
# consistent sampling effort? no: because each route within a state of a country was not sampled for each year
c<-fshort%>%group_by(CountryNum,StateNum,Route)%>%summarize(nyr=n_distinct(Year),
                                                      nRID=n_distinct(RouteDataID),
                                                      uRID=unique(RouteDataID))%>%ungroup()

# so, first we will select the routes which were sampled atleast for 20 years
cg<-c%>%filter(nyr>=20)
fshort<-fshort%>%filter(RouteDataID%in%cg$uRID)

# but now watch the time of sampling: Month column in xweather_good
xweather_good<-xweather_good%>%filter(RouteDataID%in%fshort$RouteDataID)
c0ym<-xweather_good%>%group_by(RouteDataID)%>%summarise(nm=n_distinct(Month),
                                                        um=unique(Month))%>%ungroup() # this means each route 
                                                        # in a sate in a country sampled once in a year
# but in which month?
c0ym1<-as.data.frame(table(c0ym$um)) # months saampled 4<7<5<6 # caution: whether you only want month 6?
                                     # currently I am considering all months

# number of rotes and years per country-state combo
c1<-fshort%>%group_by(CountryNum,StateNum)%>%summarize(nr=n_distinct(Route),
                                                      nyr=n_distinct(Year))%>%ungroup()

# but how many years per route in a state in a country?
c2<-fshort%>%group_by(CountryNum,StateNum,Route)%>%summarize(nyr=n_distinct(Year))%>%ungroup()
# so, the number of rows in table c2 is eqv. to the site number where we will consider the bird community

fshort<-fshort%>%unite("Country_State_Route",CountryNum,StateNum,Route,sep="_")
length(unique(fshort$Country_State_Route)) # number of unique sites/ routes

fshort_list<-split(fshort,f=fshort$Country_State_Route)
uroutes<-names(fshort_list)

#-------- create wrangled_data folder --------
resloc<-"../../DATA/for_BBS/wrangled_data/"

saveRDS(fshort_list,paste(resloc,"sourcefile_list.RDS",sep=""))

for (i in 1:length(uroutes)){
  resloc2<-paste(resloc,uroutes[i],"/",sep="")
  if(!dir.exists(resloc2)){
    dir.create(resloc2)
  }
}
#---------------------------------------------
badroutes<-c()
for (i in 1:length(fshort_list)){
  x<-fshort_list[[i]]
  resloc2<-paste(resloc,names(fshort_list)[i],"/",sep="")
  saveRDS(x,paste(resloc2,"sourcefile.RDS",sep=""))
  y<-x%>%select(Year,AOU,Stop1to50)
  y<-y%>%spread(AOU,Stop1to50,fill=0)%>%as.data.frame()
  rownames(y)<-y$Year # and colnames are species code
  y<-y[,-1]
  count_non0<-apply(y,MARGIN=2,FUN=function(x){sum(x!=0)})
  commonspmat<-y[,which(count_non0>=0.7*nrow(y))] # common sp present atleast 70% of sampling years
  
  if(ncol(commonspmat)>=2){
    rarespmat<-y[,which(count_non0<0.7*nrow(y))]
    if(ncol(rarespmat)>0){
      rarespmat<-as.matrix(rarespmat)
      commonspmat$raresp<-apply(rarespmat,MARGIN=1,FUN=sum)
    }else{
      cat("-------- No rare sp in ",names(fshort_list)[i]," ----------\n")
    }
    saveRDS(commonspmat,paste(resloc2,"input_mat_for_tailanal.RDS",sep=""))
  }else{
    badroutes<-c(badroutes,names(fshort_list)[i])
    cat("-------- commonsp is less than 2 in ",names(fshort_list)[i]," ----------\n")
  }
}
uroutes<-setdiff(uroutes,badroutes)
saveRDS(uroutes,paste(resloc,"unique_routes_all.RDS",sep=""))
#-------------------------------------------------------------
































