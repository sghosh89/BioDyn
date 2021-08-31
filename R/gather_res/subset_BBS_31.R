rm(list=ls())
`%notin%` <- Negate(`%in%`)
library(tidyverse)
r_BBS<-readRDS("../../Results/for_BBS/summary_table_detail_version.RDS") 
r_BBS<-r_BBS%>%select(siteid,Stratum_name,Stratum_area)%>%arrange(Stratum_area)
# Tundra (5 routes) and Southern alaska coast (4 routes) have no info for their area (sq. Km.)

# sort area column
#r_BBS$Stratum_area<-log10(r_BBS$Stratum_area)
hist(r_BBS$Stratum_area/1000,1000)
r_BBS$n_min<-r_BBS$Stratum_area/(r_BBS$Stratum_area[1]*5) # scaling area
r_BBS$n_int<-ceiling(r_BBS$n_min) # make it interger for sampling

xx<-r_BBS%>%group_by(Stratum_name)%>%count()%>%ungroup() # n is the max limit for sampling
r_BBS<-inner_join(r_BBS,xx,by="Stratum_name")

id<-which(r_BBS$n_int>r_BBS$n)
r_BBS$n_int[id]<-r_BBS$n[id]

id<-which(r_BBS$Stratum_name%in%c("Tundra","Southern Alaska Coast"))
r_BBS$n_int[id]<-r_BBS$n[id]

r_BBS_selected<-r_BBS%>%distinct(Stratum_name,.keep_all = T) 
sum(r_BBS_selected$n_int)  

set.seed(seed=123)
zz<-r_BBS%>%group_by(Stratum_name)%>%sample_n(n_int)

# ================= now draw the samples ==============================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
df_nb<-df%>%filter(source%notin%"BBS") # not bird result
dfb<-df%>%filter(source%in%"BBS") # bird result from BBS

dfb<-dfb%>%filter(newsite%in%zz$siteid) # sampled bird result

# now add together
res_subset<-rbind(df_nb,dfb)
saveRDS(res_subset,"../../Results/gather_res/stability_metric_all_subset_birds_31.RDS")






