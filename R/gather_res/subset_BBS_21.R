rm(list=ls())
`%notin%` <- Negate(`%in%`)
library(tidyverse)
r_BBS<-readRDS("../../Results/for_BBS/summary_table_detail_version.RDS") 
r_BBS<-r_BBS%>%select(siteid,Stratum_name,Stratum_area)%>%arrange(Stratum_area)
# Tundra (5 routes) and Southern alaska coast (4 routes) have no info for their area (sq. Km.)

# sort area column
#r_BBS$Stratum_area<-log10(r_BBS$Stratum_area)
hist(r_BBS$Stratum_area/1000,1000)
r_BBS$n_min<-r_BBS$Stratum_area/(r_BBS$Stratum_area[1]*6) # scaling area
r_BBS$n_int<-ceiling(r_BBS$n_min) # make it interger for sampling

xx<-r_BBS%>%group_by(Stratum_name)%>%count()%>%ungroup() # n is the max limit for sampling
r_BBS<-inner_join(r_BBS,xx,by="Stratum_name")

id<-which(r_BBS$n_int>r_BBS$n)
r_BBS$n_int[id]<-r_BBS$n[id]

id<-which(r_BBS$Stratum_name%in%c("Tundra","Southern Alaska Coast"))
r_BBS$n_int[id]<-r_BBS$n[id]

r_BBS_selected<-r_BBS%>%distinct(Stratum_name,.keep_all = T) 
sum(r_BBS_selected$n_int)/nrow(r_BBS)  # 26%


r_BBS_selected<-r_BBS_selected%>%dplyr::select(Stratum_name,n_int)

set.seed(seed=123)

subset_BBS<-c()
for(i in 1:nrow(r_BBS_selected)){
  selid<-r_BBS_selected$Stratum_name[i]
  nid<-r_BBS_selected$n_int[i]
  tempo<-r_BBS%>%filter(Stratum_name%in%selid)%>%slice_sample(n=nid)
  subset_BBS<-rbind(subset_BBS,tempo)
}
#zz<-r_BBS%>%group_by(Stratum_name)%>%sample_n(size=n_int)



# ================= now draw the samples ==============================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
df_nb<-df%>%filter(source%notin%"BBS") # not bird result
dfb<-df%>%filter(source%in%"BBS") # bird result from BBS

#dfb<-dfb%>%filter(newsite%in%zz$siteid) # sampled bird result
dfb<-dfb%>%filter(newsite%in%subset_BBS$siteid) # sampled bird result

# now add together
res_subset<-rbind(df_nb,dfb)
table(res_subset$REALM)/nrow(res_subset)

saveRDS(res_subset,"../../Results/gather_res/stability_metric_all_subset_birds_21.RDS")



#old<-readRDS("../../Results/gather_res/stability_metric_all_subset_birds_21.RDS")
#old_b<-old%>%filter(source=="BBS")

