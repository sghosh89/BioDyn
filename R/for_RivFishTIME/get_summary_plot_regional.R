library(tidyverse)

# plotter function for regional summary results
# INPUT:
# x_meta : meta data
# summary_table: the whole summary results
# country: you want to choose (character)
# nr: number of rows if multiplanel==TRUE
# wd,ht,cex.lab,cex.main,cex,cex.axis: plot parameters

get_summary_plot_regional<-function(x_meta,summary_table,country,label_country,nr,wd,ht,multipanel,cex.lab,cex.main,cex,cex.axis){
  
  # now show results for the country
  meta_c<-x_meta%>%filter(Country%in%country)
  summary_table_c<-summary_table%>%filter(siteid%in%meta_c$TimeSeriesID)
  
  
  df<-summary_table_c%>%select(siteid,nsp,f_nind,f_nL,f_nU,f_nneg)
  dat<-t(df)
  colnames(dat)<-dat[1,]
  dat<-dat[-1,]
  nsp<-dat[1,]
  dat<-dat[-1,]
  dat<-as.data.frame(dat)
  
  any(colnames(dat)==summary_table_c$siteid)==F # checked
  
  dat<-as.matrix(dat)
  
  ncol(dat) # total number of sites in USA
  
  # for how many sites indep. interaction were dominant?
  sum(dat[1,]>dat[2,] & dat[1,]>dat[3,] & dat[1,]>dat[4,])
  
  # for how many sites LT asymmetry were dominant?
  nLT<-sum(dat[2,]>dat[3,])
  
  # for how many sites UT asymmetry were dominant?
  nUT<-sum(dat[3,]>dat[2,])
  
  # for how many sites +ve corr were dominant?
  sum((as.numeric(dat[2,])+as.numeric(dat[3,]))>as.numeric(dat[4,]))
  
  # for how many sites -ve corr were dominant?
  sum((as.numeric(dat[2,])+as.numeric(dat[3,]))<as.numeric(dat[4,]))
  
  pdf(paste("../../Results/for_RivFishTIME/hist_targetsp_",label_country,".pdf",sep=""),width=10,height=6)
  op<-par(mar=c(5,5,5,2))
  hist(summary_table_c$nsp, breaks=100, xlim=c(0,max(summary_table_c$nsp)), xlab="Number of target sp.",
       ylab="Frequency (sites)", col="skyblue", 
       main=paste("RivFishTIME: ",nrow(summary_table_c)," sites in ",label_country, sep=""))
  par(op)
  dev.off()
  
  if(multipanel==F){
    pdf(paste("../../Results/for_RivFishTIME/summary_plot_",label_country,".pdf",sep=""),width=wd,height=ht)
    op<-par(mar=c(5,5,5,1))
    x<-barplot(dat,
               main = paste("RivFishTIME dynamics for ",label_country,": min 20 yrs, total sites =",ncol(dat),
                            ", LT sites =",nLT,
                            ", UT sites =",nUT, sep=""),
               xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.3),
               cex.lab=2,cex.main=2,las=2,
               col = c("yellow","red","skyblue","green"))
    text(x = x, y = 1, label = paste("(",summary_table_c$nsp,")",sep=""), 
         pos = 3, cex = 1, 
         col = "purple")
    legend("top",horiz=T,bty="n",cex=2,
           c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
           fill = c("yellow","red","skyblue","green","purple"))
    par(op)
    dev.off()
  }else{ # multipanel plot starts
    colnames(dat)<-NULL
    xv<-1:ncol(dat)
    sv<-split(xv,sort(xv%%nr))
    
    pdf(paste("../../Results/for_RivFishTIME/summary_plot_",label_country,".pdf",sep=""),width=wd,height=ht)
    op<-par(mar=c(2,10,5,1),mfrow=c(nr,1),mgp=c(5,1,0))
    
    for(i in 1:length(sv)){
      a<-head(sv[[i]],1)
      b<-tail(sv[[i]],1)
      if(i==1){
        x<-barplot(dat[,a:b],
                   main = paste("RivFishTIME dynamics for ",label_country,": min 20 yrs, total sites =",ncol(dat),
                                ", LT sites =",nLT,
                                ", UT sites =",nUT, sep=""),
                   xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.3),
                   cex.lab=cex.lab,cex.main=cex.main,las=2,cex.axis = cex.axis,
                   col = c("yellow","red","skyblue","green"))
        legend("top",horiz=T,bty="n",cex=cex,
               c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
               fill = c("yellow","red","skyblue","green","purple"))
      }else{
        x<-barplot(dat[,a:b],
                   main = "",
                   xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.3),
                   cex.lab=cex.lab,cex.axis = cex.axis,las=2,
                   col = c("yellow","red","skyblue","green"))
      }
      text(x = x, y = 1, label = paste("(",summary_table_c$nsp[a:b],")",sep=""), 
           pos = 3, cex = cex, 
           col = "purple")
    }
    
    par(op)
    dev.off()
  }
  
}

# call the function
good_TimeSeriesID_q3q4<-readRDS("../../DATA/for_RivFishTIME/wrangled_data/good_TimeSeriesID_q3q4.RDS")

x_meta<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_TimeseriesTable.csv")
x_meta<-x_meta%>%filter(TimeSeriesID%in%good_TimeSeriesID_q3q4)

summary_table<-readRDS("../../Results/for_RivFishTIME/summary_table.RDS")
summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

# for USA
get_summary_plot_regional(x_meta=x_meta,summary_table=summary_table,nr=1,
                         country = "USA",label_country = "North America",wd=35,ht=7,multipanel = F)
# for Northern Europe
get_summary_plot_regional(x_meta=x_meta,summary_table=summary_table,nr=5,
                         country = c("FIN","SWE"),label_country = "North Europe",wd=35,ht=30,multipanel = T,
                         cex.lab=3,cex.main=3,cex=3,cex.axis = 3)


# for Southern Europe
get_summary_plot_regional(x_meta=x_meta,summary_table=summary_table,nr=5,
                          country = c("GBR","FRA","BEL","ESP"),label_country = "South Europe",wd=35,ht=30,multipanel = T,
                          cex.lab=3,cex.main=3,cex=3,cex.axis = 3)

#################################################################################
# Now if you want to exclude count data
x<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_SurveyTable.csv") # a dataframe
z<-x %>% distinct(TimeSeriesID, .keep_all = TRUE)%>%select(TimeSeriesID,UnitAbundance)
x_meta<-inner_join(z,x_meta,by="TimeSeriesID")

x_meta_exclude_count<-x_meta%>%filter(UnitAbundance!="Count")

# for USA: it had all COUNT data for good sites

# for Northern Europe
get_summary_plot_regional(x_meta=x_meta_exclude_count,summary_table=summary_table,nr=5,
                          country = c("FIN","SWE"),
                          label_country = "North Europe(exclude count)",
                          wd=35,ht=30,multipanel = T,
                          cex.lab=3,cex.main=3,cex=3,cex.axis = 3)


# for Southern Europe
get_summary_plot_regional(x_meta=x_meta_exclude_count,summary_table=summary_table,nr=5,
                          country = c("GBR","FRA","BEL","ESP"),
                          label_country = "South Europe(exclude count)",
                          wd=35,ht=30,multipanel = T,
                          cex.lab=3,cex.main=3,cex=3,cex.axis = 3)



#################################################################################################
my_summary_boxplot<-function(summary_table,nametag){
  # for how many sites LT asymmetry were dominant?
  nLT<-sum(summary_table$f_nL>summary_table$f_nU)
  
  # for how many sites +ve corr were dominant?
  nP<-sum((summary_table$f_nL+summary_table$f_nU)>summary_table$f_nneg)
  
  # for how many sites +ve corr were dominant?
  nC<-sum((summary_table$f_nL+summary_table$f_nU)<summary_table$f_nneg)
  
  z<-summary_table%>%select(f_nind,f_nL,f_nU,f_nneg)
  colnames(z)<-c("Independent","Synchrony(rare)","Synchrony(abundant)","Compensatory")
  y <- gather(z, Pairwise.Interaction, Frequency) 
  boxplot(Frequency~Pairwise.Interaction,y,ylim=c(0,1),
          col=c("green","yellow","skyblue","red"),
          main=paste(nametag,", #sites: ",nrow(z),", #sites(more syn.): ",nP,", #sites(more comp.): ",nC))
}
###################################################################################################

# call the boxplot function

good_TimeSeriesID_q3q4<-readRDS("../../DATA/for_RivFishTIME/wrangled_data/good_TimeSeriesID_q3q4.RDS")

x_meta<-read.csv("../../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_TimeseriesTable.csv")
x_meta<-x_meta%>%filter(TimeSeriesID%in%good_TimeSeriesID_q3q4)

summary_table<-readRDS("../../Results/for_RivFishTIME/summary_table.RDS")
summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)

summary_table<-inner_join(summary_table,x_meta,by=c("siteid"="TimeSeriesID"))

# boxplot for whole data
pdf("../../Results/for_RivFishTIME/summary_boxplot.pdf",width=14,height=6)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.sub=1.5)
my_summary_boxplot(summary_table = summary_table,nametag = "RivFishTIME")
par(op)
dev.off()

# boxplot by BioRealm
sv<-split(summary_table,f=summary_table$BioRealm)
pdf("../../Results/for_RivFishTIME/summary_boxplot_by_BioRealm.pdf",width=18,height=12)
op<-par(mar=c(8,8,8,1),mgp=c(5,1,0),mfrow=c(2,2),cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=2)
for(i in 1:length(sv)){
  my_summary_boxplot(summary_table = sv[[i]],nametag = names(sv)[i])
}
par(op)
dev.off()

