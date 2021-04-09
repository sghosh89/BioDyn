# SEM with piecewiseSEM for multiple groups
rm(list=ls())
library(piecewiseSEM)
library(tidyverse)
sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")

# exclude marine realm
sm_all$REALM<-as.character(sm_all$REALM)
sm_all<-sm_all%>%filter(REALM%in%c("Freshwater","Terrestrial"))
sm_all$REALM<-as.factor(sm_all$REALM)

mydat<-sm_all[,c("siteid","iCV","phi","REALM","phi_skw","nsp","L","U")]
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U

# we will consider asymmetric community only
mydat$community<-"NT" #Symmetric
mydat$community[which(mydat$L>abs(mydat$U))]<-"LT"
mydat$community[which(mydat$L<abs(mydat$U))]<-"UT"
#mydat$subject<-as.factor(1:nrow(mydat))
mydat<-na.omit(mydat)
#mydat<-mydat%>%filter(community%in%c("LT","UT"))
mydat<-mydat%>%rename(
  stability=iCV,
  VR=phi,
  SR=phi_skw,
  R=nsp)

#----------- is there any difference between null and full model, and also between habitats --------
model1=lm(stability ~ (R + VR)*REALM, mydat) # R:REALM + VR:REALM +REALM
model2=lm(stability ~ (R + VR+SR +A +uniA)*REALM, mydat)
anova(model1,model2)
AIC(model1,model2)
#OK, model 2 is the best one

#difference between habitat:
car::Anova(model2) # for >2 group comparison: p value indicates sig. effect of each variable on response
#since R:REALM and VR:REALM are significant, meaning that 
# Richness and variance ratio effect on stability depend on habitat


# full model
model_full<-psem(
  lm(stability ~ R + VR +SR +A +uniA, mydat),
  lm(VR ~ R, mydat),
  lm(SR ~ R + A + uniA, mydat))
summary(model_full)
plot(model_full)

# now add habitat effect
model_group<-piecewiseSEM::multigroup(model_full,group="REALM")
model_group
AIC(model_full)# this AIC doesn't include the habitat effect, I need somehoe to get AIC for model_group

# plot the loop fun

# now we don't need this
#model_null<-psem(
#  lm(stability ~ R + VR, mydat),
#  lm(VR ~ R, mydat),
#  lm(SR ~ R + A + uniA, mydat))
#model_group<-piecewiseSEM::multigroup(model_null,group="REALM")
#========================
library(igraph)
library(qgraph)

par(mfrow=c(1,2))
effet=NULL
for(i in unique(mydat$REALM)){
  center=10
  left=0
  right=20
  haut=20
  bas=0
  cex=1.3
  echelle_fleche=0.1
  l=model_group$group.coefs[[paste(i)]][,c("Predictor","Response","Std.Estimate")]
  l[,3]=as.numeric(l[,3])
  l$colo="black"
  l$colo[which(l$Std.Estimate<0)]="red"
  g <- graph.data.frame(l, directed=T)
  g= g %>% set_edge_attr("color", value =l$colo)
  E(g)$width <- abs(l[,3])
  E(g)$value <- l[,3]
  edge_attr(g,"color")=l$colo
  EL=as_edgelist(g)
  EL=cbind(EL,l[,3])
  asi=abs(l[,3])/echelle_fleche
  asi[asi<5]=5
  
  qgraph(EL,edge.color=l$colo,layout="spring",
         border.color="white",label.cex=cex,label.scale=F,
         edge.label.cex = cex,edge.label.position=0.3,vsize2=6,vsize=18,title=i,
         title.cex=cex,shape="ellipse",edge.labels=T,fade=F,
         esize=max(abs(l[,3]))/echelle_fleche,asize=asi)
  
}
