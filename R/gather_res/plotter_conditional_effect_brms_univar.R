# For two realms we want to see conditional effects 
library(brms)
library(rethinking)

# so first, we need to go back to raw var
# mydat_scaled is scaled data, mydat is the raw data
pred_func <- function(post, R, VR, SR, A, uniA, REALM){
  stab <-  with(post, b_Intercept + b_R * R + b_VR*VR + b_SR*SR +
                  b_A* A + b_uniA*uniA + b_REALMTerrestrial* REALM +
                  `b_R:REALMTerrestrial` * REALM*R + `b_VR:REALMTerrestrial` * REALM*VR +
                  `b_SR:REALMTerrestrial` * REALM*SR +  
                  `b_A:REALMTerrestrial` * REALM*A + `b_uniA:REALMTerrestrial` * REALM*uniA)
  
  stab<-stab*(sd(stab))+mean(stab) # bring back raw var from scaled var
}

# srt<-pred_func(post,0,0,0,0,0,REALM=0)
# srt<-pred_func(post,0,0,0,0,0,REALM=1)

# plot conditional effects with original variables
plotter_conditional_effect_brms_univar<-function(full_model,xvar,ylm,xlab="R",ploton="varyR"){
  post <- posterior_samples(full_model) # posterior distribution
  
  #---------------------------- for Richness effect -------------------------
  if(ploton=="varyR"){
    # Terrestrial
    T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = xvar[i], SR = 0, A = 0,
                                                          uniA = 0, VR = 0, REALM = 1  ))
    #dim(T.pred)
    meanT <- apply(T.pred, 2, mean)
    ciT <- apply(T.pred, 2, HPDI, prob= .95)
    
    #Freshwater
    F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = xvar[i], SR = 0, A = 0,
                                                          uniA = 0, VR = 0, REALM = 0  ))
    #dim(F.pred)
    meanF <- apply(F.pred, 2, mean)
    ciF <- apply(F.pred, 2, HPDI, prob= .95)
    
    # now plotting the posterior for two realm
    plot(xvar,meanT, pch="", ylim = ylm, xlab=xlab, ylab="Stability")
    lines(xvar,meanT, col='green')
    shade(ciT,xvar, col=alpha("green", 0.2))
    
    lines(xvar,meanF, col='skyblue')
    shade(ciF,xvar, col=alpha("skyblue", 0.2))
  }
  
  
  #---------------------------- for Variance Ratio effect -------------------------
  if(ploton=="varyVR"){
    # Terrestrial
    T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = 0, A = 0,
                                                          uniA = 0, VR = xvar[i], REALM = 1  ))
    #dim(T.pred)
    meanT <- apply(T.pred, 2, mean)
    ciT <- apply(T.pred, 2, HPDI, prob= .95)
    
    #Freshwater
    F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = 0, A = 0,
                                                          uniA = 0, VR = xvar[i], REALM = 0  ))
    #dim(F.pred)
    meanF <- apply(F.pred, 2, mean)
    ciF <- apply(F.pred, 2, HPDI, prob= .95)
    
    # now plotting the posterior for two realm
    plot(xvar,meanT, pch="", ylim = ylm, xlab=xlab, ylab="Stability")
    lines(xvar,meanT, col='green')
    shade(ciT,xvar, col=alpha("green", 0.2))
    
    lines(xvar,meanF, col='skyblue')
    shade(ciF,xvar, col=alpha("skyblue", 0.2))
  }
  
  #---------------------------- for Skewness Ratio effect -------------------------
  if(ploton=="varySR"){
    # Terrestrial
    T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = xvar[i], A = 0,
                                                          uniA = 0, VR = 0, REALM = 1  ))
    #dim(T.pred)
    meanT <- apply(T.pred, 2, mean)
    ciT <- apply(T.pred, 2, HPDI, prob= .95)
    
    #Freshwater
    F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = xvar[i], A = 0,
                                                          uniA = 0, VR = 0, REALM = 0  ))
    #dim(F.pred)
    meanF <- apply(F.pred, 2, mean)
    ciF <- apply(F.pred, 2, HPDI, prob= .95)
    
    # now plotting the posterior for two realm
    plot(xvar,meanT, pch="", ylim = ylm, xlab=xlab, ylab="Stability")
    lines(xvar,meanT, col='green')
    shade(ciT,xvar, col=alpha("green", 0.2))
    
    lines(xvar,meanF, col='skyblue')
    shade(ciF,xvar, col=alpha("skyblue", 0.2))
  }
  
  #---------------------------- for Abs asymmetry effect -------------------------
  if(ploton=="varyA"){
    # Terrestrial
    T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = 0, A = xvar[i],
                                                          uniA = 0, VR = 0, REALM = 1  ))
    #dim(T.pred)
    meanT <- apply(T.pred, 2, mean)
    ciT <- apply(T.pred, 2, HPDI, prob= .95)
    
    #Freshwater
    F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R =0, SR = 0, A = xvar[i],
                                                          uniA = 0, VR = 0, REALM = 0  ))
    #dim(F.pred)
    meanF <- apply(F.pred, 2, mean)
    ciF <- apply(F.pred, 2, HPDI, prob= .95)
    
    # now plotting the posterior for two realm
    plot(xvar,meanT, pch="", ylim = ylm, xlab=xlab, ylab="Stability")
    lines(xvar,meanT, col='green')
    shade(ciT,xvar, col=alpha("green", 0.2))
    
    lines(xvar,meanF, col='skyblue')
    shade(ciF,xvar, col=alpha("skyblue", 0.2))
  }
  
  #---------------------------- for directional asymmetry effect -------------------------
  if(ploton=="varyuniA"){
    # Terrestrial
    T.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = 0, A = 0,
                                                          uniA = xvar[i], VR = 0, REALM = 1  ))
    #dim(T.pred)
    meanT <- apply(T.pred, 2, mean)
    ciT <- apply(T.pred, 2, HPDI, prob= .95)
    
    #Freshwater
    F.pred = sapply(1:length(xvar), function(i) pred_func(post = post, R = 0, SR = 0, A = 0,
                                                          uniA =xvar[i], VR = 0, REALM = 0  ))
    #dim(F.pred)
    meanF <- apply(F.pred, 2, mean)
    ciF <- apply(F.pred, 2, HPDI, prob= .95)
    
    # now plotting the posterior for two realm
    plot(xvar,meanT, pch="", ylim = ylm, xlab=xlab, ylab="Stability")
    lines(xvar,meanT, col='green')
    shade(ciT,xvar, col=alpha("green", 0.2))
    
    lines(xvar,meanF, col='skyblue')
    shade(ciF,xvar, col=alpha("skyblue", 0.2))
  }
  
}

#==============================================
full_model<-readRDS("../../Results/gather_res/full_model.RDS")
pdf("../../Results/gather_res/conditional_brms_univar_plot.pdf",height=3,width=6)
op<-par(mfrow=c(2,3),mar=c(4,4,1,1),mgp=c(2.5,1,0))
xvar = seq(from=2, to=70, by = 1)
plotter_conditional_effect_brms_univar(full_model=full_model,xvar=xvar,
                                       ylm = c(-100,180),
                                       xlab = "Richness",
                                       ploton="varyR")


xvar = seq(from=0.01, to=10, by = 0.1)
plotter_conditional_effect_brms_univar(full_model=full_model,xvar=xvar,
                                       ylm = c(-4.5,1),
                                       xlab = "Variance Ratio",
                                       ploton="varyVR")

xvar = seq(from=-10, to=10, by = 1)
plotter_conditional_effect_brms_univar(full_model=full_model,xvar=xvar,
                                       ylm = c(-2,1),
                                       xlab = "Skewness Ratio",
                                       ploton="varySR")
xvar = seq(from=0, to=40, by = 1)
plotter_conditional_effect_brms_univar(full_model=full_model,xvar=xvar,
                                       ylm = c(-180,100),
                                       xlab = "Total abs. asymmetry",
                                       ploton="varyA")
xvar = seq(from=-30, to=30, by = 1)
plotter_conditional_effect_brms_univar(full_model=full_model,xvar=xvar,
                                       ylm = c(-42,40),
                                       xlab = "Net asymmetry",
                                       ploton="varyuniA")

par(op)
dev.off()

#=======================================================================================

# summary plot for posterior parameter distribution
library(tidybayes)
library(tidyverse)

post<-posterior_samples(full_model)
mypost<-post
mypost<-rename(mypost, 
       b_Freshwater=b_Intercept,
       `b_Terrestrial-b_Freshwater` = b_REALMTerrestrial,
       b_R_Freshwater = b_R,
       `b_R_Terrestrial-b_R_Freshwater` = `b_R:REALMTerrestrial`,
       b_VR_Freshwater = b_VR,
       `b_VR_Terrestrial-b_VR_Freshwater` = `b_VR:REALMTerrestrial`,
       b_SR_Freshwater = b_SR,
       `b_SR_Terrestrial-b_SR_Freshwater` = `b_SR:REALMTerrestrial`,
       b_A_Freshwater = b_A,
       `b_A_Terrestrial-b_A_Freshwater` = `b_A:REALMTerrestrial`,
       b_uniA_Freshwater = b_uniA,
       `b_uniA_Terrestrial-b_uniA_Freshwater` = `b_uniA:REALMTerrestrial`
       )

gathered_post <-
  mypost %>% 
  mutate("b_Terrestrial" = 
           `b_Terrestrial-b_Freshwater` + b_Freshwater,
         "b_R_Terrestrial" = 
           `b_R_Terrestrial-b_R_Freshwater` + b_R_Freshwater,
         "b_VR_Terrestrial" = 
           `b_VR_Terrestrial-b_VR_Freshwater` + b_VR_Freshwater,
         "b_SR_Terrestrial" = 
           `b_SR_Terrestrial-b_SR_Freshwater` + b_SR_Freshwater,
         "b_A_Terrestrial" = 
           `b_A_Terrestrial-b_A_Freshwater` + b_A_Freshwater,
         "b_uniA_Terrestrial" = 
           `b_uniA_Terrestrial-b_uniA_Freshwater` + b_uniA_Freshwater,)%>%
  select("b_Terrestrial", "b_Freshwater", "b_Terrestrial-b_Freshwater",
         "b_R_Terrestrial", "b_R_Freshwater", "b_R_Terrestrial-b_R_Freshwater",
         "b_VR_Terrestrial", "b_VR_Freshwater", "b_VR_Terrestrial-b_VR_Freshwater",
         "b_SR_Terrestrial", "b_SR_Freshwater", "b_SR_Terrestrial-b_SR_Freshwater",
         "b_A_Terrestrial", "b_A_Freshwater", "b_A_Terrestrial-b_A_Freshwater",
         "b_uniA_Terrestrial", "b_uniA_Freshwater", "b_uniA_Terrestrial-b_uniA_Freshwater")%>%
  gather() %>% 
  mutate(key = factor(key, levels = c("b_Terrestrial", "b_Freshwater", "b_Terrestrial-b_Freshwater",
                                      "b_R_Terrestrial", "b_R_Freshwater", "b_R_Terrestrial-b_R_Freshwater",
                                      "b_VR_Terrestrial", "b_VR_Freshwater", "b_VR_Terrestrial-b_VR_Freshwater",
                                      "b_SR_Terrestrial", "b_SR_Freshwater", "b_SR_Terrestrial-b_SR_Freshwater",
                                      "b_A_Terrestrial", "b_A_Freshwater", "b_A_Terrestrial-b_A_Freshwater",
                                      "b_uniA_Terrestrial", "b_uniA_Freshwater", "b_uniA_Terrestrial-b_uniA_Freshwater")
  )
  ) 

p1gpost<-gathered_post %>% 
  ggplot(aes(x = value, group = key)) +
  geom_histogram(color = "grey92", fill = "grey86",
                 size = .2) +
  stat_pointinterval(aes(y = 0), 
                     point_interval = mean_qi, .width = c(.95, .50)) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_bw() + theme(panel.grid = element_blank())+
  facet_wrap(~key, scales = "free_x", ncol=3)

# numeric values for summary
fixef_tab_univar<-gathered_post %>% 
  group_by(key) %>% 
  mean_qi()%>%as.data.frame()
saveRDS(fixef_tab_univar,"../../Results/gather_res/fixef_tab_brms_univar.RDS")
# for asymmetric distribution mode_hdi would be better than mean_qi

pdf("../../Results/gather_res/posterior_parameter_brms_univer.pdf",height=12,width=10)
p1gpost
dev.off()

summary(full_model)
apply(post,2,mean) # this is same as estimate of previous line/ next line
#fixef(full_model)
















