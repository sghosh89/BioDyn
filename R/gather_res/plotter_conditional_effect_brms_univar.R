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

