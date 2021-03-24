library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)


# Input: 
# mydat = a dataframe with one column named as REALM and that is a factor
# xlab, ylab = character, colnames of mydat with which you will do the linear regression

#Output:
# A list
get_ancova_res<-function(mydat,xlab="log_phiLdM",ylab="log_iCV"){
  
  # check linearity between x and y variable
  plot_linear<-ggscatter(
    mydat, x = xlab, y = ylab,
    color = "REALM", alpha=0.3,add = "reg.line"
  )+
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = REALM)
    )+
    scale_color_manual(values=c("blue", "green"))
  
  # check homogeniety of the slopes: interaction term should be statistically not significant
  myformula<-as.formula(paste(ylab,"~","REALM*",xlab))
  mytab<-mydat %>% anova_test(myformula, type=3)
  cat("==== check homogeniety of the slopes: interaction term should be statistically not significant ==== \n")
  if(mytab$p[3]>=0.05){
    cat("=========== No significant interaction between REALM and ",xlab," ========== \n")
  }else{
    cat("=========== significant interaction between REALM and ",xlab," ========== \n")
  }
  print(mytab)
  
  # Fit the model, the covariate goes first
  myformula_add<-as.formula(paste(ylab,"~",xlab,"+REALM")) # changing the order would give different result (type=2)
  # so, I used type=3 for anova
  model <- lm(myformula_add, data = mydat)
  # Inspect the model diagnostic metrics
  model.metrics <- augment(model) %>%
    select(-.hat, -.sigma, -.fitted) # Remove details
  head(model.metrics, 3)
  # check normality of residuals using shapiro wilk test: if p>0.05 normality maintained
  cat("check normality of residuals using shapiro wilk test: if p>0.05 normality maintained \n")
  print(shapiro_test(model.metrics$.resid))
  
  # check variance for each group-residual are equal or not: if p>0.05 then it is equal
  cat("======= check variance for each group-residual are equal or not: if p>0.05 then it is equal ======= \n")
  print(model.metrics %>% levene_test(.resid ~ REALM))
  
  # check if any outliers?
  #tab_ol<-model.metrics %>% 
  #  filter(abs(.std.resid) > 3) %>%
  #  as.data.frame()
  #if(nrow(tab_ol)==0){
  #  cat("====== No outliers ====== \n")
  #}
  
  #------------- now, do the computation ----------------------------
  res.aov <- mydat %>% anova_test(myformula_add,type=3)
  res.aov.tab<-get_anova_table(res.aov)
  # this table shows after adjustment for xlab score, 
  # there was a statistically significant difference in ylab score between the different REALMs
  
  #---------- posthoc test ---------------
  library(emmeans)
  pwc <- mydat %>% 
    emmeans_test(
      as.formula(paste(ylab,"~REALM")), covariate = xlab,
      p.adjust.method = "bonferroni"
    )
  pwc
  
  emmean_pwc<-get_emmeans(pwc)
  
  # visualization
  pwc <- pwc %>% add_xy_position(x = "REALM", fun = "mean_se")
  pwc_vis<-ggline(get_emmeans(pwc), x = "REALM", y = "emmean") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
    stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc)
    )
  
  return(list(plot_linear=plot_linear,
              res.aov.tab=res.aov.tab,
              pwc=pwc,
              emmean_pwc=emmean_pwc,
              pwc_vis=pwc_vis))
}
