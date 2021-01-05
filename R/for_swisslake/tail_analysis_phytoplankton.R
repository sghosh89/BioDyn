rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../../Results/for_swisslake/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------

# ============= for lake 1 =======================
resloc2<-"../../Results/for_swisslake/L1WA/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L1WA.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 2 ===========================
resloc2<-"../../Results/for_swisslake/L2UZ/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L2UZ.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

/
# =========== for lake 3 ===========================
resloc2<-"../../Results/for_swisslake/L3LU/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L3LU.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 4 ===========================
resloc2<-"../../Results/for_swisslake/L4LZ/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L4LZ.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 5 ===========================
resloc2<-"../../Results/for_swisslake/L5SE/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L5SE.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 6 ===========================
resloc2<-"../../Results/for_swisslake/L6HA/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L6HA.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 7 ===========================
resloc2<-"../../Results/for_swisslake/L7BA/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L7BA.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

# =========== for lake 8 ===========================
resloc2<-"../../Results/for_swisslake/L8GR/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

mat<-readRDS("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_L8GR.RDS")
tail_analysis(mat=mat, resloc=resloc2, nbin=2)

#####################################################################################
# Now, do the summary results








