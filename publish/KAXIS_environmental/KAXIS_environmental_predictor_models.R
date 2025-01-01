
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)

#=============================================================================
# 1. Setup and Data Loading
#=============================================================================
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")
#removing the outliers deviated substantially from the majority of SST values (0.7 to 2.2°C), preventing the GAMs from fitting reasonable smooths across all stations. 
km_bm_sum_2 <- km_bm_sum_2 %>% filter(SST > 0)

#days since melt is unusually high for MIDOC 27, averaging the results from nearby stations to get a better estimate 
km_bm_sum_2_filtered <- km_bm_sum_2_filtered %>% filter(midoc.stn == "MIDOC26" | midoc.stn == "MIDOC28")
  #average of 110.00001 days since ice melt (from MIDOC26) and 84.99997 days since ice melt (from MIDOC 28)
  (110.00001+84.99997)/2 #=97.49999
#input 97.49999 into the days since melt value for MIDOC27 in the km_bm_sum_2 
km_bm_sum_2$days_since_melt[km_bm_sum_2$midoc.stn == "MIDOC27"] <- 97.49999

#=============================================================================
# 2. Depth integrated models for environmental predictors 
#=============================================================================
#All biomass (excluding gelatinous)
allbiom_additive_all_vars <-gam(log(bm_sum_all_taxa) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(allbiom_additive_all_vars, residuals = TRUE) +
  theme(plot.title = element_text(hjust = -8, vjust = 9 ))
summary(allbiom_additive_all_vars)
gam.check(allbiom_additive_all_vars)

#Fish biomass
fish_additive_all_vars <-gam(log(bm_sum_fish) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(fish_additive_all_vars, residuals = TRUE) 
#+ ggtitle("Fish Biomass (Log) All Taxa Additive model all vars") + 
theme(plot.title = element_text(hjust = 4, vjust = 9 ))
summary(fish_additive_all_vars)
gam.check(fish_additive_all_vars)

#cephalopod biomass 
ceph_additive_all_vars <-gam(log(bm_sum_ceph) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(ceph_additive_all_vars, residuals = TRUE) 
#+ ggtitle("Cephalopod Biomass (Log) All Taxa Additive model all vars") + 
# theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(ceph_additive_all_vars)
gam.check(ceph_additive_all_vars)

#krill biomass
krill_additive_all_vars <-gam(log(bm_sum_krill) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(krill_additive_all_vars, residuals = TRUE) + ggtitle("Krill Biomass (Log) All Taxa Additive model all vars") + 
  theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(krill_additive_all_vars)
gam.check(krill_additive_all_vars)
