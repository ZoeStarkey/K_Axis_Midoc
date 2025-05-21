library(PerformanceAnalytics)
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)


#load the data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")

#environmental performance analytics for all taxa (excluding gelatinous)
env_PA_all_taxa <- km_bm_sum_2[,c("bm_sum_all_taxa","TSM", "CUR","SST","CHLA")]
env_PA_all_taxa$bm_sum_all_taxa <- log(env_PA_all_taxa$bm_sum_all_taxa)
env_PA_all_taxa <- env_PA_all_taxa[complete.cases(env_PA_all_taxa), ]
#plot
chart.Correlation(env_PA_all_taxa, histogram=TRUE, pch=19, )

#environmental performance analytics for fish
env_PA_fish <- km_bm_sum_2[,c("bm_sum_fish","TSM", "CUR","SST","CHLA")]
env_PA_fish$bm_sum_fish <- log(env_PA_fish$bm_sum_fish)
env_PA_fish <- env_PA_fish[complete.cases(env_PA_fish), ]
#plot
chart.Correlation(env_PA_fish, histogram=TRUE, pch=19, )

#environmental performance analytics for cephalopods
env_PA_ceph <- km_bm_sum_2[,c("bm_sum_ceph","TSM", "CUR","SST","CHLA")]
env_PA_ceph$bm_sum_ceph <- log(env_PA_ceph$bm_sum_ceph)
env_PA_ceph <- env_PA_ceph[complete.cases(env_PA_ceph), ]
#plot
chart.Correlation(env_PA_ceph, histogram=TRUE, pch=19, )

#environmental performance analytics for krill
env_PA_krill <- km_bm_sum_2[,c("bm_sum_krill","TSM", "CUR","SST","CHLA")]
env_PA_krill$bm_sum_krill <- log(env_PA_krill$bm_sum_krill)
env_PA_krill <- env_PA_krill[complete.cases(env_PA_krill), ]
#plot
chart.Correlation(env_PA_krill, histogram=TRUE, pch=19, )


