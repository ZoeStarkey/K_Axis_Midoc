library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)
library(dplyr)


#load the data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth_2.Rda")

#making "day" fractional
with(km_bm_depth_2, plot(day,lunar_fraction))

#adding day_fraction to km_bm_depth
km_bm_depth_2$day_of_year <- yday(km_bm_depth_2$start_time)
km_bm_depth_2$day_fraction <- km_bm_depth_2$day_of_year + hour(km_bm_depth_2$start_time) / 24

#Temporal performance analytics for all taxa
temporal_PA_all_taxa <- km_bm_depth_2[, c("bm_depth_all_taxa", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_all_taxa$bm_depth_all_taxa <- log(temporal_PA_all_taxa$bm_depth_all_taxa)
temporal_PA_all_taxa <- temporal_PA_all_taxa[complete.cases(temporal_PA_all_taxa), ]
#plot
chart.Correlation(temporal_PA_all_taxa, histogram=TRUE, pch=19, )

#Temporal performance analytics for fish
temporal_PA_fish <- km_bm_depth_2[, c("bm_depth_fish", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_fish$bm_depth_fish <- log(temporal_PA_fish$bm_depth_fish)
temporal_PA_fish <- temporal_PA_fish[complete.cases(temporal_PA_fish), ]
#plot
chart.Correlation(temporal_PA_fish, histogram=TRUE, pch=19, )

#Temporal performance analytics for cephalopods
temporal_PA_ceph <- km_bm_depth_2[, c("bm_depth_ceph", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_ceph$bm_depth_ceph <- log(temporal_PA_ceph$bm_depth_ceph)
temporal_PA_ceph <- temporal_PA_ceph[complete.cases(temporal_PA_ceph), ]
#plot
chart.Correlation(temporal_PA_ceph, histogram=TRUE, pch=19, )
