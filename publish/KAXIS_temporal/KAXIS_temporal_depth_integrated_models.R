
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)
library(lubridate)
#=============================================================================
# 1. Setup and Data Loading
#=============================================================================
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")

#making "day" fractional
with(km_bm_sum_2, plot(day,lunar_fraction))

km_bm_sum_2$day_of_year <- yday(km_bm_sum_2$start_time)
km_bm_sum_2$day_fraction <- km_bm_sum_2$day_of_year + hour(km_bm_sum_2$start_time) / 24


#=============================================================================
# 2. Depth integrated GAMs for all biomass (excluding gelatinous)
#=============================================================================
allbiom_sum.day <- gam(log(bm_sum_all_taxa) ~ s(day_fraction),data = km_bm_sum_2)
plot_object <- draw(allbiom_sum.day, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Day") 
summary(allbiom_sum.day)
gam.check(allbiom_sum.day)
par(mfrow=c(2,2))

#Lunar fraction - illuminated disk 
allbiom_sum.lunar <- gam(log(bm_sum_all_taxa) ~ s(lunar_fraction),data = km_bm_sum_2)
plot_object <- draw(allbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Lunar Fraction")
summary(allbiom_sum.lunar)
gam.check(allbiom_sum.lunar)

#Solar angle 
allbiom_sum.solar <- gam(log(bm_sum_all_taxa) ~ s(altitude),data = km_bm_sum_2)
plot_object <- draw(allbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Solar Angle")
summary(allbiom_sum.solar)
gam.check(allbiom_sum.solar)


#=============================================================================
# 3. Depth integrated GAMs for fish
#=============================================================================
#Day 
fishbiom_sum.day <- gam(log(bm_sum_fish) ~ s(day_fraction),data = km_bm_sum_2)
plot_object <- draw(fishbiom_sum.day, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Day")
summary(fishbiom_sum.day)
gam.check(fishbiom_sum.day)

#Lunar fraction - illuminated disk 
fishbiom_sum.lunar <- gam(log(bm_sum_fish) ~ s(lunar_fraction),data = km_bm_sum_2)
plot_object <- draw(fishbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Lunar Fraction")
summary(fishbiom_sum.lunar)
gam.check(fishbiom_sum.lunar)

#Solar angle 
fishbiom_sum.solar <- gam(log(bm_sum_fish) ~ s(altitude),data = km_bm_sum_2)
plot_object <- draw(fishbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Solar Angle")
summary(fishbiom_sum.solar)
gam.check(fishbiom_sum.solar)

#=============================================================================
# 4. Depth integrated GAMs for cephalaopods
#=============================================================================
#Day
cephbiom_sum.day <- gam(log(bm_sum_ceph) ~ s(day_fraction),data = km_bm_sum_2)
plot_object <- draw(cephbiom_sum.day, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Day")
summary(cephbiom_sum.day)
gam.check(cephbiom_sum.day)

#Lunar fraction - illuminated disk 
cephbiom_sum.lunar <- gam(log(bm_sum_ceph) ~ s(lunar_fraction),data = km_bm_sum_2)
plot_object <- draw(cephbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Lunar Fraction")
summary(cephbiom_sum.lunar)
gam.check(cephbiom_sum.lunar)

#Solar angle 
cephbiom_sum.solar <- gam(log(bm_sum_ceph) ~ s(altitude),data = km_bm_sum_2)
plot_object <- draw(cephbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Solar Angle")
summary(cephbiom_sum.solar)
gam.check(cephbiom_sum.solar)


#=============================================================================
# 5. Depth integrated GAMs for krill
#=============================================================================
#Day
krillbiom_sum.day <- gam(log(bm_sum_krill) ~ s(day_fraction),data = km_bm_sum_2)
plot_object <- draw(krillbiom_sum.day, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Day")
summary(krillbiom_sum.day)
gam.check(krillbiom_sum.day)

#Lunar fraction - illuminated disk
krillbiom_sum.lunar <- gam(log(bm_sum_krill) ~ s(lunar_fraction),data = km_bm_sum_2)
plot_object <- draw(krillbiom_sum.lunar, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Lunar Fraction")
summary(krillbiom_sum.lunar)
gam.check(krillbiom_sum.lunar)

#solar angle
krillbiom_sum.solar <- gam(log(bm_sum_krill) ~ s(altitude),data = km_bm_sum_2)
plot_object <- draw(krillbiom_sum.solar, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Solar Angle")
summary(krillbiom_sum.solar)
gam.check(krillbiom_sum.solar)


