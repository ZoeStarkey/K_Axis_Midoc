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

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth_2.Rda")

#making "day" fractional
with(km_bm_sum_2, plot(day,lunar_fraction))

#adding day_fraction to km_bm_depth
km_bm_depth_2$day_of_year <- yday(km_bm_depth_2$start_time)
km_bm_depth_2$day_fraction <- km_bm_depth_2$day_of_year + hour(km_bm_depth_2$start_time) / 24

#=============================================================================
# 2. Depth structured GAMMs for fish
#=============================================================================

fish_depth.day.re <- gamm(log(bm_depth_fish) ~ depth + s(day_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.day.re$gam)
summary(fish_depth.day.re$lme)
gam.check(fish_depth.day.re$gam)

#solar angle
fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
fish_depth_solar <- draw(fish_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
#grid.text("Biomass (Logged) by depth + RE: Fish - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.solar.re$gam)
summary(fish_depth.solar.re$lme) 
gam.check(fish_depth.solar.re$gam)

#lunar fraction - illuminated disc
fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
fish_depth_lunar <- draw(fish_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.lunar.re$gam)
summary(fish_depth.lunar.re$lme)
gam.check(fish_depth.lunar.re$gam)

#=============================================================================
# 2. Depth structured GAMMs for cephalopods
#=============================================================================

#day 
ceph_depth.day.re <- gamm(log(bm_depth_ceph) ~ depth + s(day_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.day.re$gam)
summary(ceph_depth.day.re$lme)
gam.check(ceph_depth.day.re$gam)

#solar angle
ceph_depth.solar.re <- gamm(log(bm_depth_ceph) ~ depth + s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.solar.re$gam)
summary(ceph_depth.solar.re$lme)
gam.check(ceph_depth.solar.re$gam)

#lunar fraction - illuminated disk
ceph_depth.lunar.re <- gamm(log(bm_depth_ceph) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.lunar.re$gam)
summary(ceph_depth.lunar.re$lme)
gam.check(ceph_depth.lunar.re$gam)

