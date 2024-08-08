
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_DA_A2")
setwd(d)
dir.exists(d)



############. SUMMED BIOMASS. ##################
# 1. SUMMED BIOMASS - Excluding Gelatinous 
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_A2_GAM_temporal_collaboration/K4S_DA_A2_GAM_Temporal_Colab_Dataframe/km_bm_sum.Rda")


#Day
allbiom_sum.day <- gam(log(bm_sum_all_taxa) ~ s(day),data = km_bm_sum)
draw(allbiom_sum.day, residuals = TRUE) 
summary(allbiom_sum.day)


#Lunar fraction - illuminated disk 
allbiom_sum.lunar <- gam(log(bm_sum_all_taxa) ~ s(lunar_fraction),data = km_bm_sum)
draw(allbiom_sum.lunar, residuals = TRUE) 
summary(allbiom_sum.lunar)

#Solar angle 
allbiom_sum.solar <- gam(log(bm_sum_all_taxa) ~ s(altitude),data = km_bm_sum)
draw(allbiom_sum.solar, residuals = TRUE) 
summary(allbiom_sum.solar)




#  2. SUMMED BIOMASS - FISH

#Day 
fishbiom_sum.day <- gam(log(bm_sum_fish) ~ s(day),data = km_bm_sum)
draw(fishbiom_sum.day, residuals = TRUE) 
summary(fishbiom_sum.day)

#Lunar fraction - illuminated disk 
fishbiom_sum.lunar <- gam(log(bm_sum_fish) ~ s(lunar_fraction),data = km_bm_sum)
draw(fishbiom_sum.lunar, residuals = TRUE) 
summary(fishbiom_sum.lunar)


#Solar angle 
fishbiom_sum.solar <- gam(log(bm_sum_fish) ~ s(altitude),data = km_bm_sum)
draw(fishbiom_sum.solar, residuals = TRUE) 
summary(fishbiom_sum.solar)




# 3. SUMMED BIOMASS - SQUID
#Load in the dataframe 

#Day
cephbiom_sum.day <- gam(log(bm_sum_ceph) ~ s(day),data = km_bm_sum)
draw(cephbiom_sum.day, residuals = TRUE) 
summary(cephbiom_sum.day)

#Lunar fraction - illuminated disk 
cephbiom_sum.lunar <- gam(log(bm_sum_ceph) ~ s(lunar_fraction),data = km_bm_sum)
draw(cephbiom_sum.lunar, residuals = TRUE) 
summary(cephbiom_sum.lunar)

#Solar angle 
cephbiom_sum.solar <- gam(log(bm_sum_ceph) ~ s(altitude),data = km_bm_sum)
draw(cephbiom_sum.solar, residuals = TRUE) 
summary(cephbiom_sum.solar)



############. BIOMASS SEPARATED BY DEPTH. ##################
#BIOMASS SEPARATED BY DEPTH - EXCLUDING GELATINOUS 
#load in the dataframe
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_A2_GAM_temporal_collaboration/K4S_DA_A2_GAM_Temporal_Colab_Dataframe/km_bm_depth.Rda")


#day 
allbiom_depth.day.gam <- gam(log(bm_depth_all_taxa) ~ s(day, by = depth),data = km_bm_depth)
draw(allbiom_depth.day.gam, residuals = TRUE)
summary(allbiom_depth.day.gam)

allbiom_depth.day.re <- gamm(log(bm_depth_all_taxa) ~ s(day, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.day.re, residuals = TRUE)
summary(allbiom_depth.day.re$gam)
summary(allbiom_depth.day.re$lme)

#lunar fraction - illuminated disk
allbiom_depth.lunar.gam <- gam(log(bm_depth_all_taxa) ~ s(lunar_fraction, by = depth),data = km_bm_depth)
draw(allbiom_depth.lunar.gam, residuals = TRUE)
summary(allbiom_depth.lunar.gam)

allbiom_depth.lunar.re <- gamm(log(bm_depth_all_taxa) ~ s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.lunar.re, residuals = TRUE)
summary(allbiom_depth.lunar.re$gam)
summary(allbiom_depth.lunar.re$lme)


#solar angle 
allbiom_depth.solar.gam <- gam(log(bm_depth_all_taxa) ~ s(altitude, by = depth),data = km_bm_depth)
draw(allbiom_depth.solar.gam, residuals = TRUE)
summary(allbiom_depth.solar.gam)

allbiom_depth.solar.re <- gamm(log(bm_depth_all_taxa) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.solar.re, residuals = TRUE)
summary(allbiom_depth.solar.re$gam)
summary(allbiom_depth.solar.re$lme)




#BIOMASS SEPARATED BY DEPTH - FISH

#day 
fish_depth.day.gam <- gam(log(bm_depth_fish) ~ s(day, by = depth),data = km_bm_depth)
draw(fish_depth.day.gam, residuals = TRUE)
summary(fish_depth.day.gam)

fish_depth.day.re <- gamm(log(bm_depth_fish) ~ s(day, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.day.re, residuals = TRUE)
summary(fish_depth.day.re$gam)
summary(fish_depth.day.re$lme)

#lunar fraction - illuminated disk
fish_depth.lunar.gam <- gam(log(bm_depth_fish) ~ s(lunar_fraction, by = depth),data = km_bm_depth)
draw(fish_depth.lunar.gam, residuals = TRUE)
summary(fish_depth.lunar.gam)

fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.lunar.re, residuals = TRUE)
summary(fish_depth.lunar.re$gam)
summary(fish_depth.lunar.re$lme)


#solar angle 
fish_depth.solar.gam <- gam(log(bm_depth_fish) ~ s(altitude, by = depth),data = km_bm_depth)
draw(fish_depth.solar.gam, residuals = TRUE)
summary(fish_depth.solar.gam)

fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.solar.re, residuals = TRUE)
summary(fish_depth.solar.re$gam)
summary(fish_depth.solar.re$lme)


#BIOMASS SEPARATED BY DEPTH - CEPHALOPODS

#day
ceph_depth.day.gam <- gam(log(bm_depth_ceph) ~ s(day, by = depth),data = km_bm_depth)
draw(ceph_depth.day.gam, residuals = TRUE)
summary(ceph_depth.day.gam)

ceph_depth.day.re <- gamm(log(bm_depth_ceph) ~ s(day, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.day.re, residuals = TRUE)
summary(ceph_depth.day.re$gam)
summary(ceph_depth.day.re$lme)

#lunar fraction - illuminated disk
ceph_depth.lunar.gam <- gam(log(bm_depth_ceph) ~ s(lunar_fraction, by = depth),data = km_bm_depth)
draw(ceph_depth.lunar.gam, residuals = TRUE)
summary(ceph_depth.lunar.gam)

ceph_depth.lunar.re <- gamm(log(bm_depth_ceph) ~ s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.lunar.re, residuals = TRUE)
summary(ceph_depth.lunar.re$gam)
summary(ceph_depth.lunar.re$lme)


#solar angle 
ceph_depth.solar.gam <- gam(log(bm_depth_ceph) ~ s(altitude, by = depth),data = km_bm_depth)
draw(ceph_depth.solar.gam, residuals = TRUE)
summary(ceph_depth.solar.gam)

ceph_depth.solar.re <- gamm(log(bm_depth_ceph) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.solar.re, residuals = TRUE)
summary(ceph_depth.solar.re$gam)
summary(ceph_depth.solar.re$lme)











#### TESTING GAMMA ######
#gamma instead of log for fish and solar angle 
library(gamm4)
m19<- gamm4(bm_g_m3 + 0.0001 ~ s(altitude, by = depth) , data = km_df_depth, family = Gamma(), random = ~(1|midoc.stn))
par(mfrow = c(2, 2))
gam.check(m19$gam)
summary(m19$gam)
draw(m19$gam, residuals = TRUE)

#m15 <- gamm(bm_g_m3 ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ), family = Gamma(link = "inverse"))
#draw(m15, residuals = TRUE)
#summary(m15$gam)

