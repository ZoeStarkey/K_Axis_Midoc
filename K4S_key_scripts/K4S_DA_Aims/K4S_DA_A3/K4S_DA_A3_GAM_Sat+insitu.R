usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_DA_A2")
setwd(d)
dir.exists(d)



############. SUMMED BIOMASS. ##################
# 1. SUMMED BIOMASS - Excluding Gelatinous 
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#TSM
allbiom_sum.TSM <- gam(log(bm_sum_all_taxa) ~ s(TSM),data = km_bm_sum)
plot_object <- draw(allbiom_sum.TSM, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - TSM") 
summary(allbiom_sum.TSM)


#CHLA
allbiom_sum.CHLA <- gam(log(bm_sum_all_taxa) ~ s(CHLA),data = km_bm_sum)
plot_object <- draw(allbiom_sum.CHLA, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - CHLA")
summary(allbiom_sum.CHLA)

#




x#  2. SUMMED BIOMASS - FISH
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#Day 
fishbiom_sum.day <- gam(log(bm_sum_fish) ~ s(day),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.day, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Day")
summary(fishbiom_sum.day)

#Lunar fraction - illuminated disk 
fishbiom_sum.lunar <- gam(log(bm_sum_fish) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Lunar Fraction")
summary(fishbiom_sum.lunar)


#Solar angle 
fishbiom_sum.solar <- gam(log(bm_sum_fish) ~ s(altitude),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Solar Angle")
summary(fishbiom_sum.solar)




# 3. SUMMED BIOMASS - SQUID
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#Day
cephbiom_sum.day <- gam(log(bm_sum_ceph) ~ s(day),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.day, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Day")
summary(cephbiom_sum.day)

#Lunar fraction - illuminated disk 
cephbiom_sum.lunar <- gam(log(bm_sum_ceph) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Lunar Fraction")
summary(cephbiom_sum.lunar)

#Solar angle 
cephbiom_sum.solar <- gam(log(bm_sum_ceph) ~ s(altitude),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Solar Angle")
summary(cephbiom_sum.solar)


#4. SUMMED BIOMASS - KRILL 
#Day
krillbiom_sum.day <- gam(log(bm_sum_krill) ~ s(day),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.day, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Day")
summary(krillbiom_sum.day)

#Lunar fraction - illuminated disk
krillbiom_sum.lunar <- gam(log(bm_sum_krill) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.lunar, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Lunar Fraction")
summary(krillbiom_sum.lunar)
