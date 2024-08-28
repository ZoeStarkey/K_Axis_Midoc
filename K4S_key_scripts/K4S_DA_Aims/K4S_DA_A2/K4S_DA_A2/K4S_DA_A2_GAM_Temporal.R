
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)
library(lubridate)
library(dplyr)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_DA_A2")
setwd(d)
dir.exists(d)


#making day fractional
with(km_bm_sum, plot(day,lunar_fraction))

km_bm_sum$day_of_year <- yday(km_bm_sum$start_time)
km_bm_sum$day_fraction <- km_bm_sum$day_of_year + hour(km_bm_sum$start_time) / 24

#adding day_fraction to km_bm_depth
km_bm_depth$day_of_year <- yday(km_bm_depth$start_time)
km_bm_depth$day_fraction <- km_bm_depth$day_of_year + hour(km_bm_depth$start_time) / 24




############. SUMMED BIOMASS. ##################
# 1. SUMMED BIOMASS - Excluding Gelatinous 
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")


#Day
allbiom_sum.day <- gam(log(bm_sum_all_taxa) ~ s(day_fraction),data = km_bm_sum)
plot_object <- draw(allbiom_sum.day, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Day") 
summary(allbiom_sum.day)
gam.check(allbiom_sum.day)
par(mfrow=c(2,2))

#Lunar fraction - illuminated disk 
allbiom_sum.lunar <- gam(log(bm_sum_all_taxa) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(allbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Lunar Fraction")
summary(allbiom_sum.lunar)
gam.check(allbiom_sum.lunar)

#Solar angle 
allbiom_sum.solar <- gam(log(bm_sum_all_taxa) ~ s(altitude),data = km_bm_sum)
plot_object <- draw(allbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Solar Angle")
summary(allbiom_sum.solar)
gam.check(allbiom_sum.solar)




#  2. SUMMED BIOMASS - FISH
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#Day 
fishbiom_sum.day <- gam(log(bm_sum_fish) ~ s(day_fraction),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.day, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Day")
summary(fishbiom_sum.day)
gam.check(fishbiom_sum.day)

#Lunar fraction - illuminated disk 
fishbiom_sum.lunar <- gam(log(bm_sum_fish) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Lunar Fraction")
summary(fishbiom_sum.lunar)
gam.check(fishbiom_sum.lunar)


#Solar angle 
fishbiom_sum.solar <- gam(log(bm_sum_fish) ~ s(altitude),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Fish - Solar Angle")
summary(fishbiom_sum.solar)
gam.check(fishbiom_sum.solar)




# 3. SUMMED BIOMASS - SQUID
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#Day
cephbiom_sum.day <- gam(log(bm_sum_ceph) ~ s(day_fraction),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.day, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Day")
summary(cephbiom_sum.day)
gam.check(cephbiom_sum.day)

#Lunar fraction - illuminated disk 
cephbiom_sum.lunar <- gam(log(bm_sum_ceph) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.lunar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Lunar Fraction")
summary(cephbiom_sum.lunar)
gam.check(cephbiom_sum.lunar)

#Solar angle 
cephbiom_sum.solar <- gam(log(bm_sum_ceph) ~ s(altitude),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.solar, residuals = TRUE) 
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Solar Angle")
summary(cephbiom_sum.solar)
gam.check(cephbiom_sum.solar)


#4. SUMMED BIOMASS - KRILL 
#Day
krillbiom_sum.day <- gam(log(bm_sum_krill) ~ s(day_fraction),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.day, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Day")
summary(krillbiom_sum.day)
gam.check(krillbiom_sum.day)

#Lunar fraction - illuminated disk
krillbiom_sum.lunar <- gam(log(bm_sum_krill) ~ s(lunar_fraction),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.lunar, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Lunar Fraction")
summary(krillbiom_sum.lunar)
gam.check(krillbiom_sum.lunar)

#solar angle
krillbiom_sum.solar <- gam(log(bm_sum_krill) ~ s(altitude),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.solar, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Solar Angle")
summary(krillbiom_sum.solar)
gam.check(krillbiom_sum.solar)


############. BIOMASS SEPARATED BY DEPTH. ##################
#BIOMASS SEPARATED BY DEPTH - EXCLUDING GELATINOUS 
#load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")

#day
allbiom_depth.day.gam <- gam(log(bm_depth_all_taxa) ~ depth + s(day_fraction, by = depth),data = km_bm_depth)
draw(allbiom_depth.day.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth All Taxa (Exclude Gelat) - Day", x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(allbiom_depth.day.gam)
gam.check(allbiom_depth.day.gam)


allbiom_depth.day.re <- gamm(log(bm_depth_all_taxa) ~ depth + s(day_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth +RE: All Taxa (Exclude Gelat) - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(allbiom_depth.day.re$gam)
summary(allbiom_depth.day.re$lme)
par(mfrow=c(2,2))
gam.check(allbiom_depth.day.re$gam)


#lunar fraction - illuminated disk
allbiom_depth.lunar.gam <- gam(log(bm_depth_all_taxa) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth)
draw(allbiom_depth.lunar.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: All Taxa (Exclude Gelat) -Lunar",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(allbiom_depth.lunar.gam)
gam.check(allbiom_depth.lunar.gam)


allbiom_depth.lunar.re <- gamm(log(bm_depth_all_taxa) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: All Taxa (Exclude Gelat) -Lunar",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(allbiom_depth.lunar.re$gam)
summary(allbiom_depth.lunar.re$lme)
par(mfrow=c(2,2))
gam.check(allbiom_depth.lunar.re$gam)


#solar angle 
allbiom_depth.solar.gam <- gam(log(bm_depth_all_taxa) ~ depth + s(altitude, by = depth),data = km_bm_depth)
draw(allbiom_depth.solar.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: All Taxa (Exclude Gelat) - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(allbiom_depth.solar.gam)
gam.check(allbiom_depth.solar.gam)

allbiom_depth.solar.re <- gamm(log(bm_depth_all_taxa) ~ depth + s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: All Taxa (Exclude Gelat) - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(allbiom_depth.solar.re$gam)
summary(allbiom_depth.solar.re$lme)
gam.check(allbiom_depth.solar.re$gam)









#BIOMASS SEPARATED BY DEPTH - FISH
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")

fish_depth.day.re <- gamm(log(bm_depth_fish) ~ depth + s(day_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.day.re$gam)
summary(fish_depth.day.re$lme)
gam.check(fish_depth.day.re$gam)

#lunar fraction - illuminated disc
fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.lunar.re$gam)
summary(fish_depth.lunar.re$lme)
gam.check(fish_depth.lunar.re$gam)


#solar angle
fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.solar.re$gam)
summary(fish_depth.solar.re$lme) 
gam.check(fish_depth.solar.re$gam)


#AIC 
km_bm_depth <- km_bm_depth[is.finite(km_bm_depth$bm_depth_fish) & !is.na(km_bm_depth$bm_depth_fish), ]

fish_depth.solar.re.no.smooth <- gamm(log(bm_depth_fish) ~ depth, data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
summary(fish_depth.solar.re.no.smooth$lme)
summary(fish_depth.solar.re.no.smooth$gam)
fish_depth.solar.re.intercept <- gamm(log(bm_depth_fish+ 1e-6) ~ 1,data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
summary(fish_depth.solar.re.intercept$lme)
summary(fish_depth.solar.re.intercept$gam)



#BIOMASS SEPARATED BY DEPTH - CEPHALOPODS

#day
ceph_depth.day.gam <- gam(log(bm_depth_ceph) ~ depth + s(day_fraction, by = depth),data = km_bm_depth)
draw(ceph_depth.day.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: Cephalopods - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.day.gam)
gam.check(ceph_depth.day.gam)

ceph_depth.day.re <- gamm(log(bm_depth_ceph) ~ depth + s(day_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.day.re$gam)
summary(ceph_depth.day.re$lme)
gam.check(ceph_depth.day.re$gam)

#lunar fraction - illuminated disk
ceph_depth.lunar.gam <- gam(log(bm_depth_ceph) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth)
draw(ceph_depth.lunar.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: Cephalopods - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.lunar.gam)
gam.check(ceph_depth.lunar.gam)

ceph_depth.lunar.re <- gamm(log(bm_depth_ceph) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.lunar.re$gam)
summary(ceph_depth.lunar.re$lme)
gam.check(ceph_depth.lunar.re$gam)


#solar angle 
ceph_depth.solar.gam <- gam(log(bm_depth_ceph) ~ depth +  s(altitude, by = depth),data = km_bm_depth)
draw(ceph_depth.solar.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: Cephalopods - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.solar.gam)
gam.check(ceph_depth.solar.gam)

ceph_depth.solar.re <- gamm(log(bm_depth_ceph) ~ depth + s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.solar.re$gam)
summary(ceph_depth.solar.re$lme)
gam.check(ceph_depth.solar.re$gam)



#all of the gamchekcs in one place
par(mfrow=c(2,2))
#ALL BIOM 
gam.check(allbiom_depth.day.gam) 
gam.check(allbiom_depth.day.re$gam)
gam.check(allbiom_depth.lunar.gam)
gam.check(allbiom_depth.lunar.re$gam)
gam.check(allbiom_depth.solar.gam)
gam.check(allbiom_depth.solar.re$gam)

#FISH
gam.check(fish_depth.day.gam)
gam.check(fish_depth.day.re$gam)
gam.check(fish_depth.lunar.gam)
gam.check(fish_depth.lunar.re$gam)
gam.check(fish_depth.solar.gam)
gam.check(fish_depth.solar.re$gam)

#CEPHALOPODS
gam.check(ceph_depth.day.gam)
gam.check(ceph_depth.day.re$gam)
gam.check(ceph_depth.lunar.gam)
gam.check(ceph_depth.lunar.re$gam)
gam.check(ceph_depth.solar.gam)
gam.check(ceph_depth.solar.re$gam)

#KRILL
gam.check(krill_depth.day.gam)
gam.check(krill_depth.lunar.gam)
gam.check(krill_depth.solar.gam)




#KRILL UPPER 0-200M 

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")

#Filtering upper 200m 
km_bm_surface <- km_bm_depth %>%
  filter(depth == "0-200")

#day
krill_surface.day.gam <- gam(log(bm_depth_krill) ~ s(day_fraction),data = km_bm_surface)
draw(krill_surface.day.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: Krill - Day (Surface Layer)",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(krill_surface.day.gam)
gam.check(krill_surface.day.gam)

#lunar fraction
krill_surface.lunar.gam <- gam(log(bm_depth_krill) ~ s(lunar_fraction),data = km_bm_surface)
draw(krill_surface.lunar.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: Krill - Lunar Fraction (Surface Layer)",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(krill_surface.lunar.gam)
gam.check(krill_surface.lunar.gam)

#solar angle
krill_surface.solar.gam <- gam(log(bm_depth_krill) ~ s(altitude),data = km_bm_surface)
draw(krill_surface.solar.gam, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth: Krill - Solar Angle (Surface Layer)",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(krill_surface.solar.gam)
gam.check(krill_surface.solar.gam)









#### TESTING GAMMA ######
#gamma instead of log for fish and solar angle 
library(gamm4)
m19<- gamm4(bm_depth_fish + 0.0001 ~ s(altitude, by = depth) , data = km_bm_depth, family = Gamma(), random = ~(1|midoc.stn))
par(mfrow = c(2, 2))

gam.check(m19$gam)
summary(m19$gam)
draw(m19$gam, residuals = TRUE)

#m15 <- gamm(bm_g_m3 ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ), family = Gamma(link = "inverse"))
#draw(m15, residuals = TRUE)
#summary(m15$gam)



#solar angle
m_gaus <- gam(log(bm_depth_fish) ~ s(altitude, by = depth) ,data = km_bm_depth)
plot(m_gaus, residuals = TRUE,pages=1)
summary(m_gaus)
par(mfrow=c(2,2))
gam.check(m_gaus)

m_gamma <- gam(bm_depth_fish+ 0.0001 ~ s(altitude, by = depth),data = km_bm_depth, family = Gamma())
plot(m_gamma, residuals = TRUE,type="response")
summary(m_gamma)
par(mfrow=c(2,2))
gam.check(m_gamma)

m_re <- gamm(log(bm_depth_fish) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(m_re$gam, residuals = TRUE)
summary(m_re$gam)



























#PLOTTING RESIDUALS \
library(ggplot2)
library(mgcv)


##SUMMED##
plot_residuals <- function(model, data, color_var) {
  # Extract fitted values and residuals
  fitted_vals <- fitted(model)
  residuals <- residuals(model)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Fitted = fitted_vals,
    Residuals = residuals,
    Color = data[[color_var]]
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Fitted, y = Residuals, color = Color)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Fitted Values", y = "Residuals", color = color_var)
  
  return(p)
}



# For model m4 (Day)
plot_residuals(m4, km_df_sum, "midoc.stn")




#FITTED VERSUS OBSERVED
plot_fitted_vs_observed <- function(model, data, title = "Fitted Values vs Observed Values") {
  # Extract fitted values
  fitted_vals <- fitted(model)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Fitted = fitted_vals,
    Observed = log(data$bm_g_m3),  # Assuming your response variable is log-transformed
    Depth = data$depth
  )
  
  # Define your custom color palette
  depth_colours <- c(
    "0-200m" =  "#FFC000",
    "200-400m" = "#ff7c43",
    "400-600m" = "#C41E3A",
    "600-800m" = "#4A92C6",
    "800-1000m" = "darkblue"
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Fitted, y = Observed, color = Depth)) +
    geom_point(size = 2, alpha = 0.7) +  # Slightly transparent points
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 14) +  # Increase base font size
    labs(x = "Fitted Values", y = "Observed Values (log-transformed)", 
         color = "Depth", title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
      legend.position = "right",  # Move legend to the right
      aspect.ratio = 0.8  # Make the plot slightly wider than tall
    ) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +  # Add 10% expansion to x-axis
    scale_y_continuous(expand = expansion(mult = 0.1)) +  # Add 10% expansion to y-axis
    scale_color_manual(values = depth_colours) +  # Use your custom color palette
    coord_fixed(ratio = 1)  # Ensure a 1:1 aspect ratio
  
  return(p)
}

#TOTAL TAXA - GELAT by depth 
plot_fitted_vs_observed(m10, km_df_depth, title = "Fitted Values vs Observed Values (Total taxa - excluding gelat, day)")
plot_fitted_vs_observed(m11, km_df_depth, title = "Fitted Values vs Observed Values (excluding gelat, lunar)")
plot_fitted_vs_observed(m12, km_df_depth, title = "Fitted Values vs Observed Values (excluding gelat, solar angle)")

#FISH - by depth
plot_fitted_vs_observed(m13, km_df_depth, title = "Fitted Values vs Observed Values (Fish - day)")
plot_fitted_vs_observed(m14, km_df_depth, title = "Fitted Values vs Observed Values (Fish - lunar fraction)")
plot_fitted_vs_observed(m15, km_df_depth, title = "Fitted Values vs Observed Values (Fish - solar angle)")

#SQUID - by depth
plot_fitted_vs_observed(m16, km_df_depth, title = "Fitted Values vs Observed Values (Squid - day)")
plot_fitted_vs_observed(m17, km_df_depth, title = "Fitted Values vs Observed Values (Squid - lunar fraction)")
plot_fitted_vs_observed(m18, km_df_depth, title = "Fitted Values vs Observed Values (Squid - solar angle)")

















