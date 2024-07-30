#library
library(mgcv)
library(gratia)
library(dplyr)

#linear model
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#Summed data - one biom data point per station 
load("km_df_environmental_variables.Rda")
  #remove the 0-1000m
km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
#include_taxa <- c("fish")
#km_df <-  km_df[km_df$tax.grp %in% include_taxa, ]


  #remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

#summarising the data 
  #didnt work, but now is working again
km_df_sum <- km_df %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    across(-bm_g_m3, ~ first(.))
  )

# Adding day 
km_df_sum <- km_df_sum %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

#
m1 <- gam(log(total_biomass) ~ s(day),data = km_df_sum)
draw(m1, residuals = TRUE) 
summary(m1)

#Lunar fraction 
m2 <- gam(log(total_biomass) ~ s(lunar_fraction),data = km_df_sum)
draw(m2, residuals = TRUE) 
summary(m2)

 #Solar angle 
m3 <- gam(log(total_biomass) ~ s(altitude),data = km_df_sum)
draw(m3, residuals = TRUE) 
summary(m3)



 
#Summed data with depth 
load("km_df_environmental_variables.Rda")
#remove the 0-1000m
km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
#remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

# Adding day 
km_df <- km_df %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

km_df_sum_depth <- km_df %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    biomass_sum = sum(bm_g_m3, na.rm = TRUE),
    across(c(-bm_g_m3), ~ first(.))
  )

#day 
m4 <- gam(log(biomass_sum) ~ s(day, by = depth),data = km_df_sum_depth)
draw(m4, residuals = TRUE)
summary(m4)

#lunar fraction
m5 <- gam(log(biomass_sum) ~ s(lunar_fraction, by = depth),data = km_df_sum_depth)
draw(m5, residuals = TRUE)
summary(m5)

#solar angle
m6 <- gam(log(biomass_sum) ~ s(altitude, by = depth),data = km_df_sum_depth)
draw(m6, residuals = TRUE)
summary(m6)






#FISH/SQUID
#Summed data with depth 
load("km_df_environmental_variables.Rda")
#remove the 0-1000m
km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
#remove gelatinous 
include_taxa <- c("fish")
km_df <-  km_df[km_df$tax.grp %in% include_taxa, ]

# Adding day 
km_df <- km_df %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

km_df_sum_depth <- km_df %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    biomass_sum = sum(bm_g_m3, na.rm = TRUE),
    across(c(-bm_g_m3), ~ first(.))
  )

#day 
m4 <- gam(log(biomass_sum) ~ s(day, by = depth),data = km_df_sum_depth)
draw(m4, residuals = TRUE)
summary(m4)

#lunar fraction
m5 <- gam(log(biomass_sum) ~ s(lunar_fraction, by = depth),data = km_df_sum_depth, family = poisson)
draw(m5, residuals = TRUE)
summary(m5)

#solar angle
m6 <- gam(log(biomass_sum) ~ s(altitude, by = depth),data = km_df_sum_depth)
draw(m6, residuals = TRUE)
summary(m6)








#Sanity check 
new_df <- km_df_sum_depth[, c("midoc.stn", "biomass_sum", "depth")]
new_df <- km_df[, c("midoc.stn", "bm_g_m3", "depth")]

original_subset <- km_df %>%
  filter(midoc.stn == "MIDOC01" & depth == "200-400m")

# Calculate the manual sum of bm_g_m3 for this subset
manual_sum <- sum(original_subset$bm_g_m3, na.rm = TRUE)

print(manual_sum)
