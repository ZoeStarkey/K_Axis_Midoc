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
  #remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

#summarising the dat a
km_df_total <- km_df %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    across(-bm_g_m3, ~ first(.))
  )


# Adding day 
km_df_total <- km_df_total %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

#
m1 <- gam(log(total_biomass) ~ s(start_time),data = km_df_total)


#Lunar fraction 
m2 <- gam(log(total_biomass) ~ s(lunar_fraction),data = km_df_total)
draw(m2, residuals = TRUE) 
summary(m1)




new_df <- km_df_total %>%
  select(midoc.stn, start_time, day)


