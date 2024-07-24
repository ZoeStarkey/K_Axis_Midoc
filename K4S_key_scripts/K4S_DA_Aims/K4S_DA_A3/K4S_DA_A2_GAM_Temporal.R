#library
library(mgcv)
library(gratia)

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


#Lunar fraction 
m1 <- gam(log(total_biomass) ~ s(lunar_fraction, by = depth),data = km_df_total)
draw(m1, residuals = TRUE) 


summary(m1)




