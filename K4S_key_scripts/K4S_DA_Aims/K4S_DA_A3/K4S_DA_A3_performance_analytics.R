library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)


\
#All Taxa
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#Summed data - one biom data point per station 
load("km_df_environmental_variables.Rda")

#add day 
km_df <- km_df %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

#add log
km_df$logged_bm <-log(km_df$bm_g_m3)


env_vars <- km_df[, c("TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude")]
env_vars <- env_vars[complete.cases(env_vars), ]


#Minus gelatinous
km_df_filtered <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df_filtered <-  km_df_filtered[!km_df_filtered$tax.grp %in% exclude_taxa, ]



#Temporal 

temporal_variables <- km_df_filtered[,c("logged_bm", "lunar_fraction","altitude","day")]
temporal_variables <- temporal_variables[complete.cases(temporal_variables), ]
chart.Correlation(temporal_variables, histogram=TRUE, pch=19)


env_vars <- km_df_filtered[, c("TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude")]
env_vars <- env_vars[complete.cases(env_vars), ]

#just fish and swuid 

include_taxa <- c("cephalopods", "fish")
km_df_filtered <- km_df_filtered[km_df_filtered$tax.grp %in% include_taxa, ]


include_taxa <- c("cephalopods", "fish")
km_sf <- km_sf[km_sf$tax.grp %in% include_taxa, ]


env_vars <- km_sf[, c("bm_log" , "TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude" )]
env_vars <- env_vars[complete.cases(env_vars), ]

chart.Correlation(temporal_variables, histogram=TRUE, pch=19)




