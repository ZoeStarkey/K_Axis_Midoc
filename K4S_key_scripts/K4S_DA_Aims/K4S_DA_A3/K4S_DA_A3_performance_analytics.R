library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)


\
#All Taxa
env_vars <- km_df[, c("TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude")]
env_vars <- env_vars[complete.cases(env_vars), ]


#Minus gelatinous
km_df_filtered <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df_filtered <-  km_df_filtered[!km_df_filtered$tax.grp %in% exclude_taxa, ]

env_vars <- km_df_filtered[, c("TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude")]
env_vars <- env_vars[complete.cases(env_vars), ]

#just fish and swuid 

include_taxa <- c("cephalopods", "fish")
km_df_filtered <- km_df_filtered[km_df_filtered$tax.grp %in% include_taxa, ]

env_vars <- km_df_filtered[, c("TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude")]
env_vars <- env_vars[complete.cases(env_vars), ]

chart.Correlation(env_vars, histogram=TRUE, pch=19)
