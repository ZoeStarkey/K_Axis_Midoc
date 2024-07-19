library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)


#Convert to numeric 
# km_sf$CHLA <- as.numeric(km_sf$CHLA)
# km_sf$TSM <- as.numeric(km_sf$TSM)
# km_sf$CUR <- as.numeric(km_sf$CUR)

env_vars <- km_df[, c("bm_g_m3", "TSM", "CUR", "SST", "Tmin","O2_min", "SML", "lunar_fraction", "moon_phase", "altitude")]
env_vars <- env_vars[complete.cases(env_vars), ]






chart.Correlation(env_vars, histogram=TRUE, pch=19)
