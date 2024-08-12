library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)
library(dplyr)

#setting working directory
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#Logging the values
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

km_bm_sum$log_bm_sum_all_taxa <- log(km_bm_sum$bm_sum_all_taxa)
km_bm_sum$log_bm_sum_fish <- log(km_bm_sum$bm_sum_fish)
km_bm_sum$log_bm_sum_ceph <- log(km_bm_sum$bm_sum_ceph)
km_bm_sum$log_bm_sum_krill <- log(km_bm_sum$bm_sum_krill)


km_bm_sum <-km_bm_sum %>%
  dplyr::select(
    midoc.stn,
    bm_sum_all_taxa,
    log_bm_sum_all_taxa,
    bm_sum_fish,
    log_bm_sum_fish,
    bm_sum_ceph,
    log_bm_sum_ceph,
    bm_sum_krill,
    log_bm_sum_krill,
    everything()
  )

#########TOTAL TAXA############ 

#loading dataframe - values were logged and then summed 
par(mar=c(5,4,6,2))
#Temporal Data
temporal_all_taxa <- km_bm_sum[, c("log_bm_sum_all_taxa", "lunar_fraction", "altitude", "day")]
temporal_all_taxa <- temporal_all_taxa[complete.cases(temporal_all_taxa), ]
chart.Correlation(temporal_all_taxa, histogram=TRUE, pch=19, )
title(main="Temporal Vars Logged Biom All taxa (excluding gelat)", col.main="black", cex.main = 1, font.main=1, line = 3)

#Satellite Data 
satellite_all_taxa <- km_bm_sum[,c("log_bm_sum_all_taxa", "TSM", "CUR", "SST","CHLA")]
satellite_all_taxa <- satellite_all_taxa[complete.cases(satellite_all_taxa), ]
chart.Correlation(satellite_all_taxa, histogram=TRUE, pch=19)
title(main="Satellite Vars Logged Biom All taxa (excluding gelat)", col.main="black", cex.main = 1, font.main=1, line = 3)

#Environmental insitu
environ_all_taxa <- km_bm_sum[, c("log_bm_sum_all_taxa", "Tmin", "O2_min", "SML", "Smax")]
environ_all_taxa<- environ_all_taxa[complete.cases(environ_all_taxa), ]
chart.Correlation(environ_all_taxa, histogram=TRUE, pch=19)
title(main="Environmental Insitu Vars Logged Biom All taxa (excluding gelat)", col.main="black", cex.main = 1, font.main=1, line = 3)


##############FISH####################
#temporal variables 
temporal_fish <- km_bm_sum[, c("log_bm_sum_fish", "lunar_fraction", "altitude", "day")]
temporal_fish <- temporal_fish[complete.cases(temporal_all_taxa), ]
chart.Correlation(temporal_fish, histogram=TRUE, pch=19)
title(main="Temporal Vars Logged Biom Fish", col.main="black", cex.main = 1, font.main=1, line = 3)


#satellite variables
satellite_fish <- km_bm_sum[,c("log_bm_sum_fish", "TSM", "CUR", "SST","CHLA")]
satellite_fish <- satellite_fish[complete.cases(satellite_fish), ]
chart.Correlation(satellite_fish, histogram=TRUE, pch=19)
title(main="Satellite Vars Logged Biom Fish", col.main="black", cex.main = 1, font.main=1, line = 3)

#Environmental insitu
environ_fish <- km_bm_sum[, c("log_bm_sum_fish", "Tmin", "O2_min", "SML", "Smax")]
environ_fish <- environ_fish[complete.cases(environ_fish), ]
chart.Correlation(environ_fish, histogram=TRUE, pch=19)
title(main="Environmental Insitu Vars Logged Biom Fish", col.main="black", cex.main = 1, font.main=1, line = 3)

###########CEPHALOPODS##############
#temporal variables
temporal_ceph <- km_bm_sum[, c("log_bm_sum_ceph", "lunar_fraction", "altitude", "day")]
temporal_ceph <- temporal_ceph[complete.cases(temporal_ceph), ]
chart.Correlation(temporal_ceph, histogram=TRUE, pch=19)
title(main="Temporal Vars Logged Biom Cephalopods", col.main="black", cex.main = 1, font.main=1, line = 3)

#satellite variables
satellite_ceph <- km_bm_sum[,c("log_bm_sum_ceph", "TSM", "CUR", "SST","CHLA")]
satellite_ceph <- satellite_ceph[complete.cases(satellite_ceph), ]
chart.Correlation(satellite_ceph, histogram=TRUE, pch=19)
title(main="Satellite Vars Logged Biom Cephalopods", col.main="black", cex.main = 1, font.main=1, line = 3)

#Environmental insitu
environ_ceph <- km_bm_sum[, c("log_bm_sum_ceph", "Tmin", "O2_min", "SML", "Smax")]
environ_ceph <- environ_ceph[complete.cases(environ_ceph), ]
chart.Correlation(environ_ceph, histogram=TRUE, pch=19)
title(main="Environmental Insitu Vars Logged Biom Cephalopods", col.main="black", cex.main = 1, font.main=1, line = 3)

##########KRILL##################
#temporal variables
temporal_krill <- km_bm_sum[, c("log_bm_sum_krill", "lunar_fraction", "altitude", "day")]
temporal_krill <- temporal_krill[complete.cases(temporal_krill), ]
chart.Correlation(temporal_krill, histogram=TRUE, pch=19)
title(main="Temporal Vars Logged Biom Krill", col.main="black", cex.main = 1, font.main=1, line = 3)

#satellite variables
satellite_krill <- km_bm_sum[,c("log_bm_sum_krill", "TSM", "CUR", "SST","CHLA")]
satellite_krill <- satellite_krill[complete.cases(satellite_krill), ]
chart.Correlation(satellite_krill, histogram=TRUE, pch=19)
title(main="Satellite Vars Logged Biom Krill", col.main="black", cex.main = 1, font.main=1, line = 3)

#Environmental insitu
environ_krill <- km_bm_sum[, c("log_bm_sum_krill", "Tmin", "O2_min", "SML", "Smax")]
environ_krill <- environ_krill[complete.cases(environ_krill), ]
chart.Correlation(environ_krill, histogram=TRUE, pch=19)
title(main="Environmental Insitu Vars Logged Biom Krill", col.main="black", cex.main = 1, font.main=1, line = 3)





#All Taxa
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#Summed data - one biom data point per station 
load("km_df_environmental_variables.Rda")

#add day 
km_df <- km_df %>%
  mutate(day = (as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24)))


#add log
km_df$logged_bm <-log(km_df$bm_g_m3)


# env_vars <- km_df[, c("TSM", "CUR", "SST","CHLA", "Tmin", "Tmax", "O2_min", "SML", "Smax", "lunar_fraction", "moon_phase", "altitude")]
# env_vars <- env_vars[complete.cases(env_vars), ]



#filter out depth 
km_df_filtered <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")

#Minus gelatinous
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df_filtered <-  km_df_filtered[!km_df_filtered$tax.grp %in% exclude_taxa, ]



#Temporal 
temporal_vars <- km_df_filtered[,c("logged_bm", "lunar_fraction","altitude","day")]
temporal_vars <- temporal_vars[complete.cases(temporal_vars), ]
chart.Correlation(temporal_vars, histogram=TRUE, pch=19)

#Satellite Data 
sat_vars <- km_df_filtered[,c("logged_bm", "TSM", "CUR", "SST","CHLA")]
sat_vars <- sat_vars[complete.cases(sat_vars), ]
chart.Correlation(sat_vars, histogram=TRUE, pch=19)

#Environmental insitu
env_vars <- km_df_filtered[, c("logged_bm", "Tmin", "O2_min", "SML", "Smax")]
env_vars <- env_vars[complete.cases(env_vars), ]
chart.Correlation(env_vars, histogram=TRUE, pch=19)



#just fish and swuid 

include_taxa <- c("cephalopods")
km_df_filtered <- km_df_filtered[km_df_filtered$tax.grp %in% include_taxa, ]

#Temporal 
temporal_vars <- km_df_filtered[,c("logged_bm", "lunar_fraction","altitude","day")]
temporal_vars <- temporal_vars[complete.cases(temporal_vars), ]
chart.Correlation(temporal_vars, histogram=TRUE, pch=19)

#Satellite Data 
sat_vars <- km_df_filtered[,c("logged_bm", "TSM", "CUR", "SST","CHLA")]
sat_vars <- sat_vars[complete.cases(sat_vars), ]
chart.Correlation(sat_vars, histogram=TRUE, pch=19)

#Environmental insitu
env_vars <- km_df_filtered[, c("logged_bm", "Tmin", "O2_min", "SML", "Smax")]
env_vars <- env_vars[complete.cases(env_vars), ]
chart.Correlation(env_vars, histogram=TRUE, pch=19)




