library(dplyr)

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

get_lunar_fraction <- function(date, lat, lon) {
  date_posix <- as.POSIXct(date, tz = "UTC")
  moon_data <- oce::moonAngle(date_posix, longitude = lon, latitude = lat)
  return(moon_data$illuminatedFraction)
}

km_bm_depth <- km_bm_depth  %>%
  mutate(
    illum_fraction = mapply(get_lunar_fraction, start_time, lat_start, lon_start)
  )

#get lunar phase using oce 

get_lunar_phase <- function(date, lat, lon) {
  date_posix <- as.POSIXct(date, tz = "UTC")
  moon_data <- oce::moonAngle(date_posix, longitude = lon, latitude = lat)
  return(moon_data$phase)
}

km_bm_sum <- km_bm_sum %>%
  mutate(
    lunar_phase = mapply(get_lunar_phase, start_time, lat_start, lon_start)
  )

# Display the first few rows of the updated dataframe
head(df)


#max CHLA in km_bm_sum
max_row <- km_bm_sum[which.max(km_bm_sum$CHLA), ]
max_row
max_row$midoc.stn

max(km_bm_sum$CHLA, na.rm = TRUE)
min(km_bm_sum$CHLA, na.rm = TRUE)

max(km_bm_sum$TSM, na.rm = TRUE)
min(km_bm_sum$TSM, na.rm = TRUE)

max(km_bm_sum$SST, na.rm = TRUE)
min(km_bm_sum$SST, na.rm = TRUE)

max(km_bm_sum$CUR, na.rm = TRUE)
min(km_bm_sum$CUR, na.rm = TRUE)

max(km_bm_sum$altitude, na.rm = TRUE)
min(km_bm_sum$altitude, na.rm = TRUE)


