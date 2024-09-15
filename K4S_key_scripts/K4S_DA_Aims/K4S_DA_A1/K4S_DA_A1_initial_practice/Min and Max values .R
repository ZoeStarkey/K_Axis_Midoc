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

max(km_bm_sum$bm_sum_all_taxa, na.rm = TRUE)
max(km_bm_sum$TSM, na.rm = TRUE)
min(km_bm_sum$TSM, na.rm = TRUE)

max(km_bm_sum$SST, na.rm = TRUE)
min(km_bm_sum$SST, na.rm = TRUE)

max(km_bm_sum$CUR, na.rm = TRUE)
min(km_bm_sum$CUR, na.rm = TRUE)

max(km_bm_sum$altitude, na.rm = TRUE)
min(km_bm_sum$altitude, na.rm = TRUE)

#summing the total krill across all midoc stations 
sum(km_bm_sum$bm_sum_krill, na.rm = TRUE)
sum(km_bm_sum$bm_sum_all_taxa, na.rm = TRUE)
sum(km_bm_sum$bm_sum_fish, na.rm = TRUE)


load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")
#summing the krill across the 0-200m range 
sum(km_bm_depth$bm_depth_krill, na.rm = TRUE)

result <- km_bm_depth %>%
  group_by(depth) %>%
  summarise(total_krill_biomass = sum(bm_depth_krill, na.rm = TRUE)) %>%
  mutate(percentage = total_krill_biomass / sum(total_krill_biomass) * 100) %>%
  arrange(depth)

sum(km_bm_depth$bm_depth_krill, na.rm = TRUE)
sum(result$total_krill_biomass)

barplot(result$total_krill_biomass,
        names.arg = result$depth, 
        main = "Krill Biomass by Depth", 
        xlab = "Depth Category", 
        ylab = "Total Biomass")

#how many of each character type in DNC.visual
table(km_bm_sum$DNC.visual)

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")


#ALL TAXA 
biomass_summary <- km_bm_sum %>%
  filter(DNC.visual %in% c("NC", "N", "MC", "D")) %>%
  group_by(DNC.visual) %>%
  summarise(total_biomass = sum(bm_sum_all_taxa, na.rm = TRUE)) %>%
  mutate(period = case_when(
    DNC.visual == "NC" ~ "Sunset",
    DNC.visual == "N" ~ "Night",
    DNC.visual == "MC" ~ "Sunrise",
    DNC.visual == "D" ~ "Day"
  )) %>%
  dplyr::select(period, total_biomass)

print(biomass_summary)

biomass_summary_fish <- km_bm_sum %>%
  filter(DNC.visual %in% c("NC", "N", "MC", "D")) %>%
  group_by(DNC.visual) %>%
  summarise(total_biomass_fish = sum(bm_sum_fish, na.rm = TRUE)) %>%
  mutate(period = case_when(
    DNC.visual == "NC" ~ "Sunset",
    DNC.visual == "N" ~ "Night",
    DNC.visual == "MC" ~ "Sunrise",
    DNC.visual == "D" ~ "Day"
  )) %>%
  dplyr::select(period, total_biomass_fish)

print(biomass_summary_fish)



#sum of day and night 0.1913
 
 sum(km_bm_sum$ bm_sum_all_taxa, na.rm = TRUE)

