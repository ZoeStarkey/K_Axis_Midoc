library(dplyr)
library(lubridate)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#Making all taxa - gelatinous summed dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_df_environmental_variables.Rda")

# Define the depth ranges for each codend
depth_bins <- c("0-200", "200-400", "400-600", "600-800", "800-1000", "0-1000")

# Map the codends to depth ranges using the factor function
km_df$depth <- factor(km_df$cod.end, 
                      levels = c("6", "5", "4", "3", "2", "1"), 
                      labels = depth_bins)

#removing 0-1000m
remove_depth <- c("0-1000")

# Remove the specified stations from km_df
km_df <- km_df %>%
  filter(!depth %in% "0-1000")

#removing abandonded stations

abandoned_stations <- c("MIDOC02","MIDOC08", "MIDOC10", "MIDOC12", "MIDOC33")

# Remove the specified stations from km_df
km_df <- km_df %>%
  filter(!midoc.stn %in% abandoned_stations)

#delete the columns not in use 
km_df <- subset(km_df, select = -azimuth)
km_df <- subset(km_df, select = -zone.notes)


#adding a day column

# Convert start_time to Date if it's not already
km_df$date <- as.Date(km_df$start_time)

# Get the minimum date for each midoc.stn
min_dates <- tapply(km_df$date, km_df$midoc.stn, min)

# Calculate day_numeric
km_df$day <- round(as.numeric(difftime(km_df$date, min_dates[km_df$midoc.stn], units = "days")) + 1 - (16822.81 + 0.995277), 1)


#excluding the taxa 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")

# Filter the dataframe to exclude the specified taxa and only include non-NA depths
km_filtered_all_taxa <- km_df %>%
  filter(!tax.grp %in% exclude_taxa) 

#sum across the dataframe
km_sum_all_taxa <- km_filtered_all_taxa %>%
  group_by(midoc.stn) %>%
  summarize(
    bm_sum_all_taxa = sum(bm_g_m3, na.rm = TRUE),
    across(everything(), ~first(.))
  ) %>%
  ungroup()


#FISH
include_taxa <- c("fish")
km_filtered_fish <- km_df %>%
  filter(tax.grp %in% include_taxa) 


fish_biomass_summary <- km_filtered_fish %>%
  filter(tax.grp == "fish") %>%  
  group_by(midoc.stn) %>%
  summarize(bm_sum_fish = sum(bm_g_m3, na.rm = TRUE)) %>%
  ungroup()

km_sum_all_taxa_with_fish <- km_sum_all_taxa %>%
  left_join(fish_biomass_summary, by = "midoc.stn")

#SQUID
include_taxa <- c("cephalopods")
km_filtered_squid <- km_df %>%
  filter(tax.grp %in% include_taxa)

squid_biomass_summary <- km_filtered_squid %>%
  filter(tax.grp == "cephalopods") %>%  # 
  group_by(midoc.stn) %>%
  summarize(bm_sum_ceph = sum(bm_g_m3, na.rm = TRUE)) %>%
  ungroup()

km_bm_sum <- km_sum_all_taxa_with_fish %>%
  left_join(squid_biomass_summary, by = "midoc.stn")

#removing columns that are no longer useful 
km_bm_sum <- subset(km_bm_sum, select = -tax.grp)
km_bm_sum <- subset(km_bm_sum, select = -fish.grp)
km_bm_sum <- subset(km_bm_sum, select = -bm_g_m3)
km_bm_sum <- subset(km_bm_sum, select = - swept_m3)
km_bm_sum <- subset(km_bm_sum, select = - depth)
km_bm_sum <- subset(km_bm_sum, select = - cod.end)

#reordering so the biomass columns are next to each other 
km_bm_sum <-km_bm_sum %>%
  dplyr::select(
    midoc.stn,
    bm_sum_all_taxa,
    bm_sum_fish,
    bm_sum_ceph,
    everything()
  )

#saving the dataframe
save(km_bm_sum, file = "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")



### SUMMING ACCROSS MIDOC AND DEPTH 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")

km_bm_all_taxa_sum_depth <- km_df %>%
  filter(!tax.grp %in% exclude_taxa) %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    bm_depth_all_taxa = sum(bm_g_m3, na.rm = TRUE),
    across(everything(), ~first(.)),
    .groups = "drop"
  )

#FISH
include_taxa <- c("fish")
km_bm_fish_sum_depth <- km_df %>%
  filter(tax.grp %in% include_taxa) %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    bm_depth_fish = sum(bm_g_m3, na.rm = TRUE),
    across(everything(), ~first(.)),
    .groups = "drop"
  )

#SQUID
include_taxa <- c("cephalopods")
km_bm_squid_sum_depth <- km_df %>%
  filter(tax.grp %in% include_taxa) %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    bm_depth_ceph = sum(bm_g_m3, na.rm = TRUE),
    across(everything(), ~first(.)),
    .groups = "drop"
  )

km_bm_depth <- km_bm_all_taxa_sum_depth %>%
  left_join(km_bm_fish_sum_depth, by = c("midoc.stn", "depth")) %>%
  left_join(km_bm_squid_sum_depth, by = c("midoc.stn", "depth"))


km_bm_depth <-km_bm_depth %>% 
  dplyr::select(
    midoc.stn,
    bm_depth_all_taxa,
    bm_depth_fish,
    bm_depth_ceph,
    everything()
  )



save(km_bm_depth , file = "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/km_bm_depth.Rda")

