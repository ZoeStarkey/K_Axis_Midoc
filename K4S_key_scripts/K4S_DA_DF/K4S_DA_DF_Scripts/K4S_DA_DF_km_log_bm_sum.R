#loading dataframe
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_df_environmental_variables.Rda")

#adding day
km_df$date <- as.Date(km_df$start_time)

# Get the minimum date for each midoc.stn
min_dates <- tapply(km_df$date, km_df$midoc.stn, min)

# Calculate day_numeric
km_df$day <- round(as.numeric(difftime(km_df$date, min_dates[km_df$midoc.stn], units = "days")) + 1 - (16822.81 + 0.995277), 1)

#logging the biomass
km_df$logged_bm <-log(km_df$bm_g_m3)

#making a new column for everything but the "excluded taxa"
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")


# Filter the dataframe to exclude the specified taxa and only include non-NA depths
km_filtered_all_taxa <- km_df %>%
  filter(!tax.grp %in% exclude_taxa) 

#sum across the dataframe
km_sum_all_taxa <- km_filtered_all_taxa %>%
  group_by(midoc.stn) %>%
  summarize(
    log_bm_sum_all_taxa = sum(logged_bm, na.rm = TRUE),
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
  summarize(log_bm_sum_fish = sum(logged_bm, na.rm = TRUE)) %>%
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
  summarize(log_bm_sum_ceph = sum(logged_bm, na.rm = TRUE)) %>%
  ungroup()

km_log_bm_sum <- km_sum_all_taxa_with_fish %>%
  left_join(squid_biomass_summary, by = "midoc.stn")

#Krill
include_taxa <- c("krill")
km_filtered_krill <- km_df %>%
  filter(tax.grp %in% include_taxa)

krill_biomass_summary <- km_filtered_krill %>%
  filter(tax.grp == "krill") %>%  # 
  group_by(midoc.stn) %>%
  summarize(log_bm_sum_krill = sum(logged_bm, na.rm = TRUE)) %>%
  ungroup()

km_log_bm_sum <- km_log_bm_sum %>% left_join(krill_biomass_summary, by = "midoc.stn")


km_log_bm_sum <-km_log_bm_sum %>%
  dplyr::select(
    midoc.stn,
    log_bm_sum_all_taxa,
    log_bm_sum_fish,
    log_bm_sum_ceph,
    log_bm_sum_krill,
    everything()
  )

save(km_log_bm_sum, file = "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_log_bm_sum.Rda")

