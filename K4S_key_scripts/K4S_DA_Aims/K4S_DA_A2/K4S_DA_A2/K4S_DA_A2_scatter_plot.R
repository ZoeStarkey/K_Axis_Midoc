library(ggplot2)
library(readr)
library(dplyr)
library(ggtext)
library(oce)
library(suncalc)
library(sf)
library(raster)
library(stars)

library(sp)


####SETTING UP#####
#setting up directory 
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#projection 


#Setting up km 
km <- readRDS("./derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
tmp <- read_csv("./source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("./source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)
tmp <- readRDS("./derived data/codend_taxa_biomass.rds")
km <- inner_join(km, tmp); rm(tmp)
file_path <- "./derived data/midoc_stations_envdata.rda" #adding zone
tmp <- readRDS(file_path)
km <- inner_join(km, tmp); rm(tmp)


#making km a dataframe
km_df <- as.data.frame(km)

##LOADING ENVIRONMENTAL DATA
# Load SST data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_anim_GHRSST.RData")
sst <- sstGHR
rm(sstGHR)

dts <- as.POSIXlt(seq(as.Date("2016-01-18"), as.Date("2016-02-18"), by = "1 day"), tz = "UTC")
bx <- c(60, 95, -70, -51)
cx <- 0.8;


km_df$SST <- NA

for (idate in 1:length(dts)) {
  print(paste(idate, "of", length(dts)))
  thisdate <- dts[idate]
  
  # Get the SST data for the current date
  tmp <- sst[[idate]]; tmp <- tmp - 273.15
  
  # Set SST plot limits
  sstmax <- 5
  sstmin <- -1.5
  tmp[tmp > sstmax] <- sstmax
  tmp[tmp < sstmin] <- sstmin
  
  # Extract SST values for biomass data points
  km_df$SST <- extract(tmp, as(km_sf, "Spatial"))
}

#load CHLA
# Load your raster data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_CHLA_VoyagePeriod.RData")
R <- R_voy_jf
q1 <- 0.05
q2 <- 10
R[R > q2] <- q2
R[R < q1] <- q1
R <- log(R)
zz <- c(0.05, 0.1, 0.25, 0.5, 1, 2.5, 5,7.5)
log_zz <- log(zz)

# Convert raster to data frame
R_df <- as.data.frame(R, xy = TRUE)
colnames(R_df) <- c("x", "y", "value")

# Check CRS of the raster and km_sf
raster_crs <- crs(R)
km_sf_crs <- st_crs(km_sf)

# If they do not match, transform the CRS of km_sf
if (km_sf_crs != raster_crs) {
  km_sf <- st_transform(km_sf, crs = raster_crs)
}

# Initialize a column to store CHLA values
km_df$CHLA <- NA

# Extract CHLA values for biomass data points
km_df$CHLA <- extract(R, as(km_sf, "Spatial"))

#km_df <- as.data.frame(st_drop_geometry(km_sf))

#Load TSM 
# Load the TSM data
tsm <- brick("./sophie_raster/KAXIS_tsm.grd") 
tsm[tsm == 32766] <- NA_real_ # land
tsm[tsm == 32765] <- NA_real_ # OOZ
tsm[tsm > 365] <- NA_real_ # not ice covered this season

# Use the 20th layer as specified
tsm <- tsm[[20]]

# Define the projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied


# Project the TSM raster to the desired CRS
tsm_proj <- projectRaster(tsm, crs = prj)

# Transform km_sf to the same CRS as the projected raster
km_sf <- st_transform(km_sf, crs = st_crs(prj))

# Convert km_sf to Spatial object
km_sp <- as(km_sf, "Spatial")

# Initialize a column to store TSM values
km_df$TSM <- NA

# Extract TSM values for biomass data points
km_df$TSM <- raster::extract(tsm_proj, km_sp)

# Convert km_sf to a regular data frame
#km_df <- as.data.frame(st_drop_geometry(km_sf))


#add Currents 
#load current data 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_curr_big.RData")


# Crop the rasters to the specified extent
bx <- c(60, 95, -71, -55) 
mag <- crop(mag, extent(bx + c(-3, 3, -3, 3)), snap = "out")

# Calculate mn_mag
mn_mag <- max(mag)
mn_mag[mn_mag > 0.25] <- 0.255
mn_mag[mn_mag < 0.025] <- 0.025
mn_mag <- mn_mag * 100  # scale values as in the base plot

# Project the mn_mag raster to the desired CRS
mn_mag_proj <- projectRaster(mn_mag, crs = prj)

# Convert km_sf to the same CRS as the projected raster
km_sf <- st_transform(km_sf, crs = st_crs(prj))

# Convert km_sf to Spatial object
km_sp <- as(km_sf, "Spatial")

# Initialize a column to store current data values
km_df$CUR <- NA

# Extract current data values for biomass data points
km_df$CUR <- raster::extract(mn_mag_proj, km_sp)

# Convert km_sf to a regular data frame
#km_df <- as.data.frame(st_drop_geometry(km_sf))

#depth bings
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

# Map the codends to depth ranges using the factor function
km_df$depth <- factor(km$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# CREATING FUNCTION for total bm and environmental variables 
# Load necessary library
library(ggplot2)

# Define the function
TBM_scatter <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  data_filtered <- data[!data$tax.grp %in% exclude_taxa, ]
  
  # Default labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Total Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Total Biomass (excluding gelatinous) vs", x_label)
  

  p <- ggplot(data_filtered, aes_string(x = x_var, y = y_var, color = depth_var)) +
    geom_point(size = 4) +
    labs(title = title,
         x = x_label,
         y = y_label,
         color = "depth_var") +
    theme_minimal() +
    scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "#CC7722", "400-600m" = "red", "600-800m" = "magenta", "800-1000m" = "purple", "0-1000m" = "darkblue")) }

# Example usage



#Environmental factors 
plot_Tmin <- TBM_scatter(km_df, "Tmin", x_label = "Minimum Temperature (°C)")
print(plot_Tmin)

plot_Tmin_depth <- TBM_scatter(km_df, "Tmin_depth", x_label = "Depth of Minimum Temperature (m)")
print(plot_Tmin_depth)

plot_Tmax <- TBM_scatter(km_df, "Tmax", x_label = "Maximum Temperature (°C)")
print(plot_Tmax)

plot_SML <- TBM_scatter(km_df, "SML", x_label = "Mixed Layer Salinity")
print(plot_SML)

plot_Smax <- TBM_scatter(km_df, "Smax", x_label = "Maximum Salinity")
print(plot_Smax)

plot_O2_min <- TBM_scatter(km_df, "O2_min", x_label = "Minimum Oxygen")
print(plot_O2_min)

plot_days_since_melt <- TBM_scatter(km_df, "days_since_melt", x_label = "Days Since Melt")
print(plot_days_since_melt)

plot_distance_ice <- TBM_scatter(km_df, "distance_to_ice_m", x_label = "Distance to Ice (m)")
print(plot_distance_ice) ##### COME BACK TO ##### 

plot_distance_edge <- TBM_scatter(km_df, "distance_to_edge_m", x_label = "Distance to Edge (m)")
print(plot_distance_edge) ##### COME BACK TO ##### 

plot_sea_ice_conc <- TBM_scatter(km_df, "sea_ice_conc", x_label = "Sea Ice Concentration (%)")
print(plot_sea_ice_conc)

plot_chl_rs <- TBM_scatter(km_df, "chl_rs", x_label = "Chlorophyll (check units)")
print(plot_chl_rs)

plot_intChl <- TBM_scatter(km_df, "intChl", x_label = "Integrated chlorophyll (check units)") 
print(plot_intChl)

plot_SST <- TBM_scatter(km_df, "SST", x_label = "Sea Surface Temperature (°C)")
print(plot_SST)

plot_CHLA <- TBM_scatter(km_df, "CHLA", x_label = "CHLA (Sum)")
print(plot_CHLA)



# CREATING FUNCTION for fish biomass and environmental variables
TBM_scatter_fish <- function(data, x_var, y_var = "bm_g_m3", x_label = NULL, y_label = NULL, title = NULL) {
  # Default labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Fish Biomass vs", x_label)
  
  # Filter data for fish
  data_filtered <- data %>% filter(tax.grp == "fish")
  
  # Create the plot
  p <- ggplot(data_filtered, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() 
  
  return(p)
}


plot_Tmin <- TBM_scatter_fish(km_df, "Tmin", x_label = "Minimum Temperature (°C)")
print(plot_Tmin)

plot_Tmin_depth <- TBM_scatter_fish(km_df, "Tmin_depth", x_label = "Depth of Minimum Temperature (m)")
print(plot_Tmin_depth)

plot_Tmax <- TBM_scatter_fish(km_df, "Tmax", x_label = "Maximum Temperature (°C)")
print(plot_Tmax)

plot_SML <- TBM_scatter_fish(km_df, "SML", x_label = "Mixed Layer Salinity")
print(plot_SML)

plot_Smax <- TBM_scatter_fish(km_df, "Smax", x_label = "Maximum Salinity")
print(plot_Smax)

plot_O2_min <- TBM_scatter_fish(km_df, "O2_min", x_label = "Minimum Oxygen")
print(plot_O2_min)

plot_days_since_melt <- TBM_scatter_fish(km_df, "days_since_melt", x_label = "Days Since Melt")
print(plot_days_since_melt)

plot_distance_ice <- TBM_scatter_fish(km_df, "distance_to_ice_m", x_label = "Distance to Ice (m)")
print(plot_distance_ice)

plot_distance_edge <- TBM_scatter_fish(km_df, "distance_to_edge_m", x_label = "Distance to Edge (m)")
print(plot_distance_edge)

plot_sea_ice_conc <- TBM_scatter_fish(km_df, "sea_ice_conc", x_label = "Sea Ice Concentration (%)")
print(plot_sea_ice_conc)

plot_chl_rs <- TBM_scatter_fish(km_df, "chl_rs", x_label = "Chlorophyll (check units)")
print(plot_chl_rs)

plot_intChl <- TBM_scatter_fish(km_df, "intChl", x_label = "Integrated Chlorophyll (check units)")
print(plot_intChl)

plot_SST <- TBM_scatter_fish(km_df, "SST", x_label = "Sea Surface Temperature (°C)")
print(plot_SST)

plot_CHLA <- TBM_scatter_fish(km_df, "CHLA", x_label = "Chlorophyll-a (log-transformed)")
print(plot_CHLA)



# CREATING FUNCTION for fish biomass and environmental variables
TBM_scatter_ceph <- function(data, x_var, y_var = "bm_g_m3", x_label = NULL, y_label = NULL, title = NULL) {
  # Default labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Cephalopod Biomass vs", x_label)
  
  # Filter data for fish
  data_filtered <- data %>% filter(tax.grp == "cephalopods")
  
  # Create the plot
  p <- ggplot(data_filtered, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() 
  
  return(p)
}



plot_Tmin <- TBM_scatter_ceph(km_df, "Tmin", x_label = "Minimum Temperature (°C)")
print(plot_Tmin)

plot_Tmin_depth <- TBM_scatter_ceph(km_df, "Tmin_depth", x_label = "Depth of Minimum Temperature (m)")
print(plot_Tmin_depth)

plot_Tmax <- TBM_scatter_ceph(km_df, "Tmax", x_label = "Maximum Temperature (°C)")
print(plot_Tmax)

plot_SML <- TBM_scatter_ceph(km_df, "SML", x_label = "Mixed Layer Salinity")
print(plot_SML)

plot_Smax <- TBM_scatter_ceph(km_df, "Smax", x_label = "Maximum Salinity")
print(plot_Smax)

plot_O2_min <- TBM_scatter_ceph(km_df, "O2_min", x_label = "Minimum Oxygen")
print(plot_O2_min)

plot_days_since_melt <- TBM_scatter_ceph(km_df, "days_since_melt", x_label = "Days Since Melt")
print(plot_days_since_melt)

plot_distance_ice <- TBM_scatter_ceph(km_df, "distance_to_ice_m", x_label = "Distance to Ice (m)")
print(plot_distance_ice)

plot_distance_edge <- TBM_scatter_ceph(km_df, "distance_to_edge_m", x_label = "Distance to Edge (m)")
print(plot_distance_edge)

plot_sea_ice_conc <- TBM_scatter_ceph(km_df, "sea_ice_conc", x_label = "Sea Ice Concentration (%)")
print(plot_sea_ice_conc)

plot_chl_rs <- TBM_scatter_ceph(km_df, "chl_rs", x_label = "Chlorophyll (check units)")
print(plot_chl_rs)

plot_intChl <- TBM_scatter_ceph(km_df, "intChl", x_label = "Integrated Chlorophyll (check units)")
print(plot_intChl)

plot_SST <- TBM_scatter_ceph(km_df, "SST", x_label = "Sea Surface Temperature (°C)")
print(plot_SST)

plot_CHLA <- TBM_scatter_ceph(km_df, "CHLA", x_label = "Chlorophyll-a (log-transformed)")
print(plot_CHLA)





#Integrating LUNAR FRACTION 
# Create a function to get the lunar phase for a specific date and location using oce 
get_lunar_fraction <- function(date, lat, lon) {
  moon_data <- moonAngle(as.POSIXct(date, tz = "Indian/Kerguelen"), longitude = lon, latitude = lat)
  lunar_fraction <- moon_data$illuminatedFraction
  return(lunar_fraction)
}

# Apply the function to each row in the dataframe
km_df <- km_df %>%
  mutate(
    start_time = as.POSIXct(start_time, tz = "Indian/Kerguelen"),  # Convert start_time to POSIXct
    lunar_fraction = mapply(get_lunar_fraction, start_time, lat_start, lon_start)
  )

#plotting lunar fraction
plot_lunar_fraction <- TBM_scatter(km_df, "lunar_fraction", x_label = "Lunar Fraction ")
print(plot_lunar_phase)


#plotting fish lunar phase 
km_df_fish <- km_df %>% filter(tax.grp == "fish")

lunar_phase_fish <- ggplot(km_df_fish, aes(x = lunar_phase, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Fish Biomass vs Lunar Phase",
       x = "Lunar Phase",
       y = expression(paste("Biomass g m"^"-3"))) +
  theme_minimal()

#plotting fish lunar phase 
km_df_ceph <- km_df %>% filter(tax.grp == "cephalopods")

lunar_phase_ceph <- ggplot(km_df_ceph, aes(x = lunar_phase, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Ceph Biomass vs Lunar Phase",
       x = "Lunar Phase",
       y = expression(paste("Biomass g m"^"-3"))) +
  theme_minimal()

print(lunar_phase_fish)
print(lunar_phase_ceph)


#INTEGRATING SOLAR 
# Ensure start_time is converted to POSIXct
km_df <- km_df %>%
  mutate(start_time = as.POSIXct(start_time, tz="Indian/Kerguelen"))

# Add solar position columns
km_df <- km_df %>%
  rowwise() %>%
  mutate(
    solar_position = list(getSunlightPosition(date = start_time, lat = lat_start, lon = lon_start)),
    azimuth = solar_position$azimuth,
    altitude = solar_position$altitude  * 180 / pi   # Convert from radians to degrees if needed
  ) %>%
  select(-solar_position) 

#plot solar angle 
plot_sol_azimuth <- TBM_scatter(km_df, "azimuth", x_label = "Solar angle (Azimuth)")
print(plot_sol_azimuth)

plot_sol_altitude <- TBM_scatter(km_df, "altitude", x_label = "Solar angle (Altitude)")
print(plot_sol_altitude)


#fish plot solar angle 
plot_fish_azimuth <- ggplot(km_df_fish, aes(x = azimuth, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Fish Biomass vs Solar Angle (Azimuth)",
       x = "Solar angle (Azimuth)",
       y = expression(paste("Fish Biomass g m"^"-3"))) +
  theme_minimal()

plot_fish_altitude <- ggplot(km_df_fish, aes(x = altitude, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Fish Biomass vs Solar Angle (Altitude)",
       x = "Solar angle (Altitude)",
       y = expression(paste("Fish Biomass g m"^"-3"))) +
  theme_minimal()


#cephalopods solar angle 
plot_cephalopod_azimuth <- ggplot(km_df_ceph, aes(x = azimuth, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Cephalopod Biomass vs Solar Angle (Azimuth)",
       x = "Solar angle (Azimuth)",
       y = expression(paste("Cephalopod Biomass g m"^"-3"))) +
  theme_minimal()

plot_cephalopod_altitude <- ggplot(km_df_ceph, aes(x = altitude, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Cephalopod Biomass vs Solar Angle (Altitude)",
       x = "Solar angle (Altitude)",
       y = expression(paste("Cephalopod Biomass g m"^"-3"))) +
  theme_minimal()

# Print the plot
print(plot_fish_azimuth)
print(plot_fish_altitude)
print(plot_cephalopod_azimuth)
print(plot_cephalopod_altitude)


#plot for DAY 
plot_total_biomass_time <- ggplot(km_df, aes(x = start_time, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Total Biomass Over Time",
       x = "Date",
       y = expression(paste("Total Biomass g m"^"-3"))) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the plot
print(plot_total_biomass_time)


#Fish 

plot_fish_biomass_time <- ggplot(km_df_fish, aes(x = start_time, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Fish Biomass Over Time",
       x = "Date",
       y = expression(paste("Biomass g m"^"-3"))) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the plot
print(plot_fish_biomass_time)


#Fish 

plot_ceph_biomass_time <- ggplot(km_df_ceph, aes(x = start_time, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Cephalopods Biomass Over Time",
       x = "Date",
       y = expression(paste("Biomass g m"^"-3"))) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the plot
print(plot_ceph_biomass_time)




























###PLOTTING####

  #TOTAL BIOMASS##
  #Biomass (all taxa - including mixed taxon) 
ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Total Biomass ",
       x = "Midoc Station",
       y = expression(paste("Total Biomass g m"^"-3"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Biomass (all taxa, not including mixed taxon)
filtered_df <- km_df %>%
  filter(tax.grp %in% c("fish", "cephalopods", "krill", "salps", "cnidarians"))

#Convert midoc.stn to a factor with levels in the desired order
filtered_df$midoc.stn <- factor(filtered_df$midoc.stn, levels = sort(unique(filtered_df$midoc.stn)))

# Plot using ggplot2
ggplot(filtered_df, aes(x = midoc.stn, y = bm_g_m3)) +
geom_point() +
labs(title = "Scatterplot of Total Biomass (Fish, Cephalopods, Krill, Salps, Cnidarians)",
     x = "Midoc Station",
     y = expression(paste("Total Biomass g m"^"-3"))) +
theme_minimal() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#Biomass (all taxa, not including mixed taxon)
filtered_df <- km_df %>%
  filter(tax.grp %in% c("fish", "cephalopods", "krill", "salps", "cnidarians"))

#Convert midoc.stn to a factor with levels in the desired order
filtered_df$midoc.stn <- factor(filtered_df$midoc.stn, levels = sort(unique(filtered_df$midoc.stn)))

# Plot using ggplot2
ggplot(filtered_df, aes(x = midoc.stn, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Scatterplot of Total Biomass (Fish, Cephalopods, Krill)",
       x = "Midoc Station",
       y = expression(paste("Total Biomass g m"^"-3"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



#Biomass for all taxa across different midoc station according to time of day 

# Create a custom order for midoc.stn based on DNC.visual
km_df <-km_df[order(factor(km_df$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]

# Convert midoc.stn to a factor with levels in the desired order
km_df$midoc.stn <- factor(km_df$midoc.stn, levels = unique(km_df$midoc.stn))

# Create a named vector of HTML-formatted labels for the x-axis
midoc_labels <- paste0(
  "<span style='color:", 
  ifelse(km_df$DNC.visual == "D", "orange", 
         ifelse(km_df$DNC.visual == "MC", "darkblue", 
                ifelse(km_df$DNC.visual == "N", "darkblue", "red"))),
  "'>",
  km_df$midoc.stn,
  "</span>"
)

# Ensure unique labels for the x-axis
names(midoc_labels) <- km_df$midoc.stn
unique_midoc_labels <- midoc_labels[!duplicated(names(midoc_labels))]




#BIOMASS ACROSS ALL STATIONS FUNCTION
# Define the function to plot biomass for a specific taxon
plot_taxon_biomass <- function(data, taxon, save_path) {
  # Filter the data for the selected taxon
  filtered_data <- subset(data, data$tax.grp == taxon)
  
  # Generate the scatter plot
  p <- ggplot(filtered_data, aes(x = midoc.stn, y = bm_g_m3)) +
    geom_point() +
    labs(title = paste("Scatterplot of Biomass for", taxon),
         x = "Midoc Station",
         y = expression(paste("Biomass g m"^"-3"))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Define the filename
 filename <- paste(save_path, "/K4S_Plot_A2_Scatter_BM(", taxon, ").png", sep = "")
  
  # Save the plot
 ggsave(filename = filename, plot = p, width = 8, height = 6, bg = "white")
  
}
save_directory <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2" 


# Example usage of the function
fish <- plot_taxon_biomass(km_df, "fish", save_directory)
plot_taxon_biomass(km_df, "cephalopods", save_directory)
plot_taxon_biomass(km_df, "krill",save_directory)
plot_taxon_biomass(km_df, "cnidarians", save_directory)
plot_taxon_biomass(km_df, "salps", save_directory)


plot(fish)

#SUMMED DATA 
#Summed biomass for cephalopods, krill, fish
taxa_of_interest <- c("cephalopods", "krill", "fish")

# Filter the data to include only the taxa of interest
filtered_data <- subset(km_df, tax.grp %in% taxa_of_interest)

# Aggregate the biomass by station
aggregated_data <- aggregate(bm_g_m3 ~ midoc.stn, data = filtered_data, sum)

# Generate the scatter plot
ggplot(aggregated_data, aes(x = midoc.stn, y = bm_g_m3)) +
  geom_point() +
  labs(title = "Total Biomass for Cephalopods, Krill, and Fish",
       x = "Midoc Station",
       y = "Total Biomass") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#summed data for individual taxa 

plot_taxon_biomass <- function(data, taxon,save_path) {
  # Filter the data for the selected taxon
  filtered_data <- subset(data, data$tax.grp == taxon)
  
  # Aggregate the biomass by station
  aggregated_data <- aggregate(bm_g_m3 ~ midoc.stn, data = filtered_data, sum)
  
  # Generate the scatter plot
  p <- ggplot(aggregated_data, aes(x = midoc.stn, y = bm_g_m3)) +
    geom_point() +
    labs(title = paste("Summed total Biomass for", taxon),
         x = "Midoc Station",
         y = expression(paste("Biomass g m"^"-3"))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Define the filename
  filename <- paste(save_path, "/K4S_Plot_A2_Scatter_SBM(", taxon, ").png", sep = "")
  
  # Save the plot
  ggsave(filename = filename, plot = p, width = 8, height = 6, bg = "white")
}

save_directory <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2" 


# Example usage of the function
plot_taxon_biomass(km_df, "fish", save_directory)
plot_taxon_biomass(km_df, "cephalopods", save_directory)
plot_taxon_biomass(km_df, "krill", save_directory)
plot_taxon_biomass(km_df, "cnidarians", save_directory)
plot_taxon_biomass(km_df, "salps",save_directory)






#adding trend lines:
library(dplyr)
library(ggplot2)

# Define the plot_taxon_biomass function
plot_taxon_biomass <- function(data, taxon) {
  # Filter the data for the selected taxon
  filtered_data <- subset(data, data$tax.grp == taxon)
  
  # Aggregate the biomass by station
  aggregated_data <- aggregate(bm_g_m3 ~ midoc.stn, data = filtered_data, sum)
  
  # Generate the scatter plot with a trend line
  p <- ggplot(aggregated_data, aes(x = midoc.stn, y = bm_g_m3)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Summed total Biomass for", taxon),
         x = "Midoc Station",
         y = expression(paste("Biomass g m"^"-3"))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Define the filename
  #filename <- paste(save_path, "/K4S_Plot_A2_Scatter_SBM(", taxon, ").png", sep = "")
  
  # Save the plot
 # ggsave(filename = filename, plot = p, width = 8, height = 6, bg = "white")
}

# Define the directory where you want to save the plots
#save_directory <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2" 

# Example usage of the function
fish <- plot_taxon_biomass(km_df, "fish")
plot_taxon_biomass(km_df, "cephalopods", save_directory)
plot_taxon_biomass(km_df, "krill", save_directory)
plot_taxon_biomass(km_df, "cnidarians", save_directory)
plot_taxon_biomass(km_df, "salps", save_directory)

plot(fish)









#Checking how many entries of non key species 
taxa_of_interest <- c("fish", "cephalopods", "krill", "cnidarians", "salps")

# Find the count of each taxon group
taxon_summary <- km_df %>%
  group_by(tax.grp) %>%
  summarise(count = n())

# Separate the counts of the specified taxa of interest
specified_taxa_summary <- taxon_summary %>%
  filter(tax.grp %in% taxa_of_interest)

# Separate the counts of the taxa not in the specified list
other_taxa_summary <- taxon_summary %>%
  filter(!(tax.grp %in% taxa_of_interest))

# Print the summaries
print("Summary of specified taxa of interest:")
print(specified_taxa_summary)

print("Summary of other taxa:")
print(other_taxa_summary)
