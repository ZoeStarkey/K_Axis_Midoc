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
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}

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

km$lon_start_orig <- km$lon_start
km <- ll2prj(km, loncol="lon_start_orig", latcol="lat_start_orig")

#making km a dataframe
km_df <- as.data.frame(km)
#making km a sf
km_sf <- st_as_sf(km)

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
#zz <- c(0.05, 0.1, 0.25, 0.5, 1, 2.5, 5,7.5)
#log_zz <- log(zz)

km_sf <- st_transform(km_sf, crs = crs(R))

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


# Lunar Fraction
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


# Luanr Phase
  # get julian date
get_julian_date <- function(date) {
  julian_date <- as.numeric(julian(date, origin = as.POSIXct("1970-01-01", tz = "UTC"))) + 2440587.5
  return(julian_date)
}

get_moon_phase <- function(date) {
  julian_date <- get_julian_date(date)
  D <- julian_date - 2451550.1
  N <- (D / 29.53058867) %% 1
  if (N < 0) N <- N + 1  # Ensure N is positive
  phase <- ifelse(N < 0.25, 0, ifelse(N < 0.5, 1/4, ifelse(N < 0.75, 1/2, 3/4)))
  return(phase)
}


  # Function to convert start_time to moon phase
get_lunar_phase <- function(date, lat, lon) {
  return(get_moon_phase(as.POSIXct(date, tz = "Indian/Kerguelen")))
}

  # Apply the function to each row in the dataframe
km_df <- km_df %>%
  mutate(
    start_time = as.POSIXct(start_time, tz = "Indian/Kerguelen"),  # Convert start_time to POSIXct
    moon_phase = mapply(get_lunar_phase, start_time, lat_start, lon_start)
  )

#Solar Angle
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



# Define the stations to remove
abandoned_stations <- c("MIDOC02","MIDOC08", "MIDOC10", "MIDOC12", "MIDOC33")

# Remove the specified stations from km_df
km_df <- km_df %>%
  filter(!midoc.stn %in% abandoned_stations)

#saving the data_frame 
save(km_df, file = "km_df_environmental_variables.Rda")
load("km_df_environmental_variables.Rda")


#PLOTTING
  #PLOTS EXCLUDING GELATIONOUS WITHOUT SMOOTH 
TBM_scatter_aggregate <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", aggregate_func = c("sum", "mean"), x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  data_filtered <- data[!data$tax.grp %in% exclude_taxa, ]
  
  # Aggregate data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(depth_var, x_var)) %>%
    summarize(
      total_biomass = sum(!!sym(y_var), na.rm = TRUE),
      mean_biomass = mean(!!sym(y_var), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Define labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Taxa Biomass (excluding gelatinous) vs", x_var)
  
  plots <- list()
  
  # Create scatter plots for each aggregation type
  for (func in aggregate_func) {
    if (func == "sum") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "total_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +  # Add trend lines
        labs(title = paste(title, "(Sum)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["sum"]] <- p
    } else if (func == "mean") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "mean_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +  # Add trend lines
        labs(title = paste(title, "(Mean)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["mean"]] <- p
    }
  }
  
  return(plots)
}

# Example usage
# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Create scatter plots for summed and mean biomass
plot <- TBM_scatter_aggregate(km_df, "CHLA")
plot <- TBM_scatter_aggregate(km_df, "SST")
plot <- TBM_scatter_aggregate(km_df, "TSM")
plot <- TBM_scatter_aggregate(km_df, "CUR")
plot <- TBM_scatter_aggregate(km_df, "Tmin")
plot <- TBM_scatter_aggregate(km_df, "O2_min")
plot <- TBM_scatter_aggregate(km_df, "SML")
plot <- TBM_scatter_aggregate(km_df, "lunar_fraction")
plot <- TBM_scatter_aggregate(km_df, "moon_phase")
plot <- TBM_scatter_aggregate(km_df, "altitude")

# Print the plots
print(plot[["sum"]])
print(plot[["mean"]])


  #PLOTS EXCLUDING GELATINOUS WITH SMOOTH
TBM_scatter_aggregate <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", aggregate_func = c("sum", "mean"), x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  data_filtered <- data[!data$tax.grp %in% exclude_taxa, ]
  
  # Aggregate data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(depth_var, x_var)) %>%
    summarize(
      total_biomass = sum(!!sym(y_var), na.rm = TRUE),
      mean_biomass = mean(!!sym(y_var), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Define labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Taxa Biomass (excluding gelatinous) vs", x_var)
  
  plots <- list()
  
  # Create scatter plots for each aggregation type
  for (func in aggregate_func) {
    if (func == "sum") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "total_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +
        geom_smooth(method = "loess", se = FALSE) +  # Add smoothing line
        labs(title = paste(title, "(Sum)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["sum"]] <- p
    } else if (func == "mean") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "mean_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +
        geom_smooth(method = "loess", se = FALSE) +  # Add smoothing line
        labs(title = paste(title, "(Mean)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["mean"]] <- p
    }
  }
  
  return(plots)
}

# Example usage
# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Create scatter plots for summed and mean biomass
plots <- TBM_scatter_aggregate(km_df, "CHLA")
plots <- TBM_scatter_aggregate(km_df, "SST")
plots <- TBM_scatter_aggregate(km_df, "TSM")
plots <- TBM_scatter_aggregate(km_df, "CUR")
plots <- TBM_scatter_aggregate(km_df, "Tmin")
plots <- TBM_scatter_aggregate(km_df, "O2_min")
plots <- TBM_scatter_aggregate(km_df, "SML")
plots <- TBM_scatter_aggregate(km_df, "lunar_fraction")
plots <- TBM_scatter_aggregate(km_df, "moon_phase")
plots <- TBM_scatter_aggregate(km_df, "altitude")

# Print the plots
print(plots[["sum"]])
print(plots[["mean"]])


#PLOTS OF CEPHALOPOD AND FISH WITHOUT SMOOTH
TBM_scatter_aggregate <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", aggregate_func = c("sum", "mean"), x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  data_filtered <- data[!data$tax.grp %in% exclude_taxa, ]
  
  # Aggregate data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(depth_var, x_var)) %>%
    summarize(
      total_biomass = sum(!!sym(y_var), na.rm = TRUE),
      mean_biomass = mean(!!sym(y_var), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Define labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Taxa Biomass (excluding gelatinous) vs", x_var)
  
  plots <- list()
  
  # Create scatter plots for each aggregation type
  for (func in aggregate_func) {
    if (func == "sum") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "total_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +  # Add trend lines
        labs(title = paste(title, "(Sum)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["sum"]] <- p
    } else if (func == "mean") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "mean_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +  # Add trend lines
        labs(title = paste(title, "(Mean)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["mean"]] <- p
    }
  }
  
  return(plots)
}

# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Create scatter plots for summed and mean biomass
plot <- TBM_scatter_aggregate(km_df, "CHLA")
plot <- TBM_scatter_aggregate(km_df, "SST")
plot <- TBM_scatter_aggregate(km_df, "TSM")
plot <- TBM_scatter_aggregate(km_df, "CUR")
plot <- TBM_scatter_aggregate(km_df, "Tmin")
plot <- TBM_scatter_aggregate(km_df, "O2_min")
plot <- TBM_scatter_aggregate(km_df, "SML")
plot <- TBM_scatter_aggregate(km_df, "lunar_fraction")
plot <- TBM_scatter_aggregate(km_df, "moon_phase")
plot <- TBM_scatter_aggregate(km_df, "altitude")

# Print the plots
print(plot[["sum"]])
print(plot[["mean"]])

#PLOTTING - FISH/CEPHALOPODS
TBM_scatter_aggregate <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", aggregate_func = c("sum", "mean"), x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  include_taxa <- c("cephalopods")
  data_filtered <- data[data$tax.grp %in% include_taxa, ]
  
  # Aggregate data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(depth_var, x_var)) %>%
    summarize(
      total_biomass = sum(!!sym(y_var), na.rm = TRUE),
      mean_biomass = mean(!!sym(y_var), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Define labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Taxa Biomass (excluding gelatinous) vs", x_var)
  
  plots <- list()
  
  # Create scatter plots for each aggregation type
  for (func in aggregate_func) {
    if (func == "sum") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "total_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +  # Add trend lines
        labs(title = paste(title, "(Sum)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["sum"]] <- p
    } else if (func == "mean") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "mean_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +  # Add trend lines
        labs(title = paste(title, "(Mean)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["mean"]] <- p
    }
  }
  
  return(plots)
}

# Example usage
# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Create scatter plots for summed and mean biomass
plot <- TBM_scatter_aggregate(km_df, "CHLA")
plot <- TBM_scatter_aggregate(km_df, "SST")
plot <- TBM_scatter_aggregate(km_df, "TSM")
plot <- TBM_scatter_aggregate(km_df, "CUR")
plot <- TBM_scatter_aggregate(km_df, "Tmin")
plot <- TBM_scatter_aggregate(km_df, "O2_min")
plot <- TBM_scatter_aggregate(km_df, "SML")
plot <- TBM_scatter_aggregate(km_df, "lunar_fraction")
plot <- TBM_scatter_aggregate(km_df, "moon_phase")
plot <- TBM_scatter_aggregate(km_df, "altitude")

# Print the plots
print(plot[["sum"]])
print(plot[["mean"]])

  #PLTOS OF CEPHALOPODS AND FISH WITH SMOOTH 



TBM_scatter_aggregate <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", aggregate_func = c("sum", "mean"), x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  include_taxa <- c("cephalopods")
  data_filtered <- data[data$tax.grp %in% include_taxa, ]
  
  # Aggregate data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(depth_var, x_var)) %>%
    summarize(
      total_biomass = sum(!!sym(y_var), na.rm = TRUE),
      mean_biomass = mean(!!sym(y_var), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Define labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of fish biomass vs", x_var)
  
  plots <- list()
  
  # Create scatter plots for each aggregation type
  for (func in aggregate_func) {
    if (func == "sum") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "total_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +
        geom_smooth(method = "loess", se = FALSE) +  # Add smoothing line
        labs(title = paste(title, "(Sum)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["sum"]] <- p
    } else if (func == "mean") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "mean_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +
        geom_smooth(method = "loess", se = FALSE) +  # Add smoothing line
        labs(title = paste(title, "(Mean)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["mean"]] <- p
    }
  }
  
  return(plots)
}

# Example usage
# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Create scatter plots for summed and mean biomass
plots <- TBM_scatter_aggregate(km_df, "CHLA") 
plots <- TBM_scatter_aggregate(km_df, "SST") 
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "TSM")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "CUR")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "Tmin")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "O2_min")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "SML")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "lunar_fraction")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "moon_phase")
print(plots[["mean"]])
plots <- TBM_scatter_aggregate(km_df, "altitude")
print(plots[["mean"]])

# Print the plots
print(plots[["sum"]])
print(plots[["mean"]])












