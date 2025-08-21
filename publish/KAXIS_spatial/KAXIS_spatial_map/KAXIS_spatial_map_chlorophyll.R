library(ggplot2)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rworldmap)
library(stars)
library(orsifronts)
library(dplyr)
library(readr)
library(scales)
library(sp)
library(palr)

#=============================================================================
# 1. Setup and Data Loading
#=============================================================================
#set working directory 
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/publish")
setwd(d)

#projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat }

# Load chlorophyll raster data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_spatial/KAXIS_raster/KAXIS_CHLA_VoyagePeriod.RData")
R <- R_voy_jf
  # Set quantile limits and log transform
q1 <- 0.05
q2 <- 10

R[R > q2] <- q2
R[R < q1] <- q1
R <- log(R)
  # Define chlorophyll scale breaks
zz <- c(0.05, 0.1, 0.25, 0.5, 1, 2.5, 5,7.5)
log_zz <- log(zz)

# Convert reprojected raster to data frame
R_projected <- projectRaster(R, crs = prj)
R_df <- as.data.frame(R_projected, xy = TRUE)
colnames(R_df) <- c("x", "y", "value")


# Load ice data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_spatial/KAXIS_raster/KAXIS_ice_df.rda")

# Load fronts data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_spatial/KAXIS_raster/KAXIS_FEATURESpoly_five.RData")
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_spatial/KAXIS_raster/KAXIS_FEATURESpoly_extras.RData")
sf::st_crs(f3$finished) <- 4326 
sf::st_crs(f1$finished) <- 4326 

# Fronts
ofp<- spTransform(orsifronts, CRS(prj))
ofp_sf <- st_as_sf(ofp)

#load biomass data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_data/KAXIS_data_processed/km_bm_sum_2.rda")
km_bm_sum_2 <- st_as_sf(km_bm_sum_2, 
                        coords = c("lon_start", "lat_start"), 
                        crs = 4326) %>%  
  st_transform(crs = prj)

#=============================================================================
# 2. Spatial Data Processing
#=============================================================================
# Process world map
ras.ext2 <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0)
data(countriesHigh, package = "rworldxtra")
wp <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj))
wp_sf <- st_as_sf(wp)

# Process study area
ras.ext <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40)
wc <- crop(countriesHigh, ras.ext)
wcp <- spTransform(wc, CRS(prj))
wcp_sf <- st_as_sf(wcp)

# Define graticule lines
xx <- c(0, 30, 60, 90, 120, 150, 180)
yy <- c(-90, -80, -70, -60, -50, -40, -30, -20)

# reading ktr
ktr <- readRDS("KAXIS_spatial/KAXIS_raster/nav_reduced.rds")
ktr <- ll2prj(ktr, loncol="LONGITUDE", latcol="LATITUDE")
ktr_sf <- st_as_sf(ktr)

#=============================================================================
# 3. Generating plot function 
#=============================================================================
# Create a color palette
ryb <- colorRampPalette(c("#31004a", "#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(51)

spatial_map_chla <- function(data, biomass_column, decimal_places = 2) {
  # Calculate bin breaks based on the actual data range
  n_bins <- 5
  bin_range <- range(data[[biomass_column]], na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  # Create labels with specified decimal places
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  # Filter out NAs and add biomass bins
  data <- data %>%
    filter(!is.na(!!sym(biomass_column))) %>%
    mutate(biomass_bin = cut(!!sym(biomass_column), 
                             breaks = bin_breaks,
                             labels = bin_labels,
                             include.lowest = TRUE))
  
  # Transform to the specified projection
  data_sf <- data %>%
    st_transform(crs = prj)
  
  # Get number of unique bins actually present in the data
  n_actual_bins <- length(unique(data_sf$biomass_bin))
  
  # Create size and fill scales based on number of actual bins
  size_values <- seq(6, 12, length.out = n_actual_bins)
  fill_values <- if(n_actual_bins <= 2) {
    c("white", "black")[1:n_actual_bins]
  } else {
    c("white", "grey65", "grey30", "black")[1:n_actual_bins]
  }
  
  # Create the plot
  p <- ggplot() +
    # Base raster layer
    geom_raster(data = R_df, aes(x = x, y = y, fill = value), alpha = 0.8) +
    scale_fill_gradientn(colors = ryb, breaks = log_zz, labels = sprintf("%.2f", zz),
                         limits = c(log(q1), log(q2)),
                         na.value = "grey85",
                         name = expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")")),
                         guide = guide_colorbar(title.position = "left",
                                                title.hjust = 0.5,
                                                label.position = "right",
                                                barwidth = 1,
                                                barheight = 16,
                                                order = 2,
                                                frame.linewidth = 0.2,
                                                title.theme = element_text(size = 14, angle = 90),
                                                label.theme = element_text(size = 14))) +
    
    # Add fronts
    geom_sf(data = f3$finished, color = "black", linewidth = 1) +
    geom_sf(data = f1$finished, color = "black", linewidth = 1) +
    
    # Add ice
    ggnewscale::new_scale_fill() + 
    geom_tile(data = ice_df, 
              aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), 
              alpha = 1) +
    scale_fill_gradientn(colors = palr::bathy_deep_pal(56), 
                         na.value = "transparent", 
                         limits = c(0, 100),
                         name = 'Ice (%)',
                         guide = guide_colorbar(title.position = "left", 
                                                title.hjust = 0.5,
                                                label.position = "right",
                                                barwidth = 1,
                                                barheight = 8,
                                                order = 3,
                                                frame.linewidth = 0.2,
                                                title.theme = element_text(size = 14, angle = 90),
                                                label.theme = element_text(size = 14))) + 
    
    # Add geographic features
    ggnewscale::new_scale_fill() +
    geom_sf(data = wcp_sf, fill = NA, color = "black") +
    geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "darkgrey", color = NA) +
    
    # Add grid lines
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), 
             color = "gray40", linetype = "dashed") + 
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), 
             color = "gray40", linetype = "dashed") +
    
    # Add KTR line
    geom_sf(data = ktr_sf, size = 1, colour = "magenta") +
    
    # Add biomass points with dynamic scales
    geom_sf(data = data_sf, 
            aes(fill = biomass_bin, size = biomass_bin), 
            shape = 21, color = "black") +
    scale_fill_manual(
      values = fill_values,
      name = expression(paste("Biomass (g m"^-3, ")"))
    ) +
    scale_size_manual(
      values = size_values,
      name = expression(paste("Biomass (g m"^-3, ")"))
    ) +
    
    # Formatting
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(crs = st_crs(prj), 
             xlim = c(-500000, 1020000), 
             ylim = c(-1000000, 600000)) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      panel.grid = element_line(color = "gray80", linetype = "solid"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14),
      legend.box.background = element_blank(),
      legend.byrow = TRUE,
      strip.background = element_rect(fill = "white"),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 16),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      panel.grid.major = element_line(color = "grey30", linetype = "solid"),
      panel.grid.minor = element_line(color = "grey30", linetype = "solid"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    ) +
    guides(
      fill = guide_legend(
        title.position = "left", 
        title.hjust = 0.5,
        override.aes = list(size = size_values),
        order = 1,
        title.theme = element_text(size = 14, angle = 90)
      ),
      size = guide_legend(
        title.position = "left", 
        title.hjust = 0.5,
        order = 1,
        title.theme = element_text(size = 14, angle = 90)
      )
    )
  
  return(p)
}

#=============================================================================
# 3. Usage Example
#=============================================================================
#All taxa - excluding salps + cnidarians 
chla_spatial_plot_all_taxa <- spatial_map_chla(km_bm_sum_2, "bm_sum_all_taxa", decimal_places = 2)
#Fish
chla_spatial_plot_fish <- spatial_map_chla(km_bm_sum_2, "bm_sum_fish", decimal_places = 3)
#Cephalopods
chla_spatial_plot_ceph <- spatial_map_chla(km_bm_sum_2, "bm_sum_ceph", decimal_places = 4)
#Krill
chla_spatial_plot_krill <- spatial_map_chla(km_bm_sum_2, "bm_sum_krill", decimal_places = 3)

chla_spatial_plot_all_taxa 
chla_spatial_plot_fish
chla_spatial_plot_ceph 
chla_spatial_plot_krill

