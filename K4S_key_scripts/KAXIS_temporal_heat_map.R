library(ggplot2)
library(reshape2)
library(dplyr)
library(ggtext)
library(tidyr)
library(readr)
library(stars)
library(sf)
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

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_data/KAXIS_data_processed/km_bm_depth_2.rda")
#remove the NA from depth column (organisms captured at front of net)
km_bm_depth_2 <- km_bm_depth_2[!is.na(km_bm_depth_2$depth),]


#=============================================================================
# 2. Generating plot function for solar heat maps
#=============================================================================
create_heatmap_solar <- function(data, biomass_column, title, panel_bg_color = "white") {
  # First, create the correct station order based on DNC.visual
  station_order <- data %>%
    arrange(factor(DNC.visual, levels = c("MC", "D", "NC", "N"))) %>%
    distinct(midoc.stn) %>%
    pull(midoc.stn)
  
  # Reshape the data to long format for the specified biomass column
  km_heat <- data %>%
    select(midoc.stn, depth, !!sym(biomass_column), DNC.visual) %>%
    filter(!is.na(depth))
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("0-200", "200-400", "400-600", "600-800", "800-1000")
  km_heat$depth <- factor(km_heat$depth, levels = depth_bins)
  
  # Create all combinations of midoc.stn and depth
  all_combinations <- expand.grid(
    midoc.stn = station_order,  # Use our predetermined order
    depth = factor(depth_bins, levels = depth_bins),
    stringsAsFactors = FALSE
  )
  
  # Join with the original data to include DNC.visual and biomass
  heatmap_data <- all_combinations %>%
    left_join(km_heat, by = c("midoc.stn", "depth"))
  
  # Set the station order as a factor with our predetermined levels
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = station_order)
  
  # Create simplified station labels (removing "MIDOC" prefix)
  label_midoc_stn <- function(x) {
    sub("MIDOC", "", x)
  }
  
  midoc_labels <- paste0(
    "<span>",
    sapply(station_order, label_midoc_stn),  # Use station_order here
    "</span>"
  )
  names(midoc_labels) <- station_order
  
  # Identify NA values in the dataset
  na_data <- heatmap_data %>% 
    filter(is.na(!!sym(biomass_column)))
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = !!sym(biomass_column))) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey80",
                         guide = guide_colorbar(barheight = 15, barwidth = 2, title.position = "left")) +
    geom_text(data = na_data, aes(label = "\u0336\ "), size = 3, color = "black", na.rm = TRUE) +
    labs(title = NULL, x = "Station", y = "Depth (m)", fill = "Biomass (g/m³)") +
    theme_minimal() +
    theme(
      plot.margin = margin(0,0,0,0),
      axis.title.x = element_text(margin = margin(t = 15), size = 18),
      axis.title.y = element_text(margin = margin(t = 40), size = 18),
      axis.text.x = element_markdown(angle = 90, hjust = 0.5, vjust = 0.65, size = 13, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      panel.background = element_rect(fill = panel_bg_color, color = NA),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 18, angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14)
    ) +
    scale_x_discrete(labels = midoc_labels) +
    scale_y_discrete(limits = rev(levels(heatmap_data$depth))) +
    coord_fixed(ratio = 4)
}

# Usage example:
fish_heatmap_solar <- create_heatmap_solar(km_bm_depth_2, "bm_depth_fish", "Heat Map of Fish Biomass")
cephalopods_heatmap_solar <- create_heatmap_solar(km_bm_depth_2, "bm_depth_ceph", "Heat Map of Cephalopod Biomass")

# Individual plots
print(fish_heatmap_solar)
print(cephalopods_heatmap_solar)

#=============================================================================
# 3. Generating plot function for lunar heat maps 
#=============================================================================
create_heatmap_lunar <- function(data, biomass_column, title, panel_bg_color = "white") {
  # First, create the correct station order based on lunar_fraction
  station_order <- data %>%
    arrange(desc(lunar_fraction)) %>%
    distinct(midoc.stn) %>%
    pull(midoc.stn)
  
  # Reshape the data to long format for the specified biomass column
  km_heat <- data %>%
    select(midoc.stn, depth, !!sym(biomass_column), lunar_fraction) %>%
    filter(!is.na(depth))
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("0-200", "200-400", "400-600", "600-800", "800-1000")
  km_heat$depth <- factor(km_heat$depth, levels = depth_bins)
  
  # Create all combinations of midoc.stn and depth
  all_combinations <- expand.grid(
    midoc.stn = station_order,  # Use our lunar-fraction-based order
    depth = factor(depth_bins, levels = depth_bins),
    stringsAsFactors = FALSE
  )
  
  # Join with the original data to include lunar_fraction and biomass
  heatmap_data <- all_combinations %>%
    left_join(km_heat, by = c("midoc.stn", "depth"))
  
  # Set the station order as a factor with our predetermined levels
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = station_order)
  
  # Create simplified station labels
  label_midoc_stn <- function(x) {
    paste0(
      "<span>",
      sub("MIDOC", "", x),
      "</span>"
    )
  }
  
  midoc_labels <- sapply(station_order, label_midoc_stn)
  names(midoc_labels) <- station_order
  
  # Identify NA values in the dataset
  na_data <- heatmap_data %>% 
    filter(is.na(!!sym(biomass_column)))
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = !!sym(biomass_column))) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey80",
                         guide = guide_colorbar(barheight = 15, barwidth = 2, title.position = "left")) +
    geom_text(data = na_data, aes(label = "\u0336\ "), size = 3, color = "black", na.rm = TRUE) +
    labs(title = NULL, x = "Station", y = "Depth (m)", fill = "Biomass (g/m³)") +
    theme_minimal() +
    theme(
      plot.margin = margin(0,0,0,0),
      axis.title.x = element_text(margin = margin(t = 15), size = 18),
      axis.title.y = element_text(margin = margin(t = 40), size = 18),
      axis.text.x = element_markdown(angle = 90, hjust = 0.5, vjust = 0.65, size = 12, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      panel.background = element_rect(fill = panel_bg_color, color = NA),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 18, angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14)
    ) +
    scale_x_discrete(labels = midoc_labels) +
    scale_y_discrete(limits = rev(levels(heatmap_data$depth))) +
    coord_fixed(ratio = 4)
}

# Usage example:
fish_heatmap_lunar <- create_heatmap_lunar(km_bm_depth_2, "bm_depth_fish", "Heat Map of Fish Biomass")
cephalopods_heatmap_lunar <- create_heatmap_lunar(km_bm_depth_2, "bm_depth_ceph", "Heat Map of Cephalopod Biomass")

# Individual plots
print(fish_heatmap_lunar)
print(cephalopods_heatmap_lunar)





