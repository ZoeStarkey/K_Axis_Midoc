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

##TOTAL TAXA BIOMASSS##
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
km$lat_start_orig <- km$lat_start
km <- ll2prj(km, loncol="lon_start_orig", latcol="lat_start_orig")

#making km a dataframe
km_df <- as.data.frame(km)
#making km a sf
km_sf <- st_as_sf(km)

# Define the depth ranges for each codend
depth_bins <- c("0-1000", "800-1000", "600-800", "400-600", "200-400", "0-200")

# Map the codends to depth ranges using the factor function
km_sf$depth <- factor(km$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Define the stations to remove
abandoned_stations <- c("MIDOC02","MIDOC08", "MIDOC10", "MIDOC12", "MIDOC33")

# Remove the specified stations from km_df
km_sf <- km_sf %>%
  filter(!midoc.stn %in% abandoned_stations)


remove_depth <- c("0-1000")

# Remove the specified stations from km_df
km_sf <- km_sf %>%
  filter(!depth %in% "0-1000")


# Select relevant columns from the km dataframe
km_heat <- km_sf[, c("midoc.stn", "depth", "bm_g_m3")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)

# Create the heatmap using ggplot2
ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability


#TOTAL BIOMASS by time of day
# Select relevant columns from the km dataframe
km_heat <- km_sf[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]

# Remove rows where depth is NA
km_heat <- km_heat[!is.na(km_heat$depth), ]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)

# Ensure the depth is treated as a factor to maintain the order
heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)

# Merge the DNC.visual back into the aggregated data
heatmap_data <- merge(heatmap_data, km_heat[, c("midoc.stn", "DNC.visual")], by = "midoc.stn")

# Create a custom order for midoc.stn based on DNC.visual
heatmap_data <- heatmap_data[order(factor(heatmap_data$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]

# Convert midoc.stn to a factor with levels in the desired order
heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))

# Create a named vector of HTML-formatted labels for the x-axis
midoc_labels <- paste0(
  "<span style='color:", 
  ifelse(heatmap_data$DNC.visual == "D", "orange", 
         ifelse(heatmap_data$DNC.visual == "MC", "violet", 
                ifelse(heatmap_data$DNC.visual == "N", "darkblue", "red"))),
  "'>",
  heatmap_data$midoc.stn,
  "</span>"
)

# Ensure unique labels for the x-axis
names(midoc_labels) <- heatmap_data$midoc.stn
unique_midoc_labels <- midoc_labels[!duplicated(names(midoc_labels))]

# Create the heatmap using ggplot2
total_biomass_heatmap <- ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Total Biomass", x = "Midoc Station", y = "Depth", fill = "Total Biomass (g/m³)") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_markdown(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_x_discrete(labels = unique_midoc_labels) # Apply the custom colored labels

# Print the heatmap
print(total_biomass_heatmap)








#TOTAL BIOMASS - Only key taxa groups
# Filter in Fish, Cephalopods, and Krill
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")

# Filter the dataframe to exclude the specified taxa and only include non-NA depths
km_filtered <- km_sf %>%
  filter(!tax.grp %in% exclude_taxa) %>%
  filter(!is.na(depth))
# Select relevant columns from the filtered dataframe
  km_heat <- km_filtered %>%
  dplyr::select(midoc.stn, depth, bm_g_m3)

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
         
# Ensure the depth is treated as a factor to maintain the order
heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)
         
#Removing MIDOC 
label_midoc_stn <- function(x) {
  sub("MIDOC", "", x)
}

# Create the heatmap using ggplot2
total_biomass_heatmap <- ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile() + # Use tiles to represent the heatmap
    scale_fill_viridis_c(option = "rocket", direction = -1) + # Set the gradient colors for the fill
    labs(title = "Heat Map of Total Biomass (Excluding Gelatinous)", x = "Midoc Station", y = "Depth (m) ", fill = "Biomass (g/m³)") + # Add labels and title
    theme_minimal() + # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(margin = margin(t = 10)), # Increase distance between x-axis label and axis
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5) # Center x-axis labels over tick marks
  ) + 
  scale_x_discrete(labels = label_midoc_stn) # Apply the custom labels to x-axis
# Rotate x-axis labels for better readability
         
# Print the heatmap
print(total_biomass_heatmap)


# Specify the path where you want to save the plot
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Season_No_Gelat.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = total_biomass_heatmap, width =10, height = 8, dpi = 300, bg = "white")



#TOTAL BIOMASS - only key groups and time of day
create_heatmap_total <- function(data, title, panel_bg_color = "white") {
  # Filter the dataframe to include only rows where tax.grp matches the specified taxonomic group
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")
  
  # Filter the dataframe to exclude the specified taxa and only include non-NA depths
  km_filtered <- km_sf %>%
    filter(!tax.grp %in% exclude_taxa) %>%
    filter(!is.na(depth))
  # Select relevant columns from the filtered dataframe
  km_heat <- km_filtered[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]
  
  # Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
  heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("0-200", "200-400", "400-600", "600-800", "800-1000")
  heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)
  
  # Ensure all combinations of midoc.stn and depth are represented
  heatmap_data <- heatmap_data %>%
    complete(midoc.stn, depth, fill = list(bm_g_m3 = NA))
  
  # Merge the DNC.visual back into the aggregated data
  heatmap_data <- merge(heatmap_data, unique(km_heat[, c("midoc.stn", "DNC.visual")]), by = "midoc.stn", all.x = TRUE)
  
  # Create a custom order for midoc.stn based on DNC.visual
  heatmap_data <- heatmap_data[order(factor(heatmap_data$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]
  
  # Convert midoc.stn to a factor with levels in the desired order
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))
  
  # Create a named vector of HTML-formatted labels for the x-axis
  label_midoc_stn <- function(x) {
    sub("MIDOC", "", x)
  }
  
  midoc_labels <- paste0(
    "<span>",
    sapply(levels(heatmap_data$midoc.stn), label_midoc_stn),
    "</span>"
  )
  
  names(midoc_labels) <- levels(heatmap_data$midoc.stn)
  
  
  
  # Identify NA values in the dataset
  na_data <- heatmap_data %>% filter(is.na(bm_g_m3))
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile(color = "white") + # Use tiles to represent the heatmap, set color for tile borders
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey80") + # Set the gradient colors for the fill and color for NA values
    geom_text(data = na_data, aes(label = "\u0336\ "), size = 3, color = "black", na.rm = TRUE) + #
    labs(title = title, x = "Midoc Station", y = "Depth (m)", fill = "Summed Biomass (g/m³)") + # Add labels and title
    theme_minimal() +
    theme(
      axis.title.x = element_text(margin = margin(t = 40), size = 14), # Increase distance between x-axis label and axis
      axis.title.y = element_text(margin = margin(t = 40),size = 14),
      axis.text.x = element_markdown(angle = 90, hjust = 0.5, vjust = 0.65, size = 12, , color = "black"), # Center x-axis labels over tick marks and increase text size
      axis.text.y = element_text(size = 12, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5), # Add tick marks on x-axis
      axis.ticks.length = unit(5, "pt"), # Increase y-axis text size and set color to black
      panel.background = element_rect(fill = panel_bg_color, color = NA), # Set background color for the panel
      panel.grid = element_blank(), # Remove grid lines
      legend.position = "right",
      legend.title = element_text(size = 15), # Increase legend title size
      legend.text = element_text(size = 12)# Position legend to the right
    ) + 
    scale_x_discrete(labels = midoc_labels) +
    scale_y_discrete(limits = rev(levels(heatmap_data$depth))) +# Apply the custom HTML labels
    coord_fixed(ratio = 4 )# Fix the aspect ratio
}


# Usage example


total_biomass_day_heatmap<- create_heatmap_total(km_sf, "Heat Map of total taxa (Excluding Gelatinous)")

print(total_biomass_day_heatmap)

#saving

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Day_No_Gelat.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = total_biomass_heatmap, width =10, height =10, dpi = 300, bg = "white")




  ####CREATING FUNCTION FOR INDIVIDUAL TAXA##########

# Define the function to create the heatmap
create_heatmap <- function(data, tax_group, title, panel_bg_color = "white") {
  # Filter the dataframe to include only rows where tax.grp matches the specified taxonomic group
  km_filtered <- data %>%
    filter(tax.grp == tax_group) %>%
    filter(!is.na(depth))
  
  # Select relevant columns from the filtered dataframe
  km_heat <- km_filtered[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]
  
  # Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
  heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("0-200", "200-400", "400-600", "600-800", "800-1000")
  heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)
  
  # Ensure all combinations of midoc.stn and depth are represented
  heatmap_data <- heatmap_data %>%
    complete(midoc.stn, depth, fill = list(bm_g_m3 = NA))
  
  # Merge the DNC.visual back into the aggregated data
  heatmap_data <- merge(heatmap_data, unique(km_heat[, c("midoc.stn", "DNC.visual")]), by = "midoc.stn", all.x = TRUE)
  
  # Create a custom order for midoc.stn based on DNC.visual
  heatmap_data <- heatmap_data[order(factor(heatmap_data$DNC.visual, levels = c("MC", "D", "NC", "N" ))), ]
  
  # Convert midoc.stn to a factor with levels in the desired order
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))
  
  # Create a named vector of HTML-formatted labels for the x-axis
  label_midoc_stn <- function(x) {
    sub("MIDOC", "", x)
  }
  
  midoc_labels <- paste0(
    "<span>",
    sapply(levels(heatmap_data$midoc.stn), label_midoc_stn),
    "</span>"
  )
  
  names(midoc_labels) <- levels(heatmap_data$midoc.stn)
  
  
  
  # Identify NA values in the dataset
  na_data <- heatmap_data %>% filter(is.na(bm_g_m3))
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile(color = "white") + # Use tiles to represent the heatmap, set color for tile borders
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey80", 
                         guide = guide_colorbar(barheight = 15, barwidth = 2, title.position = "left")) + # Set the gradient colors for the fill and color for NA values
    geom_text(data = na_data, aes(label = "\u0336\ "), size = 3, color = "black", na.rm = TRUE) + #
    labs(title = NULL, x = "Station", y = "Depth (m)", fill = "Biomass (g/m³)") + # Add labels and title
    theme_minimal() +
    theme(
      plot.margin = margin(0,0,0,0),
      axis.title.x = element_text(margin = margin(t = 15), size = 18), # Increase distance between x-axis label and axis
      axis.title.y = element_text(margin = margin(t = 40),size = 18),
      axis.text.x = element_markdown(angle = 90, hjust = 0.5, vjust = 0.65, size = 13, , color = "black"), # Center x-axis labels over tick marks and increase text size
      axis.text.y = element_text(size = 15, color = "black"),
     # axis.ticks.x = element_line(linewidth = 0.5), # Add tick marks on x-axis
     # axis.ticks.length = unit(5, "pt"), # Increase y-axis text size and set color to black
      panel.background = element_rect(fill = panel_bg_color, color = NA), # Set background color for the panel
      panel.grid = element_blank(), # Remove grid lines
      legend.position = "right",
      legend.title = element_text(size = 18,angle = 90, hjust = 0.5), # Increase legend title size
      legend.text = element_text(size = 14),
     # legend,title.align =0.5 # Position legend to the right
    ) + 
    scale_x_discrete(labels = midoc_labels) +
    scale_y_discrete(limits = rev(levels(heatmap_data$depth))) +# Apply the custom HTML labels
    coord_fixed(ratio = 4 )# Fix the aspect ratio
}


# Usage example:
fish_heatmap <- create_heatmap(km_sf, "fish", "Heat Map of Fish Biomass")
cephalopods_heatmap <- create_heatmap(km_sf, "cephalopods", "Heat Map of Cephalopod Biomass")
cnidarians_heatmap <- create_heatmap(km_sf, "cnidarians", "Heat Map of Cnidarian Biomass")
salps_heatmap <- create_heatmap(km_sf, "salps", "Heat Map of Salp Biomass")
krill_heatmap <- create_heatmap(km_sf, "krill", "Heat Map of Krill Biomass")

#Individual plots
print(fish_heatmap)
print(cephalopods_heatmap)
print(cnidarians_heatmap)
print(salps_heatmap)
print(krill_heatmap)

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Day_Fish.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = fish_heatmap, width =10, height =, dpi = 300, bg = "white")

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Day_Cephalopods.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = cephalopods_heatmap, width =10, height =, dpi = 300, bg = "white")




#Combined plots 
library(patchwork)
#combined_heatmap <- (fish_heatmap | cephalopods_heatmap)
combined_heatmap <-   (fish_heatmap | cephalopods_heatmap)/ (fish_heatmap_lunar | ceph_heatmap_lunar)
# Print the combined plot
print(combined_heatmap)


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Day_Lunar_Fish_Cephalopods_Total.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = combined_heatmap, width =20, height =10, dpi = 500, bg = "white")


max(km$bm_g_m3, na.rm = TRUE)
min(km$bm_g_m3, na.rm = TRUE)


class(fish_heatmap)
class(cephalopods_heatmap)

#CREATING HEAT MAPS FOR LUNAR PHASES 



create_lunar_heatmap <- function(data, taxon_column, title, panel_bg_color = "white") {
  # Ensure the depth is treated as a factor to maintain the order
  data$depth <- factor(data$depth, levels = unique(data$depth))
  
  # Select relevant columns and rename the taxon column to a standard name
  heatmap_data <- data %>%
    dplyr::select(midoc.stn, depth, lunar_fraction, !!sym(taxon_column)) %>%
    rename(biomass = !!sym(taxon_column))
  
  # Ensure all combinations of midoc.stn and depth are represented
  heatmap_data <- heatmap_data %>%
    complete(midoc.stn, depth, fill = list(biomass = NA))
  
  # Create a custom order for midoc.stn based on lunar_fraction (descending order)
  heatmap_data <- heatmap_data %>%
    arrange(desc(lunar_fraction))
  
  # Convert midoc.stn to a factor with levels in the desired order
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))
  
  # Create a named vector of HTML-formatted labels for the x-axis
  label_midoc_stn <- function(x) {
    lunar_frac <- heatmap_data$lunar_fraction[heatmap_data$midoc.stn == x][1]
    paste0(sub("MIDOC", "", x))
    #"<br>(", round(lunar_frac, 2), ")")
  }
  
  midoc_labels <- sapply(levels(heatmap_data$midoc.stn), label_midoc_stn)
  names(midoc_labels) <- levels(heatmap_data$midoc.stn)
  
  # Identify NA values in the dataset
  na_data <- heatmap_data %>% filter(is.na(biomass))
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = biomass)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey80",
                         guide = guide_colorbar(barheight = 15, barwidth = 2, title.position = "left")) +
    geom_text(data = na_data, aes(label = "\u0336\ "), size = 3, color = "black", na.rm = TRUE) +
    labs(title = NULL, x = "Station", y = "Depth (m)", fill = "Biomass (g/m³)") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(margin = margin(t = 15), size = 18),
      axis.title.y = element_text(margin = margin(t = 40 ), size = 18),
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

all_taxa_heatmap_lunar <- create_lunar_heatmap(km_bm_depth, "bm_depth_all_taxa", "Heat Map of All Taxa Biomass - Lunar Fraction")
all_taxa_heatmap
fish_heatmap_lunar <- create_lunar_heatmap(km_bm_depth, "bm_depth_fish", "Heat Map of Fish Biomass - Lunar Fraction")
ceph_heatmap_lunar <- create_lunar_heatmap(km_bm_depth, "bm_depth_ceph", "Heat Map of Cephalopod Biomass - Lunar Fraction")



#saving for all taxa 
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Lunar_All_Taxa.png"
full_output_path <- file.path(output_directory, output_filename)
ggplot2::ggsave(filename = full_output_path, plot = all_taxa_heatmap_lunar, width =10, height =, dpi = 300, bg = "white")


#Saving for fish 
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Lunar_Fish.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = fish_heatmap_lunar, width =10, height =, dpi = 300, bg = "white")

#Saving for cephalopds
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Lunar_Cephalopods.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = ceph_heatmap_lunar, width =10, height =, dpi = 300, bg = "white")








#######BAR PLOTS##########
#fish day 
plot_data <- km_bm_sum %>%
  group_by(midoc.stn, DNC.visual) %>%
  summarise(total_fish = sum(bm_sum_fish, na.rm = TRUE)) %>%
  ungroup()

dnc_order <- c("MC", "D", "NC", "N")

bar_plot_fish_day <- ggplot(plot_data, aes(x = reorder(midoc.stn, as.numeric(factor(DNC.visual, levels = dnc_order))), 
                      y = total_fish, 
                      fill = factor(DNC.visual, levels = dnc_order))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("MC" = "pink", "D" = "#FFC000", "NC" = "#A52A2A", "N" = "darkblue"), guide = "none") +
  labs(x = NULL,
       y = expression(paste("Biomass (g m"^-3, ")")),
       fill = "Time of Day" ) +
  theme_minimal() +
  theme(axis.text.y= element_text( hjust = 1, size = 12, color = "black" ),
        plot.margin = margin(0,0,0,0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank ()) + 
  theme(legend.position = "none") +
  coord_fixed(ratio = 141)



combined_plot_fish_day <- bar_plot_fish_day / fish_heatmap +
  plot_layout(heights = c(1, 4)) 
combined_plot_fish_day

#cephalopods day
plot_data <- km_bm_sum %>%
  group_by(midoc.stn, DNC.visual) %>%
  summarise(total_fish = sum(bm_sum_ceph, na.rm = TRUE)) %>%
  ungroup()

dnc_order <- c("MC", "D", "NC", "N")

bar_plot_ceph_day <- ggplot(plot_data, aes(x = reorder(midoc.stn, as.numeric(factor(DNC.visual, levels = dnc_order))), 
                                           y = total_fish, 
                                           fill = factor(DNC.visual, levels = dnc_order))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("MC" = "pink", "D" = "#FFC000", "NC" = "#A52A2A", "N" = "darkblue"), guide = "none") +
  scale_y_continuous(
    breaks = c(0.000, 0.001, 0.002)) +
  labs(x = NULL,
       y = expression(paste("Biomass (g m"^-3, ")")),
       fill = "Time of Day" ) +
  theme_minimal() +
  theme(axis.text.y= element_text( hjust = 1, size = 12, color = "black" ),
        plot.margin = margin(0,0,0,0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank ()) + 
  theme(legend.position = "none") +
  coord_fixed(ratio =2422)


bar_plot_ceph_day


combined_plot_ceph_day <- bar_plot_ceph_day  / cephalopods_heatmap +
  plot_layout(heights = c(1, 4)) 

combined_plot_ceph_day 




###FISH LUANR BAR PLOT
km_bm_sum_lunar <- km_bm_sum_lunar %>%
  arrange(desc(lunar_fraction))

# Convert midoc.stn to a factor with levels in the desired order
km_bm_sum_lunar$midoc.stn <- factor(km_bm_sum_lunar$midoc.stn, levels = km_bm_sum_lunar$midoc.stn)

# Create the bar plot
bar_plot_lunar_fish <- ggplot(km_bm_sum_lunar, aes(x = midoc.stn, y = bm_sum_fish, fill = lunar_fraction)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_gradient(low = "black", high = "lightgrey") +
  labs(x = NULL,
       y = expression(paste("Biomass (g ", m^-3, ")"))) +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size = 12, color = "black"),
        plot.margin = margin(0,0,0,0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(legend.position = "none") +
  coord_fixed(ratio = 141)


bar_plot_lunar_fish

combined_plot_fish_lunar <- bar_plot_lunar_fish / fish_heatmap_lunar +
  plot_layout(heights = c(1, 4)) 
combined_plot_fish_lunar


#CEPH LUNAR BAR PLOT 
bar_plot_lunar_ceph <- ggplot(km_bm_sum_lunar, aes(x = midoc.stn, y = bm_sum_ceph, fill = lunar_fraction)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_gradient(low = "black", high = "lightgrey") +
  scale_y_continuous(
    breaks = c(0.000, 0.001, 0.002)) +
  labs( x = NULL,
        y = expression(paste("Biomass (g ", m^-3, ")"))) +
  theme_minimal() +
  theme(axis.text.y= element_text( hjust = 1, size = 12, color = "black" ),
        plot.margin = margin(0,0,0,0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank ()) + 
  theme(legend.position = "none") +
  coord_fixed(ratio = 2422)

bar_plot_lunar_ceph

combined_plot_ceph_lunar <- bar_plot_lunar_ceph / ceph_heatmap_lunar +
  plot_layout(heights = c(1, 4)) 
combined_plot_ceph_lunar



library(patchwork)
#combined_heatmap <- (fish_heatmap | cephalopods_heatmap)
combined_heatmap_bar <-   (combined_plot_fish_day | combined_plot_ceph_day)/ (combined_plot_fish_lunar | combined_plot_ceph_lunar)
# Print the combined plot
print(combined_heatmap_bar)


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_BAR_Fish_Ceph_solar_lunar.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = combined_heatmap_bar, width =21, height =13, dpi = 500, bg = "white")


combined_heatmap_solar <-   (combined_plot_fish_day | combined_plot_ceph_day)


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_BAR_Fish_Ceph_solar.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = combined_heatmap_solar, width =25, height =8, dpi = 500, bg = "white")
  

combined_heatmap_lunar <- (combined_plot_fish_lunar | combined_plot_ceph_lunar)

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_BAR_Fish_Ceph_lunar.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = combined_heatmap_lunar, width =25, height =8, dpi = 500, bg = "white")



###HEAT MAPS BY SOLAR ANGLE
library(suncalc)
km_sf <- km_sf %>%
  mutate(start_time = as.POSIXct(start_time, tz="UTC"))

# Add solar position columns
km_sf <- km_sf %>%
  rowwise() %>%
  mutate(
    solar_position = list(getSunlightPosition(date = start_time, lat = lat_start, lon = lon_start)),
    azimuth = solar_position$azimuth,
    altitude = solar_position$altitude  * 180 / pi   # Convert from radians to degrees if needed
  ) %>%
  select(-solar_position) 
#create heatmap 
create_heatmap_solar <- function(data, tax_group, title, panel_bg_color = "white") {
  # Filter the dataframe to include only rows where tax.grp matches the specified taxonomic group
  km_filtered <- data %>%
    filter(tax.grp == tax_group) %>%
    filter(!is.na(depth))
  
  # Select relevant columns from the filtered dataframe
  km_heat <- km_filtered[, c("midoc.stn", "depth", "bm_g_m3", "altitude")]
  
  # Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
  heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("0-200", "200-400", "400-600", "600-800", "800-1000")
  heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)
  
  # Ensure all combinations of midoc.stn and depth are represented
  heatmap_data <- heatmap_data %>%
    complete(midoc.stn, depth, fill = list(bm_g_m3 = NA))
  
  # Merge the altitude back into the aggregated data
  heatmap_data <- merge(heatmap_data, unique(km_heat[, c("midoc.stn", "altitude")]), by = "midoc.stn", all.x = TRUE)
  
  # Create a custom order for midoc.stn based on altitude (ascending order)
  heatmap_data <- heatmap_data[order(heatmap_data$altitude), ]
  
  # Convert midoc.stn to a factor with levels in the desired order
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))
  
  # Create a named vector of HTML-formatted labels for the x-axis
  label_midoc_stn <- function(x) {
    sub("MIDOC", "", x)
  }
  
  midoc_labels <- paste0(
    sapply(levels(heatmap_data$midoc.stn), label_midoc_stn),
    " (",
    round(heatmap_data$altitude[!duplicated(heatmap_data$midoc.stn)], 1),
    "°)"
  )
  
  names(midoc_labels) <- levels(heatmap_data$midoc.stn)
  
  # Identify NA values in the dataset
  na_data <- heatmap_data %>% filter(is.na(bm_g_m3))
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey80", 
                         guide = guide_colorbar(barheight = 15, barwidth = 2, title.position = "left")) +
    geom_text(data = na_data, aes(label = "\u0336\ "), size = 3, color = "black", na.rm = TRUE) +
    labs(title = NULL, x = "Station (with Solar Angle)", y = "Depth (m)", fill = "Biomass (g/m³)") +
    theme_minimal() +
    theme(
      plot.margin = margin(0,0,0,0),
      axis.title.x = element_text(margin = margin(t = 15), size = 18),
      axis.title.y = element_text(margin = margin(t = 40),size = 18),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      panel.background = element_rect(fill = panel_bg_color, color = NA),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 18,angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14),
    ) + 
    scale_x_discrete(labels = midoc_labels) +
    scale_y_discrete(limits = rev(levels(heatmap_data$depth))) +
    coord_fixed(ratio = 4)
}
# Usage example:
fish_heatmap_solar_fish <- create_heatmap_solar(km_sf, "fish", "Heat Map of Fish Biomass - Solar Angle")
fish_heatmap_solar_fish 
