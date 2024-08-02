library(ggplot2)
library(reshape2)
library(dplyr)
library(ggtext)

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
    labs(title = "Heat Map of Total Biomass (Excluding Gelatinous)", x = "Midoc Station", y = "Depth (m) ", fill = "Total Biomass (g/m³)") + # Add labels and title
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
# Filter in Fish, Cephalopods, and Krill
# Filter in Fish, Cephalopods, and Krill
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")

# Filter the dataframe to exclude the specified taxa and only include non-NA depths
km_filtered <- km_sf %>%
  filter(!tax.grp %in% exclude_taxa) %>%
  filter(!is.na(depth))
# Select relevant columns from the filtered dataframe
km_heat <- km_filtered %>%
  dplyr::select(midoc.stn, depth, bm_g_m3, DNC.visual)

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)

# Ensure the depth is treated as a factor to maintain the order
heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)

# Merge the DNC.visual and time_of_day back into the aggregated data
heatmap_data <- merge(heatmap_data, km_heat[, c("midoc.stn", "DNC.visual")], by = "midoc.stn")

# Create a custom order for midoc.stn based on DNC.visual
heatmap_data <- heatmap_data[order(factor(heatmap_data$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]

# Convert midoc.stn to a factor with levels in the desired order
heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))


#Removing MIDOC from lable
label_midoc_stn <- function(x) {
  sub("MIDOC", "", x)
}

#Create a named vector of HTML-formatted labels for the x-axis
midoc_labels <- paste0(
  "<span>",
  sapply(heatmap_data$midoc.stn, label_midoc_stn),
  "</span>"
)

# midoc_labels <- paste0(
#   "<span style='color:",
#   ifelse(heatmap_data$DNC.visual == "D", "orange", #orange
#          ifelse(heatmap_data$DNC.visual == "MC", "violet", #violet
#                 ifelse(heatmap_data$DNC.visual == "N", "darkblue", "red"))), #darkblue, red
#   "'>",
#   sapply(heatmap_data$midoc.stn, label_midoc_stn),
#   "</span>"
# )

# Ensure unique labels for the x-axis
names(midoc_labels) <- heatmap_data$midoc.stn
unique_midoc_labels <- midoc_labels[!duplicated(names(midoc_labels))]

# Create the heatmap using ggplot2
total_biomass_heatmap <- ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_viridis_c(option = "rocket", direction = -1) + # Set the gradient colors for the fill
  labs(title = "Heat Map of Total Biomass (Excluding Salps and Cnidarians)", x = "Midoc Station", y = "Depth (m) ", fill = "Summed Biomass (g/m³)") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(margin = margin(t = 40), size = 15), # Increase distance between x-axis label and axis
    axis.text.x = element_markdown(angle = 90, hjust = 0.5, vjust = 0.65, size = 12, color = "black"), # Center x-axis labels over tick marks and increase text size
    axis.text.y = element_text(size = 10, color = "black") # Increase y-axis text size and set color to black
      # Center x-axis labels over tick marks
  ) +  # Rotate x-axis labels for better readabilit
  scale_x_discrete(labels = unique_midoc_labels) 
  # Apply the custom colored labels

# Print the heatmap
print(total_biomass_heatmap)

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Day_No_Gelat.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = total_biomass_heatmap, width =10, height =, dpi = 300, bg = "white")




  ####CREATING FUNCTION FOR INDIVIDUAL TAXA##########


# Define the function to create the heatmap
create_heatmap <- function(data, tax_group, title) {
  # Filter the dataframe to include only rows where tax.grp matches the specified taxonomic group
 # km_filtered <- subset(km_sf, tax.grp == tax_group)
  
  include_taxa <- c(tax.grp = tax_group)
  
  
  km_filtered <- km_sf %>%
  filter(tax.grp %in% include_taxa) %>%
  filter(!is.na(depth))
  
  # Select relevant columns from the filtered dataframe
  km_heat <- km_filtered[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]
  
  # Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
  heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("800-1000", "600-800", "400-600", "200-400", "0-200")
  heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)
  
  # Merge the DNC.visual back into the aggregated data
  heatmap_data <- merge(heatmap_data, km_heat[, c("midoc.stn", "DNC.visual")], by = "midoc.stn")
  
  # Create a custom order for midoc.stn based on DNC.visual
  heatmap_data <- heatmap_data[order(factor(heatmap_data$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]
  
  # Convert midoc.stn to a factor with levels in the desired order
  heatmap_data$midoc.stn <- factor(heatmap_data$midoc.stn, levels = unique(heatmap_data$midoc.stn))
  
  # Create a named vector of HTML-formatted labels for the x-axis
  label_midoc_stn <- function(x) {
    sub("MIDOC", "", x)
  }
  
  #Create a named vector of HTML-formatted labels for the x-axis
  midoc_labels <- paste0(
    "<span>",
    sapply(heatmap_data$midoc.stn, label_midoc_stn),
    "</span>"
  )
  
  
  # midoc_labels <- paste0(
  #   "<span style='color:", 
  #   ifelse(heatmap_data$DNC.visual == "D", "orange", 
  #          ifelse(heatmap_data$DNC.visual == "MC", "violet", 
  #                 ifelse(heatmap_data$DNC.visual == "N", "darkblue", "red"))),
  #   "'>",
  #   heatmap_data$midoc.stn,
  #   "</span>"
  # )
  # 
  # Ensure unique labels for the x-axis
  names(midoc_labels) <- heatmap_data$midoc.stn
  unique_midoc_labels <- midoc_labels[!duplicated(names(midoc_labels))]
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile(color = "white", na.rm = FALSE) + # Use tiles to represent the heatmap
    scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "yellow") + # Set the gradient colors for the fill
    labs(title = title, x = "Midoc Station", y = "Depth (m)", , fill = "Biomass (g/m³)") + # Add labels and title
    theme(
      axis.title.x = element_text(margin = margin(t = 40), size = 15), # Increase distance between x-axis label and axis
      axis.text.x = element_markdown(angle = 90, hjust = 0.5, vjust = 0.65, size = 12, color = "black"), # Center x-axis labels over tick marks and increase text size
      axis.text.y = element_text(size = 10, color = "black"),
      panel.background = element_rect(fill = "blue"), # Remove grey background
    panel.grid = element_blank(),
      # Center x-axis labels over tick marks
    ) +  # Rotate x-axis labels for better readabilit
    scale_x_discrete(labels = unique_midoc_labels) # Apply the custom colored labels
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
combined_heatmap <- (fish_heatmap | cephalopods_heatmap)

# Print the combined plot
print(combined_heatmap)


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A2_HM_Day_Fish_Cephalopods.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = combined_heatmap, width =20, height =, dpi = 300, bg = "white")


max(km$bm_g_m3, na.rm = TRUE)
min(km$bm_g_m3, na.rm = TRUE)


















##TAXA SPECIFIC BIOMASS##

#FISH#


# Filter the dataframe to include only rows where tax.grp is "fish"
km_fish <- subset(km, tax.grp == "fish")

# Select relevant columns from the filtered dataframe
km_heat_fish <- km_fish[, c("midoc.stn", "depth", "bm_g_m3")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_fish <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_fish, sum)

# Create the heatmap using ggplot2
ggplot(heatmap_data_fish, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Fish Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability


#CEPHALOPODS#

# Filter the dataframe to include only rows where tax.grp is "cephalopods"
km_cephalopods <- subset(km, tax.grp == "cephalopods")

# Select relevant columns from the filtered dataframe
km_heat_cephalopods <- km_cephalopods[, c("midoc.stn", "depth", "bm_g_m3")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_cephalopods <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_cephalopods, sum)

# Create the heatmap using ggplot2
ggplot(heatmap_data_cephalopods, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Cephalopod Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability

#CNIDARIANS#
# Filter the dataframe to include only rows where tax.grp is "cnidarians"
km_cnidarians <- subset(km, tax.grp == "cnidarians")

# Select relevant columns from the filtered dataframe
km_heat_cnidarians <- km_cnidarians[, c("midoc.stn", "depth", "bm_g_m3")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_cnidarians <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_cnidarians, sum)

# Create the heatmap using ggplot2
ggplot(heatmap_data_cnidarians, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Cnidarian Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability

#KRILL#
# Filter the dataframe to include only rows where tax.grp is "krill"
km_krill <- subset(km, tax.grp == "krill")

# Select relevant columns from the filtered dataframe
km_heat_krill <- km_krill[, c("midoc.stn", "depth", "bm_g_m3")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_krill <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_krill, sum)

# Create the heatmap using ggplot2
ggplot(heatmap_data_krill, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Krill Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability

#SALPS#
# Filter the dataframe to include only rows where tax.grp is "salps"
km_salps <- subset(km, tax.grp == "salps")

# Select relevant columns from the filtered dataframe
km_heat_salps <- km_salps[, c("midoc.stn", "depth", "bm_g_m3")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_salps <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_salps, sum)

# Create the heatmap using ggplot2
ggplot(heatmap_data_salps, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Salps Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability



#TESTING OUT START TIME 

# Convert start_time to POSIXct for easier manipulation
km$start_time <- as.POSIXct(km$start_time, format="%Y-%m-%d %H:%M:%S")

# Filter the dataframe to include only rows where tax.grp is "fish"
km_fish <- subset(km, tax.grp == "fish")

# Calculate the average start time for each midoc.stn and depth
average_times <- km_fish %>%
  group_by(midoc.stn, depth) %>%
  summarize(avg_start_time = mean(as.numeric(start_time)),
            bm_g_m3 = sum(bm_g_m3, na.rm = TRUE)) %>%
  ungroup()

# Convert avg_start_time back to POSIXct for better readability if needed
average_times$avg_start_time <- as.POSIXct(average_times$avg_start_time, origin="1970-01-01")

# Create the heatmap using ggplot2
ggplot(average_times, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Fish Biomass by Midoc Station, Depth, and Start Time", 
       x = "Midoc Station", 
       y = "Depth",
       fill = "Biomass (g/m3)") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
  facet_wrap(~ avg_start_time) # Facet by the average start time



#FISH BIOMASS by time of day 


#Testing again:
# Load necessary libraries
library(ggplot2)
library(ggtext)



# Filter the dataframe to include only rows where tax.grp is "fish"
km_fish <- subset(km, tax.grp == "fish")

# Select relevant columns from the filtered dataframe
km_heat_fish <- km_fish[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_fish <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_fish, sum)

# Ensure the depth is treated as a factor to maintain the order
depth_bins <- c("0-1000m", "1000-800m", "800-600m", "600-400m", "400-200m", "200-0m")
heatmap_data_fish$depth <- factor(heatmap_data_fish$depth, levels = depth_bins)

# Merge the DNC.visual back into the aggregated data
heatmap_data_fish <- merge(heatmap_data_fish, km_heat_fish[, c("midoc.stn", "DNC.visual")], by = "midoc.stn")

# Create a custom order for midoc.stn based on DNC.visual
heatmap_data_fish <- heatmap_data_fish[order(factor(heatmap_data_fish$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]

# Convert midoc.stn to a factor with levels in the desired order
heatmap_data_fish$midoc.stn <- factor(heatmap_data_fish$midoc.stn, levels = unique(heatmap_data_fish$midoc.stn))

# Create a named vector of HTML-formatted labels for the x-axis
midoc_labels <- paste0(
  "<span style='color:", 
  ifelse(heatmap_data_fish$DNC.visual == "D", "orange", 
         ifelse(heatmap_data_fish$DNC.visual == "MC", "violet", 
                ifelse(heatmap_data_fish$DNC.visual == "N", "darkblue", "red"))),
  "'>",
  heatmap_data_fish$midoc.stn,
  "</span>"
)

# Ensure unique labels for the x-axis
names(midoc_labels) <- heatmap_data_fish$midoc.stn
unique_midoc_labels <- midoc_labels[!duplicated(names(midoc_labels))]

# Create the heatmap using ggplot2
ggplot(heatmap_data_fish, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "white", high = "blue") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Fish Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_markdown(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_x_discrete(labels = unique_midoc_labels) # Apply the custom colored labels


#cephalopods

# Filter the dataframe to include only rows where tax.grp is "cephalopod"
km_cephalopod <- subset(km, tax.grp == "cephalopods")

# Select relevant columns from the filtered dataframe
km_heat_cephalopod <- km_cephalopod[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data_cephalopod <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat_cephalopod, sum)

# Ensure the depth is treated as a factor to maintain the order
depth_bins <- c("0-1000m", "1000-800m", "800-600m", "600-400m", "400-200m", "200-0m")
heatmap_data_cephalopod$depth <- factor(heatmap_data_cephalopod$depth, levels = depth_bins)

# Merge the DNC.visual back into the aggregated data
heatmap_data_cephalopod <- merge(heatmap_data_cephalopod, km_heat_cephalopod[, c("midoc.stn", "DNC.visual")], by = "midoc.stn")

# Create a custom order for midoc.stn based on DNC.visual
heatmap_data_cephalopod <- heatmap_data_cephalopod[order(factor(heatmap_data_cephalopod$DNC.visual, levels = c("NC", "D", "MC", "N"))), ]

# Convert midoc.stn to a factor with levels in the desired order
heatmap_data_cephalopod$midoc.stn <- factor(heatmap_data_cephalopod$midoc.stn, levels = unique(heatmap_data_cephalopod$midoc.stn))

# Create a named vector of HTML-formatted labels for the x-axis
midoc_labels <- paste0(
  "<span style='color:", 
  ifelse(heatmap_data_cephalopod$DNC.visual == "D", "orange", 
         ifelse(heatmap_data_cephalopod$DNC.visual == "MC", "violet", 
                ifelse(heatmap_data_cephalopod$DNC.visual == "N", "darkblue", "red"))),
  "'>",
  heatmap_data_cephalopod$midoc.stn,
  "</span>"
)

# Ensure unique labels for the x-axis
names(midoc_labels) <- heatmap_data_cephalopod$midoc.stn
unique_midoc_labels <- midoc_labels[!duplicated(names(midoc_labels))]

# Create the heatmap using ggplot2
ggplot(heatmap_data_cephalopod, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
  geom_tile() + # Use tiles to represent the heatmap
  scale_fill_gradient(low = "yellow", high = "red") + # Set the gradient colors for the fill
  labs(title = "Heat Map of Cephalopod Biomass by Midoc Station and Depth", x = "Midoc Station", y = "Depth") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_markdown(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_x_discrete(labels = unique_midoc_labels) # Apply the custom colored labels

