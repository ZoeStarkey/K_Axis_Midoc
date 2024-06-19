library(ggplot2)
library(reshape2)
library(dplyr)
library(ggtext)

##TOTAL TAXA BIOMASSS##

# Define the depth ranges for each codend
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

# Map the codends to depth ranges using the factor function
km_sf$depth <- factor(km$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

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
km_filtered <- km_sf %>%
  filter(tax.grp %in% c("fish", "cephalopods", "krill")) %>%
  filter(!is.na(depth))
# Select relevant columns from the filtered dataframe
  km_heat <- km_filtered %>%
  select(midoc.stn, depth, bm_g_m3)

# Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
         
# Ensure the depth is treated as a factor to maintain the order
heatmap_data$depth <- factor(heatmap_data$depth, levels = depth_bins)
         
# Create the heatmap using ggplot2
total_biomass_heatmap <- ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile() + # Use tiles to represent the heatmap
    scale_fill_viridis_c(option = "rocket", direction = -1) + # Set the gradient colors for the fill
    labs(title = "Heat Map of Total Biomass (Fish, Cephalopods, Krill)", x = "Midoc Station", y = "Depth", fill = "Total Biomass (g/m³)") + # Add labels and title
    theme_minimal() + # Use a minimal theme for the plot
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability
         
# Print the heatmap
print(total_biomass_heatmap)
         

#TOTAL BIOMASS - only key groups and time of day
# Filter in Fish, Cephalopods, and Krill
# Filter in Fish, Cephalopods, and Krill
km_filtered <- km_sf %>%
  filter(tax.grp %in% c("fish", "cephalopods", "krill")) %>%
  filter(!is.na(depth))
# Select relevant columns from the filtered dataframe
km_heat <- km_filtered %>%
  select(midoc.stn, depth, bm_g_m3, DNC.visual)

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
  scale_fill_viridis_c(option = "rocket", direction = -1) + # Set the gradient colors for the fill
  labs(title = "Heat Map of Total Biomass - Excluding Salps and Cnidarians", x = "Midoc Station", y = "Depth", fill = "Total Biomass (g/m³)") + # Add labels and title
  theme_minimal() + # Use a minimal theme for the plot
  theme(axis.text.x = element_markdown(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_x_discrete(labels = unique_midoc_labels) # Apply the custom colored labels

# Print the heatmap
print(total_biomass_heatmap)






####CREATING FUNCTION FOR INDIVIDUAL TAXA##########

# Define the function to create the heatmap
create_heatmap <- function(data, tax_group, title) {
  # Filter the dataframe to include only rows where tax.grp matches the specified taxonomic group
  km_filtered <- subset(km_sf, tax.grp == tax_group)
  
  # Select relevant columns from the filtered dataframe
  km_heat <- km_filtered[, c("midoc.stn", "depth", "bm_g_m3", "DNC.visual")]
  
  # Aggregate the biomass data (bm_g_m3) by midoc.stn and depth, summing the biomass
  heatmap_data <- aggregate(bm_g_m3 ~ midoc.stn + depth, data = km_heat, sum)
  
  # Ensure the depth is treated as a factor to maintain the order
  depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")
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
  ggplot(heatmap_data, aes(x = midoc.stn, y = depth, fill = bm_g_m3)) +
    geom_tile() + # Use tiles to represent the heatmap
    scale_fill_viridis_c(option = "rocket", direction = -1) + # Set the gradient colors for the fill
    labs(title = title, x = "Midoc Station", y = "Depth") + # Add labels and title
    theme_minimal() + # Use a minimal theme for the plot
    theme(axis.text.x = element_markdown(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
    scale_x_discrete(labels = unique_midoc_labels) # Apply the custom colored labels
}


# Usage example:
fish_heatmap <- create_heatmap(km, "fish", "Heat Map of Fish Biomass")
cephalopods_heatmap <- create_heatmap(km, "cephalopods", "Heat Map of Cephalopod Biomass")
cnidarians_heatmap <- create_heatmap(km, "cnidarians", "Heat Map of Cnidarian Biomass")
salps_heatmap <- create_heatmap(km, "salps", "Heat Map of Salp Biomass")
krill_heatmap <- create_heatmap(km, "krill", "Heat Map of Krill Biomass")

#Individual plots
print(fish_heatmap)
print(cephalopods_heatmap)
print(cnidarians_heatmap)
print(salps_heatmap)
print(krill_heatmap)

#Combined plots 
combined_heatmap <- (fish_heatmap + cephalopods_heatmap) / 
  (cnidarians_heatmap + salps_heatmap + krill_heatmap) +
  plot_layout(guides = 'collect') 

# Print the combined plot
print(combined_heatmap)






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

