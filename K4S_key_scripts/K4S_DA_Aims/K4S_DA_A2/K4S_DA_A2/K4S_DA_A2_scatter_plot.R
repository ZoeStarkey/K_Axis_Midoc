library(ggplot2)
library(readr)
library(dplyr)
library(ggtext)


####SETTING UP#####

#setting up directory 
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

#Setting up km 
km <- readRDS("../derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
tmp <- read_csv("../source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("../source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)
tmp <- readRDS("../derived data/codend_taxa_biomass.rds")
km <- inner_join(km, tmp); rm(tmp)
file_path <- "../derived data/midoc_stations_envdata.rda" #adding zone
md <- readRDS(file_path)

#putting km as a dataframe
km_df <- as.data.frame(km)


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
