# Load required libraries
library(dplyr)
library(ggplot2)

# Define the function to create the scatter plot
create_scatter_plot <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", taxa = "fish") {
  # Filter the data for the specified taxa
  data_filtered <- data %>% filter(tax.grp == taxa)
  
  # Aggregate the data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(!!sym(depth_var))) %>%
    summarize(mean_CHLA = mean(!!sym(x_var), na.rm = TRUE),
              mean_biomass = mean(!!sym(y_var), na.rm = TRUE)) %>%
    ungroup()
  
  # Define labels
  x_label <- x_var
  y_label <- expression(paste("Biomass g m"^"-3"))
  title <- paste("Fish Biomass (mean) vs", x_var)
  
  # Create the scatter plot
  p <- ggplot(data_aggregated, aes_string(x = "mean_CHLA", y = "mean_biomass", color = depth_var)) +
    geom_point(size = 5) +
    labs(title = title,
         x = x_label,
         y = y_label,
         color = "Depth Bin") +
    theme_minimal() +
    scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
  
  # Print the plot
  print(p)
}



# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")
km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)


# Create the scatter plot for CHLA
create_scatter_plot(km_df, "CHLA")
create_scatter_plot(km_df, "TSM")
create_scatter_plot(km_df, "CUR")
create_scatter_plot(km_df, "SST")
create_scatter_plot(km_df, "Tmin")
create_scatter_plot(km_df, "O2_min")
create_scatter_plot(km_df, "SML")
create_scatter_plot(km_df, "lunar_fraction")
create_scatter_plot(km_df, "moon_phase")
create_scatter_plot(km_df, "altitude")




# Load required libraries
library(dplyr)
library(ggplot2)

# Define the function to create the scatter plot
create_scatter_plot <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth") {
  # Filter the data for the specified taxa
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  data_filtered <- data[!data$tax.grp %in% exclude_taxa, ]
 
  # Aggregate the data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(!!sym(depth_var))) %>%
    summarize(mean_CHLA = mean(!!sym(x_var), na.rm = TRUE),
              mean_biomass = mean(!!sym(y_var), na.rm = TRUE)) %>%
    ungroup()
  
  # Define labels
  x_label <- x_var
  y_label <- expression(paste("Biomass g m"^"-3"))
  title <- paste("All taxa Biomass - excluding gelatinous (mean) vs", x_var)
  
  # Create the scatter plot
  p <- ggplot(data_aggregated, aes_string(x = "mean_CHLA", y = "mean_biomass", color = depth_var)) +
    geom_point(size = 5) +
    labs(title = title,
         x = x_label,
         y = y_label,
         color = "Depth Bin") +
    theme_minimal() +
    scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
  
  # Print the plot
  print(p)
}

# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")
km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Ensure 'CHLA' is numeric if not already
km_df$CHLA <- as.numeric(km_df$CHLA)

# Create the scatter plot for CHLA
create_scatter_plot(km_df, "CHLA")
create_scatter_plot(km_df, "TSM")
create_scatter_plot(km_df, "CUR")
create_scatter_plot(km_df, "SST")
create_scatter_plot(km_df, "Tmin")
create_scatter_plot(km_df, "O2_min")
create_scatter_plot(km_df, "SML")
create_scatter_plot(km_df, "lunar_fraction")
create_scatter_plot(km_df, "moon_phase")
create_scatter_plot(km_df, "altitude")
