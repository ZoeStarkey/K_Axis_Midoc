library(ggplot2)
library(dplyr)
library(rlang)


usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)


load("km_df_environmental_variables.Rda")

km_df_filtered <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df_filtered <-  km_df_filtered[!km_df_filtered$tax.grp %in% exclude_taxa, ]



include_taxa <- c("fish")
km_df_filtered <- km_df_filtered[km_df_filtered$tax.grp %in% include_taxa, ]





#TSM - unbinned 
# Remove rows with NA values in TSM
km_df_filtered <- km_df_filtered %>%
  filter(!is.na(TSM))

# Plot the data with formatted x-axis labels
ggplot(km_df_filtered, aes(x = as.factor(TSM), y = bm_g_m3, fill = depth)) +
  geom_boxplot() +
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
  theme_bw() +
  xlab("TSM") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and TSM") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = function(x) sprintf("%.1f", as.numeric(as.character(x)))) +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


#zoomed in 

ggplot(km_df_filtered, aes(x = as.factor(TSM), y = bm_g_m3, fill = depth)) +
  geom_boxplot() +
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
  theme_bw() +
  xlab("TSM") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and TSM") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = function(x) sprintf("%.1f", as.numeric(as.character(x)))) +
  coord_cartesian(ylim = c(0, 0.02)) +  # Adjust the y-axis range as needed
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

#TSM - Binned 

# Define the number of bins
num_bins <- 5

# Calculate the breaks for TSM
breaks <- seq(min(km_df_filtered$TSM, na.rm = TRUE), max(km_df_filtered$TSM, na.rm = TRUE), length.out = num_bins + 1)

# Ensure the breaks are unique by adding a small epsilon if necessary
epsilon <- 1e-6
breaks <- unique(c(breaks, breaks[length(breaks)] + epsilon))

# Create bins for TSM
km_df_filtered <- km_df_filtered %>%
  mutate(TSM_binned = cut(TSM, breaks = breaks, include.lowest = TRUE, labels = paste(head(breaks, -1), tail(breaks, -1) - epsilon, sep = " - ")))

# Create bins for TSM with formatted labels
km_df_filtered <- km_df_filtered %>%
  mutate(TSM_binned = cut(TSM, breaks = breaks, include.lowest = TRUE, 
                          labels = paste(sprintf("%.2f", head(breaks, -1)), sprintf("%.2f", tail(breaks, -1)), sep = " - ")))



ggplot(km_df_filtered, aes(x = TSM_binned, y = bm_g_m3, fill = depth)) +
  geom_boxplot() +
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
  theme_bw() +
  xlab("TSM") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and TSM") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


#Trying to zoom in 
ggplot(km_df_filtered, aes(x = TSM_binned, y = bm_g_m3, fill = depth)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(width = 0.2, alpha = 0.2) +  # Add jitter for better visibility of points
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
  theme_bw() +
  xlab("TSM") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and TSM") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(ylim = c(0, 0.01)) +  # Adjust the y-axis range as needed

  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )



#BINNED FUNCTION 
# Define the function
create_boxplot <- function(data, env_var, dep_var, num_bins = 5, depth_col = "depth") {
  # Remove NAs from the specified environmental variable
  data <- data %>%
    filter(!is.na(.data[[env_var]]))
  
  # Calculate the breaks for the environmental variable
  breaks <- seq(min(data[[env_var]], na.rm = TRUE), max(data[[env_var]], na.rm = TRUE), length.out = num_bins + 1)
  
  # Ensure the breaks are unique by adding a small epsilon
  epsilon <- 1e-6
  breaks <- unique(c(breaks, breaks[length(breaks)] + epsilon))
  
  # Create bin labels
  labels <- paste(sprintf("%.2f", head(breaks, -1)), sprintf("%.2f", tail(breaks, -1)), sep = " - ")
  
  # Create bins for the environmental variable
  data <- data %>%
    mutate(binned_var = cut(.data[[env_var]], breaks = breaks, include.lowest = TRUE, labels = labels)) %>%
    filter(!is.na(binned_var))
  
  # Create the boxplot
  p <- ggplot(data, aes(x = binned_var, y = .data[[dep_var]], fill = .data[[depth_col]])) +
    geom_boxplot() +
    facet_wrap(as.formula(paste("~ reorder(", depth_col, ", desc(", depth_col, "))")), ncol = 1, scales = "fixed") +
    theme_bw() +
    xlab(env_var) +
    ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
    ggtitle(paste("Boxplot of", dep_var, " fish by Depth Categories and", env_var)) +
    scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(
      strip.text.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 0)
    )
  
  return(p)
}


#Function to saveplot
save_plot <- function(plot, filename) {
  ggsave(filename, plot = plot, path = "~/Desktop", width = 15, height = 10)
}



#Generate each plot
plot_chla <- create_boxplot(km_df_filtered, env_var = "CHLA", dep_var = "bm_g_m3")
plot_tsm <- create_boxplot(km_df_filtered, env_var = "TSM", dep_var = "bm_g_m3")
plot_cur <- create_boxplot(km_df_filtered, env_var = "CUR", dep_var = "bm_g_m3")
plot_sst <- create_boxplot(km_df_filtered, env_var = "SST", dep_var = "bm_g_m3")
plot_tmin <- create_boxplot(km_df_filtered, env_var = "Tmin", dep_var = "bm_g_m3")
plot_o2_min <- create_boxplot(km_df_filtered, env_var = "O2_min", dep_var = "bm_g_m3")
plot_sml <- create_boxplot(km_df_filtered, env_var = "SML", dep_var = "bm_g_m3")
plot_lunar_fraction <- create_boxplot(km_df_filtered, env_var = "lunar_fraction", dep_var = "bm_g_m3")
plot_moon_phase <- create_boxplot(km_df_filtered, env_var = "moon_phase", dep_var = "bm_g_m3")
plot_altitude <- create_boxplot(km_df_filtered, env_var = "altitude", dep_var = "bm_g_m3")

# Save each plot with "no_gelat" filenames
save_plot(plot_chla, "boxplot_chla_no_gelat.png")
save_plot(plot_tsm, "boxplot_tsm_no_gelat.png")
save_plot(plot_cur, "boxplot_cur_no_gelat.png")
save_plot(plot_sst, "boxplot_sst_no_gelat.png")
save_plot(plot_tmin, "boxplot_tmin_no_gelat.png")
save_plot(plot_o2_min, "boxplot_o2_min_no_gelat.png")
save_plot(plot_sml, "boxplot_sml_no_gelat.png")
save_plot(plot_lunar_fraction, "boxplot_lunar_fraction_no_gelat.png")
save_plot(plot_moon_phase, "boxplot_moon_phase_no_gelat.png")
save_plot(plot_altitude, "boxplot_altitude_no_gelat.png")

# 
save_plot(plot_chla, "boxplot_chla_ceph.png")
save_plot(plot_tsm, "boxplot_tsm_ceph.png")
save_plot(plot_cur, "boxplot_cur_ceph.png")
save_plot(plot_sst, "boxplot_sst_ceph.png")
save_plot(plot_tmin, "boxplot_tmin_ceph.png")
save_plot(plot_o2_min, "boxplot_o2_min_ceph.png")
save_plot(plot_sml, "boxplot_sml_ceph.png")
save_plot(plot_lunar_fraction, "boxplot_lunar_fraction_ceph.png")
save_plot(plot_moon_phase, "boxplot_moon_phase_ceph.png")
save_plot(plot_altitude, "boxplot_altitude_ceph.png")

#Save each plot with "fish" filenames
save_plot(plot_chla, "boxplot_chla_fish.png")
save_plot(plot_tsm, "boxplot_tsm_fish.png")
save_plot(plot_cur, "boxplot_cur_fish.png")
save_plot(plot_sst, "boxplot_sst_fish.png")
save_plot(plot_tmin, "boxplot_tmin_fish.png")
save_plot(plot_o2_min, "boxplot_o2_min_fish.png")
save_plot(plot_sml, "boxplot_sml_fish.png")
save_plot(plot_lunar_fraction, "boxplot_lunar_fraction_fish.png")
save_plot(plot_moon_phase, "boxplot_moon_phase_fish.png")
save_plot(plot_altitude, "boxplot_altitude_fish.png")






# COME BACK TO - categorical data 
create_boxplot(km_df_filtered, env_var = "bestley.zone", dep_var = "bm_g_m3")


print(plot)


#BINNED AND ZOOMED 

library(ggplot2)
library(dplyr)
library(rlang)

# Define the function
create_boxplot <- function(data, env_var, dep_var, num_bins = 5, depth_col = "depth", y_limits = NULL) {
  # Remove NAs from the specified environmental variable
  data <- data %>%
    filter(!is.na(.data[[env_var]]))
  
  # Calculate the breaks for the environmental variable
  breaks <- seq(min(data[[env_var]], na.rm = TRUE), max(data[[env_var]], na.rm = TRUE), length.out = num_bins + 1)
  
  # Ensure the breaks are unique by adding a small epsilon if necessary
  epsilon <- 1e-6
  breaks <- unique(c(breaks, breaks[length(breaks)] + epsilon))
  
  # Create bin labels
  labels <- paste(sprintf("%.2f", head(breaks, -1)), sprintf("%.2f", tail(breaks, -1)), sep = " - ")
  
  # Create bins for the environmental variable
  data <- data %>%
    mutate(binned_var = cut(.data[[env_var]], breaks = breaks, include.lowest = TRUE, labels = labels)) %>%
    filter(!is.na(binned_var))
  
  # Create the boxplot
  p <- ggplot(data, aes(x = binned_var, y = .data[[dep_var]], fill = .data[[depth_col]])) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(as.formula(paste("~ reorder(", depth_col, ", desc(", depth_col, "))")), ncol = 1, scales = "fixed") +
    theme_bw() +
    xlab(env_var) +
    ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
    ggtitle(paste("Boxplot of", dep_var, "cephalopods by Depth Categories and", env_var)) +
    scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(
      strip.text.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 0))
    
  
  # Apply y-axis limits if provided
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}

# Example usage
# Assuming km_df_filtered is your dataframe and you want to plot for TSM and bm_g_m3
# and zoom in with y-axis limits between 0 and 0.01
plot_tsm <- create_boxplot(km_df_filtered, env_var = "TSM", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_chla <- create_boxplot(km_df_filtered, env_var = "CHLA", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_cur <- create_boxplot(km_df_filtered, env_var = "CUR", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_sst <- create_boxplot(km_df_filtered, env_var = "SST", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_tmin <- create_boxplot(km_df_filtered, env_var = "Tmin", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_o2_min <- create_boxplot(km_df_filtered, env_var = "O2_min", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_sml <- create_boxplot(km_df_filtered, env_var = "SML", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_lunar_fraction <- create_boxplot(km_df_filtered, env_var = "lunar_fraction", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_moon_phase <- create_boxplot(km_df_filtered, env_var = "moon_phase", dep_var = "bm_g_m3", y_limits = c(0, 0.001))
plot_altitude <- create_boxplot(km_df_filtered, env_var = "altitude", dep_var = "bm_g_m3", y_limits = c(0, 0.001))



# Save each plot with "no_gelat" filenames
save_plot(plot_chla, "boxplot_chla_no_gelat.png")
save_plot(plot_tsm, "boxplot_tsm_no_gelat.png")
save_plot(plot_cur, "boxplot_cur_no_gelat.png")
save_plot(plot_sst, "boxplot_sst_no_gelat.png")
save_plot(plot_tmin, "boxplot_tmin_no_gelat.png")
save_plot(plot_o2_min, "boxplot_o2_min_no_gelat.png")
save_plot(plot_sml, "boxplot_sml_no_gelat.png")
save_plot(plot_lunar_fraction, "boxplot_lunar_fraction_no_gelat.png")
save_plot(plot_moon_phase, "boxplot_moon_phase_no_gelat.png")
save_plot(plot_altitude, "boxplot_altitude_no_gelat.png")

# Save each plot with "ceph" filenames
save_plot(plot_chla, "boxplot_chla_ceph.png")
save_plot(plot_tsm, "boxplot_tsm_ceph.png")
save_plot(plot_cur, "boxplot_cur_ceph.png")
save_plot(plot_sst, "boxplot_sst_ceph.png")
save_plot(plot_tmin, "boxplot_tmin_ceph.png")
save_plot(plot_o2_min, "boxplot_o2_min_ceph.png")
save_plot(plot_sml, "boxplot_sml_ceph.png")
save_plot(plot_lunar_fraction, "boxplot_lunar_fraction_ceph.png")
save_plot(plot_moon_phase, "boxplot_moon_phase_ceph.png")
save_plot(plot_altitude, "boxplot_altitude_ceph.png")

