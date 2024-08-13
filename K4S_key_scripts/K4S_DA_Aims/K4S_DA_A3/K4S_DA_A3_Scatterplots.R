load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")


library(ggplot2)
library(dplyr)
library(purrr)

# List of taxa to plot
taxa <- c("all_taxa", "fish", "ceph", "krill")

# List of environmental variables to use as x-axis
env_vars <- c("TSM", "CHLA", "SST", "CUR") 

# Function to create a single plot

create_plot <- function(data, taxon, env_var) {
  # Remove NA values for the specific taxon, environmental variable, and depth
  data_filtered <- data %>%
    filter(!is.na(!!sym(paste0("bm_depth_", taxon))), 
           !is.na(!!sym(env_var)),
           !is.na(depth))
  
  ggplot(data_filtered, aes_string(x = env_var, y = paste0("bm_depth_", taxon), color = "depth")) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = paste("Scatter plot of", taxon, "vs", env_var),
         x = env_var,
         y = paste("Biomass", taxon),
         color = "Depth (m)") +
    theme_minimal() +
    scale_color_manual(values = c("0-200" = "#FFD300", 
                                  "200-400" = "red", 
                                  "400-600" = "magenta", 
                                  "600-800" = "purple", 
                                  "800-1000" = "blue"),
                       na.value = "grey50") }


# Create all plots
plots <- crossing(taxon = taxa, env_var = env_vars) %>%
  pmap(~create_plot(km_bm_depth, ..1, ..2))

# Display plots (you might want to adjust this for your needs)
for (p in plots) {
  print(p)
}
