# Load required libraries
library(mgcv)
library(ggplot2)
library(dplyr)
library(patchwork)

# Assuming km_bm_depth is already loaded and preprocessed

# Solar model
fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

# Lunar model
fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

# Function to extract partial effects for models with 'by' variables
get_partial_effects <- function(model, term, by_var) {
  # Create a new data frame for predictions
  pred_data <- expand.grid(
    depth = unique(km_bm_depth$depth),
    altitude = seq(min(km_bm_depth$altitude), max(km_bm_depth$altitude), length.out = 100),
    lunar_fraction = seq(min(km_bm_depth$lunar_fraction), max(km_bm_depth$lunar_fraction), length.out = 100)
  )
  
  # Get predictions
  preds <- predict(model$gam, newdata = pred_data, se.fit = TRUE)
  
  # Combine predictions with the prediction data
  pred_data$fit <- preds$fit
  pred_data$se.fit <- preds$se.fit
  
  # Select relevant columns and rename
  if (term == "altitude") {
    result <- pred_data %>%
      select(x = altitude, y = fit, se = se.fit, depth)
  } else {
    result <- pred_data %>%
      select(x = lunar_fraction, y = fit, se = se.fit, depth)
  }
  
  return(result)
}

# Extract partial effects for solar and lunar models
solar_effects <- get_partial_effects(fish_depth.solar.re, "altitude", "depth")
lunar_effects <- get_partial_effects(fish_depth.lunar.re, "lunar_fraction", "depth")

# Define the desired order of depth levels
depth_order <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

# Reorder the depth factor
solar_effects$depth <- factor(solar_effects$depth, levels = depth_order)
lunar_effects$depth <- factor(lunar_effects$depth, levels = depth_order)

# Create the ggplot for solar effects
fish_depth_solar <- ggplot(solar_effects, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = y - 2*se, ymax = y + 2*se), alpha = 0.2) +
  geom_line(color = "black", size = 2) +
  facet_wrap(~ depth, scales = "free_y", ncol = 1) +
  labs(x = "Solar Angle", y = "Partial effect on log(bm_depth_fish)") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        panel.background = element_rect(fill = "grey90", color = NA),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.title = element_text(size = 35),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# Create the ggplot for lunar effects
fish_depth_lunar <- ggplot(lunar_effects, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = y - 2*se, ymax = y + 2*se), alpha = 0.2) +
  geom_line(color = "black", size = 2) +
  facet_wrap(~ depth, scales = "free_y", ncol = 1) +
  labs(x = "Illuminated fraction", y = "Partial effect on log(bm_depth_fish)") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        panel.background = element_rect(fill = "grey90", color = NA),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.title = element_text(size = 35),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# Combine plots
fish_depth_sol_lunar <- (fish_depth_solar | fish_depth_lunar)

# Display the combined plot
print(fish_depth_sol_lunar)

# Save the plot
output_directory <- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A3/K4S_Plot_A3")
output_filename <- "K4S_Plot_A3_fish_solar_lunar_partial_effects.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = fish_depth_sol_lunar, width = 17, height = 24, dpi = 500, bg = "white")

#new dataframe of lunar_fraction and midoc.stn from km_bm_sum dataframe


km_moon <- km_bm_sum %>% select(lunar_fraction, midoc.stn)
km_moon$lunar_fraction <- format(km_moon$lunar_fraction, scientific = FALSE)
