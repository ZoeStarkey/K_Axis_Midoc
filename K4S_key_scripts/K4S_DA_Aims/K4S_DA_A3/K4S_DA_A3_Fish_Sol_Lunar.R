library(mgcv)
library(ggplot2)
library(dplyr)
library(forcats)


library(dplyr)

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")

# Remove NA values from the original dataset
# km_bm_depth <- km_bm_depth %>% 
#   filter(!is.na(depth) & !is.na(altitude) & !is.na(bm_depth_fish))

fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

# Create a prediction dataframe
pred_data <- expand.grid(
  altitude = seq(min(km_bm_depth$altitude), max(km_bm_depth$altitude), length.out = 100),
  depth = unique(km_bm_depth$depth)
)

# Get predictions
preds <- predict(fish_depth.solar.re$gam, newdata = pred_data, se.fit = TRUE)
pred_data$fit <- preds$fit
pred_data$se.fit <- preds$se.fit

# Define the desired order of depth levels
depth_order <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

# Reorder the depth factor in both dataframes
pred_data$depth <- factor(pred_data$depth, levels = depth_order)
km_bm_depth$depth <- factor(km_bm_depth$depth, levels = depth_order)

# Remove any remaining NA values from pred_data
#pred_data <- pred_data %>% filter(!is.na(depth))

# Create the ggplot with vertical alignment
fish_depth_solar <- ggplot(pred_data, aes(x = altitude, y = fit)) +
  
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), alpha = 0.2) +
  
  geom_line(color = "black", size = 2) +  # Thicker black line
  
  geom_point(data = km_bm_depth, aes(y = log(bm_depth_fish)), alpha = 0.5, color = "grey40", size = 4) +
  
  facet_wrap(~ depth, scales = "free_y", ncol = 1) +  # Vertical alignment
  
  labs(x = "Solar Angle", y = NULL) +  # Changed label to "Solar Angle"
  
  theme_minimal() +  # Keep minimal theme as base
  
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        
        # Set the background for each panel to light grey
        panel.background = element_rect(fill = "grey90", color = NA),  # Grey background for each facet
        panel.grid.major = element_line(color = "white"),  # White grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        
        # Axis line customization
        axis.line = element_line(color = "white"),  # Axis lines white
        axis.text.y = element_text(size = 25),  # Increase y-axis text size
        axis.text.x = element_text(size = 25),  # Increase x-axis text size
        axis.title.x = element_text(size = 35),  # Increase x-axis title size
        
        # Adjust space between plots by setting plot background to white
        plot.background = element_rect(fill = "white", color = NA),  # White background for the whole plot
        
        # Adjust facet panel spacing to emphasize the separation
        plot.margin = margin(10, 10, 10, 10)  # Increase margin around the plot for more spacing
  )
  
  
  

print(fish_depth_solar)


#LUNAR

fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

# Create a prediction dataframe
pred_data <- expand.grid(
  lunar_fraction = seq(min(km_bm_depth$lunar_fraction), max(km_bm_depth$lunar_fraction), length.out = 100),
  depth = unique(km_bm_depth$depth)
)

# Get predictions
preds <- predict(fish_depth.lunar.re$gam, newdata = pred_data, se.fit = TRUE)
pred_data$fit <- preds$fit
pred_data$se.fit <- preds$se.fit

# Define the desired order of depth levels
depth_order <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

# Reorder the depth factor in both dataframes
pred_data$depth <- factor(pred_data$depth, levels = depth_order)
km_bm_depth$depth <- factor(km_bm_depth$depth, levels = depth_order)

# Create the ggplot with vertical alignment
fish_depth_lunar <- ggplot(pred_data, aes(x = lunar_fraction, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), alpha = 0.2) +
  geom_line(color = "black", size = 2) +
  geom_point(data = km_bm_depth, aes(y = log(bm_depth_fish)), alpha = 0.5, color = "grey40", size = 4) +
  facet_wrap(~ depth, scales = "free_y", ncol = 1) +  # Keep vertical alignment
   labs(x = "Illuminated fraction", y = NULL) +
  theme_minimal() +  # Keep minimal theme as base
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        
        # Set the background for each panel to light grey
        panel.background = element_rect(fill = "grey90", color = NA),  # Grey background for each facet
        panel.grid.major = element_line(color = "white"),  # White grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        
        # Axis line customization
        axis.line = element_line(color = "white"),  # Axis lines white
        axis.text.y = element_text(size = 25), 
        axis.text.x= element_text(size = 25), 
        axis.title.x = element_text(size = 35), # Increase y-axis text size
        
        # Adjust space between plots by setting plot background to white
        plot.background = element_rect(fill = "white", color = NA),  # White background for the whole plot
        
        # Adjust facet panel spacing to emphasize the separation
        plot.margin = margin(10, 10, 10, 10)  # Increase margin around the plot (optional for more spacing)
  )
fish_depth_sol_lunar <- (fish_depth_solar |fish_depth_lunar)

fish_depth_sol_lunar


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A3/K4S_Plot_A3")
output_filename <- "K4S_Plot_A3_fish_solar_lunar_test.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = fish_depth_sol_lunar , width =17, height =24, dpi = 500, bg = "white")






#CEPHALOPODS AND LUNAR##############################
Define the desired depth ranges
selected_depths <- c("200-400", "400-600")

# Create a prediction dataframe
pred_data <- expand.grid(
  lunar_fraction = seq(min(km_bm_depth$lunar_fraction), max(km_bm_depth$lunar_fraction), length.out = 100),
  depth = selected_depths
)

# Get predictions
preds <- predict(ceph_depth.lunar.re$gam, newdata = pred_data, se.fit = TRUE)
pred_data$fit <- preds$fit
pred_data$se.fit <- preds$se.fit

# Filter the original data for the selected depths
km_bm_depth_filtered <- km_bm_depth %>% 
  filter(depth %in% selected_depths)

# Ensure depth is a factor with the correct order
pred_data$depth <- factor(pred_data$depth, levels = selected_depths)
km_bm_depth_filtered$depth <- factor(km_bm_depth_filtered$depth, levels = selected_depths)

# Create the ggplot with vertical alignment
ceph_depth_lunar <- ggplot(pred_data, aes(x = lunar_fraction, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), alpha = 0.2) +
  geom_line(color = "black", size = 2) +  # Thicker black line
  geom_point(data = km_bm_depth_filtered, aes(y = log(bm_depth_ceph)), alpha = 0.5, color = "grey40", size = 4) +
  facet_wrap(~ depth, scales = "free_y", ncol = 1) +  # Vertical alignment
  labs(x = "Lunar Fraction", y = NULL ) +
  theme_minimal() +  # Keep minimal theme as base
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", size = 16),
    panel.background = element_rect(fill = "grey90", color = NA),  # Grey background for each facet
    panel.grid.major = element_line(color = "white"),  # White grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "white"),  # Axis lines white
    axis.text.y = element_text(size = 25),  # Increase y-axis text size
    axis.text.x = element_text(size = 25),  # Increase x-axis text size
    axis.title = element_text(size = 35),  # Increase axis title size
    plot.background = element_rect(fill = "white", color = NA),  # White background for the whole plot
    plot.margin = margin(10, 10, 10, 10),  # Increase margin around the plot for more spacing
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center the title
    plot.subtitle = element_text(size = 14, hjust = 0.5)  # Center the subtitle
  ) +
  ggtitle("Cephalopod Biomass by Lunar Fraction",
          subtitle = "Depths: 200-400m and 400-600m")

# Print the plot
print(ceph_depth_lunar)

# Save the plot
ggsave("ceph_depth_lunar_selected.png", ceph_depth_lunar, width = 10, height = 12, dpi = 300)
