




library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")
km_bm_sum <- km_bm_sum %>% filter(SST > 0)


#TEST 3
# Fit the GAM model
fish_additive_all_vars <- gam(log(bm_sum_fish) ~ s(SST) + s(CUR) + s(CHLA) + s(TSM), data = km_bm_sum)

# Extract the ggplot objects from draw()
p_list <- draw(fish_additive_all_vars, residuals = TRUE, return_objects = TRUE)

# Function to modify existing geom_point layer
# modify_geom_point <- function(plot, new_color) {
#   plot$layers[[which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))]]$aes_params$colour <- new_color
#   return(plot)
# }

modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}
# Customize each plot individually
fish_SST <- modify_geom_point(p_list[[1]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Sea surface temperature (°C)")+
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "black"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  ) 
fish_CUR <- modify_geom_point(p_list[[2]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Current speed (cm ", s^-1, ")")))+
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text = element_text(color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "black"),
    axis.title = element_text(color = "grey30")
  )
fish_CHLA <- modify_geom_point(p_list[[3]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "black"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )
fish_TSM <- modify_geom_point(p_list[[4]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL) +  # Remove the title
  xlab("Time since melt (days)") +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "black"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

# Combine the plots back into a grid layout
(fish_SST/ fish_CUR/ fish_CHLA/ fish_TSM)


###CEPHALOPODS
# Define the new model
ceph_additive_all_vars <- gam(log(bm_sum_ceph) ~ s(SST) + s(CUR) + s(CHLA) + s(TSM), data = km_bm_sum)

# Extract the ggplot objects from draw()
p_list_ceph <- draw(ceph_additive_all_vars, residuals = TRUE, return_objects = TRUE)

# Function to modify existing geom_point layer
modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}

# Customize each plot individually for cephalopod model
ceph_SST <- modify_geom_point(p_list_ceph[[1]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Sea surface temperature (°C)")+
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

ceph_CUR <- modify_geom_point(p_list_ceph[[2]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Current speed (cm ", s^-1, ")")))+
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text = element_text(color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.title = element_text(color = "grey30")
  )

ceph_CHLA <- modify_geom_point(p_list_ceph[[3]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

ceph_TSM <- modify_geom_point(p_list_ceph[[4]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL) +  # Remove the title
  xlab("Time since melt (days)") +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

# Combine the plots back into a grid layout
(fish_SST/ fish_CUR/ fish_CHLA/ fish_TSM) | (ceph_SST / ceph_CUR / ceph_CHLA / ceph_TSM)

#################KRILL####################
# Define the new model for krill
krill_additive_all_vars <- gam(log(bm_sum_krill) ~ s(SST) + s(CUR) + s(CHLA) + s(TSM), data = km_bm_sum)

# Extract the ggplot objects from draw()
p_list_krill <- draw(krill_additive_all_vars, residuals = TRUE, return_objects = TRUE)

# Function to modify existing geom_point layer
modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}

# Customize each plot individually for krill model
krill_SST <- modify_geom_point(p_list_krill[[1]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)   +  # Remove the title
  xlab("Sea surface temperature (°C)")+
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

krill_CUR <- modify_geom_point(p_list_krill[[2]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Current speed (cm ", s^-1, ")")))+
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text = element_text(color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.title = element_text(color = "grey30")
  )

krill_CHLA <- modify_geom_point(p_list_krill[[3]], "grey40") + 
  theme_minimal() +
   labs(title = NULL, subtitle = NULL, caption = NULL) +  # Remove the title
  xlab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

krill_TSM <- modify_geom_point(p_list_krill[[4]], "grey40") + 
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)+  # Remove the title
  xlab("Time since melt (days)") +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

# Combine the plots back into a grid layout
satellite_vars_fish_ceph_krill <- (fish_SST/ fish_CUR/ fish_CHLA/ fish_TSM) | (ceph_SST / ceph_CUR / ceph_CHLA / ceph_TSM) | (krill_SST / krill_CUR / krill_CHLA / krill_TSM)


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A3/K4S_Plot_A3")
output_filename <- "K4S_Plot_A3_sat_all.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = satellite_vars_fish_ceph_krill , width =11, height =12, dpi = 500, bg = "white")


#combining stat signifcant for powerpoint 
statistically_sig_fish_krill <- (fish_SST|krill_CUR|krill_CHLA)
statistically_sig_fish_krill
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A3/K4S_Plot_A3")
output_filename <- "K4S_Plot_A3_sat_all_powerpoint.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = statistically_sig_fish_krill , width =10, height =2.5, dpi = 500, bg = "white")


