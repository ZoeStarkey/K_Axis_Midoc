library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)
library(lubridate)
library(dplyr)

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")

km_bm_depth$day_of_year <- yday(km_bm_depth$start_time)
km_bm_depth$day_fraction <- km_bm_depth$day_of_year + hour(km_bm_depth$start_time) / 24

## FISH SOLAR 

fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))

                        
                            
gam_model <- fish_depth.solar.re$gam
p_list <- draw(gam_model, residuals = TRUE, return_objects = TRUE)


modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  return(plot)
}

depth_ranges <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

plot_list <- lapply(seq_along(p_list), function(i) {
  modify_geom_point(p_list[[i]], "grey40") +
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    xlab("Altitude") +
    ylab(paste("s(altitude):", depth_ranges[i], "m")) +
    theme(
      panel.background = element_rect(fill = "grey90", color = NA),
      panel.grid.major = element_line(color = "white", size = 0.5),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y = element_text(size = 14, color = "black"),
      axis.title.x = element_text(size = 15, colour = "black"),
      axis.title.y = element_text(size = 15, colour = "black"),
      axis.text = element_text(color = "grey30"),
      axis.title = element_text(color = "grey30")
    )
})     


library(gridExtra)
grid.arrange(grobs = plot_list, ncol = 2)

#FISH LUNAR
fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
gam_model <- fish_depth.lunar.re$gam


p_list <- draw(gam_model, residuals = TRUE, return_objects = TRUE)
modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  return(plot)
}


depth_ranges <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

plot_list <- lapply(seq_along(p_list), function(i) {
  modify_geom_point(p_list[[i]], "grey40") +
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    xlab("Lunar fraction") +
    ylab(paste("s(lunar fraction):", depth_ranges[i], "m")) +
    theme(
      panel.background = element_rect(fill = "grey90", color = NA),
      panel.grid.major = element_line(color = "white", size = 0.5),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y = element_text(size = 14, color = "black"),
      axis.title.x = element_text(size = 15, colour = "black"),
      axis.title.y = element_text(size = 15, colour = "black"),
      axis.text = element_text(color = "grey30"),
      axis.title = element_text(color = "grey30")
    )
})

library(gridExtra)
grid.arrange(grobs = plot_list, ncol = 2)

#PUTTING FISH PLOTS TOGETHER

fish_depth_solar_re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

gam_model_solar <- fish_depth_solar_re$gam
p_list_solar <- draw(gam_model_solar, residuals = TRUE, subtitle = NULL, return_objects = TRUE)

fish_depth_lunar_re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

gam_model_lunar <- fish_depth_lunar_re$gam
p_list_lunar <- draw(gam_model_lunar, residuals = TRUE, return_objects = TRUE)

modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  return(plot)
}


fish_solar <- lapply(fish_solar, function(p) p + theme(plot.subtitle = element_blank(), axis.title.y = element_blank()))
fish_lunar <- lapply(fish_lunar, function(p) p + theme(plot.subtitle = element_blank()))

fish_depth_solar_lunar <- (fish_solar[[1]] | fish_lunar[[1]]) /
  (fish_solar[[2]] | fish_lunar[[2]]) /
  (fish_solar[[3]] | fish_lunar[[3]]) /
  (fish_solar[[4]] | fish_lunar[[4]]) /
  (fish_solar[[5]] | fish_lunar[[5]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")
fish_depth_solar_lunar

fish_depth_solar_lunar <- (fish_solar[[1]] /fish_solar[[2]] /fish_solar[[3]] /fish_solar[[4]] /fish_solar[[5]] ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")
fish_depth_solar_lunar 
library(gridExtra)
grid.arrange(grobs = plot_list, ncol = 2)
fish_depth_solar_lunar 
# 
# 
# 
# 
# customize_plot <- function(plot, is_bottom = FALSE, is_left = FALSE) {
#   p <- modify_geom_point(plot, "grey40") +
#     theme_minimal() +
#     labs(title = NULL, subtitle = NULL, caption = NULL)  +
#     #labs(title = NULL, x = NULL, y = NULL) +  # Remove all labels initially
#     theme(
#       panel.background = element_rect(fill = "grey90", color = NA),
#       panel.grid.major = element_line(color = "white", size = 0.5),
#       axis.text = element_text(size = 15, color = "black"),
#       axis.title = element_text(size = 17, color = "black"),
#       plot.title = element_blank()
#     )
#   
#   if (is_bottom) {
#     p <- p + theme(axis.title.x = element_text())  # Show x-axis label only for bottom panels
#   } else {
#     p <- p + theme(axis.title.x = element_blank())  # Hide x-axis label for non-bottom panels
#   }
#   
#   if (!is_left) {
#     p <- p + theme(axis.title.y = element_blank())  # Hide y-axis label for right panels
#   }
#   
#   return(p)
# }
# remove_extra_labels <- function(plot) {
#   # Remove the "By: depth" label
#   plot$labels$title <- NULL
#   
#   # Remove the "Basis: TPRS" label
#   if (!is.null(plot$labels$subtitle)) {
#     plot$labels$subtitle <- NULL
#   }
#   
#   return(plot)
# }
# 
# fish_solar <- lapply(seq_along(p_list_solar), function(i) {
#   is_bottom <- i == length(p_list_solar)
#   # First remove extra labels, then customize
#   p <- remove_extra_labels(p_list_solar[[i]])
#   p <- customize_plot(p, is_bottom = is_bottom, is_left = TRUE)
#   if (is_bottom) {
#     p <- p + xlab("Solar angle (°)")
#   }
#   p 
# })
# 
# fish_lunar <- lapply(seq_along(p_list_lunar), function(i) {
#   is_bottom <- i == length(p_list_lunar)
#   # First remove extra labels, then customize
#   p <- remove_extra_labels(p_list_lunar[[i]])
#   p <- customize_plot(p, is_bottom = is_bottom, is_left = FALSE)
#   if (is_bottom) {
#     p <- p + xlab("Illuminated lunar fraction")
#   }
#   p  # No y-label for right panels
# })
# 
# library(patchwork)
# 
# fish_solar <- lapply(fish_solar, function(p) p + theme(plot.subtitle = element_blank()))
# fish_lunar <- lapply(fish_lunar, function(p) p + theme(plot.subtitle = element_blank()))
# 
# fish_depth_solar_lunar <- (fish_solar[[1]] | fish_lunar[[1]]) /
#   (fish_solar[[2]] | fish_lunar[[2]]) /
#   (fish_solar[[3]] | fish_lunar[[3]]) /
#   (fish_solar[[4]] | fish_lunar[[4]]) /
#   (fish_solar[[5]] | fish_lunar[[5]]) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "none") +
# theme(plot.subtitle = element_blank()) 

print(combined_plot)
combined_plot



output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A3_fish_depth_solar_lunar.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = fish_depth_solar_lunar , width =10, height =13, dpi = 500, bg = "white")


#CEPHALOPODS

fish_depth_solar_re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

gam_model_solar <- fish_depth_solar_re$gam
p_list_solar <- draw(gam_model_solar, residuals = TRUE, return_objects = TRUE)

fish_depth_lunar_re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),
                            data = km_bm_depth, 
                            random = list(midoc.stn = ~ 1))

gam_model_lunar <- fish_depth_lunar_re$gam
p_list_lunar <- draw(gam_model_lunar, residuals = TRUE, return_objects = TRUE)

modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  return(plot)
}



customize_plot <- function(plot, is_bottom = FALSE, is_left = FALSE) {
  p <- modify_geom_point(plot, "grey40") +
    theme_minimal() +
    labs(title = NULL, x = NULL, y = NULL) +  # Remove all labels initially
    theme(
      panel.background = element_rect(fill = "grey90", color = NA),
      panel.grid.major = element_line(color = "white", size = 0.5),
      axis.text = element_text(size = 15, color = "black"),
      axis.title = element_text(size = 17, color = "black"),
      plot.title = element_blank()
    )
  
  if (is_bottom) {
    p <- p + theme(axis.title.x = element_text())  # Show x-axis label only for bottom panels
  } else {
    p <- p + theme(axis.title.x = element_blank())  # Hide x-axis label for non-bottom panels
  }
  
  if (!is_left) {
    p <- p + theme(axis.title.y = element_blank())  # Hide y-axis label for right panels
  }
  
  return(p)
}
remove_extra_labels <- function(plot) {
  # Remove the "By: depth" label
  plot$labels$title <- NULL
  
  # Remove the "Basis: TPRS" label
  if (!is.null(plot$labels$subtitle)) {
    plot$labels$subtitle <- NULL
  }
  
  return(plot)
}

fish_solar <- lapply(seq_along(p_list_solar), function(i) {
  is_bottom <- i == length(p_list_solar)
  # First remove extra labels, then customize
  p <- remove_extra_labels(p_list_solar[[i]])
  p <- customize_plot(p, is_bottom = is_bottom, is_left = TRUE)
  if (is_bottom) {
    p <- p + xlab("Solar angle (°)")
  }
  p 
})

fish_lunar <- lapply(seq_along(p_list_lunar), function(i) {
  is_bottom <- i == length(p_list_lunar)
  # First remove extra labels, then customize
  p <- remove_extra_labels(p_list_lunar[[i]])
  p <- customize_plot(p, is_bottom = is_bottom, is_left = FALSE)
  if (is_bottom) {
    p <- p + xlab("Illuminated lunar fraction")
  }
  p  # No y-label for right panels
})

library(patchwork)

fish_depth_solar_lunar <- (fish_solar[[1]] | fish_lunar[[1]]) /
  (fish_solar[[2]] | fish_lunar[[2]]) /
  (fish_solar[[3]] | fish_lunar[[3]]) /
  (fish_solar[[4]] | fish_lunar[[4]]) /
  (fish_solar[[5]] | fish_lunar[[5]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_plot)
combined_plot


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A3_fish_depth_solar_lunar.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = fish_depth_solar_lunar , width =12, height =15, dpi = 500, bg = "white")


#CEPHALOPOD
ceph_depth.lunar.re <- gamm(log(bm_depth_ceph) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))


# Extract GAM component
gam_model_lunar_ceph <- ceph_depth.lunar.re$gam

# Create plot list
p_list_lunar_ceph <- draw(gam_model_lunar_ceph, residuals = TRUE, return_objects = TRUE)

# The modify_geom_point, customize_plot, and remove_extra_labels functions remain the same as in your previous code

# Create plot list for cephalopods (lunar only)
ceph_lunar <- lapply(seq_along(p_list_lunar_ceph), function(i) {
  is_bottom <- i == length(p_list_lunar_ceph)
  p <- remove_extra_labels(p_list_lunar_ceph[[i]])
  p <- customize_plot(p, is_bottom = is_bottom, is_left = TRUE)  # Set is_left to TRUE for y-axis label
  if (is_bottom) {
    p <- p + xlab("Illuminated lunar fraction")
  }
  if (i == 1) {  # Add y-axis label only to the top plot
    p 
  }
  p 
})

# Combine plots vertically
ceph_depth_lunar <- 
  ceph_lunar[[2]] /
  ceph_lunar[[3]]  +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# Print the combined plot
print(ceph_depth_lunar)

# Save the plot
output_directory <- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A3_ceph_depth_lunar.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = ceph_depth_lunar , width =9, height =9, dpi = 500, bg = "white")



#Ceph and fish luanr 
ceph_fish_depth_lunar <- 
  (fish_lunar[[2]] | ceph_lunar[[2]]) /
  ( fish_lunar[[5]]| ceph_lunar[[3]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(ceph_fish_depth_lunar)

output_directory <- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_Plot_A2")
output_filename <- "K4S_Plot_A3_ceph_fish_depth_lunar.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = ceph_fish_depth_lunar , width =10, height =6, dpi = 500, bg = "white")

