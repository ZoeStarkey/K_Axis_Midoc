library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)
library(lubridate)
#=============================================================================
# 1. Setup and Data Loading
#=============================================================================

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth_2.Rda")

#making "day" fractional
with(km_bm_depth_2, plot(day,lunar_fraction))

#adding day_fraction to km_bm_depth
km_bm_depth_2$day_of_year <- yday(km_bm_depth_2$start_time)
km_bm_depth_2$day_fraction <- km_bm_depth_2$day_of_year + hour(km_bm_depth_2$start_time) / 24

#=============================================================================
# 2. Depth structured GAMMs for fish
#=============================================================================

fish_depth.day.re <- gamm(log(bm_depth_fish) ~ depth + s(day_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.day.re$gam)
summary(fish_depth.day.re$lme)
gam.check(fish_depth.day.re$gam)

#solar angle
fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ depth + s(altitude, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
fish_depth_solar <- draw(fish_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
summary(fish_depth.solar.re$gam)
summary(fish_depth.solar.re$lme) 
gam.check(fish_depth.solar.re$gam)

#lunar fraction - illuminated disc
fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
fish_depth_lunar <- draw(fish_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Fish - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(fish_depth.lunar.re$gam)
summary(fish_depth.lunar.re$lme)
gam.check(fish_depth.lunar.re$gam)

#=============================================================================
# 3. Depth structured GAMMs for cephalopods
#=============================================================================

#day 
ceph_depth.day.re <- gamm(log(bm_depth_ceph) ~ depth + s(day_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.day.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Day",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.day.re$gam)
summary(ceph_depth.day.re$lme)
gam.check(ceph_depth.day.re$gam)

#solar angle
ceph_depth.solar.re <- gamm(log(bm_depth_ceph) ~ depth + s(altitude, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.solar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Solar Angle",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.solar.re$gam)
summary(ceph_depth.solar.re$lme)
gam.check(ceph_depth.solar.re$gam)

#lunar fraction - illuminated disk
ceph_depth.lunar.re <- gamm(log(bm_depth_ceph) ~ depth + s(lunar_fraction, by = depth),data = km_bm_depth_2, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.lunar.re, residuals = TRUE) + theme(plot.margin = margin(t = 30, r = 20, b = 40, l = 20, unit = "pt"))
grid.text("Biomass (Logged) by depth + RE: Cephalopods - Lunar Fraction",  x = unit(0.19, "npc"), y = unit(0.05, "npc"), just = c("left", "bottom" ),  gp = gpar(fontsize = 14, fontface = "bold"))
summary(ceph_depth.lunar.re$gam)
summary(ceph_depth.lunar.re$lme)
gam.check(ceph_depth.lunar.re$gam)

#=============================================================================
# 3. Presentation
#=============================================================================
p_list_fish_depth_solar_re <- draw(fish_depth.solar.re , residuals = TRUE, return_objects = TRUE)
p_list_fish_depth_lunar_re <- draw(fish_depth.lunar.re , residuals = TRUE, return_objects = TRUE)
p_list_ceph_depth_lumar_re <- draw(ceph_depth.lunar.re , residuals = TRUE, return_objects = TRUE)

depth_ranges <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

modify_geom_point <- function(plot, new_color, new_size = 1.5) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  return(plot)
}

fish_solar <- lapply(seq_along(p_list_fish_depth_solar_re), function(i) {
  modify_geom_point(p_list_fish_depth_solar_re[[i]], "black") +  
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    ylab(paste(depth_ranges[i], "m")) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      axis.text = element_text(size = 14, color = "black"), 
      axis.title = element_text(size = 14, colour = "black"),
      axis.ticks= element_line(color = "black", size = 0.5),
    )
})
fish_solar <- lapply(fish_solar, function(p) p + theme(plot.subtitle = element_blank(), axis.title.y = element_blank()))

fish_lunar <- lapply(seq_along(p_list_fish_depth_lunar_re), function(i) {
  modify_geom_point(p_list_fish_depth_lunar_re[[i]], "black") +  
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    ylab(paste(depth_ranges[i], "m")) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      axis.text = element_text(size = 14, color = "black"), 
      axis.title = element_text(size = 14, colour = "black"),
      axis.ticks= element_line(color = "black", size = 0.5),
    )
})
fish_lunar <- lapply(fish_lunar, function(p) p + theme(plot.subtitle = element_blank(), axis.title.y = element_blank()))

fish_depth_solar_lunar <- (fish_solar[[1]] | fish_lunar[[1]]) /
  (fish_solar[[2]] | fish_lunar[[2]]) /
  (fish_solar[[3]] | fish_lunar[[3]]) /
  (fish_solar[[4]] | fish_lunar[[4]]) /
  (fish_solar[[5]] | fish_lunar[[5]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")
fish_depth_solar_lunar 


#=============================================================================
# 3. Presentation
#=============================================================================
p_list_fish_depth_solar_re <- draw(fish_depth.solar.re , residuals = TRUE, return_objects = TRUE)
p_list_fish_depth_lunar_re <- draw(fish_depth.lunar.re , residuals = TRUE, return_objects = TRUE)
p_list_ceph_depth_lumar_re <- draw(ceph_depth.lunar.re , residuals = TRUE, return_objects = TRUE)

depth_ranges <- c("0-200", "200-400", "400-600", "600-800", "800-1000")

modify_geom_point <- function(plot, new_color, new_size = 1.5) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  return(plot)
}

#3.1 creating the fish and solar angle plots 

fish_solar <- lapply(seq_along(p_list_fish_depth_solar_re), function(i) {
  is_bottom <- i == length(p_list_fish_depth_solar_re) 
  modify_geom_point(p_list_fish_depth_solar_re[[i]], "black") +
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    {if(is_bottom) xlab("Solar angle (°)") else xlab(NULL)} +   # Only add xlab if it's the bottom plot
    ylab(paste(depth_ranges[i])) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      axis.text = element_text(size = 10, color = "black"), 
      axis.title = element_text(size = 12, colour = "black"),
      axis.ticks = element_line(color = "black", size = 0.5),
    )
})

#3.2 creating the fish and lunar fraction plots 
fish_lunar <- lapply(seq_along(p_list_fish_depth_lunar_re), function(i) {
  is_bottom <- i == length(p_list_fish_depth_lunar_re) 
  
  modify_geom_point(p_list_fish_depth_lunar_re[[i]], "black") +
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    {if(is_bottom) xlab("Illuminated lunar fraction") else xlab(NULL)} +    # Only add xlab if it's the bottom plot
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      axis.text = element_text(size = 10, color = "black"), 
      axis.title.x = if(is_bottom) element_text(size = 12) else element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_line(color = "black", size = 0.5)
    )
})



#3.3 combining the fish and solar angle and lunar fraction plots



fish_depth_solar_lunar <- (fish_solar[[1]] | fish_lunar[[1]]) /
  (fish_solar[[2]] | fish_lunar[[2]]) /
  (fish_solar[[3]] | fish_lunar[[3]]) /
  (fish_solar[[4]] | fish_lunar[[4]]) /
  (fish_solar[[5]] | fish_lunar[[5]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

fish_depth_solar_lunar

ggsave(
  filename = "~/Desktop/fish_depth_solar_lunar.tiff",
  plot = fish_depth_solar_lunar,
  width = 190,      # mm (full double-column)
  height = 240,     # mm (square-ish for 2x2 layout)
  units = "mm",
  dpi = 500,
  compression = "lzw"
)

# output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_figures")
# output_filename <- "KAXIS_fish_solar_lunar_plot.png"
# full_output_path <- file.path(output_directory, output_filename)
# ggsave(filename = full_output_path, plot = fish_depth_solar_lunar , width =10, height =13, dpi = 500, bg = "white")

#3.4 creating the cephalopod lunar plots 
ceph_lunar <- lapply(seq_along(p_list_ceph_depth_lumar_re ), function(i) {
  is_middle <- i == 3
  modify_geom_point(p_list_ceph_depth_lumar_re [[i]], "black") +
    theme_minimal() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    {if(is_middle) xlab("Illuminated lunar fraction") else xlab(NULL)} +   # Only add xlab if it's the bottom plot
    ylab(paste(depth_ranges[i])) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2),
      axis.text = element_text(size = 14, color = "black"), 
      axis.title = element_text(size = 14, colour = "black"),
      axis.ticks = element_line(color = "black", size = 0.5),
    )
})

ceph_depth_lunar <- 
  ceph_lunar[[2]] /
  ceph_lunar[[3]]  +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_figures")
output_filename <- "KAXIS_ceph_lunar_plot.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = ceph_depth_lunar , width =9, height =9, dpi = 500, bg = "white")
