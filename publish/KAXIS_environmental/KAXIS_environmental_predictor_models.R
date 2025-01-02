
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)

#=============================================================================
# 1. Setup and Data Loading
#=============================================================================
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")
#removing the outliers deviated substantially from the majority of SST values (0.7 to 2.2°C), preventing the GAMs from fitting reasonable smooths across all stations. 
km_bm_sum_2 <- km_bm_sum_2 %>% filter(SST > 0)

#days since melt is unusually high for MIDOC 27, averaging the results from nearby stations to get a better estimate 
km_bm_sum_2_filtered <- km_bm_sum_2_filtered %>% filter(midoc.stn == "MIDOC26" | midoc.stn == "MIDOC28")
  #average of 110.00001 days since ice melt (from MIDOC26) and 84.99997 days since ice melt (from MIDOC 28)
  (110.00001+84.99997)/2 #=97.49999
#input 97.49999 into the days since melt value for MIDOC27 in the km_bm_sum_2 
km_bm_sum_2$days_since_melt[km_bm_sum_2$midoc.stn == "MIDOC27"] <- 97.49999

#=============================================================================
# 2. Depth integrated models for environmental predictors 
#=============================================================================
#All biomass (excluding gelatinous)
allbiom_additive_all_vars <-gam(log(bm_sum_all_taxa) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(allbiom_additive_all_vars, residuals = TRUE) +
  theme(plot.title = element_text(hjust = -8, vjust = 9 ))
summary(allbiom_additive_all_vars)
gam.check(allbiom_additive_all_vars)

#Fish biomass
fish_additive_all_vars <-gam(log(bm_sum_fish) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(fish_additive_all_vars, residuals = TRUE) 
#+ ggtitle("Fish Biomass (Log) All Taxa Additive model all vars") + 
theme(plot.title = element_text(hjust = 4, vjust = 9 ))
summary(fish_additive_all_vars)
gam.check(fish_additive_all_vars)

#cephalopod biomass 
ceph_additive_all_vars <-gam(log(bm_sum_ceph) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(ceph_additive_all_vars, residuals = TRUE) 
#+ ggtitle("Cephalopod Biomass (Log) All Taxa Additive model all vars") + 
# theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(ceph_additive_all_vars)
gam.check(ceph_additive_all_vars)

#krill biomass
krill_additive_all_vars <-gam(log(bm_sum_krill) ~ s(SST)+ s(CUR) + s(chl_rs) +s(days_since_melt), data = km_bm_sum_2)
draw(krill_additive_all_vars, residuals = TRUE) + ggtitle("Krill Biomass (Log) All Taxa Additive model all vars") + 
  theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(krill_additive_all_vars)
gam.check(krill_additive_all_vars)

#=============================================================================
# 3. Presentation 
#=============================================================================
# Extract the ggplot objects from draw()
p_list_fish <- draw(fish_additive_all_vars, residuals = TRUE, return_objects = TRUE)
p_list_ceph <- draw(ceph_additive_all_vars, residuals = TRUE, return_objects = TRUE)
p_list_krill <- draw(krill_additive_all_vars, residuals = TRUE, return_objects = TRUE)
modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}

#3.1 Preparing fish plots for presentation 

 
fish_SST <-
modify_geom_point(p_list_fish[[1]], "black") + 
  theme_minimal() +
  labs(title = "Fish", subtitle = NULL, caption = NULL)  +
  xlab("Sea surface temperature (°C)")+
  ylab("Sea surface temperature (°C)")+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 1),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title = element_text(size = 14, colour = "black"),
    axis.ticks= element_line(color = "black", size = 0.5)
  ) 

fish_CUR <- modify_geom_point(p_list_fish[[2]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  
  xlab(expression(paste("Current speed (cm ", s^-1, ")"))) +
  ylab(expression(paste("Current speed (cm ", s^-1, ")")))+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title = element_text(size = 14, colour = "black"),
    axis.ticks= element_line(color = "black", size = 0.5),
  )


fish_chl_rs <- modify_geom_point(p_list_fish[[3]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  + 
  xlab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  ylab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title = element_text(size = 14, colour = "black"),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

fish_days_since_melt <- modify_geom_point(p_list_fish[[4]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Time since melt (days)") +
  ylab("Time since melt (days)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title = element_text(size = 14, colour = "black"),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

# Combine the plots
(fish_SST/ fish_CUR/ fish_chl_rs/ fish_days_since_melt) 

#3.2 Preparing ceph plots for presentation 
ceph_SST <- modify_geom_point(p_list_ceph[[1]], "black") + 
  theme_minimal() +
  labs(title = "Cephalopods", subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Sea surface temperature (°C)")+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 1),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  ) 

ceph_CUR <- modify_geom_point(p_list_ceph[[2]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Current speed (cm ", s^-1, ")")))+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

ceph_chl_rs <- modify_geom_point(p_list_ceph[[3]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

ceph_days_since_melt <- modify_geom_point(p_list_ceph[[4]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Time since melt (days)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

# combine the plots
(ceph_SST/ ceph_CUR/ ceph_chl_rs/ ceph_days_since_melt)

#3.3 Preparing krill plots for presentation
krill_SST <- modify_geom_point(p_list_krill[[1]], "black") + 
  theme_minimal() +
  labs(title = "Krill", subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Sea surface temperature (°C)")+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 1),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  ) 

krill_CUR <- modify_geom_point(p_list_krill[[2]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Current speed (cm ", s^-1, ")")))+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

krill_chl_rs <- modify_geom_point(p_list_krill[[3]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab(expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")"))) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  ) 

krill_days_since_melt <- modify_geom_point(p_list_krill[[4]], "black") +
  theme_minimal() +
  labs(title = NULL, subtitle = NULL, caption = NULL)  +  # Remove the title
  xlab("Time since melt (days)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text = element_text(size = 14, color = "black"), 
    axis.title.x = element_text(size = 14, colour = "white"),
    axis.title.y = element_blank(),
    axis.ticks= element_line(color = "black", size = 0.5),
  )

# combine the plots
(krill_SST/ krill_CUR/ krill_chl_rs/ krill_days_since_melt)

#3.4 Combine all the plots
environmental_predictor_fish_ceph_krill <- (fish_SST/ fish_CUR/ fish_chl_rs/ fish_days_since_melt) | (ceph_SST / ceph_CUR / ceph_chl_rs / ceph_days_since_melt) | (krill_SST / krill_CUR / krill_chl_rs/ krill_days_since_melt)

#save the plot 
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/publish/KAXIS_figures")
output_filename <- "KAXIS_environmental_predictor_GAM.png"
full_output_path <- file.path(output_directory, output_filename)
ggsave(filename = full_output_path, plot = environmental_predictor_fish_ceph_krill , width =11, height =13, dpi = 500, bg = "white")
 
