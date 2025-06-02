library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)
library(dplyr)
library(lubridate)
library(ggcorrplot)
library(patchwork)


#load the data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")

#making "day" fractional
with(km_bm_sum_2, plot(day,lunar_fraction))

#adding day_fraction to km_bm_sum
km_bm_sum_2$day_of_year <- yday(km_bm_sum_2$start_time)
km_bm_sum_2$day_fraction <- km_bm_sum_2$day_of_year + hour(km_bm_sum_2$start_time) / 24

#Temporal performance analytics for all taxa
temporal_PA_all_taxa <- km_bm_sum_2[, c("bm_sum_all_taxa", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_all_taxa$bm_sum_all_taxa <- log(temporal_PA_all_taxa$bm_sum_all_taxa)
temporal_PA_all_taxa <- temporal_PA_all_taxa[complete.cases(temporal_PA_all_taxa), ]
#plot
chart.Correlation(temporal_PA_all_taxa, histogram=TRUE, pch=19, )

#Temporal performance analytics for fish
temporal_PA_fish <- km_bm_sum_2[, c("bm_sum_fish", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_fish$bm_sum_fish <- log(temporal_PA_fish$bm_sum_fish)
temporal_PA_fish <- temporal_PA_fish[complete.cases(temporal_PA_fish), ]
#plot
chart.Correlation(temporal_PA_fish, histogram=TRUE, pch=19, )

#Temporal performance analytics for cephalopods
temporal_PA_ceph <- km_bm_sum_2[, c("bm_sum_ceph", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_ceph$bm_sum_ceph <- log(temporal_PA_ceph$bm_sum_ceph)
temporal_PA_ceph <- temporal_PA_ceph[complete.cases(temporal_PA_ceph), ]
#plot
chart.Correlation(temporal_PA_ceph, histogram=TRUE, pch=19, )

#Temporal performance analytics for krill
temporal_PA_krill <- km_bm_sum_2[, c("bm_sum_krill", "lunar_fraction", "altitude", "day_fraction")]
temporal_PA_krill$bm_sum_krill <- log(temporal_PA_krill$bm_sum_krill)
temporal_PA_krill <- temporal_PA_krill[complete.cases(temporal_PA_krill), ]
#plot
chart.Correlation(temporal_PA_krill, histogram=TRUE, pch=19, )



###########################Plot heat maps#########################################
#creating the function 
plot_ggcorr <- function(data, var_labels) {
  cor_matrix <- cor(data, use = "complete.obs")
  
  # Rename columns and rows
  colnames(cor_matrix) <- var_labels
  rownames(cor_matrix) <- var_labels
  
  # Generate the ggcorrplot
  ggcorrplot(cor_matrix,
             method = "square",
             type = "lower",
             lab = TRUE,
             lab_size = 6,
             lab_col = "black",
             tl.cex = 12,
             show.legend = TRUE) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation Coefficient",
      guide = guide_colorbar(
        barheight = 35,
        title.position = "left",   
        label.position = "right"  
      )
    ) +
    theme(
      axis.text.x = element_text(size = 16, colour = 'black'),
      axis.text.y = element_text(size = 16, colour = 'black'),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      legend.title = element_text(size = 18, hjust = 0.5, face = "bold", angle = 90),
      legend.text  = element_text(size = 16)
    )
}



# Variable labels for all
labels <- c("Biomass", "Lunar fraction", "Solar angle", "Day")

fish_data  <- temporal_PA_fish[, sapply(temporal_PA_fish, is.numeric)]
ceph_data  <- temporal_PA_ceph[, sapply(temporal_PA_ceph, is.numeric)]
krill_data <- temporal_PA_krill[, sapply(temporal_PA_krill, is.numeric)]
all_taxa   <- temporal_PA_all_taxa[, sapply(temporal_PA_all_taxa, is.numeric)]

# Create each plot
p_all_taxa   <- plot_ggcorr(temporal_PA_all_taxa,   labels) + ggtitle("Total Biomass")
p_fish  <- plot_ggcorr(fish_data,  labels) + ggtitle("Fish")
p_ceph  <- plot_ggcorr(ceph_data,  labels) + ggtitle("Cephalopods")
p_krill <- plot_ggcorr(krill_data, labels) + ggtitle("Krill")

# Use patchwork to combine the plots (2x2 grid)
Temporal_PA_heatmap <- (p_all_taxa + p_fish) / (p_ceph + p_krill) +
  plot_layout(guides = "collect") & theme(legend.position = "right")


Temporal_PA_heatmap
# Save the combined plot
ggsave("Temporal_PA_heatmap.png", plot = Temporal_PA_heatmap, width = 12, height = 10, dpi = 300)

