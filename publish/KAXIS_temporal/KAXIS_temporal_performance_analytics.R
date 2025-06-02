library(PerformanceAnalytics)
library(ggplot2)
library(mgcv)
library(caret)
library(dplyr)
library(lubridate)
library(corrplot)
library(gridExtra)
library(grid)


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

#Creating function  
plot_correlation_matrix <- function(data, var_names) {
  # Calculate correlation matrix
  cor_matrix <- cor(data)
  
  # Apply new labels
  if (!is.null(var_names) && length(var_names) == ncol(cor_matrix)) {
    rownames(cor_matrix) <- var_names
    colnames(cor_matrix) <- var_names
  }
  
  # Plot
  corrplot(cor_matrix,
           method = "color",
           type = "lower",
           col = colorRampPalette(c("blue", "white", "red"))(200),
           tl.col = "black",
           tl.cex = 1.3,
           tl.srt = 0,
           tl.offset = 1.2,
           addCoef.col = "black",
           number.cex = 1.2,
           diag = FALSE,
           mar = c(0, 0, 0, 0))  # No margin for title
}


#Plot for all taxa 
plot_correlation_matrix(
  data = temporal_PA_all_taxa,
  var_names = c("Biomass (all taxa)", "Lunar fraction", "Solar Angle", "Day")
)


# Fish
plot_correlation_matrix(
  data = temporal_PA_fish,
  var_names = c("Fish biomass", "Lunar fraction", "Solar Angle", "Day")
)

# Cephalopods
plot_correlation_matrix(
  data = temporal_PA_ceph,
  var_names = c("Cephalopod biomass", "Lunar fraction", "Solar Angle", "Day")
)

# Krill
plot_correlation_matrix(
  data = temporal_PA_krill,
  var_names = c("Krill biomass", "Lunar fraction", "Solar Angle", "Day")
)

# Save the correlation matrix plots



# Function to create and capture a corrplot as a replayable plot
library(ggcorrplot)
library(patchwork)

plot_ggcorr <- function(data, var_labels) {
  # Compute correlation matrix
  cor_matrix <- cor(data, use = "complete.obs")
  
  # Rename columns and rows for nicer labels
  colnames(cor_matrix) <- var_labels
  rownames(cor_matrix) <- var_labels
  
  # Generate the ggcorrplot
  ggcorrplot(cor_matrix,
             method = "square",
             type = "lower",
             lab = TRUE,
             lab_size = 4,
             tl.cex = 12,
             colors = c("blue", "white", "red"),
             show.legend = TRUE)
}


# Variable labels for all
labels <- c("Biomass", "Lunar fraction", "Solar angle", "Day")

# Make sure your datasets are numeric-only and NA-free
fish_data  <- temporal_PA_fish[, sapply(temporal_PA_fish, is.numeric)]
ceph_data  <- temporal_PA_ceph[, sapply(temporal_PA_ceph, is.numeric)]
krill_data <- temporal_PA_krill[, sapply(temporal_PA_krill, is.numeric)]
all_data   <- temporal_PA_all_taxa[, sapply(temporal_PA_all_taxa, is.numeric)]

# Create each plot
p_all   <- plot_ggcorr(all_data,   labels) + ggtitle("All taxa")
p_fish  <- plot_ggcorr(fish_data,  labels) + ggtitle("Fish")
p_ceph  <- plot_ggcorr(ceph_data,  labels) + ggtitle("Cephalopods")
p_krill <- plot_ggcorr(krill_data, labels) + ggtitle("Krill")

# Use patchwork to combine the plots (2x2 grid)
final_plot <- (p_all + p_fish) / (p_ceph + p_krill) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

final_plot
