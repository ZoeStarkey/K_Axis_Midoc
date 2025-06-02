library(PerformanceAnalytics)
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)


#load the data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")

#removing the outliers deviated substantially from the majority of SST values (0.7 to 2.2°C)
km_bm_sum_2 <- km_bm_sum_2 %>% filter(SST > 0) 

#environmental performance analytics for all taxa (excluding gelatinous)
env_PA_all_taxa <- km_bm_sum_2[,c("bm_sum_all_taxa","TSM", "CUR","SST","CHLA")]
env_PA_all_taxa$bm_sum_all_taxa <- log(env_PA_all_taxa$bm_sum_all_taxa)
env_PA_all_taxa <- env_PA_all_taxa[complete.cases(env_PA_all_taxa), ]
#plot
chart.Correlation(env_PA_all_taxa, histogram=TRUE, pch=19, )

#environmental performance analytics for fish
env_PA_fish <- km_bm_sum_2[,c("bm_sum_fish","TSM", "CUR","SST","CHLA")]
env_PA_fish$bm_sum_fish <- log(env_PA_fish$bm_sum_fish)
env_PA_fish <- env_PA_fish[complete.cases(env_PA_fish), ]
#plot
chart.Correlation(env_PA_fish, histogram=TRUE, pch=19, )

#environmental performance analytics for cephalopods
env_PA_ceph <- km_bm_sum_2[,c("bm_sum_ceph","TSM", "CUR","SST","CHLA")]
env_PA_ceph$bm_sum_ceph <- log(env_PA_ceph$bm_sum_ceph)
env_PA_ceph <- env_PA_ceph[complete.cases(env_PA_ceph), ]
#plot
chart.Correlation(env_PA_ceph, histogram=TRUE, pch=19, )

#environmental performance analytics for krill
env_PA_krill <- km_bm_sum_2[,c("bm_sum_krill","TSM", "CUR","SST","CHLA")]
env_PA_krill$bm_sum_krill <- log(env_PA_krill$bm_sum_krill)
env_PA_krill <- env_PA_krill[complete.cases(env_PA_krill), ]
#plot
chart.Correlation(env_PA_krill, histogram=TRUE, pch=19, )

#############plot heatmap############
plot_ggcorr <- function(data, var_labels) {
  cor_matrix <- cor(data, use = "complete.obs")
  
  # Rename columns and rows
  colnames(cor_matrix) <- var_labels
  rownames(cor_matrix) <- var_labels
  
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
      legend.title = element_text(size = 16, hjust = 0.5, face = "bold", angle = 90),
      legend.text  = element_text(size = 16)
    )
}

# ---- Variable labels for environmental data ----
env_labels <- c("Biomass", "TSM", "Current speed", "SST", "Chl–a")

# ---- Create plots for each group ----
p_all_env   <- plot_ggcorr(env_PA_all_taxa, env_labels) + ggtitle("Total Biomass")
p_fish_env  <- plot_ggcorr(env_PA_fish,     env_labels) + ggtitle("Fish")
p_ceph_env  <- plot_ggcorr(env_PA_ceph,     env_labels) + ggtitle("Cephalopods")
p_krill_env <- plot_ggcorr(env_PA_krill,    env_labels) + ggtitle("Krill")

# ---- Combine the plots into a 2x2 grid ----
env_PA_heatmap <- (p_all_env + p_fish_env) / (p_ceph_env + p_krill_env) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

env_PA_heatmap

# Save the combined plot
ggsave("KAXIS_environmental_performance_analytics.png", 
       plot = env_PA_heatmap, 
       width = 12, height = 10, dpi = 300) 

