usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

km_df_filtered <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
include_taxa <- c("fish")
data_filtered <- km_df_filtered[km_df_filtered$tax.grp %in% include_taxa, ]

  ggplot(km_df_filtered, aes(x = as.factor(moon_phase), y = bm_g_m3, fill = depth)) +
    geom_boxplot() +
    facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed")  +
    theme_bw() +
    xlab("Moon Phase") +
    ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
    ggtitle("Boxplot of Biomass by Depth Categories and Moon Phase") +
    scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(strip.text.y = element_text(angle = 0))
