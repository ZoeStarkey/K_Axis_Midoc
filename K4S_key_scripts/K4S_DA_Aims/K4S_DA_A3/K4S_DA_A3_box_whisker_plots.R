usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

km_df_filtered <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
km_df_filtered <-  km_df_filtered[km_df_filtered$tax.grp %in% exclude_taxa, ]



include_taxa <- c("cephalopods")
km_df_filtered <- km_df_filtered[km_df_filtered$tax.grp %in% include_taxa, ]


#CHLA
library(dplyr)
library(ggplot2)

# Remove rows with NA values in CHLA and format CHLA to 1 decimal place
km_df_filtered <- km_df_filtered %>%
  filter(!is.na(CHLA)) %>%
  mutate(CHLA_formatted = sprintf("%.2f", CHLA),
         CHLA_formatted = as.numeric(CHLA_formatted))

ggplot(km_df_filtered, aes(x = as.factor(CHLA_formatted), y = bm_g_m3, fill = depth)) +
  geom_boxplot() +
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
  theme_bw() +
  xlab("CHLA") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and Chlorophyll") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


#SST
library(dplyr)
library(ggplot2)

km_df_filtered <- km_df_filtered %>%
  filter(!is.na(SST)) %>%
  mutate(SST_formatted = sprintf("%.2f", SST),
         SST_formatted = as.numeric(SST_formatted))

ggplot(km_df_filtered, aes(x = as.factor(SST_formatted), y = bm_g_m3, fill = depth)) +
  geom_boxplot() +
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
  theme_bw() +
  xlab("SST") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and SST") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )



create_equal_width_boxplot <- function(data, env_var, response_var, depth_var, num_bins = 5) {
  
  # Ensure the environmental variable is numeric
  if (!is.numeric(data[[env_var]])) {
    stop("The environmental variable must be numeric.")
  }
  
  # Create equal-width bins
  breaks <- seq(min(data[[env_var]], na.rm = TRUE), max(data[[env_var]], na.rm = TRUE), length.out = num_bins + 1)
  labels <- paste0(sprintf("%.2f", head(breaks, -1)), "-", sprintf("%.2f", tail(breaks, -1)))
  data$env_binned <- cut(data[[env_var]], breaks = breaks, include.lowest = TRUE, labels = labels)
  
  # Create the boxplot
  plot <- ggplot(data, aes_string(x = "env_binned", y = response_var, fill = depth_var)) +
    geom_boxplot() +
    facet_wrap(as.formula(paste("~ reorder(", depth_var, ", desc(", depth_var, "))")), ncol = 1, scales = "fixed") +
    theme_bw() +
    xlab(paste("Binned", env_var)) +
    ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
    ggtitle(paste("Boxplot of", response_var, "by Depth Categories and Binned", env_var)) +
    scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(strip.text.y = element_text(angle = 0))
  
  return(plot)
}

# Example usage
plot_CHLA_equal_width <- create_equal_width_boxplot(km_df_filtered, "CHLA_formatted", "bm_g_m3", "depth", num_bins = 5)
print(plot_CHLA_equal_width)




create_equal_width_boxplot(km_df_filtered, "SST_formatted", "bm_g_m3", "depth", num_bins = 5)
print(plot_SST_equal_width)








# Plot with binned CUR values
ggplot(km_df_filtered, aes(x = CUR_binned, y = bm_g_m3, fill = depth)) +
  geom_boxplot() +
  facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed")  +
  theme_bw() +
  xlab("Binned CUR") +
  ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
  ggtitle("Boxplot of Biomass by Depth Categories and Binned Current") +
  scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(strip.text.y = element_text(angle = 0))



#MOON PHASE
  ggplot(km_df_filtered, aes(x = as.factor(moon_phase), y = bm_g_m3, fill = depth)) +
    geom_boxplot() +
    facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed")  +
    theme_bw() +
    xlab("Moon Phase") +
    ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
    ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and Moon Phase") +
    scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(strip.text.y = element_text(angle = 0))
  
  
  #SST 
  km_df_filtered <- km_df_filtered %>%
    filter(!is.na(CHLA)) %>%
    mutate(CHLA_formatted = sprintf("%.2f", CHLA))
  
  ggplot(km_df_filtered, aes(x = as.factor(CHLA_formatted), y = bm_g_m3, fill = depth)) +
    geom_boxplot() +
    facet_wrap(~ reorder(depth, desc(depth)), ncol = 1, scales = "fixed") +
    theme_bw() +
    xlab("CHLA") +
    ylab(expression(paste("Biomass (g m"^"-3", ")"))) +
    ggtitle("Boxplot of Biomass excluding gelatinous by Depth Categories and Chlorophyll") +
    scale_fill_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(
      strip.text.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  

  

  summary(km_df_filtered$SST)
  
  
  binned_summary <- km_df_filtered %>%
    filter(!is.na(SST)) %>%
    mutate(SST_binned = cut(SST, breaks = seq(floor(min(SST, na.rm = TRUE)), ceiling(max(SST, na.rm = TRUE)), length.out = 6), include.lowest = TRUE)) %>%
    count(SST_binned)
  
  binned_summary
  
  

 

