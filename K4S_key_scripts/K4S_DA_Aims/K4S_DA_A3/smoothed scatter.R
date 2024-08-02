TBM_scatter_aggregate <- function(data, x_var, y_var = "bm_g_m3", depth_var = "depth", aggregate_func = c("sum", "mean"), x_label = NULL, y_label = NULL, title = NULL) {
  # Exclude specified taxa
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  data_filtered <- data[!data$tax.grp %in% exclude_taxa, ]
  
  # Aggregate data by depth bins
  data_aggregated <- data_filtered %>%
    group_by_at(vars(depth_var, x_var)) %>%
    summarize(
      total_biomass = sum(!!sym(y_var), na.rm = TRUE),
      mean_biomass = mean(!!sym(y_var), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Define labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- expression(paste("Biomass g m"^"-3"))
  if (is.null(title)) title <- paste("Scatterplot of Taxa Biomass (excluding gelatinous) vs", x_var)
  
  plots <- list()
  
  # Create scatter plots for each aggregation type
  for (func in aggregate_func) {
    if (func == "sum") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "total_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +
        geom_smooth(method = "loess", se = FALSE) +  # Add smoothing line
        labs(title = paste(title, "(Sum)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["sum"]] <- p
    } else if (func == "mean") {
      p <- ggplot(data_aggregated, aes_string(x = x_var, y = "mean_biomass", color = depth_var, group = depth_var)) +
        geom_point(size = 5) +
        geom_smooth(method = "loess", se = FALSE) +  # Add smoothing line
        labs(title = paste(title, "(Mean)"),
             x = x_label,
             y = y_label,
             color = "Depth Bin") +
        theme_minimal() +
        scale_color_manual(values = c("0-200m" = "#FFD300", "200-400m" = "red", "400-600m" = "magenta", "600-800m" = "purple", "800-1000m" = "blue", "0-1000m" = "white"))
      plots[["mean"]] <- p
    }
  }
  
  return(plots)
}

# Example usage
# Define the depth bins
depth_bins <- c("0-1000m", "800-1000m", "600-800m", "400-600m", "200-400m", "0-200m")

km_df$depth <- factor(km_df$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Create scatter plots for summed and mean biomass
plots <- TBM_scatter_aggregate(km_df, "CHLA")
plots <- TBM_scatter_aggregate(km_df, "SST")
plots <- TBM_scatter_aggregate(km_df, "TSM")
plots <- TBM_scatter_aggregate(km_df, "CUR")
plots <- TBM_scatter_aggregate(km_df, "Tmin")
plots <- TBM_scatter_aggregate(km_df, "O2_min")
plots <- TBM_scatter_aggregate(km_df, "SML")
plots <- TBM_scatter_aggregate(km_df, "lunar_fraction")
plots <- TBM_scatter_aggregate(km_df, "moon_phase")
plots <- TBM_scatter_aggregate(km_df, "altitude")

# Print the plots
print(plots[["sum"]])
print(plots[["mean"]])
