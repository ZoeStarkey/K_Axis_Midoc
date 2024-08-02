# # Define the number of bins
# num_bins <- 5
# 
# # Calculate equal-width breaks
min_value <- min(km_sf_other$other_biomass, na.rm = TRUE)
max_value <- max(km_sf_other$other_biomass, na.rm = TRUE)
breaks <- seq(min_value, max_value, length.out = num_bins + 1)

# Format breaks for labels
formatted_breaks <- format(breaks, digits = 2, nsmall = 2) # DECIMALS
#formatted_breaks <- formatC(breaks, format = "e", digits = 2) #SCIENTIFIC NOTATION
labels <- paste0("(", head(formatted_breaks, -1), " - ", tail(formatted_breaks, -1), "]")

# Bin the data
km_sf_other$binned_biomass <- cut(km_sf_other$other_biomass, breaks = breaks, labels = labels, include.lowest = TRUE, include.highest = TRUE)




create_excluded_taxa_plot <- function(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj) {
  km_sf <- st_as_sf(km)
  
  # List of taxa to exclude
  exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
  
  # Filter data for taxa not in the exclude list and aggregate biomass
  km_sf_other <- km_sf %>%
    filter(!tax.grp %in% exclude_taxa) %>%
    group_by(midoc.stn) %>%
    summarize(
      other_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Bin the biomass into 5 equal sections
  # breaks <- quantile(km_sf_other$other_biomass, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  # formatted_breaks <- format(breaks, digits = 2, nsmall = 2) # DECIMALS
  # #formatted_breaks <- formatC(breaks, format = "e", digits = 2) #SCIENTIFIC NOTATION
  # labels <- paste0("(", head(formatted_breaks, -1), " - ", tail(formatted_breaks, -1), "]")
  # km_sf_other$binned_biomass <- cut(km_sf_other$other_biomass, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  
  # Define the number of bins
  num_bins <- 5
  
  # Calculate equal-width breaks
  min_value <- min(km_sf_other$other_biomass, na.rm = TRUE)
  max_value <- max(km_sf_other$other_biomass, na.rm = TRUE)
  breaks <- seq(min_value, max_value, length.out = num_bins + 1)
  
  # Format breaks for labels
  formatted_breaks <- format(breaks, digits = 2, nsmall = 2) # DECIMALS
  #formatted_breaks <- formatC(breaks, format = "e", digits = 2) #SCIENTIFIC NOTATION
  labels <- paste0("(", head(formatted_breaks, -1), " - ", tail(formatted_breaks, -1), "]")
  
  # Bin the data
  km_sf_other$binned_biomass <- cut(km_sf_other$other_biomass, breaks = breaks, labels = labels, include.lowest = TRUE, include.highest = TRUE)

  # Ensure factor levels are set and maintained
  km_sf_other$binned_biomass <- factor(km_sf_other$binned_biomass, levels = labels)
  
  # Define a color palette for the bins
  color_palette <- c("white", "grey80","grey40", "grey20", "black") 
  
  p <- ggplot() +
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    
    # Add ice
    ggnewscale::new_scale_fill() + 
    geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
    scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100)) +
    labs(fill = 'Ice (%)') +
    
    # Add the zoomed-in countries layer
    ggnewscale::new_scale_fill() + 
    geom_sf(data = wcp_sf, fill = NA, color = "black") +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_other, aes(fill = binned_biomass, size = binned_biomass), shape = 21, color = "black") +
    
    # Use a discrete scale for fill based on the bins
    scale_fill_manual(
      values = color_palette,
      name = expression(paste("Other Taxa Biomass g m"^"-3")),
      drop = FALSE,  # Ensure all factor levels are shown in the legend
      labels = labels
    ) +
    
    
    # Use a discrete scale for size based on the bins
  scale_size_manual(
  values = seq(4, 20),  # Increase the range for larger bubbles
  name = expression(paste("Other Taxa Biomass g m"^"-3")),
  drop = FALSE,  # Ensure all factor levels are shown in the legend
  labels = labels
) +
    
    labs(x = "Longitude", y = "Latitude") +
    
    # Projection
    coord_sf(crs = st_crs(prj), xlim = c(-550000, 1000000), ylim = c(-1000000, 600000)) +
    theme(
      legend.position = "right",
      panel.grid = element_line(color = "gray80", linetype = "solid"),
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(angle = 90, hjust = 0.5),
      legend.box.background = element_blank(),
      legend.byrow = TRUE,
      strip.background = element_rect(fill = NA)
    ) +
    guides(fill = guide_legend(title.position = "left", title.hjust = 0.5),
           size = guide_legend(title.position = "left", title.hjust = 0.5))
  
  return(p)
}

#Plotting 
excluded_taxa_plot <- create_excluded_taxa_plot(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
print(excluded_taxa_plot)


# Specify the path where you want to save the plot
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_TSM")
output_filename <- "K4S_Plot_A1_TSM_No_Gelat.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = excluded_taxa_plot, width =7, height = 8, dpi = 300)
