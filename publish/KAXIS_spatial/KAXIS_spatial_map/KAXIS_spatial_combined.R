spatial_map_TSM <- function(data, biomass_column, decimal_places = 2) {
  # Calculate bin breaks
  n_bins <- 5
  bin_range  <- range(data[[biomass_column]], na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  # Labels
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  # Filter and bin
  data <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(biomass_column))) %>%
    dplyr::mutate(
      biomass_bin = cut(!!rlang::sym(biomass_column),
                        breaks = bin_breaks, labels = bin_labels, include.lowest = TRUE)
    )
  
  data_sf <- sf::st_transform(data, crs = prj)
  n_actual_bins <- length(unique(data_sf$biomass_bin))
  
  # ONE source of truth for bubble sizes (map + legend)
  size_values <- seq(3, 8, length.out = n_actual_bins)
  
  # Colours
  fill_values <- if (n_actual_bins <= 2) {
    c("white", "black")[1:n_actual_bins]
  } else {
    c("white", "grey65", "grey30", "black")[1:n_actual_bins]
  }
  
  # Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = wcp_sf, fill = NA) +
    stars::geom_stars(
      data = tsm_e_ll,
      aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))),
      alpha = 0.8
    ) +
    cmocean::scale_fill_cmocean(
      name = "curl", na.value = NA,
      guide = ggplot2::guide_colorbar(
        title = "Days",
        title.position = "left",
        title.hjust = 0.5,
        label.position = "right",
        barwidth  = 0.7,  # thinner
        barheight = 9,    # shorter
        order = 5,
        frame.linewidth = 0.2,
        title.theme = ggplot2::element_text(size = 8, angle = 90),
        label.theme = ggplot2::element_text(size = 7)
      )
    ) +
    
    # Fronts
    ggplot2::geom_sf(data = f3$finished, color = "black", linewidth = 1) +
    ggplot2::geom_sf(data = f1$finished, color = "black", linewidth = 1) +
    
    # Ice
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data = ice_df,
      ggplot2::aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218),
      alpha = 1
    ) +
    ggplot2::scale_fill_gradientn(
      colors = palr::bathy_deep_pal(56),
      na.value = "transparent",
      limits = c(0, 100),
      name = "Ice (%)",
      guide = ggplot2::guide_colorbar(
        title.position = "left",
        title.hjust = 0.5,
        label.position = "right",
        barwidth  = 0.7,
        barheight = 4,
        order = 3,
        frame.linewidth = 0.2,
        title.theme = ggplot2::element_text(size = 8, angle = 90),
        label.theme = ggplot2::element_text(size = 7)
      )
    ) +
    
    # Geographic features
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = wcp_sf, fill = NA, color = "black") +
    ggplot2::geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
    ggplot2::geom_sf(data = wp_sf, fill = "darkgrey", color = NA) +
    
    # Grid lines
    ggplot2::annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy),
                      color = "gray40", linetype = "dashed") +
    ggplot2::annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx),
                      color = "gray40", linetype = "dashed") +
    
    # KTR line
    ggplot2::geom_sf(data = ktr_sf, size = 1, colour = "magenta") +
    
    # Biomass points (legend will match these sizes)
    ggplot2::geom_sf(
      data = data_sf,
      ggplot2::aes(fill = biomass_bin, size = biomass_bin),
      shape = 21, color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      name = expression(paste("Biomass (g m"^-3, ")"))
    ) +
    ggplot2::scale_size_manual(
      values = size_values,
      name = expression(paste("Biomass (g m"^-3, ")"))
    ) +
    
    # Formatting
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::coord_sf(crs = sf::st_crs(prj),
                      xlim = c(-500000, 1020000),
                      ylim = c(-1000000, 600000)) +
    ggplot2::theme(
      legend.position   = "right",
      legend.box        = "vertical",
      legend.key.height = grid::unit(0.3, "cm"),
      legend.key.width  = grid::unit(0.3, "cm"),
      legend.text       = ggplot2::element_text(size = 4),
      legend.box.spacing = unit(0, "pt"),     # gap between panel and legend
      legend.margin      = margin(0,0,0,0),   # padding inside legend area
      legend.box.margin  = margin(0,0,0,0),
      plot.margin        = margin(2, 2, 2, 2, unit = "pt"),
      legend.title      = ggplot2::element_text(size = 8, angle = 90),
      panel.grid        = ggplot2::element_line(color = "gray80", linetype = "solid"),
      strip.background  = ggplot2::element_rect(fill = "white"),
      axis.title        = ggplot2::element_text(size = 10),
      panel.background = element_rect(fill = "white", colour = NA),
      axis.text         = ggplot2::element_text(size = 9)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        order = 1,
        title.theme = ggplot2::element_text(size = 8, angle = 90),
        label.theme = ggplot2::element_text(size = 7)
      ),
      size = ggplot2::guide_legend(
        title.position = "left",
        title.hjust = 0.5,
        order = 1,
        title.theme = ggplot2::element_text(size = 8, angle = 90),
        label.theme = ggplot2::element_text(size = 7)
      )
    )
  
  return(p)
}


tsm_spatial_plot_fish <- spatial_map_TSM(km_bm_sum_2, "bm_sum_fish", decimal_places = 3)

#tsm_spatial_plot_fish

# Combine into 2x2 with panel labels
combined <- (
  tsm_spatial_plot_fish| tsm_spatial_plot_fish
) /
  (
    tsm_spatial_plot_fish | tsm_spatial_plot_fish
  ) +
  plot_annotation(
    tag_levels = "A"
  )
#combined

ggsave(
  filename = "~/Desktop/combined_spatial_plots2.tiff",
  plot = combined,
  width = 285,      # mm (full double-column)
  height = 270,     # mm (square-ish for 2x2 layout)
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

