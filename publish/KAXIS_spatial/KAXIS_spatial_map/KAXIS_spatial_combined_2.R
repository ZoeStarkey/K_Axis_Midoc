## --- global scaling helpers (put once, above the functions) ---
sf <- 190/285
SZ <- function(x) x * sf

library(patchwork)
## =========================
## TSM
## =========================
spatial_map_TSM <- function(data, biomass_column, decimal_places = 2) {
  n_bins     <- 5
  bin_range  <- range(data[[biomass_column]], na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  data <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(biomass_column))) %>%
    dplyr::mutate(
      biomass_bin = cut(!!rlang::sym(biomass_column),
                        breaks = bin_breaks, labels = bin_labels, include.lowest = TRUE)
    )
  
  data_sf <- sf::st_transform(data, crs = prj)
  n_actual_bins <- length(unique(data_sf$biomass_bin))
  
  size_values <- seq(SZ(3), SZ(8), length.out = n_actual_bins)
  fill_values <- if (n_actual_bins <= 2) c("white", "black")[1:n_actual_bins]
  else                    c("white", "grey65", "grey30", "black")[1:n_actual_bins]
  
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
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(7),
        order = 2,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggplot2::geom_sf(data = f3$finished, color = "black", linewidth = SZ(0.6)) +
    ggplot2::geom_sf(data = f1$finished, color = "black", linewidth = SZ(0.6)) +
    
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
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(4),
        order = 3,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = wcp_sf, fill = NA, color = "black") +
    ggplot2::geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = SZ(1.0)) +
    ggplot2::geom_sf(data = wp_sf, fill = "darkgrey", color = NA) +
    
    ggplot2::annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy),
                      color = "gray40", linetype = "dashed") +
    ggplot2::annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx),
                      color = "gray40", linetype = "dashed") +
    
    ggplot2::geom_sf(data = ktr_sf, size = SZ(0.6), colour = "magenta") +
    
    ggplot2::geom_sf(
      data = data_sf,
      ggplot2::aes(fill = biomass_bin, size = biomass_bin),
      shape = 21, color = "black"
    ) +
    ggplot2::scale_fill_manual(values = fill_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    ggplot2::scale_size_manual(values = size_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::coord_sf(crs = sf::st_crs(prj),
                      xlim = c(-500000, 1020000),
                      ylim = c(-1000000, 600000)) +
    ggplot2::theme(
      legend.position    = "right",
      legend.box         = "vertical",
      legend.key.height  = grid::unit(SZ(0.3), "cm"),
      legend.key.width   = grid::unit(SZ(0.3), "cm"),
      legend.text        = ggplot2::element_text(size = SZ(4)),
      legend.box.spacing = grid::unit(SZ(8), "pt"),
      legend.margin      = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.box.margin  = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      plot.margin        = ggplot2::margin(SZ(2), SZ(2), SZ(2), SZ(2), unit = "pt"),
      legend.title       = ggplot2::element_text(size = 8, angle = 90),
      panel.grid         = ggplot2::element_line(color = "gray80", linetype = "solid"),
      strip.background   = ggplot2::element_rect(fill = "white"),
      axis.title.x       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(t = SZ(10))),
      axis.title.y       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(r = SZ(10))),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      axis.text          = ggplot2::element_text(size = SZ(9))
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      ),
      size = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5), angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      
    )
  
  return(p)
}


## =========================
## CURRENTS
## =========================
spatial_map_currents <- function(data, biomass_column, decimal_places = 2) {
  n_bins     <- 5
  bin_range  <- range(data[[biomass_column]], na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  data <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(biomass_column))) %>%
    dplyr::mutate(
      biomass_bin = cut(!!rlang::sym(biomass_column),
                        breaks = bin_breaks,
                        labels = bin_labels,
                        include.lowest = TRUE)
    )
  
  data_sf <- sf::st_transform(data, crs = prj)
  n_actual_bins <- length(unique(data_sf$biomass_bin))
  
  size_values <- seq(SZ(3), SZ(8), length.out = n_actual_bins)
  fill_values <- if (n_actual_bins <= 2) c("white", "black")[1:n_actual_bins]
  else                    c("white", "grey65", "grey30", "black")[1:n_actual_bins]
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = mn_mag_df,
      ggplot2::aes(x = x, y = y, fill = value),
      alpha = 0.8,
      interpolate = TRUE
    ) +
    ggplot2::scale_fill_gradientn(
      colors = cols2(100),
      limits = c(0, 26),
      name = expression(paste("Current speed (cm ", s^-1, ")")),
      guide = ggplot2::guide_colorbar(
        title.position = "left",
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(7),
        order = 2,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
    )
    ) +
    
    ggplot2::geom_sf(data = f3$finished, color = "black", linewidth = SZ(0.6)) +
    ggplot2::geom_sf(data = f1$finished, color = "black", linewidth = SZ(0.6)) +
    
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
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(4),
        order = 3,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = wcp_sf, fill = NA, color = "black") +
    ggplot2::geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = SZ(1.0)) +
    ggplot2::geom_sf(data = wp_sf, fill = "darkgrey", color = NA) +
    
    ggplot2::annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy),
                      color = "gray40", linetype = "dashed") +
    ggplot2::annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx),
                      color = "gray40", linetype = "dashed") +
    
    ggplot2::geom_sf(data = ktr_sf, size = SZ(0.6), colour = "magenta") +
    
    ggplot2::geom_sf(
      data = data_sf,
      ggplot2::aes(fill = biomass_bin, size = biomass_bin),
      shape = 21, color = "black"
    ) +
    ggplot2::scale_fill_manual(values = fill_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    ggplot2::scale_size_manual(values = size_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::coord_sf(crs = sf::st_crs(prj),
                      xlim = c(-500000, 1020000),
                      ylim = c(-1000000, 600000)) +
    ggplot2::theme(
      legend.position    = "right",
      legend.box         = "vertical",
      legend.key.height  = grid::unit(SZ(0.3), "cm"),
      legend.key.width   = grid::unit(SZ(0.3), "cm"),
      legend.text        = ggplot2::element_text(size = 5),
      legend.box.spacing = grid::unit(SZ(8), "pt"),
      legend.margin      = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.box.margin  = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.title       = ggplot2::element_text(size = SZ(9), angle = 90),
      panel.grid         = ggplot2::element_line(color = "gray80", linetype = "solid"),
      strip.background   = ggplot2::element_rect(fill = "white"),
      axis.title.x       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(t = SZ(10))),
      axis.title.y       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(r = SZ(10))),
      axis.text          = ggplot2::element_text(size = SZ(9)),
      plot.margin        = ggplot2::margin(SZ(2), SZ(2), SZ(2), SZ(2), unit = "pt"),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5),
        override.aes = list(size = size_values)
      ),
      size = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5) ))
      return(p)
}


## =========================
## CHL-a
## =========================
spatial_map_chla <- function(data, biomass_column, decimal_places = 2) {
  n_bins     <- 5
  bin_range  <- range(data[[biomass_column]], na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  data <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(biomass_column))) %>%
    dplyr::mutate(
      biomass_bin = cut(!!rlang::sym(biomass_column),
                        breaks = bin_breaks,
                        labels = bin_labels,
                        include.lowest = TRUE)
    )
  
  data_sf <- sf::st_transform(data, crs = prj)
  n_actual_bins <- length(unique(data_sf$biomass_bin))
  
  size_values <- seq(SZ(3), SZ(8), length.out = n_actual_bins)
  fill_values <- if (n_actual_bins <= 2) c("white", "black")[1:n_actual_bins]
  else                    c("white", "grey65", "grey30", "black")[1:n_actual_bins]
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = R_df,
      ggplot2::aes(x = x, y = y, fill = value),
      alpha = 0.8
    ) +
    ggplot2::scale_fill_gradientn(
      colors = ryb,
      breaks = log_zz,
      labels = sprintf("%.2f", zz),
      limits = c(log(q1), log(q2)),
      na.value = NA,
      name = expression(paste("Chl-", italic("a"), " (mg ", m^-3, ")")),
      guide = ggplot2::guide_colorbar(
        title.position = "left",
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(7),
        order = 2,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggplot2::geom_sf(data = f3$finished, color = "black", linewidth = SZ(0.6)) +
    ggplot2::geom_sf(data = f1$finished, color = "black", linewidth = SZ(0.6)) +
    
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
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(4),
        order = 3,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = wcp_sf, fill = NA, color = "black") +
    ggplot2::geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = SZ(1.0)) +
    ggplot2::geom_sf(data = wp_sf, fill = "darkgrey", color = NA) +
    
    ggplot2::annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy),
                      color = "gray40", linetype = "dashed") +
    ggplot2::annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx),
                      color = "gray40", linetype = "dashed") +
    
    ggplot2::geom_sf(data = ktr_sf, size = SZ(0.6), colour = "magenta") +
    
    ggplot2::geom_sf(
      data = data_sf,
      ggplot2::aes(fill = biomass_bin, size = biomass_bin),
      shape = 21, color = "black"
    ) +
    ggplot2::scale_fill_manual(values = fill_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    ggplot2::scale_size_manual(values = size_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::coord_sf(crs = sf::st_crs(prj),
                      xlim = c(-500000, 1020000),
                      ylim = c(-1000000, 600000)) +
    ggplot2::theme(
      legend.position    = "right",
      legend.box         = "vertical",
      legend.key.height  = grid::unit(SZ(0.3), "cm"),
      legend.key.width   = grid::unit(SZ(0.3), "cm"),
      legend.text        = ggplot2::element_text(size = 5),
      legend.box.spacing = grid::unit(SZ(8), "pt"),
      legend.margin      = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.box.margin  = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.title       = ggplot2::element_text(size = SZ(9), angle = 90),
      panel.grid         = ggplot2::element_line(color = "gray80", linetype = "solid"),
      strip.background   = ggplot2::element_rect(fill = "white"),
      axis.title.x       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(t = SZ(10))),
      axis.title.y       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(r = SZ(10))),
      axis.text          = ggplot2::element_text(size = SZ(9)),
      plot.margin        = ggplot2::margin(SZ(2), SZ(2), SZ(2), SZ(2), unit = "pt")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      ),
      size = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    )
  
  return(p)
}


## =========================
## SST
## =========================
spatial_map_SST <- function(data, biomass_column, decimal_places = 2) {
  n_bins     <- 5
  bin_range  <- range(data[[biomass_column]], na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  data <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(biomass_column))) %>%
    dplyr::mutate(
      biomass_bin = cut(!!rlang::sym(biomass_column),
                        breaks = bin_breaks,
                        labels = bin_labels,
                        include.lowest = TRUE)
    )
  
  data_sf <- sf::st_transform(data, crs = prj)
  n_actual_bins <- length(unique(data_sf$biomass_bin))
  
  size_values <- seq(SZ(3), SZ(8), length.out = n_actual_bins)
  fill_values <- if (n_actual_bins <= 2) c("white", "black")[1:n_actual_bins]
  else                    c("white", "grey65", "grey30", "black")[1:n_actual_bins]
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = tmp_df,
      ggplot2::aes(x = Longitude, y = Latitude, fill = SST),
      alpha = 0.8
    ) +
    ggplot2::scale_fill_gradientn(
      colours = cols1,
      limits  = c(sstmin, sstmax),
      na.value = "transparent",
      name = expression(SST ~ (degree * C)),
      guide = ggplot2::guide_colorbar(
        title.position = "left",
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(7),
        order = 2,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggplot2::geom_sf(data = f3$finished, color = "black", linewidth = SZ(0.6)) +
    ggplot2::geom_sf(data = f1$finished, color = "black", linewidth = SZ(0.6)) +
    
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
        title.hjust    = 0.5,
        label.position = "right",
        barwidth  = SZ(0.7),
        barheight = SZ(4),
        order = 3,
        frame.linewidth = SZ(0.2),
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    ) +
    
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = wcp_sf, fill = NA, color = "black") +
    ggplot2::geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = SZ(1.0)) +
    ggplot2::geom_sf(data = wp_sf, fill = "darkgrey", color = NA) +
    
    ggplot2::annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy),
                      color = "gray40", linetype = "dashed") +
    ggplot2::annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx),
                      color = "gray40", linetype = "dashed") +
    
    ggplot2::geom_sf(data = ktr_sf, size = SZ(0.6), colour = "magenta") +
    
    ggplot2::geom_sf(
      data = data_sf,
      ggplot2::aes(fill = biomass_bin, size = biomass_bin),
      shape = 21, color = "black"
    ) +
    ggplot2::scale_fill_manual(values = fill_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    ggplot2::scale_size_manual(values = size_values,
                               name = expression(paste("Biomass (g m"^-3, ")"))) +
    
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::coord_sf(crs = sf::st_crs(prj),
                      xlim = c(-500000, 1020000),
                      ylim = c(-1000000, 600000)) +
    ggplot2::theme(
      legend.position    = "right",
      legend.box         = "vertical",
      legend.key.height  = grid::unit(SZ(0.3), "cm"),
      legend.key.width   = grid::unit(SZ(0.3), "cm"),
      legend.text        = ggplot2::element_text(size = 5),
      legend.margin      = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.box.margin  = ggplot2::margin(SZ(0), SZ(0), SZ(0), SZ(0)),
      legend.box.spacing = grid::unit(SZ(8), "pt"),
      legend.title       = ggplot2::element_text(size = 6, angle = 90),
      panel.grid         = ggplot2::element_line(color = "gray80", linetype = "solid"),
      strip.background   = ggplot2::element_rect(fill = "white"),
      axis.title.x       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(t = SZ(10))),
      axis.title.y       = ggplot2::element_text(size = SZ(12), margin = ggplot2::margin(r = SZ(10))),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      axis.text          = ggplot2::element_text(size = 6),
      plot.margin        = ggplot2::margin(SZ(6), SZ(2), SZ(4), SZ(2), unit = "pt"),
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5), angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      ,
      size = ggplot2::guide_legend(
        title.position = "left", title.hjust = 0.5, order = 1,
        title.theme = ggplot2::element_text(size = 5, angle = 90),
        label.theme = ggplot2::element_text(size = 5)
      )
    )
  
  return(p)
}

tsm_spatial_plot_fish <- spatial_map_TSM(km_bm_sum_2, "bm_sum_fish", decimal_places = 3)
current_spatial_plot_fish <- spatial_map_currents(km_bm_sum_2, "bm_sum_fish", decimal_places = 3)
chla_spatial_plot_fish <- spatial_map_chla(km_bm_sum_2, "bm_sum_fish", decimal_places = 3)
SST_spatial_plot_fish <- spatial_map_SST(km_bm_sum_2, "bm_sum_fish", decimal_places = 3)

combined <- (
  SST_spatial_plot_fish | current_spatial_plot_fish
) /
  (
    chla_spatial_plot_fish | tsm_spatial_plot_fish
  ) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 8, face = "bold"),
    plot.margin = margin(t = -2, r = 4, b = -2, l = 4, unit = "pt") # add this line
  )

#combin

ggsave(
  filename = "~/Desktop/combined_spatial_plots_23_08.tiff",
  plot = combined,
  width = 190,                      # mm
  height = 170,                # ≈ 160 mm
  units = "mm",
  dpi = 600,
  compression = "lzw"
)


#TESTING COMBINED
combined <- (
  SST_spatial_plot_fish | current_spatial_plot_fish
) /
  (
    chla_spatial_plot_fish | tsm_spatial_plot_fish
  ) +
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.margin = margin(0,0,0,0))) &   # no outer padding
  theme(
    plot.tag.position = c(0.1, 1.05),   # inside top-left (x,y in [0,1])
    plot.tag = element_text(size = 8, face = "bold", margin = margin(0,0,0,0))
  )

ggsave(
  filename = "~/Desktop/combined_spatial_plots_24_08.tiff",
  plot = combined,
  width = 190,                      # mm
  height = 170,                # ≈ 160 mm
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

#TO DO: 
#make titles bigger 
#make labels smaller for CHLA 
