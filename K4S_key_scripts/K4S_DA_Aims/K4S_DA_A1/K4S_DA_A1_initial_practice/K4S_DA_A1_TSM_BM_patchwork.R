# Load necessary libraries
library(sf)
library(dplyr)
library(ggplot2)
library(stars)
library(cmocean)
library(ggnewscale)
library(patchwork)

# Define a function to create plots for different taxa without midoc.stn labels
create_taxon_plot <- function(km_sf, taxon) {
  km_taxon <- km_sf %>%
    filter(tax.grp == taxon)
  
  km_sf_total_taxon <- km_taxon %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  ggplot() +
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total_taxon, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")) )+ 
    scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
    
    #scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3",")")),
     #                    guide = guide_colourbar(order = 1)) +
    #scale_size_continuous(name = expression(paste("Total Biomass m"^"-3",")")),
      #                    range = c(1, 10), guide = guide_legend(order = 1)) +
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(crs = st_crs(prj), xlim = c(-1000000, 1000000), ylim = c(-1000000, 600000)) +
    theme(
      legend.position = "right",
      panel.grid = element_line(color = "gray80", linetype = "solid"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(angle = 90, hjust = 0.5),
      legend.box.background = element_blank(),
      legend.byrow = TRUE,
      strip.background = element_rect(fill = NA)
    )
}

# Create plots for each taxon
fish_plot <- create_taxon_plot(km_sf, "fish")
krill_plot <- create_taxon_plot(km_sf, "krill")
cnidarians_plot <- create_taxon_plot(km_sf, "cnidarians")
salps_plot <- create_taxon_plot(km_sf, "salps")
cephalopods_plot <- create_taxon_plot(km_sf, "cephalopods")

# Use patchwork to combine the plots
combined_plot <- (fish_plot + krill_plot + cnidarians_plot + salps_plot + cephalopods_plot) +
  plot_layout(guides = "collect") 

# Display the combined plot
print(combined_plot)



 #Questions: 
#how do I get it to only be one legend 
#how do i get the colours and sizes to be consistent across all the plots 
plot(fish_plot)

