library(ggplot2)
library(RColorBrewer)
library(ggplot2)
library(stars)
library(sf)
library(ggnewscale)
library(orsifronts)
library(rworldxtra)
library(rworldmap)
library(viridis)
library(readr)
library(dplyr)
library(cmocean) 
library(colorspace)
library(stars)
library(raster)
library(scales)
library(gridExtra)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}
ofp<- spTransform(orsifronts, CRS(prj))
ofp_sf <- st_as_sf(ofp)
# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

##full SO extent
ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely
data(package = "rworldmap")
data(countriesHigh) 
wp  <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj))
wp_sf <- st_as_sf(wp)

ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40) #
wc <- crop(countriesHigh, ras.ext)
wcp <-spTransform(wc, CRS(prj))
wcp_sf <- st_as_sf(wcp)

ktr <- readRDS("../derived data/nav_reduced.rds")
ktr <- ll2prj(ktr, loncol="LONGITUDE", latcol="LATITUDE")
ktr_sf <- st_as_sf(ktr)

cbathy <- raster(paste0("/Users/zoestarkey/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/source data/bathy.tif"))
# lines
cbc   <- rasterToContour(cbathy, levels=c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0))
cbct  <- spTransform(cbc, CRS(prj))
# for filled
e <- extent(-5411853 , 6235554, -1577161,  1358628)
e <- extent(-5411853 , 6235554, -1577161,  2000000)

cropped_bathy <- crop(cbathy, e)

pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))
bathy_df <- as.data.frame(pbathy, xy = TRUE)
colnames(bathy_df) <- c("x", "y", "value")



load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/ice_df.rda")

km <- readRDS("../derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
tmp <- read_csv("../source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("../source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)
tmp <- readRDS("../derived data/codend_taxa_biomass.rds")
km <- inner_join(km, tmp); rm(tmp)
file_path <- "../derived data/midoc_stations_envdata.rda" #adding zone
md <- readRDS(file_path)
#ZS: adding zones to km dataset from md dataset
km <- merge(km, md[, c("midoc.stn", "zone")], by = "midoc.stn") 
km <- ll2prj(km, loncol="lon_start", latcol="lat_start")

km_sf <- st_as_sf(km)

#Define the stations to remove
abandoned_stations <- c("MIDOC02","MIDOC08", "MIDOC10", "MIDOC12", "MIDOC33")

# Remove the specified stations from km_df
km_sf <- km_sf %>%
  filter(!midoc.stn %in% abandoned_stations)

exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")
# 
#Filter data for taxa not in the exclude list and aggregate biomass
km_sf_total <- km_sf %>%
  filter(!tax.grp %in% exclude_taxa) %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    lon_end = first(lon_end),
    lat_end = first(lat_end)
  )

# Calculate bin breaks for biomass 
n_bins <- 5  # can adjust this number for more or fewer bins
bin_range <- range(km_sf_total$total_biomass, na.rm = TRUE)
bin_breaks <- pretty(bin_range, n = n_bins)

# Create labels with exact ranges, using two decimal places
bin_labels <- paste0(
  sprintf("%.2f", bin_breaks[-length(bin_breaks)]),
  " - ",
  sprintf("%.2f", bin_breaks[-1])
)

# Modify the mutate step in km_sf_total (if not already done)
km_sf_total <- km_sf_total %>%
  mutate(biomass_bin = cut(total_biomass, 
                           breaks = bin_breaks,
                           labels = bin_labels,
                           include.lowest = TRUE))
#creating another column in km_sf_total with the midoc number 
km_sf_total$midoc.n <- as.numeric(substr(km_sf_total$midoc.stn, 6,7))


km_sf_total <- km_sf_total %>%
  mutate(coords = st_transform(geometry, crs = st_crs(prj))) %>%
  mutate(x = st_coordinates(coords)[,1],
         y = st_coordinates(coords)[,2])

#adding fronts 
#loading KAXIS_FEATURESpoly_five.RData
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_FEATURESpoly_five.RData")
summary(f3)

# Remove the old CRS
sf::st_crs(f3$finished) <- NA

# Set the new CRS
sf::st_crs(f3$finished) <- 4326  # Assuming the new CRS is EPSG code 4326 (WGS 84)



#loading KAXIS_FEATURESpoly_extras.RData
extra <- load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_FEATURESpoly_extras.RData")
summary(f1)

# Remove the old CRS
sf::st_crs(f1$finished) <- NA

# Set the new CRS
sf::st_crs(f1$finished) <- 4326  #


cbct_sf <- st_as_sf(cbct)


library(sf)
bathy <- 
ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9, "Blues")), 
                       breaks = c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,0),
                       limits = c(-8000, 0),
                       na.value = "grey85",
                       name = "Bathymetry (m)",
                       guide = guide_colorbar(title.position = "left",
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 14,
                                              order = 2,
                                              frame.linewidth = 0.2,
                                              title.theme = element_text(size = 10, angle = 90),
                                              label.theme = element_text(size = 8)))   +
  geom_sf(data = cbct_sf, aes(), color = "grey60", size = 0.5, alpha = 0.7) +
  # Add f3$finished and f1$finished 
  geom_sf(data = f3$finished, color = "purple", linewidth = 0.75) +
  geom_sf(data = f1$finished, color = "purple", linewidth = 0.75) +
  
  # Add ice layer
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 1) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
                       name = 'Ice (%)',
                       guide = guide_colorbar(title.position = "left", 
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 10,
                                              order = 3,
                                              frame.linewidth = 0.2,
                                              title.theme = element_text(size = 10, angle = 90),
                                              label.theme = element_text(size = 8))) +
 
  
  #adding midoc stations
  ggnewscale::new_scale_fill() +
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  geom_sf(data = ofp_sf, color = "darkblue", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1, colour = "black") +
  ggnewscale::new_scale_fill() +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA)  +
  geom_sf(data = km_sf_total, aes(fill = biomass_bin), shape = 21, color = "black", size = 6, show.legend = FALSE) +
  geom_sf_text(data = km_sf_total, aes(label = midoc.n), 
               size = 2.5, color = "white", fontface = "bold") +
 # geom_text(data = km_sf_total, aes(x = lon_end, y = lat_end, label = midoc.n), size = 3, hjust = 0.5, vjust = 1) +
  scale_fill_manual(
    values = c("grey40", "grey40", "grey40", "grey40"),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  scale_size_manual(
    values = c(7.5, 7.5,7.5, 7.5),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  
  # guides(
  #   fill = guide_legend(
  #     title.position = "left", 
  #     title.hjust = 0.5,
  #    override.aes = list(size = c(8, 8, 8, 8)),
  #     order = 1,
  #     title.theme = element_text(size = 14, angle = 90),
  #     keywidth = unit(1, "cm"),
  #     keyheight = unit(1, "cm"),
  #     default.unit = "cm",
  #     background = element_rect(fill = "white", colour = "white")
  #   ),
  #   size = "none"  ) +
  
  # Add labels and set coordinate system
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-900000, 1000000), ylim = c(-1000000, 1200000)) +
  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5, size = 10),
    legend.text = element_text(size = 10),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 12),  # Increased axis title size
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.margin = margin(t = -5, r = 10, b = -10, l = 10, unit = "pt")
  ) 

#28/08/2025
ggsave(
  filename = "~/Desktop/bahty_plot.tiff",
  plot = bathy ,
  width = 190,      # mm (full double-column)
  height = 190,     # mm (square-ish for 2x2 layout)
  units = "mm",
  dpi = 500,
  compression = "lzw"
)


output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Bathy")
output_filename <- "K4S_Plot_A1_bathy.tiff"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = bathy, width = 9, height = 8, bg = "white")

output_directory <- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Bathy")
output_filename  <- "K4S_Plot_A1_bathy.tiff"
full_output_path <- file.path(output_directory, output_filename)


# install.packages("ragg")  # if needed
library(ragg)

output_directory <- paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Bathy")
full_output_path <- file.path(output_directory, "K4S_Plot_A1_bathy_fullpage_500dpi.tiff")


w_px <- 3740
h_px <- round(w_px * (1820/2048))  # maintain same aspect ≈ 3322 px

ggsave(
  filename = file.path(output_directory, "K4S_Plot_A1_bathy_fullpage.png"),
  plot     = bathy,
  device   = ragg::agg_png,
  units    = "px", width = w_px, height = h_px,
  res      = 500,
  bg       = "white"
)

ggsave(
  filename = file.path(output_directory, "K4S_Plot_A1_bathy_fullpage.tiff"),
  plot     = bathy,
  device   = ragg::agg_tiff,
  units    = "px", width = w_px, height = h_px,
  res      = 500, compression = "lzw",
  bg       = "white"
)





###########


bathy_grey <- 
ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
                       colors = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greys")[1:7]))(100),
                       breaks = c(-6000,-5000,-4000,-3000,-2000,-1000,0),
                       limits = c(-6000, 0),
                       na.value = "grey85",
                       name = "Depth (m)",
                       guide = guide_colorbar(title.position = "top",
                                              title.hjust = 0.1 ,
                                              title.vjust = 3,
                                             # label.position = "right",
                                              barwidth = 1,
                                              barheight = 25,
                                              order = 1,
                                              frame.linewidth = 0.2,
                                              ticks.colour = "black",
                                              title.theme = element_text(size = 20, angle = 0),
                                              label.theme = element_text(size = 18)))   +
  geom_sf(data = cbct_sf, aes(), color = "grey60", size = 0.5, alpha = 0.7) +
  # Add f3$finished and f1$finished 
  #geom_sf(data = f3$finished, color = "purple", linewidth = 1) +
 # geom_sf(data = f1$finished, color = "purple", linewidth = 1) +
  
  # Add ice layer
  # ggnewscale::new_scale_fill() + 
  # geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 1) +
  # scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
  #                      name = 'Ice (%)',
  #                      guide = guide_colorbar(title.position = "left", 
  #                                             title.hjust = 0.5,
  #                                             label.position = "right",
  #                                             barwidth = 1,
  #                                             barheight = 15,
  #                                             order = 3,
  #                                             frame.linewidth = 0.2,
  #                                             title.theme = element_text(size = 14, angle = 90),
  #                                             label.theme = element_text(size = 14))) +
  # 
  
  #adding midoc stations
  ggnewscale::new_scale_fill() +
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
 # geom_sf(data = ofp_sf, color = "darkblue", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 0.3, colour = "black") +
  ggnewscale::new_scale_fill() +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA)  +
  geom_sf(data = km_sf_total, aes(fill = biomass_bin, size = biomass_bin), shape = 21, color = "black", show.legend = FALSE) +
  # geom_sf_text(data = km_sf_total, aes(label = midoc.n), 
  #              size = 2.5, color = "white", fontface = "bold") +
  # geom_text(data = km_sf_total, aes(x = lon_end, y = lat_end, label = midoc.n), size = 3, hjust = 0.5, vjust = 1) +
  scale_fill_manual(
    values = c("black", "black", "black", "black"),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  scale_size_manual(
    values = c(4,4,4,4),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  
  # guides(
  #   fill = guide_legend(
  #     title.position = "left", 
  #     title.hjust = 0.5,
  #    override.aes = list(size = c(8, 8, 8, 8)),
  #     order = 1,
  #     title.theme = element_text(size = 14, angle = 90),
  #     keywidth = unit(1, "cm"),
  #     keyheight = unit(1, "cm"),
  #     default.unit = "cm",
  #     background = element_rect(fill = "white", colour = "white")
  #   ),
  #   size = "none"  ) +
  
  # Add labels and set coordinate system
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-1000000, 1000000), ylim = c(-1000000, 1200000)) +
  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.text = element_text(size = 25),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 20),  # Increased axis title size
    axis.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  ) 

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Bathy")
output_filename <- "K4S_Plot_A1_bathy_grey_scale.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = bathy_grey, width = 8, height = 11, bg = NA)



#BATHY Lunar fraction
km_sf_total_df <- km_sf_total %>% st_drop_geometry()

# Now, join km_sf_total_df with km_bm_sum
km_sf_total_df_joined <- km_sf_total_df %>%
  left_join(km_bm_sum %>% select(midoc.stn, lunar_fraction), by = "midoc.stn")

# Convert back to an sf object
km_sf_total_updated <- st_sf(km_sf_total_df_joined, geometry = st_geometry(km_sf_total))

bathy_lunar <-
ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Blues")[1:7]))(100),
    breaks = c(-6000,-5000,-4000,-3000,-2000,-1000,0),
    limits = c(-6000, 0),
    na.value = "grey85",
    name = "Depth (m)",
    guide = guide_colorbar(title.position = "top",
                           title.hjust = 0.1,
                           title.vjust = 3,
                           barwidth = 1,
                           barheight = 20,
                           order = 1,
                           frame.linewidth = 0.2,
                           ticks.colour = "black",
                           title.theme = element_text(size = 20, angle = 0),
                           label.theme = element_text(size = 18))) +
  geom_sf(data = cbct_sf, aes(), color = "grey60", size = 0.5, alpha = 0.7) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 0.3, colour = "black") +
  ggnewscale::new_scale_fill() +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  geom_sf(data = km_sf_total_updated, aes(fill = lunar_fraction, size = biomass_bin), shape = 21, color = "black") +
  scale_fill_gradientn(
    colors = c("black", "white"),
    limits = c(0, 1),
    na.value = "grey50",
    name = "Lunar Phase",
    guide = "none"
    # guide = guide_colorbar(title.position = "left",
    #                        title.hjust = 0.5,
    #                        barwidth = 1,
    #                        barheight = 15,
    #                        frame.linewidth = 0.2,
    #                        title.theme = element_text(size = 14, angle = 90),
    #                        label.theme = element_text(size = 14))
  ) +
  scale_size_manual(
    values = c(6,6,6,6),
    guide = "none"  # This removes the legend for biomass
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-600000, 1200000), ylim = c(-1000000, 700000)) +
  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.text = element_text(size = 25),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )

output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Bathy")
output_filename <- "K4S_Plot_A1_bathy_lunar.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = bathy_lunar, width = 8, height = 11, bg = NA)
# Save the plot as a TIFF file
