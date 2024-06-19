#Projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}

# Fronts
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

# reading ktr 
ktr <- readRDS("../derived data/nav_reduced.rds")
ktr <- ll2prj(ktr, loncol="LONGITUDE", latcol="LATITUDE")
ktr_sf <- st_as_sf(ktr)

#Setting up km 
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

#Aggreating biomass data by midoc stqtion 
km_sf <- st_as_sf(km)
km_sf_total <- km_sf %>% # Aggregate biomass data by midoc station 
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    lon_end = first(lon_end),
    lat_end = first(lat_end)
  )
# Load your raster data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_curr_big.RData")

# Crop the rasters to the specified extent
mag <- crop(mag, extent(bx + c(-3, 3, -3, 3)), snap = "out")

# Calculate mn_mag
mn_mag <- max(mag)
mn_mag[mn_mag > 0.25] <- 0.255
mn_mag[mn_mag < 0.025] <- 0.025
mn_mag <- mn_mag * 100  # scale values as in the base plot

#make mn_mag into a dataframe 
mn_mag_df <- as.data.frame(mn_mag, xy = TRUE)
colnames(mn_mag_df) <- c("x", "y", "value")

#reProjecting 
R_projected <- projectRaster(mn_mag, crs = prj)
mn_mag_df <- as.data.frame(R_projected, xy = TRUE)
colnames(mn_mag_df) <- c("x", "y", "value")



#colours
cols2 <- colorRampPalette(c("lightyellow","#EEDC94" ,"orange", "red", "darkred"))




ggplot() +
  geom_raster(data = mn_mag_df, aes(x = x, y = y, fill = value), interpolate = TRUE) +
  scale_fill_gradientn(colors = cols2(100), limits = c(0, 26), name = expression(paste("cm ", s^-1))) +
  ggnewscale::new_scale_fill() +  # Add new_scale_fill before adding new fill layers
  # Add the zoomed-in countries layer
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  # Add the ofp layer
  geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1) +
  geom_sf(data = km_sf_total, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")),
                       breaks = pretty_breaks(5))+ 
  #scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
  scale_size_binned(name = expression(paste("Total Biomass m"^"-3")),
                    range = c(0,10),
                    breaks = pretty_breaks(5),
                    transform = "exp",
                    nice.breaks = F) +
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
  ) +
  guides(fill = guide_legend(title.position = "left", title.hjust = 0.5),
         size = guide_legend(title.position = "left", title.hjust = 0.5))



# Create plot function for each taxon
create_taxon_plot <- function(km_sf, taxon, save_path, mn_mag_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj) {
  km_taxon <- km_sf %>%
    filter(tax.grp == taxon)
  
  km_sf_total_taxon <- km_taxon %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  p <- ggplot() +
    geom_raster(data = mn_mag_df, aes(x = x, y = y, fill = value), interpolate = TRUE) +
    scale_fill_gradientn(colors = cols2(100), limits = c(0, 26), name = expression(paste("cm ", s^-1))) +
    ggnewscale::new_scale_fill() +  # Add new_scale_fill before adding new fill layers
    # Add the zoomed-in countries layer
    geom_sf(data = wcp_sf, fill = NA, color = "black") +
    # Add the ofp layer
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total_taxon, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")),
                         breaks = pretty_breaks(5)) +
    scale_size_binned(name = expression(paste("Total Biomass m"^"-3")),
                      range = c(0, 10),
                      breaks = pretty_breaks(5),
                      transform = "exp",
                      nice.breaks = F) +
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
    ) +
    guides(fill = guide_legend(title.position = "left", title.hjust = 0.5),
           size = guide_legend(title.position = "left", title.hjust = 0.5))
  
  # Define the filename
  filename <- paste(save_path, "/K4S_Plot_A1_CS(", taxon, ").png", sep = "")
  
  # Save the plot
  ggsave(filename = filename, plot = p, width = 8, height = 6, bg = "white")
}

# Define the save directory
save_directory <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1"

# Create plots for each taxon
taxa_groups <- c("fish", "krill", "cnidarians", "salps", "cephalopods")

for (taxon in taxa_groups) {
  create_taxon_plot(km_sf, taxon, save_directory, mn_mag_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
}


