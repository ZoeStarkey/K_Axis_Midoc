
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
library(scales)
library(orsifronts)
library(palr)
library(raster)
library(rgdal)


usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

#Projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}

# SST, ice, ssh etc. stored for 18 Jan - 18 Feb (download from CLIOTOP VM)
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_anim_data.RData")
SST <- sst
SSH <- ssh
SSHa <- ssha

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_anim_GHRSST.RData")
sst <- sstGHR
rm(sstGHR)

#K-Axis frontal features
dts <- as.POSIXlt(seq(as.Date("2016-01-18"), as.Date("2016-02-18"), by = "1 day"), tz = "UTC")
bx <- c(60, 95, -70, -51)
cx <- 0.8;

#idate
for (idate in 1:length(dts)) { #length(dts)
  print(paste(idate,"of",length(dts)))
  thisdate <- dts[idate]
  par(new=FALSE,mar=c(2,1.25,1.5,5.75), oma=c(1,1,1,0), family="serif") }

# Get the SST data for the current date
tmp <- sst[[idate]]; tmp <- tmp - 273.15

# Set SST plot limits
sstmax <- 5
sstmin <- -1.5
tmp[tmp > sstmax] <- sstmax
tmp[tmp < sstmin] <- sstmin

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

km_sf <- st_as_sf(km)
# Define the depth ranges for each codend
depth_bins <- c("0-1000", "800-1000", "600-800", "400-600", "200-400", "0-200")

# Map the codends to depth ranges using the factor function
km_sf$depth <- factor(km$cod.end, levels = c("1", "2", "3", "4", "5", "6"), labels = depth_bins)

# Define the stations to remove
abandoned_stations <- c("MIDOC02","MIDOC08", "MIDOC10", "MIDOC12", "MIDOC33")

# Remove the specified stations from km_df
km_sf <- km_sf %>%
  filter(!midoc.stn %in% abandoned_stations)


# Remove the specified stations from km_df
km_sf <- km_sf %>%
  filter(!depth %in% "0-1000")


# ADD ICE (leftover from KAXIS_MAPS_2017)
file_path <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/k-axis_data_ICE_LONGLAT_20160218.tif"
icefile <- raster(file_path)
# Reproject the ice raster to the desired projection
icefile_proj <- projectRaster(icefile, crs = prj)

# Convert to data frame for plotting
ice_df <- as.data.frame(rasterToPoints(icefile_proj))

# Replace 0 values with NA
ice_df[ice_df$layer == 0, "layer"] <- NA

ice_df <- ice_df[ice_df$k.axis_data_ICE_LONGLAT_20160218 > 25, ]










#Aggreating biomass data by midoc stqtion 

# km_sf <- st_as_sf(km)
# km_sf_total <- km_sf %>% # Aggregate biomass data by midoc station 
#   group_by(midoc.stn) %>%
#   summarize(
#     total_biomass = sum(bm_g_m3, na.rm = TRUE),
#     lon_end = first(lon_end),
#     lat_end = first(lat_end)
#   )

# Convert raster to data frame
#tmp_df <- as.data.frame(rasterToPoints(tmp))

# Rename columns for convenience
#colnames(tmp_df) <- c("Longitude", "Latitude", "SST")

#reprojecting
tmp_projected <- projectRaster(tmp, crs = prj)
tmp_df <- as.data.frame(tmp_projected, xy = TRUE)
colnames(tmp_df) <- c("Longitude", "Latitude", "SST")


#setting up colour pallete 
ryb <- colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
cols1 <- ryb(56); cols1 <- cols1[c(1:22, 24:56)] 

#########CREATING THE TOTAL TAXA PLOT#############
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


# Calculate bin breaks for biomass (if not already done)
n_bins <- 5  # You can adjust this number for more or fewer bins
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

# Now create the plot
SST_total <- ggplot() +
  # Add the base raster layer for SST
  geom_raster(data = tmp_df, aes(x = Longitude, y = Latitude, fill = SST)) +
  scale_fill_gradientn(colours = cols1, 
                       limits = c(sstmin, sstmax), 
                       na.value = "transparent",
                       name = expression(SST ~ (degree * C)),
                       guide = guide_colorbar(title.position = "left",
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 8)) +
  # Add ice
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
                       name = 'Ice (%)',
                       guide = guide_colorbar(title.position = "left", 
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 8)) +
  
  # Add f3$finished and f1$finished (if these exist in your SST data)
  geom_sf(data = f3$finished, color = "black", linewidth = 1) +
  geom_sf(data = f1$finished, color = "black", linewidth = 1) +
  
  ggnewscale::new_scale_fill() +
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1) +
  geom_sf(data = km_sf_total, aes(fill = biomass_bin, size = biomass_bin), shape = 21, color = "black") +
  scale_fill_manual(
    values = c("white", "grey65", "grey30", "black"),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  scale_size_manual(
    values = c(6, 8, 10, 12),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-500000, 1020000), ylim = c(-1000000, 600000)) +
  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = "white")
  ) +
  guides(
    fill = guide_legend(
      title.position = "left", 
      title.hjust = 0.5,
      override.aes = list(size = c(6, 8, 10, 12)),
      order = 1
    ),
    size = guide_legend(
      title.position = "left", 
      title.hjust = 0.5,
      order = 1
    ),
    colour = guide_colorbar(
      title.position = "left", 
      title.hjust = 0.5,
      label.position = "right",
      barwidth = 1,
      barheight = 8,
      order = 2
    )
  ) +
  theme(
    legend.position = "right",
    legend.box = "vertical"
  )




output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_SST")
output_filename <- "K4S_Plot_A1_SST_TBM.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = SST_total, width = 10, height = 8, bg = "white")














# Create ggplot
ggplot() +
   geom_raster(data = tmp_df, aes(x = Longitude, y = Latitude, fill = SST)) +
    scale_fill_gradientn(colours = cols1, limits = c(sstmin, sstmax), na.value = "transparent") +
   labs(fill = expression(SST ~ (degree * C)))  +

  #add ice
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100)) +
  labs(fill = 'Ice (%)') +

  # Add the zoomed-in countries layer
  ggnewscale::new_scale_fill() + 
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  # Add the ofp layer
  geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1) +
  geom_sf(data = km_sf_total, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
  #scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass g m"^"-3")),
                   #    breaks = pretty_breaks(5))+ 
   scale_fill_gradientn(
     colors = c("white", "grey90", "grey40", "grey20", "black"),
     name = expression(paste("Total Biomass g m"^"-3")),
     breaks = pretty_breaks(5)) +
  #scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
  scale_size_binned(name = expression(paste("Total Biomass g m"^"-3")),
                    range = c(0,10),
                    breaks = pretty_breaks(5),
                    transform = "exp",
                    nice.breaks = F) +
  labs(x = "Longitude", y = "Latitude") +

  
  #projection
  coord_sf(crs = st_crs(prj), xlim = c(-550000, 1000000), ylim = c(-1000000, 600000)) +
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





















#individul taxa plot 
# Function to create individual taxa plots
create_taxa_plot <- function(km, taxa_name, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf) {
  km_sf <- st_as_sf(km)
  
  # Filter data for the specific taxa group and aggregate biomass
  km_sf_taxa <- km_sf %>%
    filter(tax.grp == taxa_name) %>%
    group_by(midoc.stn) %>%
    summarize(
      taxa_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  ggplot() +
    geom_raster(data = tmp_df, aes(x = Longitude, y = Latitude, fill = SST)) +
    scale_fill_gradientn(colours = cols1, limits = c(-1.5, 5), na.value = "transparent") +
    labs(fill = expression(SST ~ (degree * C))) +
    
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
    geom_sf(data = km_sf_taxa, aes(fill = taxa_biomass, size = taxa_biomass), shape = 21, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "grey90", "grey40", "grey20", "black"),
      name = expression(paste(taxa_name, " Biomass g m"^"-3")),
      breaks = pretty_breaks(5)) +
    scale_size_binned(name = expression(paste(taxa_name, " Biomass g m"^"-3")),
                      range = c(0, 10),
                      breaks = pretty_breaks(5),
                      transform = "exp",
                      nice.breaks = FALSE) +
    labs(x = "Longitude", y = "Latitude") +
    
    # Projection
    coord_sf(crs = st_crs(prj), xlim = c(-550000, 1000000), ylim = c(-1000000, 600000)) +
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
}

# Generate and display plots for each taxa group individually
fish_plot <- create_taxa_plot(km, "fish", tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(fish_plot)

cnidarians_plot <- create_taxa_plot(km, "cnidarians", tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(cnidarians_plot)

cephalopods_plot <- create_taxa_plot(km, "cephalopods", tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(cephalopods_plot)

salps_plot <- create_taxa_plot(km, "salps", tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(salps_plot)

krill_plot <- create_taxa_plot(km, "krill", tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(krill_plot)


#Everything else 


#All other taxa 

# Function to create plot for all taxa except the specified ones
create_excluded_taxa_plot <- function(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf) {
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
  
  ggplot() +
    geom_raster(data = tmp_df, aes(x = Longitude, y = Latitude, fill = SST)) +
    scale_fill_gradientn(colours = cols1, limits = c(-1.5, 5), na.value = "transparent") +
    labs(fill = expression(SST ~ (degree * C))) +
    
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
    geom_sf(data = km_sf_other, aes(fill = other_biomass, size = other_biomass), shape = 21, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "grey90", "grey40", "grey20", "black"),
      name = expression(paste("Other Taxa Biomass g m"^"-3")),
      breaks = pretty_breaks(5)) +
    scale_size_binned(name = expression(paste("Other Taxa Biomass g m"^"-3")),
                      range = c(0, 10),
                      breaks = pretty_breaks(5),
                      transform = "exp",
                      nice.breaks = FALSE) +
    labs(x = "Longitude", y = "Latitude") +
    
    # Projection
    coord_sf(crs = st_crs(prj), xlim = c(-550000, 1000000), ylim = c(-1000000, 600000)) +
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
}

# Generate and display plot for all other taxa except the specified ones
excluded_taxa_plot <- create_excluded_taxa_plot(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(excluded_taxa_plot)



