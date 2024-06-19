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

#setting directory
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

#TSM
bx <- c(60, 95, -71, -55)  
ice_dts <- as.POSIXlt(as.Date(paste(1997:2016,"-02-16 UTC",sep=""))) # 20y
load(paste("../sophie_raster/KAXIS_ssh.RData",sep=""))

tsm <- brick("../sophie_raster/KAXIS_tsm.grd") 
tsm[tsm==32766] <- NA_real_ #land
tsm[tsm==32765] <- NA_real_ # OOZ
tsm[tsm>365] <- NA_real_ # not ice covered this season
tsm <- tsm[[20]]


last_day <- dim(tsm)[3] #30 # date of last CTD dip feb 16th (survey end)
# tsm_e <- crop(tsm,extent(c(-5411853 , 6235554, -1577161,  1358628))) # crop to E Ant only
# e <- extent(-5411853 , 6235554, -1577161,  1358628)
tsm_e <- tsm
tsm_e_ll <- projectRaster(tsm_e, crs = CRS(prj), res = 1e4) #raster(e, crs = prj, res = 1e4)) 
tsm_e_ll <- st_as_stars(tsm_e_ll)

use <- tsm_e_ll[[last_day]]; 
ryb <- colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
cols1 <- ryb(56); cols1 <- cols1[c(1:23, 32:54)] 


#Fronts
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

# add ice (leftover from KAXIS_MAPS_2017)
file_path <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/k-axis_data_ICE_LONGLAT_20160218.tif"
icefile <- raster(file_path)
icefile <- stack(file_path)
ice_layer <- icefile[[1]]
ice_layer[ice_layer == 0] <- NA_real_
ice_df <- as.data.frame(rasterToPoints(ice_layer))




#TOTAL BIOMASS PLOT - only key taxon of interest 
km_filtered <- km_sf %>%
  filter(tax.grp %in% c("fish", "cephalopods", "krill"))

# Aggregate biomass data by midoc station including only fish, cephalopods, and krill
km_sf_total <- km_filtered %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    lon_end = first(lon_end),
    lat_end = first(lat_end)
  )

# Plot Total Biomass: fish, cephalopods, krill with colour and size 
ggplot() +
  # Base layer
  geom_sf(data = wcp_sf, fill = NA) +
  geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
  scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                          legend.title = element_text(angle = 90)),
                                                            title = "days since melt"), na.value = NA) +
  # Biomass layer 
  new_scale_fill() +
  geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  # Add points and labels for aggregated data
  geom_sf(data = ktr_sf, size = 0.5) + #voyage track
  geom_sf(data = km_sf_total, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
  scale_fill_viridis_c(option = "viridis", name = expression(paste("Total Biomass m"^"-3")),
                       breaks = pretty_breaks(5))+ 
  #scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
  scale_size_binned(name = expression(paste("Total Biomass m"^"-3")),
                    range = c(0,10),
                    breaks = pretty_breaks(5),
                    transform = "exp",
                    nice.breaks = F) +
  # Add f3$finished and f1$finished
  geom_sf(data = f3$finished, color = "red") +
  geom_sf(data = f1$finished, color = "red") +
  
  #adding in ice
  ggnewscale::new_scale_fill() + 
  geom_raster(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100)) +
  labs(fill = 'Ice (%)') +
  
  
  # Customising plot
  labs(x = "Longitude", y = "Latitude", fill = "Total Biomass", size = "Total Biomass") +
  coord_sf(crs = st_crs(prj), xlim = c(-1000000, 1000000), ylim = c(-1000000, 600000)) +
  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    legend.background = element_blank(),  # Make legend background transparent
    legend.key = element_blank(),  # Remove legend key
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = NA)
  ) +
  guides(fill = guide_legend(title.position = "left", title.hjust = 0.5),
         size = guide_legend(title.position = "left", title.hjust = 0.5))




#Creating plot function for each taxon

# Define variables xx and yy used for annotation
xx <- 75  
yy <- -60 

# Create plot function for each taxon
create_taxon_plot <- function(km_sf, taxon, save_path) {
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
    scale_fill_viridis_c(option = "viridis", name = expression(paste("Total Biomass m"^"-3")),
                         breaks = pretty_breaks(5))+ 
    #scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
    scale_size_binned(name = expression(paste("Total Biomass m"^"-3")),
                      range = c(0,10),
                      breaks = pretty_breaks(5),
               transform = "exp",
               nice.breaks = F) +
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
  
    #Customising plot
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
  filename <- paste(save_path, "/K4S_Plot_A1_TSM(", taxon, ").png", sep = "")
  
  # Save the plot
  ggsave(filename = filename, plot = p, width = 8, height = 6, bg = "white")
}

save_directory <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1" 


# Create plots for each taxon
fish_plot <- create_taxon_plot(km_sf, "fish", save_directory);fish_plot
krill_plot <- create_taxon_plot(km_sf, "krill",  save_directory)
cnidarians_plot <- create_taxon_plot(km_sf, "cnidarians",  save_directory)
salps_plot <- create_taxon_plot(km_sf, "salps",  save_directory)
cephalopods_plot <- create_taxon_plot(km_sf, "cephalopods",  save_directory)

# Use patchwork to combine the plots
combined_plot <- (fish_plot + krill_plot + cnidarians_plot + salps_plot + cephalopods_plot) +
  plot_layout(guides = "collect")

# Print the combined plot
print(combined_plot)

plot(fish_plot)
plot(krill_plot)
