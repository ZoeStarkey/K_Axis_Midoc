library(ggplot2)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rworldmap)
library(stars)
library(orsifronts)
library(dplyr)
library(readr)
library(scales)


#setting directory
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

# Load your raster data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_CHLA_VoyagePeriod.RData")
R <- R_voy_jf
q1 <- 0.05
q2 <- 10
R[R > q2] <- q2
R[R < q1] <- q1
R <- log(R)
zz <- c(0.05, 0.1, 0.25, 0.5, 1, 2.5, 5,7.5)
log_zz <- log(zz)

# Convert raster to data frame
R_df <- as.data.frame(R, xy = TRUE)
colnames(R_df) <- c("x", "y", "value")



# Create a color palette
ryb <- colorRampPalette(c("#31004a", "#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(51)

# Plot using ggplot2
ggplot(R_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colors = ryb, breaks = log_zz, labels = sprintf("%.2f", zz),
                       limits = c(log(q1), log(q2)),
                       name = expression(paste("Chl-a (mg ", m^-3, ")"))) +
  coord_fixed(xlim = c(60, 95), ylim = c(-71, -54)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right") 

#ADD ICE 

load("/Users/zoestarkey/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_DF_A1/ice_df.rda")





#trying to add the sf layers 

#projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}

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


# Fronts
ofp<- spTransform(orsifronts, CRS(prj))
ofp_sf <- st_as_sf(ofp)
# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)


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



# Convert reprojected raster to data frame
R_projected <- projectRaster(R, crs = prj)


R_df <- as.data.frame(R_projected, xy = TRUE)
colnames(R_df) <- c("x", "y", "value")




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



#TOTAL BIOMASS PLOT - only key taxon of interest
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps")

# Filter data for taxa not in the exclude list and aggregate biomass
km_sf_total <- km_sf %>%
  filter(!tax.grp %in% exclude_taxa) %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    lon_end = first(lon_end),
    lat_end = first(lat_end)
  )

# Calculate the bin breaks
bin_breaks <- quantile(km_sf_total$total_biomass, probs = seq(0, 1, 0.2))

# Create labels with exact ranges, now using three decimal places
bin_labels <- paste0(
  #c("Very Low (", "Low (", "Medium (", "High (", "Very High ("),
  sprintf("%.3f", bin_breaks[-length(bin_breaks)]),
  " - ",
  sprintf("%.3f", bin_breaks[-1])
)

# Modify the mutate step in km_sf_total
km_sf_total <- km_sf_total %>%
  mutate(biomass_bin = cut(total_biomass, 
                           breaks = bin_breaks,
                           labels = bin_labels,
                           include.lowest = TRUE))


Chla_total <-
 ggplot() +
 # Add the base raster layer
  geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = ryb, breaks = log_zz, labels = sprintf("%.2f", zz),
                       limits = c(log(q1), log(q2)),
                       na.value = "grey85",
                       name = expression(paste("Chl-a (mg ", m^-3, ")")),
                       guide = guide_colorbar(title.position = "left",
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 8)) +
  #add ice
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
                       name = 'Ice (%)',
                       guide = guide_colorbar(title.position = "left", 
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 8)) +
  
  #  add f3$finished and f1$finished
  geom_sf(data = f3$finished, color = "black", linewidth = 1 ) +
  geom_sf(data = f1$finished, color = "black", linewidth = 1 )+
  
  
  ggnewscale::new_scale_fill() +  # Add new_scale_fill before adding new fill layers
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  # Add the ofp layer
  geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1) +
  geom_sf(data = km_sf_total, aes(fill = biomass_bin, size = biomass_bin), shape = 21, color = "black") +
  geom_sf(data = km_sf_total, aes(fill = biomass_bin, size = biomass_bin), shape = 21, color = "black") +
  scale_fill_manual(
    values = c("white", "grey85", "grey65", "grey30", "black"),
    name = expression(paste("Summed Biomass (g m"^-3, ")"))
  ) +
  scale_size_manual(
    values = c(4, 6, 8, 10, 12),
    name = expression(paste("Summed Biomass (g m"^-3, ")"))
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
  )  +# ... (keep your other layers and settings)

  guides(
    fill = guide_legend(
      title.position = "left", 
      title.hjust = 0.5,
      override.aes = list(size = c(4, 6, 8, 10, 12)),
      order = 1  # This will place it at the top
    ),
    size = guide_legend(
      title.position = "left", 
      title.hjust = 0.5,
      order = 1  # This ensures size legend stays with fill legend
    ),
    colour = guide_colorbar(
      title.position = "left", 
      title.hjust = 0.5,
      label.position = "right",
      barwidth = 1,
      barheight = 8,
      order = 2  # This will place it below the biomass legend
    ),
) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    # ... (keep your other theme settings)
  )




#save the plot 
output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_CHLA")
output_filename <- "K4S_Plot_A1_CHLA_TBM.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = Chla_total, width = 10, height = 8, bg = "white")










  

#Individual taxon function
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(sf)

# Create plot function for each taxon
create_taxon_plot <- function(km_sf, taxon, save_path, R_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj) {
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
    # Add the base raster layer
    geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(colors = ryb, breaks = log_zz, labels = sprintf("%.2f", zz),
                         limits = c(log(q1), log(q2)),
                         name = expression(paste("Chl-a (mg ", m^-3, ")"))) +
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
    scale_fill_viridis_c(option = "mako", name = expression(paste("Total Biomass m"^"-3")),
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
  filename <- paste(save_path, "/K4S_Plot_A1_CHLA(", taxon, ").png", sep = "")
  
  # Save the plot
  ggsave(filename = filename, plot = p, width = 8, height = 6, bg = "white")
}

# Define the save directory
save_directory <- "~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1"

# Create plots for each taxon
taxa_groups <- c("fish", "krill", "cnidarians", "salps", "cephalopods")

for (taxon in taxa_groups) {
  create_taxon_plot(km_sf, taxon, save_directory, R_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
}








ggplot() +
  geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = ryb, breaks = log_zz, labels = sprintf("%.2f", zz),
                       limits = c(log(q1), log(q2)),
                       na.value = "#D3D3D3",
                       name = expression(paste("Chl-a (mg ", m^-3, ")")))  +
  
  # Add ice
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100)) +
  labs(fill = 'Ice (%)') +
  theme(legend.title.position = "left",
        legend.title = element_text(angle = 90)) +
  
  
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
  #scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
  scale_fill_gradientn(
    colors = c("white", "grey90", "grey40", "grey20", "black"),
    name = expression(paste("Biomass g m"^"-3")),
    breaks = pretty_breaks(5)) +
  scale_size_binned(name = expression(paste("Biomass g m"^"-3")),
                    range = c(0, 10),
                    breaks = pretty_breaks(5),
                    transform = "exp",
                    nice.breaks = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-550000, 1000000), ylim = c(-1000000, 600000)) +  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = NA)
  ) +
  guides(fill = guide_legend(title.position = "left", title.hjust = 0.5),
         size = guide_legend(title.position = "left", title.hjust = 0.5))



# Create plot function for each taxon
create_taxon_plot <- function(km_sf, taxon, R_df, ryb, log_zz, zz, q1, q2, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj) {
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
    geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colors = ryb, 
      breaks = log_zz, 
      labels = sprintf("%.2f", zz),
      limits = c(log(q1), log(q2)),
      na.value = "#D3D3D3",
      name = expression(paste("Chl-a (mg ", m^-3, ")"))
    ) +
    
    # Add ice
    ggnewscale::new_scale_fill() + 
    geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
    scale_fill_gradientn(
      colors = palr::bathy_deep_pal(56), 
      na.value = "transparent", 
      limits = c(0, 100)
    ) +
    labs(fill = 'Ice (%)') +
    theme(
      legend.title.position = "left",
      legend.title = element_text(angle = 90)
    ) +
    
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
    scale_fill_gradientn(
      colors = c("white", "grey90", "grey40", "grey20", "black"),
      name = expression(paste("Biomass g m"^"-3")),
      breaks = pretty_breaks(5)
    ) +
    scale_size_binned(
      name = expression(paste("Biomass g m"^"-3")),
      range = c(0, 10),
      breaks = pretty_breaks(5),
      transform = "exp",
      nice.breaks = FALSE
    ) +
    labs(x = "Longitude", y = "Latitude") +
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
    guides(
      fill = guide_legend(title.position = "left", title.hjust = 0.5),
      size = guide_legend(title.position = "left", title.hjust = 0.5)
    )
  
  return(p)
}

# Example usage (assuming you have the required data)
# Load necessary data
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_curr_big.RData")

# Assuming the data frames `km_sf`, `R_df`, `ryb`, `log_zz`, `zz`, `q1`, `q2`, `ice_df`, `wcp_sf`, `ofp_sf`, `wp_sf`, `ktr_sf`, `prj` are already loaded and defined

# Create and print the fish plot
fish_plot <- create_taxon_plot(km_sf, "fish", R_df, ryb, log_zz, zz, q1, q2, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
print(fish_plot)
ceph_plot <- create_taxon_plot(km_sf, "cephalopods", R_df, ryb, log_zz, zz, q1, q2, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
print(ceph_plot)
krill_plot <- create_taxon_plot(km_sf, "krill", R_df, ryb, log_zz, zz, q1, q2, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
print(krill_plot)
cnidarian_plot <- create_taxon_plot(km_sf, "cnidarians", R_df, ryb, log_zz, zz, q1, q2, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
print(cnidarian_plot)
salp_plot <- create_taxon_plot(km_sf, "salps", R_df, ryb, log_zz, zz, q1, q2, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, prj)
print(salp_plot)



#All other taxa 

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
  
  p <- ggplot() +
    geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colors = ryb, 
      breaks = log_zz, 
      labels = sprintf("%.2f", zz),
      limits = c(log(q1), log(q2)),
      na.value = "#D3D3D3",
      name = expression(paste("Chl-a (mg ", m^-3, ")"))
    ) +
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
}

# Generate and display plot for all other taxa except the specified ones
excluded_taxa_plot <- create_excluded_taxa_plot(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf)
print(excluded_taxa_plot)



# mixed plot 
create_mixed_plot <- function(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, tsm_e_ll) {
  km_sf <- st_as_sf(km)
  
  # List of taxa to exclude
  exclude_taxa <- c("krill", "cephalopods", "cnidarians", "salps", "fish")
  
  # Filter data for taxa not in the exclude list and aggregate biomass
  km_sf_mixed <- km_sf %>%
    filter(!tax.grp %in% exclude_taxa) %>%
    group_by(midoc.stn) %>%
    summarize(
      mixed_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Generate the plot using ggplot2
  p <- ggplot() +
    geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colors = ryb, 
      breaks = log_zz, 
      labels = sprintf("%.2f", zz),
      limits = c(log(q1), log(q2)),
      na.value = "#D3D3D3",
      name = expression(paste("Chl-a (mg ", m^-3, ")"))
    ) +
    
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
    geom_sf(data = km_sf_mixed, aes(fill = mixed_biomass, size = mixed_biomass), shape = 21, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "grey90", "grey40", "grey20", "black"),
      name = expression(paste("Mixed Taxa Biomass g m"^"-3")),
      breaks = scales::pretty_breaks(5)) +
    scale_size_binned(name = expression(paste("Mixed Taxa Biomass g m"^"-3")),
                      range = c(0, 10),
                      breaks = scales::pretty_breaks(5),
                      transform = "exp",
                      nice.breaks = FALSE) +
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

# Generate and display plot for all other taxa except the specified ones
mixed_taxa_plot <- create_mixed_plot(km, tmp_df, ice_df, wcp_sf, ofp_sf, wp_sf, ktr_sf, tsm_e_ll)
print(mixed_taxa_plot)








  