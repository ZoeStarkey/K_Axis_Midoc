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

# ADD ICE (leftover from KAXIS_MAPS_2017)
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/ice_df.rda")




###########TOTAL BIOMASS PLOT EXCLUDING GELATINOUS ORGANISMS ################
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps","mixed/other invertebrates")

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

# Now create the plot
TSM_total <-
  ggplot() +
  # Base layer for time since melt
  geom_sf(data = wcp_sf, fill = NA) +
  geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.8) +
    scale_fill_cmocean( name = "curl", na.value = NA,
      guide = guide_colorbar(
        title = "Days",
        title.position = "left",
        title.hjust = 0.5,
        label.position = "right",
        barwidth = 1,
        barheight = 16,
        order = 2,
        frame.linewidth = 0.2,
        # ticks.linewidth = 0.5,
        title.theme = element_text(size = 14, angle = 90),
        label.theme = element_text(size = 14)
      )
    ) + 
  
    
    # Add f3$finished and f1$finished 
    geom_sf(data = f3$finished, color = "black", linewidth = 1) +
    geom_sf(data = f1$finished, color = "black", linewidth = 1) +
    
    
  # Add ice
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 1) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
                       name = 'Ice (%)',
                       guide = guide_colorbar(title.position = "left", 
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 8,
                                              order = 3,
                                              frame.linewidth = 0.2,
                                              #ticks.linewidth = 0.5,
                                              title.theme = element_text(size = 14, angle = 90),
                                              label.theme = element_text(size = 14))) + 
  
 
  
  ggnewscale::new_scale_fill() +
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1, colour = "magenta") +
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
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.text = element_text(size = 14),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 20),  # Increased axis title size
    axis.text = element_text(size = 16),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    panel.grid.major = element_line(color = "grey60", linetype = "solid"),  # Change graticule lines to grey
    panel.grid.minor = element_line(color = "grey60", linetype = "solid"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  ) +
    guides(
      fill = guide_legend(
        title.position = "left", 
        title.hjust = 0.5,
        override.aes = list(size = c(6, 8, 10, 12)),
        order = 1,
        title.theme = element_text(size = 14, angle = 90),
        keywidth = unit(1, "cm"),
        keyheight = unit(1, "cm"),
        default.unit = "cm",
        background = element_rect(fill = "transparent", colour = NA)
      ),
      size = guide_legend(
        title.position = "left", 
        title.hjust = 0.5,
        order = 1,
        title.theme = element_text(size = 14, angle = 90),
        background = element_rect(fill = "transparent", colour = NA)
      ),
      ) +
   theme(
     legend.position = "right",
     legend.box = "vertical"
    )




output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_TSM")
output_filename <- "K4S_Plot_A1_TSM_TBM.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = TSM_total, width = 10, height = 8, bg = "white")




#############INDIVIDUAL TAXA PLOTS#############################

plot_tsm_biomass <- function(include_taxa, decimal_places = 2, output_directory = NULL, output_filename = NULL) {
  # Filter data for specified taxa and aggregate biomass
  km_sf_total <- km_sf %>%
    filter(tax.grp %in% include_taxa) %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Calculate bin breaks
  n_bins <- 5
  bin_range <- range(km_sf_total$total_biomass, na.rm = TRUE)
  bin_breaks <- pretty(bin_range, n = n_bins)
  
  # Create labels with specified decimal places
  bin_labels <- paste0(
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-length(bin_breaks)]),
    " - ",
    sprintf(paste0("%.", decimal_places, "f"), bin_breaks[-1])
  )
  
  # Modify km_sf_total with bins
  km_sf_total <- km_sf_total %>%
    mutate(biomass_bin = cut(total_biomass, 
                             breaks = bin_breaks,
                             labels = bin_labels,
                             include.lowest = TRUE))
  
  # Create the plot
  TSM_plot <- 
    ggplot() +
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.8) +
    scale_fill_cmocean(name = "curl", na.value = NA,
                       guide = guide_colorbar(
                         title = "Days",
                         title.position = "left",
                         title.hjust = 0.5,
                         label.position = "right",
                         barwidth = 1,
                         barheight = 16,
                         order = 2,
                         frame.linewidth = 0.2,
                         title.theme = element_text(size = 14, angle = 90),
                         label.theme = element_text(size = 14)
                       )
    ) + 
    geom_sf(data = f3$finished, color = "black", linewidth = 1) +
    geom_sf(data = f1$finished, color = "black", linewidth = 1) +
    ggnewscale::new_scale_fill() + 
    geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 1) +
    scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
                         name = 'Ice (%)',
                         guide = guide_colorbar(title.position = "left", 
                                                title.hjust = 0.5,
                                                label.position = "right",
                                                barwidth = 1,
                                                barheight = 8,
                                                order = 3,
                                                frame.linewidth = 0.2,
                                                title.theme = element_text(size = 14, angle = 90),
                                                label.theme = element_text(size = 14))) + 
    ggnewscale::new_scale_fill() +
    geom_sf(data = wcp_sf, fill = NA, color = "black") +
    geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    geom_sf(data = ktr_sf, size = 1, colour = "magenta") +
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
      panel.grid = element_line(color = "gray40", linetype = "solid"),
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.title = element_text(angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14),
      legend.box.background = element_blank(),
      legend.byrow = TRUE,
      strip.background = element_rect(fill = "white"),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 16),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      panel.grid.major = element_line(color = "grey60", linetype = "solid"),  # Change graticule lines to grey
      panel.grid.minor = element_line(color = "grey60", linetype = "solid"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    ) +
    guides(
      fill = guide_legend(
        title.position = "left", 
        title.hjust = 0.5,
        override.aes = list(size = c(6, 8, 10, 12)),
        order = 1,
        title.theme = element_text(size = 14, angle = 90),
        keywidth = unit(1, "cm"),
        keyheight = unit(1, "cm"),
        default.unit = "cm",
        background = element_rect(fill = "transparent", colour = NA)
      ),
      size = guide_legend(
        title.position = "left", 
        title.hjust = 0.5,
        order = 1,
        title.theme = element_text(size = 14, angle = 90),
        background = element_rect(fill = "transparent", colour = NA)
      )
    ) +
    theme(
      legend.position = "right",
      legend.box = "vertical"
    )
  
  # Save the plot if output directory and filename are provided
  if (!is.null(output_directory) && !is.null(output_filename)) {
    full_output_path <- file.path(output_directory, output_filename)
    ggsave(filename = full_output_path, plot = TSM_plot, width = 10, height = 8, bg = "white")
    cat("Plot saved to:", full_output_path, "\n")
  } else {
    cat("Plot was not saved. To save the plot, provide both output_directory and output_filename.\n")
  }
  
  return(TSM_plot)
}

# Example usage:
# Fish plot
TSM_fish
TSM_fish <- plot_tsm_biomass(
  include_taxa = c("fish"),
  decimal_places = 3,
  output_directory = paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_TSM"),
  output_filename = "K4S_Plot_A1_TSM_Fish.png"
)

# Cephalopod plot
TSM_cephalopods <- plot_tsm_biomass(
  include_taxa = c("cephalopods"),
  decimal_places = 4,
  output_directory = paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_TSM"),
  output_filename = "K4S_Plot_A1_TSM_Cephalopods.png"
)

# Krill plot
TSM_krill <- plot_tsm_biomass(
  include_taxa = c("krill"),
  decimal_places = 3,
  output_directory = paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_TSM"),
  output_filename = "K4S_Plot_A1_TSM_Krill.png"
)








#MIXED PLOT######
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "fish", "cephalopods", "krill")
# 
#Filter data for taxa not in the exclude list and aggregate biomass
km_sf_total <- km_sf %>%
  filter(!tax.grp %in% exclude_taxa) %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass_mixed = sum(bm_g_m3, na.rm = TRUE),
    lon_end = first(lon_end),
    lat_end = first(lat_end)
  )

# Calculate the bin breaks using pretty breaks
n_bins <- 5  # You can adjust this number for more or fewer bins
bin_range <- range(km_sf_total$total_biomass_mixed, na.rm = TRUE)
bin_breaks <- pretty(bin_range, n = n_bins)

# Create labels with exact ranges, using four decimal places
bin_labels <- paste0(
  sprintf("%.3f", bin_breaks[-length(bin_breaks)]),
  " - ",
  sprintf("%.3f", bin_breaks[-1])
)

# Modify the mutate step in km_sf_total
km_sf_total <- km_sf_total %>%
  mutate(mixed_biomass_bin = cut(total_biomass_mixed, 
                                 breaks = bin_breaks,
                                 labels = bin_labels,
                                 include.lowest = TRUE))


# Now create the plot
TSM_mixed <-
  ggplot() +
  # Base layer for time since melt
  geom_sf(data = wcp_sf, fill = NA) +
  geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
  scale_fill_cmocean( name = "curl", na.value = NA,
                      guide = guide_colorbar(
                        title = "Days",
                        title.position = "left",
                        title.hjust = 0.5,
                        label.position = "right",
                        barwidth = 1,
                        barheight = 16,
                        order = 2,
                        frame.linewidth = 0.2,
                        # ticks.linewidth = 0.5,
                        title.theme = element_text(size = 14, angle = 90),
                        label.theme = element_text(size = 14)
                      )
  ) + 
  
  
  # Add f3$finished and f1$finished 
  geom_sf(data = f3$finished, color = "black", linewidth = 1) +
  geom_sf(data = f1$finished, color = "black", linewidth = 1) +
  
  
  # Add ice
  ggnewscale::new_scale_fill() + 
  geom_tile(data = ice_df, aes(x = x, y = y, fill = k.axis_data_ICE_LONGLAT_20160218), alpha = 0.8) +
  scale_fill_gradientn(colors = palr::bathy_deep_pal(56), na.value = "transparent", limits = c(0, 100),
                       name = 'Ice (%)',
                       guide = guide_colorbar(title.position = "left", 
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 8,
                                              order = 3,
                                              frame.linewidth = 0.2,
                                              #ticks.linewidth = 0.5,
                                              title.theme = element_text(size = 14, angle = 90),
                                              label.theme = element_text(size = 14))) + 
  
  
  
  ggnewscale::new_scale_fill() +
  geom_sf(data = wcp_sf, fill = NA, color = "black") +
  geom_sf(data = ofp_sf, color = "black", linetype = "dashed", linewidth = 1.0) +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
  annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
  annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
  geom_sf(data = ktr_sf, size = 1, colour = "grey30") +
  geom_sf(data = km_sf_total, aes(fill = mixed_biomass_bin , size = mixed_biomass_bin ), shape = 21, color = "black") +
  scale_fill_manual(
    values = c("white", "grey65", "grey45", "grey30", "black"),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  scale_size_manual(
    values = c(4, 6, 8, 10, 12),
    name = expression(paste("Biomass (g m"^-3, ")"))
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-500000, 1020000), ylim = c(-1000000, 600000)) +
  theme(
    legend.position = "right",
    panel.grid = element_line(color = "gray80", linetype = "solid"),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.text = element_text(size = 14),
    legend.box.background = element_blank(),
    legend.byrow = TRUE,
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 20),  # Increased axis title size
    axis.text = element_text(size = 16),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  ) +
  guides(
    fill = guide_legend(
      title.position = "left", 
      title.hjust = 0.5,
      override.aes = list(size = c(4, 6, 8, 10, 12)),
      order = 1,
      title.theme = element_text(size = 14, angle = 90),
      keywidth = unit(1, "cm"),
      keyheight = unit(1, "cm"),
      default.unit = "cm",
      background = element_rect(fill = "transparent", colour = NA)
    ),
    size = guide_legend(
      title.position = "left", 
      title.hjust = 0.5,
      order = 1,
      title.theme = element_text(size = 14, angle = 90),
      background = element_rect(fill = "transparent", colour = NA)
    ),
  ) +
  theme(
    legend.position = "right",
    legend.box = "vertical"
  )




output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_TSM")
output_filename <- "K4S_Plot_A1_TSM_Mixed.png"
full_output_path <- file.path(output_directory, output_filename)

ggsave(filename = full_output_path, plot = TSM_mixed, width = 10, height = 8, bg = "white")































