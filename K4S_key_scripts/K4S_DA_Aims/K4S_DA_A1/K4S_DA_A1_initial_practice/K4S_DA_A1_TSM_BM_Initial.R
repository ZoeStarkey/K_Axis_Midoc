
install.packages("graticule")
install.packages("proj4")
install.packages("raster")
install.packages("rworldxtra")
install.packages("remotes")
install.packages("rworldmap")
install.packages("rgeos")
install.packages("rgdal")
remotes::install_github("rspatial/terra")
install.packages('terra', repos='https://rspatial.r-universe.dev')

library(graticule)
library(raster)
library(rworldxtra)
data(countriesHigh)
library(rworldmap)
# data(countriesHigh)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(readr)
library(sf)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

# projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied

ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}

# plot extent
library(remotes)
#remotes::install_github("rspatial/terra")

ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40) # extent for zoomed map
ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

# cropped and full versions of world map
# cropped version is used to make a suitable extent for plotting, full is over-plotted so that the continents don't  show the "wedge" defined by the extent
data(package = "rworldmap")
data(countriesHigh) 
#ZS:install.packages("rgdal", type = "source")
#ZS: remotes::install_github("r-spatial/rgdal")
wc <- crop(countriesHigh, ras.ext)
wcp <-spTransform(wc, CRS(prj))
wp  <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj))

# K-Axis stations
kos <- readRDS("../source data/k_axis_oceanog_summ.Rda")
kos <- ll2prj(kos, loncol="longitude", latcol="latidue")

ktr <- readRDS("../derived data/nav_reduced.rds")
# colnames(ktr) <- c("wp","lat","lon","wp.grp")
ktr <- ll2prj(ktr, loncol="LONGITUDE", latcol="LATITUDE")

km <- readRDS("../derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
tmp <- read_csv("../source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("../source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)
tmp <- readRDS("../derived data/codend_taxa_biomass.rds")
km <- inner_join(km, tmp); rm(tmp)


#ZS:Zone
file_path <- "../derived data/midoc_stations_envdata.rda"
md <- readRDS(file_path)
#ZS: adding zones to km dataset from md dataset
km <- merge(km, md[, c("midoc.stn", "zone")], by = "midoc.stn") 

#Finding the unique names in Zone
unique_values <- unique(km$zone)
#Zone colours

km$zone.col <- NA
km$zone.col <- ifelse(km$zone == "Southern", "blue", 
                      ifelse(km$zone == "Subpolar", "red",
                             ifelse(km$zone == "Antarctic ACC", "yellow", 
                                    ifelse(km$zone == "Subpolar - ASF", "green", 
                                           ifelse(km$zone == "Antarctic KP", "dark blue", "orange")))))
km <- km[km$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC10", "MIDOC12","MIDOC13","MIDOC33")==F,]
km$pcol <- "black"
km <- ll2prj(km, loncol="lon_start", latcol="lat_start")


# day/night/crep colours
#km$DNC.col <- NA
#km$DNC.col <- ifelse(km$DNC.visual=="D", "yellow", ifelse(km$DNC.visual=="N", "dark blue", ifelse(km$DNC.visual=="NC", "orange", "violet")))
# just non-problem stations
#km <- km[km$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC10", "MIDOC12","MIDOC13","MIDOC33")==F,]
#km$pcol <- "black"
#km <- ll2prj(km, loncol="lon_start", latcol="lat_start")

##TODO: make grey circles around 2, 12 and 33: cannot be included in quantitative comparisons


#time since melt 
usr <- Sys.info()["user"]
wfile <- read.csv("../source data/v3_201516030_waypoints_dec.csv",header=TRUE)

colnames(wfile) <-c("waypt","lat","lon","grp")
wfile <- wfile[-which(wfile$lon > 96),]
ctdstns <- read.table('../source data/stn_loc.txt')

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

par(new=FALSE,mar=c(1.25,1.5,0.15,5.5), oma=c(1,1,1,1), family="serif")
layout(matrix(1:2,2,1,byrow=FALSE), widths=c(3,3), heights=rep(5,5))

plot(extent(bx),bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
mtext(paste("Days since ice melt at survey end (Feb 16 2016)"),side=3,line=1.5,cex=1.5)


plot(tsm_e_ll[[last_day]], col=cols1, zlim=c(-25,185),axes=FALSE,legend=FALSE)




# bathy
# reading in using raadtools if available
# etopo
#cbathy<- crop(readtopo("etopo2"), ras.ext2)
# or gebco
#cbathy<- readtopo("gebco_08", xylim=extent(ras.ext2))
# or if file is present locally as raster tif
cbathy <- raster("../source data/bathy.tif")
# lines
cbc   <- rasterToContour(cbathy, levels=c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0))
cbct  <- spTransform(cbc, CRS(prj))
# for filled
e <- extent(-5411853 , 6235554, -1577161,  1358628)
pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))


# for filled bathy
add_bathy <- function(cols="blues")  {
  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
  if(cols == "greys" ){plotcols <- gray.colors(9, alpha=0.6)}
  if(cols == "blues" ){plotcols <- c(rev(RColorBrewer::brewer.pal(9,"Blues")))}
  image(as(pbathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
}

# ice
# reding in with raadtools if loaded
# icejan <- readice("2016-01-15")
# icenov <- readice("2015-11-15")
# or if the files are present locally as raster tifs
icejan <- raster("../source data/icejan.tif")
icenov <- raster("../source data/icenov.tif")

pij<- projectRaster(icejan, raster(e, crs = prj, res = 1e4))
pin<- projectRaster(icenov, raster(e, crs = prj, res = 1e4))

# fronts
library(orsifronts)
ofp<- spTransform(orsifronts, CRS(prj))

# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

# geographic labels
glabs <- data.frame(
  lon = c(74, 76, 63, 78, 78, 75, 77, 77, 70, 76.5, 78),
  lat = c(-49, -53, -70, -57.5, -58.15, -68, -59, -59.65, -57, -56.5, -65.345),
  lab= c("Kerguelen Is.","Heard Is.","Antarctica","Kerguelen","Plateau","Prydz Bay","Banzare","Bank","Elan Bank","Fawn Trough","Princess Elizabeth\nTrough")
)
glabs <- ll2prj(glabs)

keepOnlyMostComplexLine <- function(x) {
  for (iObj in seq_len(nrow(x))) {
    if (inherits(x, "SpatialLinesDataFrame")) {
      wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x)
        nrow(x@coords)))
      x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
    }
  }
  }
  

#loading KAXIS_FEATURESpoly_five.RData
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_FEATURESpoly_five.RData")
summary(f3)

# Remove the old CRS
sf::st_crs(f3$finished) <- NA

# Set the new CRS
sf::st_crs(f3$finished) <- 4326  # Assuming the new CRS is EPSG code 4326 (WGS 84)

plot(f3$finished)

#loading KAXIS_FEATURESpoly_extras.RData
extra <- load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_FEATURESpoly_extras.RData")
summary(f1)

# Remove the old CRS
sf::st_crs(f1$finished) <- NA

# Set the new CRS
sf::st_crs(f1$finished) <- 4326  # Assuming the new CRS is EPSG code 4326 (WGS 84)

plot(f1$finished)





  
  #ZS: Converting to ggplot 
  # Load necessary libraries
  library(ggplot2)
  library(sf)
  library(raster)
  library(ggspatial)
  library(stars)
  library(ggnewscale)
  
  ## Convert spatial data to sf objects
    wcp_sf <- st_as_sf(wcp)
    cbct_sf <- st_as_sf(cbct)
    ofp_sf <- st_as_sf(ofp)
    wp_sf <- st_as_sf(wp)
    km_sf <- st_as_sf(km)
    ktr_sf <- st_as_sf(ktr)
    bathy_stars <- st_as_stars(pbathy)
    km_sf_total <- km_sf %>% # Aggregate biomass data by midoc station 
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
    
    
   library(cmocean) 
  ggplot() +
    # Base layer
   geom_sf(data = wcp_sf, fill = NA) +
   geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1,151))), alpha = 0.7, show.legend = TRUE) +
   #geom_stars(data = tsm_e_ll, aes(fill = time_since_melt_20160216.nc), alpha = 0.7, show.legend = TRUE) +
    #scale_fill_viridis_c(name = "time since melt", guide = guide_colourbar(theme = theme(legend.title.position = "left",
    #                                                                                    legend.title = element_text(angle = 90))), na.value = NA) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                                         legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    #guides(fill = guide_legend(title = "time since melt", title.position = "left")) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
   annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
   annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    # Add points and labels for aggregated data
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total, aes(fill = total_biomass), shape = 21, size = 3, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3",")")), guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                                                          legend.title = element_text(angle = 90),
                                                                                                          legend.background = element_blank())), 
                         na.value = NA) +
    geom_text(data = km_sf_total, aes(x = lon_end, y = lat_end, label = NA), size = 3, hjust = 0.5, vjust = 1) +
    # Add f3$finished
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    # Customize the plot
    labs(x = "Longitude", y = "Latitude", fill = "Total Biomass") +
    coord_sf(crs = st_crs(prj), xlim = c(-1000000, 1000000), ylim = c(-1000000, 600000)) +
    theme(
      legend.position = "right",
      panel.grid = element_line(color = "gray80", linetype = "solid"),
      legend.background = element_blank(),  # Make legend background transparent
      legend.key = element_blank(),  # Remove legend key,
      legend.title = element_text(angle = 90, hjust = 0.5),
      legend.box.background = element_blank(),
      legend.byrow = TRUE,
      strip.background = element_rect(fill=NA)
    ) +
    guides(fill = guide_legend(title.position = "left", legend.key = element_blank()))

  
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(stars)
  library(cmocean)
  library(ggnewscale) 
  
  
  #TOTAL BIOMASS - Cnidarians + Salps 
  # Filter out cnidarians and salps
  km_filtered <- km_sf %>%
    filter(!(tax.grp %in% c("cnidarians", "salps")))
  
  # Aggregate biomass data by midoc station excluding cnidarians and salps
  km_sf_total <- km_filtered %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )

  # Plot Total Biomass- Cnidarians + salps with colour and size 
  ggplot() +
    # Base layer
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    # Add points and labels for aggregated data
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")) )+ 
    scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
    geom_text(data = km_sf_total, aes(x = lon_end, y = lat_end, label = NA), size = 3, hjust = 0.5, vjust = 1) +
    # Add f3$finished and f1$finished
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    # Customize the plot
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
  
  
  #TOTAL FISH BIOMASS 
  # Filter data to include only fish
  km_fish <- km_sf %>%
    filter(tax.grp == "fish")
  
  # Aggregate biomass data by midoc station for fish only
  km_sf_total_fish <- km_fish %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Plot the data
  ggplot() +
    # Base layer
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    # Add points and labels for aggregated data
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total_fish, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3",")")),
                         guide = "legend") +
    scale_size_continuous(name = expression(paste("Total Biomass m"^"-3",")")),
                          guide = "legend") +
    geom_text(data = km_sf_total_fish, aes(x = lon_end, y = lat_end, label = NA), size = 3, hjust = 0.5, vjust = 1) +
    # Add f3$finished and f1$finished
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    # Customize the plot
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
  
  
  #TOTAL BIOMASS KRILL
  
  # Filter data to include only krill
  km_krill <- km_sf %>%
    filter(tax.grp == "krill")
  
  # Aggregate biomass data by midoc station for krill only
  km_sf_total_krill <- km_krill %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Plot the data
  ggplot() +
    # Base layer
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    # Add points and labels for aggregated data
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total_krill, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")) )+ 
    scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
    geom_text(data = km_sf_total_krill, aes(x = lon_end, y = lat_end, label = NA), size = 3, hjust = 0.5, vjust = 1) +
    # Add f3$finished and f1$finished
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    # Customize the plot
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
  
  
  #TOTAL BIOMASS CEPHALOPOD
  
  # Filter data to include only cephalopod
  km_cephalopods <- km_sf %>%
    filter(tax.grp == "cephalopods")
  
  # Aggregate biomass data by midoc station for krill only
  km_sf_total_cephalopods <- km_cephalopods %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Plot the data
  ggplot() +
    # Base layer
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    # Add points and labels for aggregated data
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total_cephalopods, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")) )+ 
    scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
    geom_text(data = km_sf_total_krill, aes(x = lon_end, y = lat_end, label = NA), size = 3, hjust = 0.5, vjust = 1) +
    # Add f3$finished and f1$finished
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    # Customize the plot
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
  
  
  #TOTAL BIOMASS CEPHALOPOD
  
  # Filter data to include only cephalopod
  km_cnidarians<- km_sf %>%
    filter(tax.grp == "cnidarians")
  
  # Aggregate biomass data by midoc station for krill only
  km_sf_total_cnidarians <- km_cnidarians %>%
    group_by(midoc.stn) %>%
    summarize(
      total_biomass = sum(bm_g_m3, na.rm = TRUE),
      lon_end = first(lon_end),
      lat_end = first(lat_end)
    )
  
  # Plot the data
  ggplot() +
    # Base layer
    geom_sf(data = wcp_sf, fill = NA) +
    geom_stars(data = tsm_e_ll, aes(fill = scales::oob_squish((time_since_melt_20160216.nc), c(-1, 151))), alpha = 0.7, show.legend = TRUE) +
    scale_fill_cmocean(name = "curl", guide = guide_colourbar(theme = theme(legend.title.position = "left",
                                                                            legend.title = element_text(angle = 90)),
                                                              title = "days since melt"), na.value = NA) +
    new_scale_fill() +
    geom_sf(data = ofp_sf, color = "#053061", linetype = "dashed", linewidth = 1.0) +
    geom_sf(data = wp_sf, fill = "dark grey", color = NA) +
    annotate("segment", x = xx, xend = xx, y = min(yy), yend = max(yy), color = "gray40", linetype = "dashed") +
    annotate("segment", y = yy, yend = yy, x = min(xx), xend = max(xx), color = "gray40", linetype = "dashed") +
    # Add points and labels for aggregated data
    geom_sf(data = ktr_sf, size = 1) +
    geom_sf(data = km_sf_total_cnidarians, aes(fill = total_biomass, size = total_biomass), shape = 21, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = expression(paste("Total Biomass m"^"-3")) )+ 
    scale_size_continuous(name = expression(paste("Total Biomass m"^"-3"))) +
    geom_text(data = km_sf_total_krill, aes(x = lon_end, y = lat_end, label = NA), size = 3, hjust = 0.5, vjust = 1) +
    # Add f3$finished and f1$finished
    geom_sf(data = f3$finished, color = "red") +
    geom_sf(data = f1$finished, color = "red") +
    # Customize the plot
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
  
  
 