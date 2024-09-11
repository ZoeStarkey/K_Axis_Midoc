library(ggplot2)

prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied
ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}
wcp <-spTransform(wc, CRS(prj))
wcp_sf <- st_as_sf(wcp)

cbathy <- raster(paste0("/Users/zoestarkey/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/source data/bathy.tif"))
# lines
cbc   <- rasterToContour(cbathy, levels=c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0))
cbct  <- spTransform(cbc, CRS(prj))
# for filled
e <- extent(-5411853 , 6235554, -1577161,  1358628)
cropped_bathy <- crop(cbathy, e)

pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))
bathy_df <- as.data.frame(pbathy, xy = TRUE)
colnames(bathy_df) <- c("x", "y", "value")


load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/ice_df.rda")


library(sf)
ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9, "Blues")), 
                       breaks = c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0),
                       limits = c(-8000, 0),
                       na.value = "grey85",
                       name = "Bathymetry (m)",
                       guide = guide_colorbar(title.position = "left",
                                              title.hjust = 0.5,
                                              label.position = "right",
                                              barwidth = 1,
                                              barheight = 16,
                                              frame.linewidth = 0.2,
                                              title.theme = element_text(size = 14, angle = 90),
                                              label.theme = element_text(size = 14))) +  # Corrected parenthesis
  
  # Add ice layer
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
                                              title.theme = element_text(size = 14, angle = 90),
                                              label.theme = element_text(size = 14))) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = wp_sf, fill = "dark grey", color = NA)  +
  
  # Add labels and set coordinate system
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = st_crs(prj), xlim = c(-500000, 1020000), ylim = c(-1000000, 600000)) 

