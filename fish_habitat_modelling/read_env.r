library(tidyverse)
library(raadtools)
library(trip)

b <- readRDS("~/kaxis/derived data/codend_fish_biomass.rds")
m <- readRDS("~/kaxis/derived data/midoc_stations_checked.rds")

b <- data.frame(b)
m <- data.frame(m)

##make a raster that matches the extent of the data
coordinates(m) <- c("lon_start", "lat_start")
res <- 0.1 ##set resolutuion of raster in degrees
nlat <- ceiling(diff(range(m@coords[,1])))/res ##set the number of columns
nlon <- ceiling(diff(range(m@coords[,2])))/res ##set the number of rowss

mras <- raster(extent(m)+2, nrows=nlon, ncols=nlat) ##make the raster

##read in static layers
##list of layers to read in
env_layers=c("bathymetry.nc", "bathymetry_slope.nc", "chl_summer_climatology.nc", "seaice_gt85.nc",
             "ssh_spatial_gradient.nc", "vertical_velocity_500.nc")

envfiles=file.path("/rdsi/PUBLIC/raad/data/webdav.data.aad.gov.au/data/environmental/derived/antarctic/netcdf",env_layers)
bathy =stack(envfiles)
stat <- resample(bathy, mras, method="bilinear") ##reset to k-axis raster

