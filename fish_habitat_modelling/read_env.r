library(tidyverse)
library(raadtools)
library(trip)

m <- readRDS("~/kaxis/derived data/midoc_stations_checked.rds")
mras <- readRDS("~/kaxis/fish_habitat_modelling/k-axis_raster.rds")
projection(mras) <- "+proj=longlat +datum=WGS84"
m <- data.frame(m)

##read in static layers
##list of layers to read in
env_layers=c("bathymetry.nc", "bathymetry_slope.nc",  "vertical_velocity_500.nc")

envfiles=file.path("/rdsi/PUBLIC/raad/data/webdav.data.aad.gov.au/data/environmental/derived/antarctic/netcdf",env_layers)
bathy =stack(envfiles)
stat <- resample(bathy, mras, method="bilinear") ##reset to k-axis raster

##read in dynamic layers
m$dates <- strftime(m$start_time, "%Y-%m-%d", tz="UMT")

##############################################################
##read in the SST 
sst <- readsst(unique(m$dates), xylim=extent(mras), lon180 = FALSE) #uses the extent of the original, un-projected trip grid
m.sst <- mean(sst)
msst <- resample(m.sst, mras, method = "bilinear") ##Fit to k-axis grid
plot(msst)

##############################################################
##read in the SST and calculate SST gradient
sstg <- mean(terrain(sst, opt="slope", neighbors = 8))
msstg <- resample(sstg, mras, method = "bilinear") ##Fit to k-axis grid
plot(msstg)

###################################################################
##read in the SSHa
ssh <-readssh(unique(m$dates), xylim=extent(mras), ssha=F, lon180 = FALSE)
mssh <- mean(ssh, na.rm=T)
mssh <- resample(mssh, mras, method = "bilinear")##Fit to k-axis grid
plot(mssh)

###################################################################
##read in the SSHa
ssha <-readssh(unique(m$dates), xylim=extent(mras), ssha=T, lon180 = FALSE)
mssha <- mean(ssha, na.rm=T)
mssha <- resample(mssha, mras, method = "bilinear")##Fit to k-axis grid
plot(mssha)

##############################################################
##read in the SSHa and express and variance
v.ssha <- calc(ssha, var, na.rm=T)
vssha <- resample(v.ssha, mras, method = "bilinear")##Fit to k-axis grid
plot(vssha)

###################################################
##read in the wind
wind <- mean(readwind(unique(m$dates), xylim=extent(mras), magonly = TRUE, lon180 = FALSE), na.rm=T)
mwind <- resample(wind, mras, method = "bilinear")##Fit to k-axis grid
plot(mwind)

##################################################################
##read in the currents
m.curr <- mean(readcurr(unique(m$dates), xylim=extent(mras), magonly = TRUE, lon180 = FALSE ), na.rm=T)
mcurr <- resample(m.curr, mras, method = "bilinear")##Fit to k-axis grid
plot(mcurr)

##################################################################
##read in the ice
ice <- mean(readice(unique(m$dates)), na.rm=T)
mice <- projectRaster(ice, mras) 
mice[mice==0] <- NA
plot(mice)

##################################################################
##read in the chla
m.chl<- mean(readchla(unique(m$dates), xylim=extent(mras), algorithm  = "johnson"), na.rm=T)
mchl <- resample(m.chl, mras, method = "bilinear")##Fit to k-axis grid
plot(mchl)

#################################################################
##read in time since melt

dsm <- mean(readderivice(unique(m$dates), time.resolution = c("daily"), product = c("time_since_melt")), na.rm=TRUE)
mdsm<- projectRaster(dsm, mras)
mdsm[mdsm > (364*1)] <- 364*1 ##remove locations where ice has not been present for a year (or 2)
plot(mdsm)

#################################################################
##read in distance ot ice edge
  readDTIE <- function(date) {
    out <- lapply(1:length(date), function(i){
      cat(sprintf('\nprocessing %d of %d\n',i,length(date))); flush.console()
      dist <- distance_to_ice_edge(unique(date[i]), threshold = 15)
      tmp <- list(file=dist)
    })
    out
  } 

d <- readDTIE(unique(m$dates))
tmp <- lapply(d, function(x) x$file)
dtie <- do.call(stack, tmp) 
dtie <- mean(dtie, na.rm=TRUE)
mdtie<- projectRaster(dtie, mras)
plot(mdtie)
################################################################
## make a stack of all envornmental layers
env.stck <- stack(stat, msst, msstg, mssh, mssha, vssha, mwind, mcurr, mice, mchl, mdsm, mdtie)
names(env.stck) <- c("bathy", "bath_g", "v_vel500", "sst", "sstg", "ssh", "ssha", "vssha", "wind", "curr", "ice", "chl", "dsm", "dtie")

saveRDS(env.stck, file="~/kaxis/fish_habitat_modelling/env_data.rds")
