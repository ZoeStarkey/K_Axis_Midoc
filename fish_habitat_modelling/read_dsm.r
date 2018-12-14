library(tidyverse)
library(raadtools)

m <- readRDS("~/kaxis/derived data/midoc_stations_checked.rds")
mras <- readRDS("~/kaxis/fish_habitat_modelling/k-axis_raster.rds")
projection(mras) <- "+proj=longlat +datum=WGS84"

m <- data.frame(m)

m$dates <- strftime(m$start_time, "%Y-%m-%d", tz="UMT")

#################################################################
##read in time since melt
m.dsm <- mean(readderivice(m$dates, product = c("time_since_melt"), xylim = extent(mras)), na.rm=T)
mdsm <- resample(m.dsm, mras, method = "bilinear")##Fit to k-axis grid
plot(mdsm)