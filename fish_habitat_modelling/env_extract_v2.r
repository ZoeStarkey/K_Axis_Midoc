##extract the environmental data for each station
##version 2 extracts directrly fro data repository
rm(list=ls(all=TRUE))
library(raster)
library(raadtools)
env.stck <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
mras <- readRDS("~/kaxis/fish_habitat_modelling/k-axis_raster.rds")
m <- readRDS("~/kaxis/derived data/midoc_stations_checked.rds")

projection(mras) <- "+proj=longlat +datum=WGS84"
m <- data.frame(m)
m1 <- m
m1$dates <- strptime(m1$start_time, "%Y-%m-%d", tz="UMT")
#coordinates(m1) <- c("lon_start", "lat_start")
#projection(m1) <- "+proj=longlat +datum=WGS84"
#projection(env.stck) <- "+proj=longlat +datum=WGS84"

saveRDS(env.stck, file="~/kaxis/fish_habitat_modelling/env_stk.rds")

##exctract varaibles and add to the station data
m1$bathy <- extract(env.stck$bathy, m1[,c("lon_end", "lat_end")], method="bilinear")
m1$bath_g <- extract(env.stck$bath_g, m1[,c("lon_end", "lat_end")], method="bilinear")
m1$v_vel500 <- extract(env.stck$v_vel500, m1[,c("lon_end", "lat_end")], method="bilinear")
m1$sst <- extract(readsst, m1[,c("lon_end", "lat_end", "dates")], lon180 = FALSE,method="bilinear")
m1$sstg <- extract(env.stck$sstg, m1[,c("lon_end", "lat_end")], method="bilinear")
m1$ssh <- extract(readssh, m1[,c("lon_end", "lat_end", "dates")], lon180 = FALSE, method="bilinear")
m1$ssha <- extract(readsst, m1[,c("lon_end", "lat_end", "dates")], ssha=F, lon180 = FALSE, method="bilinear")
m1$wind <- extract(readwind, m1[,c("lon_end", "lat_end", "dates")], lon180 = FALSE,method="bilinear")
m1$curr <- extract(readcurr, m1[,c("lon_end", "lat_end", "dates")], lon180 = FALSE,method="bilinear")
m1$ice <- extract(readice, m1[,c("lon_end", "lat_end", "dates")])
m1$chl <- extract(readchla, m1[,c("lon_end", "lat_end", "dates")])
m1$dsm <- extract(readderivice, m1[,c("lon_end", "lat_end", "dates")], product = c("time_since_melt"), method="bilinear")
m1$dtie <- extract(distance_to_ice_edge, m1[,c("lon_end", "lat_end", "dates")], threshold = 15)

saveRDS(m1, file="~/kaxis/fish_habitat_modelling/station_env.rds")
