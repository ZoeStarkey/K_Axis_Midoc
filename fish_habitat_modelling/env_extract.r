##extract the environmental data for each station

library(raster)

env.stck <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
ras <- readRDS("~/kaxis/fish_habitat_modelling/k-axis_raster.rds")
m <- readRDS("~/kaxis/derived data/midoc_stations_checked.rds")

projection(mras) <- "+proj=longlat +datum=WGS84"
m <- data.frame(m)
m1 <- m
coordinates(m1) <- c("lon_start", "lat_start")
projection(m1) <- "+proj=longlat +datum=WGS84"
projection(env.stck) <- "+proj=longlat +datum=WGS84"

##exctract varaibles and add to the station data
m1$bathy <- extract(env.stck$bathy, m1, method="bilinear")
m1$bath_g <- extract(env.stck$bath_g, m1, method="bilinear")
m1$v_vel500 <- extract(env.stck$v_vel500, m1, method="bilinear")
m1$sst <- extract(env.stck$sst, m1, method="bilinear")
m1$sstg <- extract(env.stck$sstg, m1, method="bilinear")
m1$ssh <- extract(env.stck$ssh, m1, method="bilinear")
m1$ssha <- extract(env.stck$ssha, m1, method="bilinear")
m1$vssha <- extract(env.stck$vssha, m1, method="bilinear")
m1$wind <- extract(env.stck$wind, m1, method="bilinear")
m1$curr <- extract(env.stck$curr, m1, method="bilinear")
m1$ice <- extract(env.stck$ice, m1, method="bilinear")
m1$chl <- extract(env.stck$chl, m1, method="bilinear")

saveRDS(m1, file="~/kaxis/fish_habitat_modelling/station_env.rds")
