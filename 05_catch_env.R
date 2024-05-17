# Script to line up fish data with various environmental datasets and acoustic data and do some basic plots and models
# 9-Nov-2017
# Rowan Trebilco

library(tidyverse) 
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/")
setwd(d)


# station data
km <- readRDS("./derived data/midoc_stations_checked.rds") 
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7)) #ZS: Creating seperate column for each MIDOC station number 
tmp <- read_csv("./source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp) #ZS: joining km and tmp together, but removing the tmp dataframe 
tmp <- read_csv(("./source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp) #

# nav
nav<- readRDS("./derived data/nav_reduced.rds")

# match bestley zones
bz <- read_csv("./source data/midoc_stations_zones.csv")
km$zone <- bz$bestley.zone[match(km$midoc.stn, bz$midoc.stn)] #ZS: creating new column called zones and filling it with bestley zone 
                                                              #ZSQ: why not use join function? is it because you want to retain the original dataset? 

# ktr <- read_csv("./source data/v3_201516030_waypoints_dec.csv")
# colnames(ktr) <- c("wp","lat","lon","wp.grp")
# plot checking locations etc
# ggplot(km, aes(y=lat_start, x=lon_start)) + geom_point() + geom_text(aes(label=substr(midoc.stn,6,8))) + geom_path(data=ktr, aes(x=lon, y=lat)) + geom_path(data=nav, aes(x=LONGITUDE, y=LATITUDE)) # this is a reminder that some of the locations in the waypoints file are incorrect - better to use nav.

# oceanographic summary data
oc <- readRDS("./source data/k_axis_oceanog_summ.Rda")
# deep parameters
dp <- read_csv("./source data/KAXIS_deep_param.csv", col_names=c('station', 'Smax', 'O2_min', 'Tmax', 'Tmin','y','m','d'))

# # compare 'deep parameters' with previous summary info
oc <- inner_join(oc, dp) %>% select(-Tmin_value) #ZS: joins oc and dp together, then selects everything but Tmin_value
# oc$Tmin_value - oc$Tmin # Tmins are the same
rm(dp) 

# chlorophyll and ice
chl_ice <- read_rds("./derived data/k-axis-midoc-satdata-extraction_2017-11-09.rds")

## TODO: locations of stations 4 and 5 are a bit off...
## in the short-term, this can be corrected with linear interpolation as per below; but a better solution would be to re-extract
## probably immaterial for chlorophyll given the approach; but should be checked for ice
# ggplot(km, aes(x=lon_start, y=lat_start)) + geom_point() + geom_path(data=nav, aes(x=LONGITUDE, y=LATITUDE)) +geom_point(data=chl_ice, aes(x=LONGITUDE,y=LATITUDE), col="blue", size=3) + geom_text(data=chl_ice, aes(label=substr(midoc.stn, 6,8), x=LONGITUDE, y=LATITUDE))

# linear interpolation of variables based on time
oc$gmt <- ISOdatetime(2016, oc$month, oc$day, 0, 0, 0, tz = "GMT") #ZS: time based on month and day columns of oc
km$Tmin <- approxfun(oc$gmt, oc$Tmin, rule = 2)(km$start_time) #ZS: approxfun = function that interpolates between given data points
km$Tmin_depth <- approxfun(oc$gmt, oc$Tmin_depth, rule = 2)(km$start_time)
km$Tmax <- approxfun(oc$gmt, oc$Tmax, rule = 2)(km$start_time)
km$SML <- approxfun(oc$gmt, oc$SML_preferred_estimate, rule = 2)(km$start_time)
km$Smax <- approxfun(oc$gmt, oc$Smax, rule = 2)(km$start_time)
km$O2_min <- approxfun(oc$gmt, oc$O2_min, rule = 2)(km$start_time)
km$days_since_melt <- approxfun(chl_ice$datetime, chl_ice$days_since_melt, rule = 2)(km$start_time)
km$distance_to_ice_m <- approxfun(chl_ice$datetime, chl_ice$distance_to_ice_m, rule = 2)(km$start_time)
km$distance_to_edge_m <- approxfun(chl_ice$datetime, chl_ice$distance_to_edge_m, rule = 2)(km$start_time)
km$sea_ice_conc <- approxfun(chl_ice$datetime, chl_ice$sea_ice_conc, rule = 2)(km$start_time)
km$sea_ice_conc <- approxfun(chl_ice$datetime, chl_ice$sea_ice_conc, rule = 2)(km$start_time)
km$chl_rs <- approxfun(chl_ice$datetime, chl_ice$chl, rule = 2)(km$start_time)

km <- km %>% select(-bestley.zone, -DNC.auto)

# in-situ integrated chlorophyll from Karen Westwood
intchl <- readxl::read_xlsx("./source data/Final Integrated Chlorophyll.xlsx", skip = 4)
colnames(intchl) <- c("CTD.n", "lat", "lon", "no", "intChl", "comments")

# CTD station locations from this file were visually matched to closest midoc stations using the following plot; with the matches recorded in midoc_CTD_matching.xlsx
# ggplot(nav, aes(y=LATITUDE, x=LONGITUDE)) +geom_path() + geom_point(data=km, aes(x=lon_start, y=lat_start), col="blue") + geom_point(data=km, aes(x=lon_end, y=lat_end), col="red") +geom_point(data=intchl, aes(x=lon, y=-lat), col="green") + geom_text(data=km, aes(x=lon_end, y=lat_end, label=substr(midoc.stn, 6,8)), col="red", hjust=0) + geom_text(data=intchl, aes(x=lon, y=-lat, label=CTD.n), col="dark green", hjust=1.2)

ctd.midoc <- readxl::read_xlsx("./source data/midoc_CTD_matching.xlsx", skip=1)
intchl$midoc <- ctd.midoc$MIDOC[match(intchl$no, ctd.midoc$CTD)]

km$intChl <- intchl$intChl[match(km$midoc.stn, paste0("MIDOC",intchl$midoc))] #ZS: matching midocs and pasting the word midoc infront 

saveRDS(km, "./derived data/midoc_stations_envdata.rda")
readr::write_csv(km, "./derived data/midoc_stations_envdata.csv")


