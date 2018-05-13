# K_axis_midoc_station_location_checking.R
# script to compare definitions of station locations and resolve discrepancies - for presentation in publications
# 30 April 2018

setwd("/Users/rowan/GitHub/K_axis_midoc/source\ data/")
setwd("/Users/dougt/GitHub/K_axis_midoc/source\ data/")
library(readr)
library(dplyr)
library(ggplot2)
AW <- read_csv("SIA results_Kaxis_MIDOC15_36_AW.csv")

# station locations Andrea has been using (just main transects)
AW_st <- AW %>% select(Station, lat, lon) %>% distinct()

# station locations from Sophie
SB_st <- readRDS("k_axis_oceanog_summ.Rda")

# station locations from MIDOC logger
md_st <- readRDS("midoc_stations_locations_times.rds")

# onboard logger data from midoc
md <- readRDS("midoc_data.rds") ## note that something is wrong with the locations here

# also have voyage track for reference
ktr <- read_csv("v3_201516030_waypoints_dec.csv") 
colnames(ktr) <- c("wp","lat","lon","wp.grp")

vt<- readRDS("thinned_voyage_track.rds")

ggplot(data=vt, aes(x=LONGITUDE ,y=LATITUDE)) + geom_point(col="grey", size=.2) + theme_bw() + 
	geom_text(data=SB_st, aes(x=longitude ,y=latidue, label=station), col = "green") +
	geom_text(data=md_st, aes(x=LONGITUDE ,y=LATITUDE, label=substr(midoc.stn, 6,8)),col = "blue") +
	geom_text(data=AW_st, aes(x=lon ,y=lat, label=Station),col = "purple") +
	geom_point(data=md_st, aes(x=LONGITUDE ,y=LATITUDE), col="red")

# to illustrate the problems with the midoc logger data
last_plot() + 	geom_point(data=md %>% filter(Latitude< -55 & Longitude>69), aes(x=Longitude, y=Latitude))

