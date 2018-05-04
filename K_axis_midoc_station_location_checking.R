# K_axis_midoc_station_location_checking.R
# script to compare definitions of station locations and resolve discrepancies - for presentation in publications
# 30 April 2018

setwd("/Users/rowan/GitHub/K_axis_midoc/source\ data/")
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

# voyage waypoints
ktr <- read_csv("v3_201516030_waypoints_dec.csv")
colnames(ktr) <- c("wp","lat","lon","wp.grp")

# voyage track
vt <- readRDS("thinned_voyage_track.rds")


ggplot(data=subset(ktr, ktr$lat< -55), aes(y=lat, x=lon)) +
	geom_point(col="black") +
	geom_point(data=vt, aes(x=LONGITUDE, y=LATITUDE), col="grey", cex=0.5)+
	geom_label(data=SB_st, col="blue", alpha=0.5, aes(x=longitude, y=latidue, label=station)) +
	geom_label(data=AW_st, col="red", alpha=0.5, aes(x=lon, y=lat, label=Station)) +
	geom_label(data=md_st, col="green", alpha=0.5, aes(x=LONGITUDE, y=LATITUDE, label=midoc.stn)) +
	theme_bw()


