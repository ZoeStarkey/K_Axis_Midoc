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

# onboard logger data from midoc
md <- readRDS("midoc_data.rds") ## note that something is wrong with the locations here
# what's happening for the NA deployment on Jan 28th? Might be MIDOC13?

# plotting to check
ggplot(md, aes(x=datetime, y=Pressure..dbar., colour=status)) + geom_point() + facet_wrap(~deployment, scales = "free_x")
ggsave("midoc_deployment_pressure.pdf", height=16, width=20)

md$midoc<- 


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

# checking start and end points for calculating swept volumes
smn <- readRDS("scanmar_nets.rds")
nav <- readRDS("navigation_1min.rds")

ggplot(ktr, aes(x=lon, y=lat)) + geom_path() + geom_path(data=nav, aes(y=LATITUDE, x=LONGITUDE), col="blue")

# start and end for each midoc shot
md.se <- smn %>% group_by(station) %>% summarise(t.start=first(time), t.end=last(time))


# start and end for each codend
ce.se <- smn %>% group_by(station, status) %>% summarise(t.start=first(time), t.end=last(time))

# super-impose these on the midoc plot from about, laid out linear; make sure all lines up
ggplot(md %>% filter(filenum>1), aes(x=datetime, y=Pressure..dbar.)) + geom_path() + geom_text(data=md_st, aes(x=datetime, y=500, label=substr(midoc.stn,6,7), col="red")) + 
	geom_vline(data=md.se, aes(xintercept=t.start),colour="dark green")+
	geom_vline(data=md.se, aes(xintercept=t.end),colour="red")


