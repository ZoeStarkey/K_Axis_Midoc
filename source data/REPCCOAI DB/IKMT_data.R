library(tidyverse)
library(raadtools)
library(trip)

#REPCCOAI fish data
#read in abundance poisson data 2017 & 2018
library(readr)
pa <- read.csv("Poisson_Baptiste.csv")

#read in station data for 2017 only (Boris to provide 2018 data?)
sd <- read.delim("IKMT_station_data.txt")
sd <- data.frame(sd)
colnames(sd)[24] <- "cable.down.speed.per.mn"
colnames(sd)[25] <- "cable.up.speed.per.mn"

read.csv()
#merge fish abundace and station datasets
#pull out 2017 data from pa
pa_sd <- merge(pa, sd, by="station.IKMT")

##calculate total swept area
#https://www.hydrobios.de/product/ikmt-isaacs-kidd-midwater-trawl-net - Mouth opening: 7m2
#sd$total.distance
#sd$estimated.depth (check depth.uncertainty)
#evaluation of the sampling effort in order to correct abundance values (depth is one of them but perhaps it is not sufficient, maybe add trawl time). 

##read in CTD data for station IK2017-1 only!!
ctd <- read.csv("data_CTD_Andrea.csv")
ctd$depth <-   ctd$profondeur
ctd$pres.decibars <- ctd$PRES..decibar.
ctd$temp.deg.C <- ctd$TEMP..degree_Celsius.
ctd$PSAL.psu <- ctd$PSAL..psu.

obs <- readRDS("IKMT_fish_data.rds")
colnames(obs)[1] <- "station.IKMT"
ikmt <- readRDS("IKMT_station_data.rds")
#ikmt <- c("station.IKMT","bottom.duration", "cable.length", "estimated.depth", "max.recorded.depth", "depth.uncertainty", "sample", "total.distance", "avg.speed", "down.speed")
coord <- readRDS("IKMT_coords.rds")
#coord <- coord[coord$IKMT.start.end=="start",]

obs_coord <- merge(obs, coord, by="station.IKMT")
obs_coord_data <- merge(obs_coord, ikmt, by="station.IKMT")
write_rds(obs_coord_data, "ikmt_fish_data_2017.rds")
ik <- readRDS("~/ObsAustral_2017_fish/ikmt_fish_data_2017.rds")
str(ik)


