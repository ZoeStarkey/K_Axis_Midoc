##runs "monthly_sum_functions" to give monthy summaries
rm(list=ls(all=TRUE))
library(raster)
library(trip)
setwd("/user/mark/esap")
load("~/esap/data/metadata.Rdata")
load("~/esap/data/filt_trips.Rdata")
source('~/esap/code/analysis/month_sum_functions.r')
names(meta)
##1. restrict to seals deployed at Kerguelen
m <- merge(d3.filt, esap_meta[,c("ref", "location", "sex")], by.x="id", by.y="ref", all.x=TRUE)

k <- m[grep("Kerguelen", m$location, ),]
k$location <- "kerguelen"
##convert to -180 to 180
##this is a primative fix that needs to be fixed at the SSM level
##need to talk with Ian
k <- k[k$lon >= 0,]


save(k, file="kerg_locs.RData")

##make trip object and raster at user defined resolution
gres <- 10000
r <- makeRaster(k, gres)
ptr <- r[[1]]
gt1 <- r[[2]]

##estimate an optimal grid size for a particular time of the year - typically a month
k$month <- as.POSIXlt(k$date)$mon
