# simple generic map for k-axis stations

# raadtools can be used for sourcing environmental data directly rather than loading from local files, but you have to be on the aad network (or using an raadtools rstudio server)
# library(raadtools)

# remaining packages should all be available on CRAN
library(graticule)
library(raster)
library(rworldxtra)
data(countriesHigh)
# library(rworldmap)
# data(countriesHigh)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(readr)

# projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"

ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
}

# plot extent
ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40) # extent for zoomed map
ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

# cropped and full versions of world map
# cropped version is used to make a suitable extent for plotting, full is over-plotted so that the continents don't  show the "wedge" defined by the extent
data(countriesHigh)
wc <- crop(countriesHigh, ras.ext)
wcp <-spTransform(wc, CRS(prj))
wp  <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj))

# K-Axis stations
kos <- readRDS("./source data/k_axis_oceanog_summ.Rda")
kos <- ll2prj(kos, loncol="longitude", latcol="latidue")

ktr <- read_csv("./source data/v3_201516030_waypoints_dec.csv")
colnames(ktr) <- c("wp","lat","lon","wp.grp")
ktr <- ktr[-1,]
ktr <- ll2prj(ktr)

km <- readRDS("./source data/midoc_stations_locations_times.rds")
tmp <- read_csv("./source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("./source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)]

# day/night/crep colours
km$DNC.col <- NA
km$DNC.col <- ifelse(km$DNC.visual=="D", "yellow", ifelse(km$DNC.visual=="N", "dark blue", ifelse(km$DNC.visual=="NC", "violet", "orange")))
km <- ll2prj(km, loncol="LONGITUDE", latcol="LATITUDE")

# bathy
  # reading in using raadtools if available
  # etopo
  #cbathy<- crop(readtopo("etopo2"), ras.ext2)
  # or gebco
  #cbathy<- readtopo("gebco_08", xylim=extent(ras.ext2))
  # or if file is present locally as raster tif
  cbathy <- raster("./source data/bathy.tif")
  # lines
  cbc   <- rasterToContour(cbathy, levels=c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0))
  cbct  <- spTransform(cbc, CRS(prj))
  # for filled
  e <- extent(-5411853 , 6235554, -1577161,  1358628)
  pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))
  

# for filled bathy
add_bathy <- function(cols="blues")  {
  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
  if(cols == "greys" ){plotcols <- gray.colors(9, alpha=0.6)}
  if(cols == "blues" ){plotcols <- c(rev(RColorBrewer::brewer.pal(9,"Blues")))}
  image(as(pbathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
}

# ice
  # reding in with raadtools if loaded
  # icejan <- readice("2016-01-15")
  # icenov <- readice("2015-11-15")
  # or if the files are present locally as raster tifs
  icejan <- raster("./source data/icejan.tif")
  icenov <- raster("./source data/icenov.tif")

pij<- projectRaster(icejan, raster(e, crs = prj, res = 1e4))
pin<- projectRaster(icenov, raster(e, crs = prj, res = 1e4))

# fronts
library(orsifronts)
ofp<- spTransform(orsifronts, CRS(prj))

# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

# if there are locations you want to show
# (note that the structure() call below just does the same thing as reading a table of locations from text/csv)
  # locs <- structure(list(Cruise = structure(c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 
  # 2L), .Label = c("", "V2", "V3"), class = "factor"), Station = structure(c(7L, 
  # 3L, 4L, 9L, 6L, 2L, 8L, 5L), .Label = c("", "R23", "R29", "R37", 
  # "R8", "T11", "T20", "T29", "T40"), class = "factor"), Type = structure(c(3L, 
  # 2L, 2L, 3L, 3L, 2L, 3L, 2L), .Label = c("", "R", "T"), class = "factor"), 
  #     Date.Time = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L), .Label = c("", 
  #     "2016-01-01T00:00", "2016-12-21T00:00"), class = "factor"), 
  #     Long = c(81.886667, 83.353333, 83.580267, 63.9278, 83.8392, 
  #     85.033367, 77.616784, 110.0646846), Lat = c(-59.921667, -57.93, 
  #     -60.319233, -66.17905, -64.266017, -61.933375, -61.283533, 
  #     -66.1313351), Depth = c(220L, 20L, 10L, 20L, 15L, 10L, 60L, 
  #     160L), Species = structure(c(3L, 4L, 4L, 2L, 2L, 3L, 2L, 
  #     5L), .Label = c("", "E. superba", "E. triacantha", "E. vallentini", 
  #     "E.crystallorophias"), class = "factor")), .Names = c("Cruise", 
  # "Station", "Type", "Date.Time", "Lon", "Lat", "Depth", "Species"
  # ), row.names = c(NA, 8L), class = "data.frame")
  # # convert to spatial points df
  # coordinates(locs) <- c("Lon", "Lat")
  # # define projection as longlat
  # projection(locs) <- "+proj=longlat +datum=WGS84"
  # # reproject to map
  # locs <- spTransform(locs, CRS(prj))

# geographic labels
glabs <- data.frame(
  lon = c(74, 76, 63, 78, 78, 75, 77, 77, 70, 75, 78),
  lat = c(-49, -53, -70, -57.5, -58, -68, -59, -59.5, -57, -56.5, -65.345),
  lab= c("Kerguelen Is.","Heard Is.","Antarctica","Kerguelen","Plateau","Prydz Bay","Banzare","Bank","Elan Bank","Fawn Trough","Princess Elizabeth Trough")
)
glabs <- ll2prj(glabs)


# TODO:
# orsi fronts
# max/min ice extent
# subset to midoc (/RMT/CTD) stations
# add labelling

pdf("K_axis_stations_good_midoc_shots_DNC.pdf", width=6, height=6)
op<- par(mar=rep(0,4), oma=rep(0.5,4))
plot(wcp, border=NA)

add_bathy() # filled bathy
plot(cbct, col="grey", add=T) # bathy contours
# ice and fronts
plot(ofp, add = TRUE, col="#053061", lty=3, lwd=2)
plot(rasterToContour(pij, lev = 15),add = TRUE, lty=1, col="blue", lwd=1.5)
plot(rasterToContour(pin, lev = 15),add = TRUE, lty=1, col="blue", lwd=1.5)

plot(wp, add=T, col="darkgrey", border=F)
plot(graticule(lons = xx, lats = yy,  proj = prj), add=T, lty=2, col="gray40")
points(ktr, type="l", lwd=1.5)
# can add large points for midoc stations by un-commenting line below
points(kos, pch=19, col="gray20", cex=0.6)

# points(km, pch=19, col="green") # all placed midoc went in water
# points(km[km$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")==F,], pch=19, col="green") # things where
points(km[km$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")==F,], pch=19, col=km$DNC.col) # things where

g1labs <- graticule_labels(lons=c(150, 120,90,60), xline=180, yline=-50, proj=projection(prj))
text(coordinates(g1labs[g1labs$islon, ]), lab=parse(text=g1labs$lab[g1labs$islon]), pos=3, cex=0.8, col="gray30")
g2labs <- graticule_labels(lats=c(-40, -50,-60,-70), , xline=60, yline=-50, proj=projection(prj))
text(coordinates(g2labs[!g2labs$islon, ]), lab=parse(text=g2labs$lab[!g2labs$islon]), pos=1, cex=0.8,col="gray30")


text(glabs[1:9,], labels=glabs$lab[1:9], family="Times", cex=0.8)
text(glabs[10:11,], labels=glabs$lab[10:11], family="Times", srt=25, cex=0.6)

box()

# now for the inset
source("setup_inset.R")
par(new = TRUE)#, mar=c(0,0,0,0))
par(fig = c(0.6325, 0.6325+0.3, 0.0025, 0.0025+0.2))
plot(w,col="white",border=FALSE)
plot(g3,add=TRUE,lty=3)
plot(macca,col="white",border=FALSE,add=TRUE,pch=19,cex=0.3)
g <- graticule(c(60, 95), c(-70, -55), proj = "+proj=laea +lat_0=-90 +datum=WGS84")
pg <- spTransform(g, CRS(pprj))
plot(gPolygonize(pg), col = NA, add=T, border=TRUE)
plot(g4,add=TRUE,lwd=2)
pltg()

par(op)
dev.off()
