# laurence_midoc_map.R
# map of sampling station locations for Laurence Clark DNA fish diet analysis DSRII paper
# 6 September 2018
# updated version

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/laurence dsrII paper map")
setwd(d)

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

# projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"


# plot extent
ras.ext   <- raster(xmn=60, xmx=95, ymn=-70, ymx=-40) # extent for zoomed map
ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

# cropped and full versions of world map
# cropped version is used to make a suitable extent for plotting, full is over-plotted so that the continents don't  show the "wedge" defined by the extent
wc <- crop(countriesHigh, ras.ext)
wcp <-spTransform(wc, CRS(prj))
wp  <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj))

# bathy
cbathy <- raster("../source data/bathy.tif")
#cbathy<- readtopo("gebco_08", xylim=extent(ras.ext2))
# lines
cbc   <- rasterToContour(cbathy, levels=c(-500,-1000,-2000,-3000, -6000))
cbct  <- spTransform(cbc, CRS(prj))
# for filled
e <- extent(-5411853 , 6235554, -1577161,  1358628)
pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))



# for filled bathy
add_bathy <- function()  {
  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
  plotcols <- gray.colors(9)
  image(as(pbathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
}

# ice
# icejan <- readice("2016-01-15")
# icenov <- readice("2015-11-15")
# 
# pij<- projectRaster(icejan, raster(e, crs = prj, res = 1e4))
# pin<- projectRaster(icenov, raster(e, crs = prj, res = 1e4))

# fronts
library(orsifronts)
ofp<- spTransform(orsifronts, CRS(prj))

# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

# midoc stations
midoc_stns <- readRDS("../derived data/midoc_stations_checked.rds")
coordinates(midoc_stns) <- c("lon_start", "lat_start")
projection(midoc_stns) <- "+proj=longlat +datum=WGS84"
midoc_stns <- spTransform(midoc_stns, CRS(prj))

# track
nav <- readRDS("../derived data/nav_reduced.rds")
coordinates(nav) <- c("LONGITUDE", "LATITUDE")
projection(nav) <- "+proj=longlat +datum=WGS84"
nav <- spTransform(nav, CRS(prj))

pdf("laurence_midoc_map.pdf", width=4, height=4)
op<- par(mar=rep(0,4), oma=rep(0.5,4))
plot(wcp, border=NA)

add_bathy() # filled bathy
plot(cbct, col="grey", add=T) # bathy contours
# ice and fronts
plot(ofp, add = TRUE, col="#053061", lty=3, lwd=2)

plot(wp, add=T, col="darkgrey", border=F)
plot(graticule(lons = xx, lats = yy,  proj = prj), add=T, lty=2, col="gray40")
lines(nav$LONGITUDE, nav$LATITUDE)
points(midoc_stns, pch=21, col="gray40")


sstns <- midoc_stns[midoc_stns$midoc.stn %in% c("MIDOC16","MIDOC17","MIDOC22","MIDOC23"),]
points(sstns, pch=19)
text(sstns, labels=substr(sstns$midoc.stn,6,7), pos=2, cex=0.7, offset=0.3)

g1labs <- graticule_labels(lons=c(150, 120,90,60), xline=180, yline=-50, proj=projection(prj))
text(coordinates(g1labs[g1labs$islon, ]), lab=parse(text=g1labs$lab[g1labs$islon]), pos=3, cex=0.8, col="gray30")
g2labs <- graticule_labels(lats=c(-40, -50,-60,-70), , xline=60, yline=-50, proj=projection(prj))
text(coordinates(g2labs[!g2labs$islon, ]), lab=parse(text=g2labs$lab[!g2labs$islon]), pos=1, cex=0.8,col="gray30")


box()
par(op)
dev.off()