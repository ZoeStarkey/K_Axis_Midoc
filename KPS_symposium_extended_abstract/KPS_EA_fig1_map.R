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

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/KPS_symposium_extended_abstract")
setwd(d)


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
kos <- readRDS("../source data/k_axis_oceanog_summ.Rda")
kos <- ll2prj(kos, loncol="longitude", latcol="latidue")

ktr <- readRDS("../derived data/nav_reduced.rds")
# colnames(ktr) <- c("wp","lat","lon","wp.grp")
ktr <- ll2prj(ktr, loncol="LONGITUDE", latcol="LATITUDE")

km <- readRDS("../derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
tmp <- read_csv("../source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("../source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)

# day/night/crep colours
km$DNC.col <- NA
km$DNC.col <- ifelse(km$DNC.visual=="D", "yellow", ifelse(km$DNC.visual=="N", "dark blue", ifelse(km$DNC.visual=="NC", "orange", "violet")))
# just non-problem stations
km <- km[km$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC10", "MIDOC12","MIDOC13","MIDOC33")==F,]
km$pcol <- "black"
km <- ll2prj(km, loncol="lon_start", latcol="lat_start")

##TODO: make grey circles around 2, 12 and 33: cannot be included in quantitative comparisons

# bathy
  # reading in using raadtools if available
  # etopo
  #cbathy<- crop(readtopo("etopo2"), ras.ext2)
  # or gebco
  #cbathy<- readtopo("gebco_08", xylim=extent(ras.ext2))
  # or if file is present locally as raster tif
  cbathy <- raster("../source data/bathy.tif")
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
  icejan <- raster("../source data/icejan.tif")
  icenov <- raster("../source data/icenov.tif")

pij<- projectRaster(icejan, raster(e, crs = prj, res = 1e4))
pin<- projectRaster(icenov, raster(e, crs = prj, res = 1e4))

# fronts
library(orsifronts)
ofp<- spTransform(orsifronts, CRS(prj))

# grat lines
xx <- c(0,30, 60, 90, 120,150,180); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)

# geographic labels
glabs <- data.frame(
  lon = c(74, 76, 63, 78, 78, 75, 77, 77, 70, 76.5, 78),
  lat = c(-49, -53, -70, -57.5, -58.15, -68, -59, -59.65, -57, -56.5, -65.345),
  lab= c("Kerguelen Is.","Heard Is.","Antarctica","Kerguelen","Plateau","Prydz Bay","Banzare","Bank","Elan Bank","Fawn Trough","Princess Elizabeth\nTrough")
)
glabs <- ll2prj(glabs)

keepOnlyMostComplexLine <- function(x) {
  for (iObj in seq_len(nrow(x))) {
    if (inherits(x, "SpatialLinesDataFrame")) {
      wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x)
        nrow(x@coords)))
      x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
    }
    
  }
  x
}


pdf("KPS_EA_fig1_map.pdf", width=4.5, height=4.5)
op<- par(mar=rep(0,4), oma=rep(0.5,4))
plot(wcp, border=NA)

add_bathy() # filled bathy
plot(cbct, col="grey", add=T) # bathy contours
# ice and fronts
plot(ofp, add = TRUE, col="#053061", lty=3, lwd=2)
plot(rasterToContour(pij, lev = 15),add = TRUE, lty=1, col="blue", lwd=1.5)
plot(keepOnlyMostComplexLine(rasterToContour(pin, lev = 15)),add = TRUE, lty=1, col="purple", lwd=1.5)

plot(wp, add=T, col="darkgrey", border=F)
plot(graticule(lons = xx, lats = yy,  proj = prj), add=T, lty=2, col="gray40")
points(ktr, type="l", lwd=1)
# can add large points for midoc stations by un-commenting line below
#points(kos, pch=19, col="gray20", cex=0.6)

# points(km, pch=19, col="green") # all placed midoc went in water
# points(km[km$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")==F,], pch=19, col="green") # things where
points(km, pch=21, bg=(km$DNC.col), cex=1.8, col=km$pcol)
# TeachingDemos::shadowtext(coordinates(km), lab=as.character(km$midoc.n), cex=.5, col=km$pcol, bg="white")
text(coordinates(km), lab=as.character(km$midoc.n), cex=.5)
# can use bg=grDevices::adjustcolor(km$DNC.col, alpha=.6)

g1labs <- graticule_labels(lons=c(150, 120,90,60), xline=180, yline=-55, proj=projection(prj))
TeachingDemos::shadowtext(coordinates(g1labs[g1labs$islon, ]), lab=parse(text=g1labs$lab[g1labs$islon]), cex=0.8, col="gray30", bg="white")
g2labs <- graticule_labels(lats=c(-40, -50,-60,-70), , xline=95, yline=-50, proj=projection(prj))
TeachingDemos::shadowtext(coordinates(g2labs[!g2labs$islon, ]), lab=parse(text=g2labs$lab[!g2labs$islon]), pos=3, cex=0.8,col="gray30", , bg="white")


TeachingDemos::shadowtext(coordinates(glabs[1:9,]), lab=glabs$lab[1:9], family="Times", cex=0.8, col="black", bg="white")

TeachingDemos::shadowtext(coordinates(glabs[10:11,]), lab=glabs$lab[10:11], family="Times", srt=25, cex=0.6, col="black", bg="white")

# add a key
## could do it by plotting points... but may be difficult to get aligned vertically with projection
ll <- data.frame(lat=c(-68,-69,-70,-71), lon= rep(90,4), pcol=c("yellow","orange","dark blue","violet"))
ll<- ll2prj(ll)

# hack
xs <- rep(ll$lon[1]+400000,4)
points(x=xs, y=ll$lat-60000, bg=as.character(ll$pcol), pch=21, cex=2)
text(xs, y=ll$lat-60000, labels=c("day", "sunset", "night", "sunrise"), pos=4, family="Times", cex=0.8)


box()

par(op)
dev.off()
