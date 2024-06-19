# simple generic map for k-axis stations

# raadtools can be used for sourcing environmental data directly rather than loading from local files, but you have to be on the aad network (or using an raadtools rstudio server)
# library(raadtools)

# remaining packages should all be available on CRAN
install.packages("graticule")
install.packages("proj4")
install.packages("raster")
install.packages("rworldxtra")
install.packages("remotes")
install.packages("rworldmap")
install.packages("rgeos")
install.packages("rgdal")
#ZS:install.packages("rgdal", type = "source")
#ZS: remotes::install_github("r-spatial/rgdal")

library(tidyverse)
library(graticule)
library(raster)
library(rworldxtra)
data(countriesHigh)
 library(rworldmap)
# data(countriesHigh)
library(sp)
library(rgeos)

library(rgdal)
library(dplyr)
library(readr)
#remotes::install_github("rspatial/terra")
#install.packages('terra', repos='https://rspatial.r-universe.dev')

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

# projection
prj<- "+proj=laea +lat_0=-60 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0" #ZS: defines a Lambert Azimuthal Equal Area projection centered at 60 degrees South latitude and 75 degrees East longitude, using the WGS84 datum and ellipsoid, with no datum transformation applied

ll2prj <- function(dat, proj=prj, loncol="lon", latcol="lat"){
  coordinates(dat) <- c(loncol, latcol)
  projection(dat) <- "+proj=longlat +datum=WGS84"
  dat <- spTransform(dat, CRS(prj))
  dat
} #ZS: transforms spatial data from latitude-longitude coordinate system to a specified projection 

# plot extent
library(remotes)
#remotes::install_github("rspatial/terra")

ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40) # extent for zoomed map
ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

# cropped and full versions of world map
# cropped version is used to make a suitable extent for plotting, full is over-plotted so that the continents don't  show the "wedge" defined by the extent
data(package = "rworldmap")
data(countriesHigh) 
wc <- crop(countriesHigh, ras.ext) #ZS: cropping countries high to the extent for zoomed map 
wcp <-spTransform(wc, CRS(prj))  #ZS: cropped map of outlined countries
wp  <- spTransform(crop(countriesHigh, ras.ext2), CRS(prj)) #ZS: full map 


# K-Axis stations
kos <- readRDS("../source data/k_axis_oceanog_summ.Rda")
kos <- ll2prj(kos, loncol="longitude", latcol="latidue")

ktr <- readRDS("../derived data/nav_reduced.rds")
# colnames(ktr) <- c("wp","lat","lon","wp.grp")
ktr <- ll2prj(ktr, loncol="LONGITUDE", latcol="LATITUDE")

km <- readRDS("../derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7)) #ZS: getting individual midoc stations 
tmp <- read_csv("../source data/midoc_stations_zones.csv") #ZS: adding midoc stations zones to km 
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("../source data/midoc_crepuscular.csv")) 
km <- inner_join(km, tmp); rm(tmp)
  

#ZS: Tmin_depth
  # ZS: Merge Tmin_depth data into km
  file_path <- "../derived data/midoc_stations_envdata.rda"
  md <- readRDS(file_path)

  km <- merge(km, md[, c("midoc.stn", "Tmin_depth")], by = "midoc.stn")  #change this to inner join 
  num_colors <- 100  
  breaks <- seq(min(km$Tmin_depth), max(km$Tmin_depth), length.out = num_colors)
  color_palette <- colorRampPalette(c("lightyellow","yellow","orange","red" ,"darkred"))  # Adjust the colors as desired
  km$Tmin_depth_col <- color_palette(num_colors)[findInterval(km$Tmin_depth, breaks)]
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
  cbc   <- rasterToContour(cbathy, levels=c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)) #bathymetry contours
  cbct  <- spTransform(cbc, CRS(prj))
  plot(cbct)
  # for filled
  e <- extent(-5411853 , 6235554, -1577161,  1358628) #ZS: spatial coverage of the raster grid (xmin,xmax,ymin,ymax)
  pbathy <- projectRaster(cbathy, raster(e, crs = prj, res = 1e4))


# for filled bathy
add_bathy <- function(cols="blues")  {
  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
  if(cols == "greys" ){plotcols <- gray.colors(9, alpha=0.6)}
  if(cols == "blues" ){plotcols <- c(rev(RColorBrewer::brewer.pal(9,"Blues")))}
  image(as(pbathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
}
  #ZS: image : function to plot raster data, add = true : raster layer should be overlayed to existing plot

# ice
  # reding in with raadtools if loaded
  # icejan <- readice("2016-01-15")
  # icenov <- readice("2015-11-15")
  # or if the files are present locally as raster tifs
  icejan <- raster("../source data/icejan.tif")
  icenov <- raster("../source data/icenov.tif")

  
pij<- projectRaster(icejan, raster(e, crs = prj, res = 1e4)) #creating a new rasterlayer
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
    #ZS: COME BACK TO- need to understand 
    
  }
  x
}


pdf("KPS_EA_fig1_map_Tmin_depth.pdf", width=4.5, height=4.5)
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
points(km, pch=21, bg=(km$Tmin_depth_col), cex=1.8, col=km$pcol)
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
#ll <- data.frame(lat=c(-67,-68,-69,-70,-71,-72), lon= rep(90,6), pcol=c("blue","red","yellow","green","dark blue", "orange"))
#ll<- ll2prj(ll)

# hack
#xs <- rep(ll$lon[1]+400000,6)
#points(x=xs, y=ll$lat-60000, bg=as.character(ll$pcol), pch=21, cex=2)
#text(xs, y=ll$lat-60000, labels=c("Southern", "Subpolar", "Antarctic ACC", "Subpolar - ASF","Antarctic KP", "Antarctic FTJ"), pos=4, family="Times", cex=0.8)
 
 #ZS: Tmin_depth key boxes
 # legend_data <- data.frame(
   # Tmin_depth = seq(min(km$Tmin_depth), max(km$Tmin_depth), length.out = 10),
   #color = palette(10))
 # legend("bottomleft", inset = 0.02,
       #legend = round(legend_data$Tmin_depth, 1),
      # fill = legend_data$color,
      # bty = "n", 
      # title = "Tmin_depth",
      # cex = 0.7)
  
  
  
  #legend_data <- data.frame(
   # Tmin_depth = seq(min(km$Tmin_depth), max(km$Tmin_depth), length.out = 100),  # Adjust the number of legend entries as desired
    #color = color_palette(100)  # Adjust the number of legend entries as desired
  #)
  
  # Specify the position of the legend

  #legend_x <- 0.9  # Adjust as needed
  #legend_y <- 0.1  # Adjust as needed
  
  # Specify the values for the legend
  #legend_values <- seq(min(km$Tmin_depth), max(km$Tmin_depth), length.out = 100)
  
  
  # Plot legend
  #image.plot(z = matrix(1:100, nrow = 1), col = palette_colors, axes = FALSE, 
            # xlab = "", ylab = "", main = "Tmin_depth Color Legend", legend.only = TRUE, 
            # horizontal = FALSE, legend.width = 0.4, legend.shrink = 0.3, 
            # legend.loc = c(legend_x, legend_y))
  
  
  box()
par(op)
dev.off()


