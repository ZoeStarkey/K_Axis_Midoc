# for the global inset panel
#Load world map and re-project to polar centric view.
data(countriesLow, package="rworldmap")
raster.map <- raster(xmn=-180, xmx=180, ymn=-90, ymx=-20)
mp <- crop(countriesLow, extent(raster.map)+c(0,0,0,+0.5),snap="out")
pprj <- "+proj=laea +lat_0=-60 +lon_0=90 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"
w <- spTransform(mp, CRS(pprj))

#Option to add graticule lines to map.

xx <- c(0,90,180,270,360); yy <- c(-80,-60,-40,-20)
g3 <- graticule(xx,yy,proj=CRS(pprj))
g4 <- graticule(xx,-20,proj=CRS(pprj))
g3labs1 <- graticule_labels(lons=180,xline=180,yline=-15,proj=CRS(pprj))
g3labs2 <- graticule_labels(lons=0,xline=180,yline=-15,proj=CRS(pprj))

#Function to add longitudinal labels
pltg <- function() {
  p <- par(xpd=NA)
  text(coordinates(g3labs1[g3labs1$islon, ]), lab=parse(text=g3labs1$lab[g3labs1$islon]), adj=c(0.1,0.5), cex=0.8)
  text(coordinates(g3labs2[g3labs2$islon, ]), lab=parse(text=g3labs2$lab[g3labs2$islon]), adj=c(0.5,0.5), cex=0.8)
 par(p)
}

#Create a marker for Kerguelen Island (for reference).
colony <- as.data.frame(t(c(69.5,-49.25)))
colnames(colony) <- c("lon","lat")
coordinates(colony) <- c("lon","lat")
projection(colony) <- "+proj=longlat +datum=WGS84"
macca <- spTransform(colony,CRS(pprj))

