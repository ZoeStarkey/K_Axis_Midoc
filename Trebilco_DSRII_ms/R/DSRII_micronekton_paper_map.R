# Map for DSRII special issue paper on micronekton community structure

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/Trebilco_DSRII_ms")
setwd(d)

# 1 updated map
	# group as day or night as per Walters habitat modelling
	# show Bestley frontal features
	# also show max chl?? ASK MIKE FOR THIS IF DECIDE TO SHOW

	library(graticule)
	library(raster)
	library(rworldxtra)
	data(countriesHigh)
	# library(rworldmap)
	# data(countriesHigh)
	library(sp)
	library(rgeos)
	library(rgdal)
	library(tidyverse)
	library(mapview)
	library(viridis)

	# plot extent
	ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-45) # extent for zoomed map
	ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

	data(countriesHigh)
	wc <- crop(countriesHigh, ras.ext)

	icefile<- "../source data/glaciers/"
	sh  <- readOGR(paste0(icefile,"coastal_ice_features_2010_to_2013/"),"ice_shelves_dataset_309") %>% crop(.,ras.ext)    # newer AAD mapping of major ice 
	ice_bergs <- readOGR(paste0(icefile,"coastal_ice_features_2010_to_2013/"), "icebergs_dataset_309") %>% crop(.,ras.ext)  # newer AAD mapping of major 
	ice_sh <- readOGR(paste0(icefile, "/ice_poly_2003"), "ice_poly_2003") %>% crop(.,ras.ext)  # minor features from original GA 
	# coast <- readOGR(paste0(icefile,"/coast_poly_2003"), "all_coast_poly_2003") %>% crop(.,ras.ext)
	icejan <- raster("../source data/icejan.tif") %>% projectRaster(ras.ext)
  	icenov <- raster("../source data/icenov.tif") %>% projectRaster(ras.ext)

  	# bathy
	cbathy <- raster("../source data/bathy.tif") %>% crop(., ras.ext)
	add_bathy <- function(bathy)  {
	  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
	  plotcols <- gray.colors(9)
	  image(as(bathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
	}

	# bestley fronts
	load("../source data/KAXIS_vectors/KAXIS_FEATURESpoly_five.RData")

	# stations
	ktr <- readRDS("../derived data/nav_reduced.rds")
	km <- readRDS("../derived data/midoc_stations_checked.rds")
	km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
	tmp <- read_csv("../source data/midoc_stations_zones.csv")
	km <- inner_join(km, tmp); rm(tmp)
	tmp <- read_csv(("../source data/midoc_crepuscular.csv"))
	km <- inner_join(km, tmp); rm(tmp)
	km <- km[km$midoc.stn%in%unique(lyrbm$midoc.stn),]
	naf <- c("MIDOC08","MIDOC10","MIDOC12")
	dnpal <- viridis(10)[c(4,10)]

	## TODO: colour code these with different outline or transparency: stations where the entire water column wasn't sampled
	# MIDOC01: volume swept may not be directly comparible with other stations as net was skewed (cables sensors not working)
	# MIDOC02: entire water column was sampled, but depth strata not as planned - corrected in processing 
	# MIDOC08: nets did not trigger; all catch in CE1; only relevant for full water-column comparisons
	# MIDOC10: top stratum only at same station as 8
	# MIDOC12: CE5 and 6 combined
	# MIDOC13: to 800m; depths corrected
	# MIDOC33: only to 150 m; NOT RELEVANT AND SHOULD BE EXCLUDED; replaced by 34
	
	km <- km %>% mutate(all.layers=ifelse(midoc.stn%in%naf,F,T)) %>%
			     mutate(day.night=ifelse(DNC.visual=="D","day","night")) %>%
			     mutate(dncol=ifelse(day.night=="day",dnpal[2],dnpal[1])) %>%
			     select(midoc.stn,lat_start,lon_start,mean_grnd_spd, midoc.n, bestley.zone, zone.notes, day.night,all.layers, dncol)
	
	# new map 
	pdf("./figs/DSRII_map.pdf", height=6, width=5)
	par(family="Times", mar= c(4.1, 4.1, .5,.5), oma=c(0,0,0,0))
	plot(c(extent(ras.ext)@xmin,extent(ras.ext)@xmax), c(extent(ras.ext)@ymin,extent(ras.ext)@ymax), type="n", las=1, xlab=expression(paste("Longitude (",degree,"E)")), ylab=expression(paste("Latitude (",degree,"S)")), xaxs="i", yaxs="i", tcl=.4, axes = F )
	axis(1)
	axis(2, at=seq(-40,-70,-5), labels=seq(40,70,5), las=1)
	add_bathy(cbathy)

	# ice
	icols <- RColorBrewer::brewer.pal(9,"Blues")[c(2:4)]
	plot(sh, add=T, col=icols[3], border=F)
	plot(ice_bergs, add=T,col=icols[2], border=F)
	plot(ice_sh, add=T,col=icols[1], border=F)

	# Bestley frontal features
	ctdcols = c(rgb(0.1922, 0.2118, 0.5843), rgb(0.3864, 0.6088, 0.7849), rgb(0.9936, 0.7543, 0.4381))

	#ice
	plot(rasterToContour(icejan, lev = 15),add = TRUE, lty=1, col="blue", lwd=1.5)
	plot(rasterToContour(icenov, lev = 15),add = TRUE, lty=1, col="purple", lwd=1.5)

	# plot(f3$finished$geometry[5], add=T,col="black",lwd=3) # FTJ
	plot(f3$finished$geometry[4], add=T,col=ctdcols[1],lwd=3) # ASF
	# plot(f3$finished$geometry[3], add=T,col=ctdcols[2],lwd=3) # eddy
	plot(f3$finished$geometry[2], add=T,col=ctdcols[3],lwd=3) #saccf
	plot(f3$finished$geometry[1], add=T,col=ctdcols[2],lwd=3,lty=5) #SB

	# basemap
	plot(wc, add=T, border="dark grey",bg="white", col="white")

	# voyage track
	points(ktr, type="l", lwd=1)
	points(km$lon_start,km$lat_start, pch=21, bg=km$dncol, cex=1.8, col=(ifelse(km$midoc.stn%in%naf, "white","black")))
	text(km$lon_start,km$lat_start, lab=as.character(km$midoc.n), cex=.5, col=ifelse(km$day.night=="day", "black","white"))

	# geographic labels
	glabs <- data.frame(
	  lon = c(74, 76, 63, 78, 78, 75, 77, 77, 70, 76.5, 78),
	  lat = c(-49, -53, -70, -57.5, -58.15, -68, -59, -59.65, -57, -56.5, -65.345),
	  lab= c("Kerguelen Is.","Heard Is.","Antarctica","Kerguelen","Plateau","Prydz Bay","Banzare","Bank","Elan Bank","Fawn Trough","Princess Elizabeth\nTrough")
	)

TeachingDemos::shadowtext(glabs$lon[1:9], glabs$lat[1:9], lab=glabs$lab[1:9], family="Times", cex=0.8, col="black", bg="white")

TeachingDemos::shadowtext(glabs$lon[10:11],glabs$lat[10:11], lab=glabs$lab[10:11], family="Times", srt=25, cex=0.6, col="black", bg="white")
	
	box()
	rect(xleft = 85.5, xright = 102.5, ybottom =-54 ,ytop = -46, col="white", border=F)
	lx <- 87
	ly <- c(-47,-48,-49,-50,-51,-52,-53)
	points(bg=dnpal, x=rep(lx,2), y=ly[1:2], cex=1.8, pch=21)
	segments(x0 =lx-.5, x1=lx+.5, y0=ly[3:7], y1=ly[3:7],col=c(ctdcols[3],ctdcols[2],ctdcols[1], "purple","blue"),lwd=2)
	text(c("day","night", "SACCF","SB","ASF","November ice extent","January ice extent"), x=lx, y=ly,pos=4, family="Times", cex=.8)

	box()

	dev.off()