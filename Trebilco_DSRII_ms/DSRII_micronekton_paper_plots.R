# Plots for DSRII special issue paper on micronekton community structure

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
	library(dplyr)

	# plot extent
	ras.ext   <- raster(xmn=60, xmx=105, ymn=-70, ymx=-40) # extent for zoomed map
	ras.ext2  <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0) # full SO extent to make fill play nicely

	data(countriesHigh)
	wc <- crop(countriesHigh, ras.ext)

	icefile<- "../source data/glaciers/"
	sh  <- readOGR(paste0(icefile,"coastal_ice_features_2010_to_2013/"),"ice_shelves_dataset_309") %>% crop(.,ras.ext)    # newer AAD mapping of major ice 
	ice_bergs <- readOGR(paste0(icefile,"coastal_ice_features_2010_to_2013/"), "icebergs_dataset_309") %>% crop(.,ras.ext)  # newer AAD mapping of major 
	ice_sh <- readOGR(paste0(icefile, "/ice_poly_2003"), "ice_poly_2003") %>% crop(.,ras.ext)  # minor features from original GA 
	# coast <- readOGR(paste0(icefile,"/coast_poly_2003"), "all_coast_poly_2003") %>% crop(.,ras.ext)

	# bathy
	cbathy <- raster("../source data/bathy.tif") %>% crop(., ras.ext)
	add_bathy <- function()  {
	  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
	  plotcols <- gray.colors(9)
	  image(as(bathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
	}


# Plan for analysis:
	# group into same depth strata as used in Walters fish habitat modelling
	# epipelagic; upper and lower mesopelagic
	# this allows inclusion of 
	# also aligns with seapodym etc.


# bubbles for biomass plots
	# TOD/date (x, whole voyage) vs depth (y) (for comparison with swarms acoustic plots)
	# 1 plot for each taxon

	# Depth (y) vs Latitude (x) (for comparison with Salinity DOTS plots)

	# PLUS A VERSION SUPERIMPOSED ON SONOGRAM - one for each voyage leg
		# up to 7 plots (* are priorities)
		# 1: 1--6
		# 2: 7--14
		# 3: 15--23*
		# 4: 23--27*
		# 5: 27--31
		# 6: 31--36*
		# 7: 36--40*

# NASC vs biomass

# ? Size distributions (resolved by depth)


# Models:
	# catch for each taxon vs environmental predictors
		# shot time, Chl, days since melt, Tmin depth

	# total NASC vs catch
		# 1 biomass
		# 2 abundance
		# Scatter plots first; probably linear models; may include depth and time of day

