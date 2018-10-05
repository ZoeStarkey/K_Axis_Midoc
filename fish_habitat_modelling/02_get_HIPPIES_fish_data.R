# 02_get_HIPPIES_fish_data.R
# script to bring in HIPPIES fish data, and to convert to format appropriate for habitat modelling

library(tidyverse)
library(readxl)
library(rworldxtra)
data(countriesHigh)
library(raster)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/fish_habitat_modelling")
setwd(d)

# bathy data and map for looking at where shots were
roi <- c(70, 80, -60, -50) ## specific lat, lon coordinates for region of interest (you will need to change this as you see fit)

wc <- crop(countriesHigh, extent(roi))
wcf <- fortify(wc)
bathy <- raster("../source data/bathy.tif")
cbathy <- crop(bathy, extent(roi))
b.spdf <- as(cbathy, "SpatialPixelsDataFrame")
bdf <- as.data.frame(b.spdf)

# fish table
hfda <- read_csv("../source data/HIPPIES DB/Fish Data.csv")

# haul table
hh <- read_csv("../source data/HIPPIES DB/haul.csv")

# species list table... check if this aligns properly
sp <- read_xlsx("../source data/HIPPIES DB/SpeciesList.xlsx")

# relevant columns of fish data
hfd <- hfda %>% dplyr::select("Index","Serial Number","HaulID","Fish Type","Weight (g)")

# match species to catch data
hfd$sp <- sp$SpeciesNameLong[match(hfd$`Fish Type`, sp$SpeciesID)]

# relevant columns from haul data 
hh <- hh %>% dplyr::select("HaulID","HaulNumber",`Gear Type`,"Fishing Depth (m)","Start Haul Lat","Start Haul Lon","End Haul Lat","End Haul Lon","Tow Distance (NM)","Tow Speed (knots)","Comments")

# plots/checks
	# depth ~ distance, gear type and speed
	hh %>% ggplot(aes(x=`Fishing Depth (m)`, y=`Tow Distance (NM)`, col=as.factor(`Gear Type`), size=`Tow Speed (knots)`)) + geom_point()
	
	# plot locations (starts and ends) on map
	ggplot(bdf) + geom_tile(data=bdf, aes(x=x, y=y, fill=bathy)) + geom_point(data= hh, aes(x=`Start Haul Lon`, y=`Start Haul Lat`, col=as.factor(`Gear Type`)))

hfd <- left_join(hfd, hh)

# restrict to only IYGPT shots
hp_rmt <- hfd %>% filter(`Gear Type`==34)

hmd <- hfd %>% filter(`Gear Type`==35)

# summarise biomass and abundance to densities per volume swept

	# initially station totals
	hmd %>% group_by(HaulID, sp)

	# then to depth strata
	hist(hh$`Fishing Depth (m)`, breaks=100)
	hmd<- hmd %>% mutate(depth.strat = ifelse(`Fishing Depth (m)`<200, 'epi', (ifelse(`Fishing Depth (m)`>600, 'ubathy','meso'))))

	hp_bm_layer <- hmd %>% group_by(HaulID, sp, depth.strat) %>% summarise(bm_sp_tot=sum(`Weight (g)`))
	
	# going forward: check that there's just one unique swept value for each combination of depth strat and haul - a sanity check of number of rows  * distance / first value or something along those lines 
	layer_swept <- hmd %>% group_by(HaulID,depth.strat) %>% summarise(swept=)
	
	# correct to densities
	


	# select just 