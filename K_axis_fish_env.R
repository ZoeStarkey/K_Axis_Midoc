# Script to line up fish data with various environmental datasets and acoustic data and do some basic plots and models
# 9-Nov-2017
# Rowan Trebilco

library(readr)
library(dplyr)
library(ggplot2)
setwd("/Users/dougt/GitHub/K_axis_midoc")


# station data
stn <- readRDS("./source data/midoc_stations_locations_times.rds")
# match bestley zones
bz <- read_csv("./source data/midoc_stations_zones.csv")

stn$zone <- bz$bestley.zone[match(stn$midoc.stn, bz$midoc.stn)]
stn <- stn %>% arrange(midoc.stn)

# oceanographic summary data
oc <- readRDS("./source data/k_axis_oceanog_summ.Rda")

# linear interpolation of Tmin and mixed layer depths based on time
oc$gmt <- ISOdatetime(2016, oc$month, oc$day, 0, 0, 0, tz = "GMT")
stn$Tmin_value <- approxfun(oc$gmt, oc$Tmin_value, rule = 2)(stn$datetime)
stn$Tmin_depth <- approxfun(oc$gmt, oc$Tmin_depth, rule = 2)(stn$datetime)
stn$SML <- approxfun(oc$gmt, oc$SML_preferred_estimate, rule = 2)(stn$datetime)

# day/night/crepuscular classifications
dnc <- read_csv("./source data/midoc_crepuscular.csv")

# acoustic data
acoustic <- readRDS("./derived data/midoc_acoustic_sums.rda")

# chlorophyll and ice
chl_ice <- read_rds("./derived data/k-axis-midoc-satdata-extraction_2017-11-09.rds")

# biomass of taxa:
bm.tax <- readRDS("./derived data/codend_taxa_biomass.rds")
bm.tax <- acoustic %>% ungroup() %>% select(sum_NASC, midoc.stn, cod.end) %>% inner_join(bm.tax) %>% 
	inner_join(stn)
bm.tax <- chl_ice %>% select(midoc.stn, days_since_melt, distance_to_ice_m, distance_to_edge_m, sea_ice_conc, chl) %>% inner_join(bm.tax)
bm.tax<- dnc %>% select(midoc.stn, DNC.visual) %>% inner_join(bm.tax)

## TODO: check that the filter is actually working for station!!!!!

# biomass plots
	# first cut with ggplot
	# total biomass
	p.d<- bm.tax %>% filter(cod.end%in%"front.of.net"==F & midoc.stn%in%c("TRIAL","2","8","10","12","13","33")==F) %>%
	filter(tax.grp%in%"NA"==F)
	ggplot(data=p.d, aes(x=cod.end, weight=bm, fill=tax.grp, facets=midoc.stn), geom="bar", ylab="biomass") +
	geom_bar() +
	theme_bw() +
	coord_flip() +
	facet_wrap(~midoc.stn)
	# ggsave("biomass_by_coarse_taxonomic_grp190216.pdf")

	# proportions
	ggplot(data=p.d, aes(x=cod.end, weight=pbm, fill=tax.grp, facets=midoc.stn), geom="bar", ylab="% biomass") +
	geom_bar() +
	theme_bw() +
	facet_wrap(~midoc.stn) +
	coord_flip()
	#ggsave("proportion_biomass_by_coarse_taxonomic_grp190216.pdf")


# NASC vs. biomass
ggplot(p.d %>% filter(tax.grp %in% c("cephalopods","cnidarians","fish","krill","salps")), 
	aes(y=bm, x=sum_NASC, color=DNC.visual)) +
	geom_point() +
	facet_grid(cod.end ~ tax.grp) +
	scale_x_log10() + scale_y_log10() +
	geom_smooth(method='lm', aes(color=NA)) +
	theme_bw()

# Biomass vs. chlorophyll
ggplot(p.d %>% filter(tax.grp %in% c("cephalopods","cnidarians","fish","krill","salps"),bm<1e5), 
	aes(y=bm, x=chl, colour=DNC.visual)) +
	geom_point() +
	facet_grid(cod.end ~ tax.grp) +
#	scale_x_log10() + scale_y_log10() +
	geom_smooth(method='lm',aes(color=NA)) +
	theme_bw()

## Pairs plots!
	bm.wide <- bm.tax %>% spread(key=tax.grp, value=bm, fill=0)
	library(ggally)
	
	# different taxa



	# for each taxon, a splom of env predictors vs. biomass
	bm.wide %>% select(days_since_melt, distance_to_ice_m, chl, Tmin_depth, fish) %>% ggpairs() 
	# can add colour to select and ., mapping=ggplot2::aes(colour = cod.end) to plot statement but ugly
## fish! models


# biomass of fish groups:
bm.fish <- readRDS("./derived data/codend_fish_biomass.rds")
bm.fish <- acoustic %>% ungroup() %>% select(sum_NASC, midoc.stn, cod.end) %>% inner_join(bm.fish)
bm.fish <- bm.fish %>% inner_join(stn)

# map layer data
	# bathy
	bathy <- raster("./source data/bathy.tif")
  # lines
  bc   <- rasterToContour(bathy, levels=c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0))
  bc <- fortify(bc, as="lonlat", grp="grp")
  ggplot(bc, aes(long, lat, group = group)) +geom_path()

  cbct  <- spTransform(cbc, CRS(prj))
	





