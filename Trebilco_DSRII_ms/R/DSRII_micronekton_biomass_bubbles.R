# biomass bubble plots for DSRII manuscript

library(tidyverse)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/Trebilco_DSRII_ms")
setwd(d)

env <-readRDS("../derived data/midoc_stations_envdata.rda")
km  <- readRDS("../derived data/midoc_stations_checked.rds")

# also surfaces if useful, extracted by mark for habitat modelling
# env2<-readRDS("/Users/rowan/GitHub/K_axis_midoc/fish_habitat_modelling/env_data.rds")

stnbm <- readRDS("DSRII_station_biomass_abundance_data.RDS")
lyrbm <- readRDS("DSRII_station_strata_biomass_abundance_data.RDS")

# join biomass and env
bmenv <- km %>% select(midoc.stn,start_time,end_time,lat_start,lon_start) %>% inner_join(lyrbm) %>% mutate(lyr_dpth = ifelse(d.strat=="epipeliagic", 100, (ifelse(d.strat=="upper mesopelagic",400,800))))

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