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

env$T1<- ifelse(env$midoc.n%in%c(1:6),T,F)
env$T2<- ifelse(env$midoc.n%in%c(7:14),T,F)
env$T3<- ifelse(env$midoc.n%in%c(15:23),T,F)
env$T4<- ifelse(env$midoc.n%in%c(23:27),T,F)
env$T5<- ifelse(env$midoc.n%in%c(27:31),T,F)
env$T6<- ifelse(env$midoc.n%in%c(31:36),T,F)
env$T7<- ifelse(env$midoc.n%in%c(36:40),T,F)

# join biomass and env
bmenv <- km %>% select(midoc.stn,start_time,end_time,lat_start,lon_start) %>% inner_join(lyrbm) %>% mutate(lyr_dpth = ifelse(d.strat=="epipelagic", 100, (ifelse(d.strat=="upper mesopelagic",400,800)))) %>% filter(tax.grp!="NA")

bmenv$tax.grp2 <- bmenv$tax.grp
bmenv$tax.grp2[grep("mixed", bmenv$tax.grp2)] <- "other"

bmenv <- env %>% select(midoc.stn, zone, zone.notes) %>% data.frame() %>% inner_join(bmenv)
# bubbles for biomass plots
	# TOD/date (x, whole voyage) vs depth (y) (for comparison with swarms acoustic plots)
	# 1 plot for each taxon
	
	ggplot(bmenv, aes(x=start_time, y=-lyr_dpth, size=bm_m3, colour=zone)) + geom_point() + facet_grid(tax.grp2~.) 
	
	# version with day/night
	ggplot(bmenv, aes(x=start_time, y=-lyr_dpth, size=bm_m3, colour=zone)) + geom_point() + facet_grid(tax.grp2~.)

	# Depth (y) vs Latitude (x) (for comparison with Salinity DOTS plots)

	ggplot(bmenv, aes(x=lat_start, y=-lyr_dpth, size=bm_m3, colour=zone)) + geom_jitter(height=30) + facet_grid(tax.grp2~.)

# TODO: alternative presentation with biomass (y) ~ latitude; faceted by layer; 1 plot for each major taxon.

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


# Environmental predictor plots and models:
	# add solar angle
	# dates as POSIXct
	tmp <- data.frame(date=env$start_time, lat=env$lat_start, lon=env$lon_start)
	tmp <- suncalc::getSunlightPosition(data=tmp, keep="altitude")
	env$solar.ele <- tmp$altitude; rm(tmp)
	# catch for each taxon vs environmental predictors
		# quick check of correspondence between solar angle and classification
	ggplot(env,aes(y=solar.ele, x=DNC.visual, label=midoc.n)) +geom_text()
		# 11 and 13 are outliers with v. low solar angle 
		# NOT CLEAR WHATS GOING ON WITH THESE, SO EXCLUDE FROM ANALYSES WITH SOLAR ANGLE


### ADAPTING CODE FROM HERE DOWN FROM PREVIOUS CODE FOR EXTENDED ABSTRACT

# biomass vs predictors for each depth layer (major taxa: fish; cnidarians)
	# not squids, as covered in Lin papers
	# not krill - covered in other papers

	# predictors to consider
	# Tmin depth, chl_RS, chl_insitu, solar elevation
	# maybe also SSH and bathymetric gradient (from habitat modelling)
	# not distance to ice or ice edge or day since melt; as strongly correlated with Tmin dpth; 



# acoustics plots and models
	# total NASC vs catch
		# 1 biomass
		# 2 abundance
		# Scatter plots first; probably linear models; may include depth and time of day
	
	# BIOMASS (ALSO ADAPT CODE FOR ABUNDANCE)
	bm.tax <- readRDS("./derived data/codend_taxa_biomass.rds")
	bm.ac <- mda %>% ungroup() %>% select(sum_NASC, midoc.stn, cod.end) %>% inner_join(bm.tax) %>%
	inner_join(md)

	p.d<- bm.ac %>% filter(cod.end %in% as.character(c(2:6)) & midoc.stn%in%naf==F) %>%
	filter(tax.grp%in%"NA"==F) %>% inner_join(ced)


	rsqs<- p.d %>% filter(tax.grp %in% c("cephalopods","cnidarians","fish","krill","salps")) %>%
 	group_by(tax.grp, depth) %>% do(fit=lm(bm_g_m3 ~ sum_NASC, data=.)) %>% summarize(depth=depth,tax.grp=tax.grp, rsq=round(summary(fit)$r.squared, digits=2))

	p.d %>% filter(tax.grp %in% c("cephalopods","cnidarians","fish","krill","salps")) %>%
	ggplot(aes(y=bm_g_m3, x=sum_NASC, color=DNC.visual)) +
	geom_point() +
	scale_colour_manual(name="", values=c("yellow2","violet","dark blue","orange"), label=c("day","sunrise","night","sunset")) +
	facet_grid(depth ~ tax.grp) +
	scale_x_log10() + scale_y_log10() +
	geom_smooth(method='lm', aes(y=bm_g_m3, x=sum_NASC), colour="dark grey") +
	geom_text(data=rsqs, mapping = aes(x=1, y=1e-1, label=rsq), color="dark grey", size=4) +
	theme_bw() +
	xlab("total acoustic backscatter (NASC)") + ylab(expression(paste("biomass (",g/m^3,")")))

