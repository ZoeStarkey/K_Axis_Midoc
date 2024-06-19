#KPS_EA_models.R
library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc")
setwd(d)
getwd()

# master table with station data and environmental data
md <- readRDS("./derived data/midoc_stations_envdata.rda")
ced <- tibble(cod.end=as.character(c(2:6)), depth=seq(900,100,by=-200))

# acoustic data
mda <- readRDS("./derived data/midoc_acoustic_sums.rda")


# biomass of taxa:
bm.tax <- readRDS("./derived data/codend_taxa_biomass.rds")
bm.ac <- mda %>% ungroup() %>% select(sum_NASC, midoc.stn, cod.end) %>% inner_join(bm.tax) %>%
	inner_join(md)

pdir <- paste0("/Users/", usr, "/GitHub/K_axis_midoc/KPS_symposium_extended_abstract")
# biomass plots
	# first cut with ggplot
	# total biomass
	naf <- c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")
	p.d<- bm.ac %>% filter(cod.end %in% as.character(c(2:6)) & midoc.stn%in%naf==F) %>%
	filter(tax.grp%in%"NA"==F) %>% inner_join(ced)


	# ggplot(data=p.d, aes(x=cod.end, weight=bm, fill=tax.grp, facets=midoc.stn), geom="bar", ylab="biomass") +
	# geom_bar() +
	# theme_bw() +
	# coord_flip() +
	# facet_wrap(~midoc.stn) +
	# theme(axis.text.x = element_text(angle = 90, hjust = 1))
	# # ggsave("biomass_by_coarse_taxonomic_grp190216.pdf")

	# # proportions
	# ggplot(data=p.d, aes(x=cod.end, weight=pbm, fill=tax.grp, facets=midoc.stn), geom="bar", ylab="% biomass") +
	# geom_bar() +
	# theme_bw() +
	# facet_wrap(~midoc.stn) +
	# coord_flip() +
	# theme(axis.text.x = element_text(angle = 90, hjust = 1))

	#ggsave("proportion_biomass_by_coarse_taxonomic_grp190216.pdf")


# NASC vs. biomass
	# for correlation
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
	xlab("total acoustic backscatter (NASC)") + ylab(expression(paste("biomass (",g/m^3,")"))) +
	ggsave(paste0(pdir,"/nasc_biomass_plot.pdf"), height=5.5, width=7)

# Biomass vs. chlorophyll
# remotely sensed
ggplot(p.d %>% filter(tax.grp %in% c("cephalopods","cnidarians","fish","krill","salps")), 
	aes(y=bm_g_m3, x=chl_rs, colour=DNC.visual)) +
	geom_point() +
	scale_x_log10() + scale_y_log10() +
	scale_colour_manual(name="", values=c("yellow","violet","dark blue","orange"), label=c("day","sunrise","night","sunset")) +
	facet_grid(cod.end ~ tax.grp) +
	geom_smooth(method='lm',color="dark grey") +
	theme_bw()

# in situ
ggplot(p.d %>% filter(tax.grp %in% c("cephalopods","cnidarians","fish","krill","salps")), 
	aes(y=bm_g_m3, x=intChl, colour=DNC.visual)) +
	geom_point() +
	scale_x_log10() + scale_y_log10() +
	scale_colour_manual(name="", values=c("yellow","violet","dark blue","orange"), label=c("day","sunrise","night","sunset")) +
	facet_grid(cod.end ~ tax.grp) +
	geom_smooth(method='lm',color="dark grey") +
	theme_bw()

# really interesting: strong relationships with salps at surface and depth for in-situ chlorophyll; some association for fish

## Pairs plots!
	fbm.wide <-  p.d %>% filter(tax.grp=="fish") %>% group_by(midoc.stn) %>% summarise(bm_g_m3=(sum(bm, na.rm=T))/sum(swept_m3, na.rm=T)) %>% inner_join(md) %>% spread(midoc.stn,value=bm_g_m3, fill=0)
	library(GGally)	

	# TODO:
	# GOING ON FROM HERE FOR DSRII paper
	# change name of this file to KPS_EA_cacht_acoustic
	# start new script for DSRII, move env predictor stuff to this new script

	# move select from below to inner-join step above

	# for each taxon, a splom of env predictors vs. biomass
	bm.wide %>% select(days_since_melt, distance_to_ice_m, chl_rs, intChl, Tmin_depth, DNC.visual, fish) %>% ggpairs() 
	bm.wide %>% select(days_since_melt, distance_to_ice_m, chl_rs, intChl, Tmin_depth, cnidarians) %>% ggpairs() 
	bm.wide %>% select(days_since_melt, distance_to_ice_m, chl_rs, intChl, Tmin_depth, krill) %>% ggpairs() 
	bm.wide %>% select(days_since_melt, distance_to_ice_m, chl_rs, intChl, Tmin_depth, salps) %>% ggpairs() 
	bm.wide %>% select(days_since_melt, distance_to_ice_m, chl_rs, intChl, Tmin_depth, cephalopods) %>% ggpairs() 

	# can add colour to select and ., mapping=ggplot2::aes(colour = cod.end) to plot statement but ugly
## fish! models


# biomass of fish groups:
bm.fish <- readRDS("./derived data/codend_fish_biomass.rds")
bm.fish <- acoustic %>% ungroup() %>% select(sum_NASC, midoc.stn, cod.end) %>% inner_join(bm.fish)
bm.fish <- bm.fish %>% inner_join(stn)


#library(lme4)
libraruy(MuMIn)


env.dat <- bm.wide %>% select(midoc.stn, DNC.visual, chl, distance_to_edge_m, days_since_melt, LONGITUDE, LATITUDE, Tmin_depth) %>% distinct(midoc.stn, .keep_all = T)
env.nasc <- bm.tax %>% filter(tax.grp%in%c("fish","cephalopods","cnidarians","krill","salps")) %>%
	ungroup() %>%
	group_by(midoc.stn) %>%
	summarise(sum_NASC=sum(sum_NASC, na.rm=T)) %>%
	inner_join(env.dat)

mod.dat <- bm.tax %>% filter(cod.end%in%"1"==F, tax.grp%in%c("fish","cephalopods","cnidarians","krill","salps")) %>%group_by(midoc.stn, tax.grp) %>% summarise(bm.tax = sum(bm)) %>% spread(key=tax.grp, value=bm.tax, fill=0) %>% inner_join(env.nasc) 

options(na.action = "na.fail")
library(MuMIn)
library(coefplot2)

# nasc vs. biomass of taxa, full water column
m1.sat <- lm(log(sum_NASC) ~ log(fish+1) + log(cephalopods+1) + log(cnidarians+1) + log(krill+1) + log(salps+1)  , data=mod.dat)
m1.d <- dredge(m1.sat)
m1.d
m1.avg<- model.avg(subset(m1.d, delta<=2))
coefplot(m1.sat)

# version of this for only CE5 & 6
mod.dat56 <- bm.tax %>% filter(cod.end%in%"1"==F, tax.grp%in%c("fish","cephalopods","cnidarians","krill","salps", cod.end%in%c("5",6))) %>%group_by(midoc.stn, tax.grp) %>% summarise(bm.tax = sum(bm)) %>% spread(key=tax.grp, value=bm.tax, fill=0) %>% inner_join(env.nasc)

m1a.sat <- lm(log(sum_NASC) ~ log(fish+1) + log(cephalopods+1) + log(cnidarians+1) + log(krill+1) + log(salps+1)  , data=mod.dat56)
m1.d <- dredge(m1.sat)
m1.d
m1.avg<- model.avg(subset(m1.d, delta<=2))



# fish biomass vs env predictors
m2.sat <- lm(log(fish+1) ~ DNC.visual + chl + distance_to_edge_m + days_since_melt + LONGITUDE + LATITUDE + Tmin_depth, data=mod.dat)
m2.d <- dredge(m1.sat)
m2.d
m2.avg<- model.avg(subset(m1.d, delta<=2))

# same as M2, non log
m3.sat <- lm(fish ~ DNC.visual + chl + distance_to_edge_m + days_since_melt + LONGITUDE + LATITUDE + Tmin_depth, data=mod.dat)
coefplot2(m3.sat)
coefplot2(m2.sat)

# standardised predictors
mod.dat2 <- mod.dat %>% select(fish, DNC.visual, chl, distance_to_edge_m, days_since_melt, LONGITUDE, LATITUDE, Tmin_depth)

library(mgcv)
gam1<- gam(fish ~ + s(chl) + s(distance_to_edge_m) + s(Tmin_depth) + s(LATITUDE), data=mod.dat)


mod.dat.CE <- bm.tax %>% filter(cod.end%in%"1"==F, tax.grp%in%c("fish","cephalopods","cnidarians","krill","salps")) %>%group_by(midoc.stn, tax.grp, cod.end) %>% summarise(bm.tax = sum(bm)) %>% spread(key=tax.grp, value=bm.tax, fill=0) %>% inner_join(env.nasc) 

m4.sat <- lm(log(fish +1) ~ DNC.visual*cod.end + chl + distance_to_edge_m + days_since_melt + LONGITUDE + LATITUDE + Tmin_depth, data=mod.dat.CE)

	coefplot2(m4.sat) ## a lot going on here... would need to do partial dependency plots to unpack it=

mod.dat.CE$depth <- ifelse(mod.dat.CE$cod.end%in%"2", 900, ifelse(mod.dat.CE$cod.end%in%"3", 700, ifelse(mod.dat.CE$cod.end%in%"4", 500, ifelse(mod.dat.CE$cod.end%in%"5", 300,100))))

m5.sat <- lm(log(fish +1) ~ DNC.visual*depth + chl + distance_to_edge_m + days_since_melt + LONGITUDE + LATITUDE + Tmin_depth, data=mod.dat.CE)
m5.avg<- model.avg(subset(m5.d, delta<=2))

m5.pd <- cbind(data.frame(coef=coef(m5.avg)), confint(m5.avg))

par(mar=c(2,10,2,1), las=1)
plot(c(-10,10), c(0, nrow(m5.pd)), axes=F, type="n", ylab="")
axis(1)
axis(2, at=1:nrow(m5.pd), labels = row.names(m5.pd))
abline(v=0, lty="dashed")
points(m5.pd$coef, 1:nrow(m5.pd), pch=19)
segments(m5.pd[,2], 1:nrow(m5.pd), m5.pd[,3],1:nrow(m5.pd))