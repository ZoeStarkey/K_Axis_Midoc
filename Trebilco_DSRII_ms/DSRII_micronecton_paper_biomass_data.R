# Extraction of midoc biomass data specific to DSRII micronekton overview paper
# Summaries to the same depth strata used in Walters Habitat modelling paper (epipelagic, upper and lower mesopelagic; along with full water column).

library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/fish_habitat_modelling")
setwd(d)

# TODO: what's the equivalent data file to codend_fish_biomass for all taxa?
# fbm <- readRDS("../derived data/codend_fish_biomass.rds")
ce <- readRDS("../derived data/midoc_cod_ends_checked.rds")


#
# 1) Full water-column biomass and abundance (sum across cod-ends 2:6)
#



swept <- ce %>% filter(CE>1) %>% group_by(midoc.stn) %>% summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))


mdbm <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp"), cod.end%in%c(as.character(2:6))) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% inner_join(swept) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	saveRDS(kbm, "kreff_grp_bm.RDS")