# 01_get_kax_fish_data.R
# script to bring in k-axis fish data specific to habitat modelling

library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/fish_habitat_modelling")
setwd(d)

fbm <- readRDS("../derived data/codend_fish_biomass.rds")
ce <- readRDS("../derived data/midoc_cod_ends_checked.rds")

# biomasses below need to be in densities
swept <- ce %>% filter(CE>1) %>% group_by(midoc.stn) %>% summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))

# biomass for Kreffichthys type myctos
kbm <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp"), cod.end%in%c(as.character(2:6))) %>%
			   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% inner_join(swept) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
saveRDS(kbm, "kreff_grp_bm.RDS")

# biomass for other myctos (primarily electrona and gymno)
egbm <- fbm %>% filter(fish.grp%in%c("Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid"), cod.end%in%c(as.character(2:6))) %>%
			   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% inner_join(swept) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
saveRDS(egbm, "ele_gymno_grp_bm.RDS")

# biomass for bathylagiids
bbm <- fbm %>% filter(fish.grp%in%c("Bathylagiids"), cod.end%in%c(as.character(2:6))) %>%
			   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% inner_join(swept) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
saveRDS(bbm, "bathy_grp_bm.RDS")