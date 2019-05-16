# Extraction of midoc biomass data specific to DSRII micronekton overview paper
# Summaries to the same depth strata used in Walters Habitat modelling paper (epipelagic, upper and lower mesopelagic; along with full water column).

library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/derived data")
setwd(d)


mds   <- readRDS("midoc_stations_checked.rds")
ce <- readRDS("midoc_cod_ends_checked.rds")
ktr <- readRDS("nav_reduced.rds")
ced <- tibble(cod.end=as.character(c(2:6)), depth=seq(900,100,by=-200))
mde  <- readRDS("midoc_stations_envdata.rda")
md.crep<- read_csv("../source data/midoc_crepuscular.csv")
bm.tax <- readRDS("codend_taxa_biomass.rds")

swept <- ce %>% filter(CE>1) %>% group_by(midoc.stn) %>% summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))

# naf <- c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")
# stations where depth strata are not directly comparable
# trial station is still not relevant
# subsetting against this list is is no-longer neccessary, as 

#
# 1) Full water-column biomass and abundance (sum across cod-ends 2:6)
#
bm.stn.tax <- bm.tax %>% filter(cod.end%in%c(as.character(2:6))) %>%
				 group_by(midoc.stn, tax.grp) %>% 
				 mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>%  
				 summarise(bm.g=sum(bm, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% ungroup() %>%
				 inner_join(swept) %>%
				 mutate(stn.bm.m3=bm.g/swept_stn_m3, stn.n.m3=n/swept_stn_m3) %>% filter(tax.grp!="NA")

saveRDS(bm.stn.tax, "../Trebilco_DSRII_ms/DSRII_station_biomass_abundance_data.RDS")

#
# 2) Biomass and abundance for layers
#
	# epipelagic
	swept.epi <- ce %>% filter(CE==6) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))

	txbm.epi <- bm.tax %>% filter(cod.end%in%c(as.character(2:6))) %>%
				 group_by(midoc.stn, tax.grp) %>% 
				 mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% 
				 summarise(bm.g=sum(bm, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>%
				 right_join(swept.epi) %>% 
				 mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3) %>% arrange(midoc.stn) %>% filter(tax.grp!="NA")

	txbm.epi$d.strat <- "epipelagic"

	## EXCLUDE STATION 12 from epipelagic
	
	# upper mesopelagic
	swept.meso <- ce %>% filter(CE%in%c(4:5)) %>% filter(max.dep<650) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))
	# code end classifications for depth are OK, apart from MIDOC02, where CE4 went to 948m
	
	txbm.meso <- bm.tax %>% filter(cod.end %in% c(as.character(2:5))) %>%
				filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				group_by(midoc.stn, tax.grp) %>% 
				mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>%
				summarise(bm.g=sum(bm, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>%
				right_join(swept.meso) %>%
				mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3) %>% arrange(midoc.stn)
	
	txbm.meso$d.strat <- "upper mesopelagic"

	# lower mesopelagic/upper bathypelagic
	swept.ubathy <- ce %>% filter(CE%in%c(2:3)) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))
	
	txbm.ubathy <- bm.tax %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp"), cod.end %in% c(as.character(2:3))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(bm, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.ubathy) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	txbm.ubathy[is.na(txbm.ubathy$bm_m3),]$bm_m3<- 0
	txbm.ubathy[is.na(txbm.ubathy$n_m3),]$n_m3<- 0
	txbm.meso$d.strat <- "upper mesopelagic"

	txbm <- bind_rows(txbm.epi, txbm.meso,txbm.ubathy)

saveRDS(txbm, "../Trebilco_DSRII_ms/DSRII_station_strata_biomass_abundance_data.RDS")

