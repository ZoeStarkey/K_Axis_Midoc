# 01_get_kax_fish_data.R
# script to bring in k-axis fish data specific to habitat modelling

library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/fish_habitat_modelling")
setwd(d)

fbm <- readRDS("../derived data/codend_fish_biomass.rds")
ce <- readRDS("../derived data/midoc_cod_ends_checked.rds")

#
# 1) Full water-column biomass and abundance (sum across cod-ends 2:6)
#

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


# quick look at depths to see how good cod-ends are for grouping, or whether max/mid depth should be used
p1<- ggplot(data=ce, aes(group=CE, y=-min.dep, x=CE)) + geom_boxplot() 
p2<- ggplot(data=ce, aes(group=CE, y=-mid.dep, x=CE)) + geom_boxplot()
p3<- ggplot(data=ce, aes(group=CE, y=-max.dep, x=CE)) + geom_boxplot()

ggpubr::ggarange(c(p1,p2,p3), ncol=1)

# because there are hauls where the the max depth doesn't align well with the cod-end number; better to re-assign depth bins manually

# double check of the problem midoc stations to bear in mind

#
# 2) Biomass and abundance for epipelagic (0-200 m)
#

# biomasses below need to be in densities
	swept.epi <- ce %>% filter(CE==6) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))
	# note that a hard cut-off of 200m max depth limits this to only 2 stations
	# the mean for max depth of CE6 is 207.5, and max is 216 - ok to just retain all.

	# biomass for Kreffichthys type myctos
	kbm.epi <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp"), cod.end == "6") %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.epi) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3) %>% arrange(midoc.stn)
	kbm.epi[is.na(kbm.epi$bm_m3),]$bm_m3<- 0
	kbm.epi[is.na(kbm.epi$n_m3),]$n_m3<- 0
	saveRDS(kbm.epi, "kreff_grp_bm_epi.RDS")

	# biomass for other myctos (primarily electrona and gymno)
	egbm.epi <- fbm %>% filter(fish.grp%in%c("Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid"), cod.end == "6") %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.epi) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3) %>% arrange(midoc.stn)
	egbm.epi[is.na(egbm.epi$bm_m3),]$bm_m3<- 0
	egbm.epi[is.na(egbm.epi$n_m3),]$n_m3<- 0
	saveRDS(egbm.epi, "ele_gymno_grp_bm_epi.RDS")

	# biomass for bathylagiids
	bbm.epi <- fbm %>% filter(fish.grp%in%c("Bathylagiids"), cod.end == "6") %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.epi) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3) %>% arrange(midoc.stn)
	bbm.epi[is.na(bbm.epi$bm_m3),]$bm_m3<- 0
	bbm.epi[is.na(bbm.epi$n_m3),]$n_m3<- 0
	saveRDS(bbm.epi, "bathy_grp_bm_epi.RDS")


#
# 3) Biomass and abundance for mesopelagic (and upper bathypelagic) (200-600 m)
#

# biomasses below need to be in densities
	swept.meso <- ce %>% filter(CE%in%c(4:5)) %>% filter(max.dep<650) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))
	# code end classifications for depth are OK, apart from MIDOC02, where CE4 went to 948m

	# biomass for Kreffichthys type myctos
	kbm.meso <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp"), cod.end %in% c(as.character(2:5))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.meso) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	kbm.meso[is.na(kbm.meso$bm_m3),]$bm_m3<- 0
	kbm.meso[is.na(kbm.meso$n_m3),]$n_m3<- 0
	saveRDS(kbm.meso, "kreff_grp_bm_meso.RDS")

	# biomass for other myctos (primarily electrona and gymno)
	egbm.meso <- fbm %>% filter(fish.grp%in%c("Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid"), cod.end %in% c(as.character(2:5))) %>%  filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.meso) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	
	egbm.meso[is.na(egbm.meso$bm_m3),]$bm_m3<- 0
	egbm.meso[is.na(egbm.meso$n_m3),]$n_m3<- 0
	saveRDS(egbm.meso, "ele_gymno_grp_bm_meso.RDS")

	# biomass for bathylagiids
	bbm.meso <- fbm %>% filter(fish.grp%in%c("Bathylagiids"), cod.end %in% c(as.character(2:5))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.meso) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	bbm.meso[is.na(bbm.meso$bm_m3),]$bm_m3<- 0
	bbm.meso[is.na(bbm.meso$n_m3),]$n_m3<- 0
	saveRDS(bbm.meso, "bathy_grp_bm_meso.RDS")


#
# 4) Biomass and abundance for upper bathypelagic (600-1000 m)
#
swept.ubathy <- ce %>% filter(CE%in%c(2:3)) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))

	# biomass for Kreffichthys type myctos
	kbm.ubathy <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp"), cod.end %in% c(as.character(2:3))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.ubathy) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	kbm.ubathy[is.na(kbm.ubathy$bm_m3),]$bm_m3<- 0
	kbm.ubathy[is.na(kbm.ubathy$n_m3),]$n_m3<- 0
	saveRDS(kbm.ubathy, "kreff_grp_bm_ubathy.RDS")

	# biomass for other myctos (primarily electrona and gymno)
	egbm.ubathy <- fbm %>% filter(fish.grp%in%c("Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid"), cod.end %in% c(as.character(2:3))) %>%  filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.ubathy) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	
	egbm.ubathy[is.na(egbm.ubathy$bm_m3),]$bm_m3<- 0
	egbm.ubathy[is.na(egbm.ubathy$n_m3),]$n_m3<- 0
	saveRDS(egbm.ubathy, "ele_gymno_grp_bm_ubathy.RDS")

	# biomass for bathylagiids
	bbm.ubathy <- fbm %>% filter(fish.grp%in%c("Bathylagiids"), cod.end %in% c(as.character(2:3))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.ubathy) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	bbm.ubathy[is.na(bbm.ubathy$bm_m3),]$bm_m3<- 0
	bbm.ubathy[is.na(bbm.ubathy$n_m3),]$n_m3<- 0
	saveRDS(bbm.ubathy, "bathy_grp_bm_ubathy.RDS")